//! Terminal that controls the main application flow, includes the main event loop.
use crate::{interpreter::ReplInterpreter, state::AppState, Action};
use copypasta::ClipboardProvider;
use crossterm::event::{Event, KeyCode, KeyModifiers, MouseEventKind};
use std::time::Duration;
use tui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::Style,
    text::Text,
    widgets::{Block, Borders, Paragraph},
    Terminal as TuiTerminal,
};

const SHORTCUTS_HELP: &str = "\
Shortcut help:
* CTRL-C > cancel the current command
* CTRL-D > close down the program (non graceful)
* UP > Scroll commands backwards
* DOWN > Scroll commands forwards

Built-in commands:
* clear > clears the terminal screen
";

/// A TUI application.
pub struct Application<'a, I>
where
    I: ReplInterpreter,
{
    interpreter: I,
    state: AppState<'a>,
}

impl<'a, I> Application<'a, I>
where
    I: ReplInterpreter,
{
    pub fn new(interpreter: I) -> Self {
        let state = AppState::new();
        crossterm::terminal::enable_raw_mode().unwrap();

        Application { interpreter, state }
    }

    /// Start the blocking TUI event/render loop.
    pub fn start_event_loop(&mut self) -> std::io::Result<()> {
        let stdout = std::io::stdout();
        let backend = CrosstermBackend::new(stdout);
        let mut terminal = TuiTerminal::new(backend).unwrap();

        terminal.clear()?;
        let mut first = true;
        loop {
            terminal.draw(|f| {
                let chunks = Self::layout_for_frame(f.size());
                self.state.set_input_box(chunks[0]);
                if first {
                    // initialization actions
                    self.state.initial_line();
                    first = false;
                }

                match crossterm::event::poll(Duration::from_millis(10)) {
                    Ok(true) => {
                        match crossterm::event::read() {
                            Ok(event) => {
                                let action = self
                                    .process_event(event)
                                    .map_or_else(|| Action::None, |x| x);
                                match self.exec_action(action) {
                                    Some(Action::Exit) => {
                                        self.state.terminate = true;
                                    }
                                    Some(Action::Chain(chain)) => {
                                        self.exec_or_break(chain);
                                    }
                                    Some(Action::Newline) => {
                                        self.state.newline();
                                    }
                                    Some(Action::None) | None => {}
                                    _ => self.state.report_error(
                                        "Cannot perform that action in this context.",
                                    ),
                                }
                            }
                            Err(err) => {
                                self.state.print_error(&err);
                                self.state.terminate = true;
                            }
                        };
                    }
                    Ok(false) => {}
                    Err(err) => {
                        self.state.print_error(&err);
                        self.state.terminate = true;
                    }
                }

                let input = Self::draw_input_box(self.state.input_box_frame_content());
                f.render_widget(input, chunks[0]);

                self.state.info_box = chunks[1];
                let output = Self::draw_output_box(self.state.info_box_frame_content());
                f.render_widget(output, chunks[1]);

                self.state.side_effects(f);
            })?;
            if self.state.terminate {
                break;
            }
        }
        terminal.clear()?;
        terminal.flush()?;
        Ok(())
    }

    fn layout_for_frame(area: Rect) -> Vec<Rect> {
        Layout::default()
            .direction(Direction::Vertical)
            .margin(2)
            .constraints([Constraint::Length(10), Constraint::Min(2)].as_ref())
            .split(area)
    }

    fn draw_input_box(text: Text) -> Paragraph {
        Paragraph::new(text)
            .style(Style::default())
            .block(Block::default().borders(Borders::ALL).title(" Input "))
    }

    fn draw_output_box(text: Text) -> Paragraph {
        Paragraph::new(text)
            .style(Style::default())
            .block(Block::default().borders(Borders::ALL).title(" Ouput "))
    }

    fn exec_action<'b>(&'b mut self, action: Action<'a>) -> Option<Action<'a>> {
        match action {
            Action::Chain(chain) => return self.exec_or_break(chain),
            Action::Continue => {
                self.state.cursor.effect_on = false;
            }
            Action::Command(cmd) => {
                self.state.cmd_history.reset_call_stack();
                self.state.cmd_history.push_in_call(cmd.clone());
                self.state.clear_info_box();
                if self.built_cmd_exec(&cmd) {
                    if let Some(action) = self.interpreter.cmd_executor(&cmd) {
                        return Some(
                            self.exec_action(action)
                                .map_or_else(|| Action::Newline, |a| a.compose(Action::Newline)),
                        );
                    }
                }
                if cmd == "clear" {
                    self.state.initial_line();
                } else {
                    self.state.newline();
                }
            }
            Action::Read => {
                self.state.reading = true;
                self.state.newline();
            }
            Action::StopReading => {
                self.state.reading = false;
                self.state.newline();
            }
            Action::Discard => {
                self.state.reading = true;
                self.state.newline();
            }
            Action::Newline => {
                self.state.reading = self.interpreter.is_reading();
                self.state.newline();
            }
            Action::WriteInfoText(text) => self.state.print_text(text),
            Action::WriteInputText(text) => {
                text.chars().for_each(|c| self.state.input_char(c));
            }
            Action::Sleep(val) => std::thread::sleep(Duration::from_millis(val)),
            Action::Exit => return Some(Action::Exit),
            _ => {}
        }
        None
    }

    /// Returns true if the input command it's a builtin command, produces a side-effect otherwise.
    fn built_cmd_exec(&mut self, cmd: &str) -> bool {
        match cmd {
            "clear" => {
                self.state.clear_info_box();
                self.state.clear_input_box();
                false
            }
            "shortcuts" => {
                self.state.clear_info_box();
                self.print_text(SHORTCUTS_HELP);
                false
            }
            _non_executable_cmd => true,
        }
    }

    fn exec_or_break(&mut self, chain: impl Iterator<Item = Action<'a>>) -> Option<Action<'a>> {
        let mut chain = Box::new(chain.into_iter()) as Box<dyn Iterator<Item = _>>;
        loop {
            let mut chained = Vec::new();
            for action in chain.filter_map(|a| self.exec_action(a)) {
                if let Action::Chain(new_chain) = action {
                    chained.extend(new_chain);
                } else if action.exit() {
                    return Some(Action::Exit);
                }
            }
            if chained.is_empty() {
                break;
            }
            chain = Box::new(chained.into_iter());
        }
        None
    }

    fn process_event(&mut self, event: Event) -> Option<Action<'a>> {
        match event {
            Event::Key(key) => {
                let action = match key.code {
                    KeyCode::Char(c) if key.modifiers == KeyModifiers::NONE => {
                        self.state.input_char(c);
                        self.interpreter.digest(c)
                    }
                    KeyCode::Enter => self.interpreter.digest('\n'),
                    KeyCode::Backspace => {
                        if !self.state.cursor.at_start_of_the_line() {
                            self.state.delete();
                            match self.interpreter.delete_last() {
                                Some(Action::Discard) => {
                                    self.state.reading = false;
                                    self.state.cursor.effect_on = true;
                                    return Some(Action::None);
                                }
                                Some(other) => return Some(other),
                                None => {}
                            }
                        }
                        Action::Continue
                    }
                    KeyCode::Esc => Action::Exit,
                    KeyCode::Char(c) if key.modifiers == KeyModifiers::CONTROL => {
                        self.ctrl_action(c)
                    }
                    KeyCode::Up => {
                        return self.show_previous_command();
                    }
                    KeyCode::Down => {
                        return self.show_following_command();
                    }
                    _ => Action::Continue,
                };
                self.exec_action(action)
            }
            Event::Mouse(me) => {
                match me.kind {
                    MouseEventKind::ScrollDown => {}
                    MouseEventKind::ScrollUp => {}
                    _ => {}
                }
                None
            }
            _ => None,
        }
    }

    fn ctrl_action(&mut self, key: char) -> Action<'a> {
        match key {
            'd' => Action::Exit,
            'c' => Action::Discard,
            'v' => {
                // copy from clipboard
                self.state
                    .clipboard
                    .get_contents()
                    .ok()
                    .map(|string| Action::WriteInputText(string))
                    .unwrap_or_else(|| Action::None)
            }
            _ => Action::None,
        }
    }

    /// Prints a text to the output box.
    pub fn print_text<'b, T: Into<Text<'a>>>(&'b mut self, text: T) {
        self.state.print_text(text.into());
    }

    /// Show previous command in the input box.
    fn show_previous_command(&mut self) -> Option<Action<'a>> {
        if self.state.reading {
            None
        } else if let Some(prev_cmd) = self.state.cmd_history.get_previous_call() {
            self.show_input_command(prev_cmd);
            Some(Action::Continue)
        } else {
            None
        }
    }

    /// Show next command in the input box.
    fn show_following_command(&mut self) -> Option<Action<'a>> {
        if self.state.reading {
            None
        } else if let Some(following_cmd) = self.state.cmd_history.get_following_call() {
            self.show_input_command(following_cmd);
            Some(Action::Continue)
        } else {
            self.state.clear_input_line();
            self.interpreter.drop_command();
            self.state.cmd_history.reset_call_stack();
            None
        }
    }

    fn show_input_command(&mut self, cmd: String) {
        self.state.clear_input_line();
        self.interpreter.drop_command();
        cmd.chars().for_each(|c| self.state.input_char(c));
        cmd.chars().for_each(|c| {
            self.interpreter.digest(c);
        })
    }
}
