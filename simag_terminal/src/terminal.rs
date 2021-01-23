//! Terminal that controls the main application flow, includes the main event loop.

use copypasta::{ClipboardContext, ClipboardProvider};
use crossterm::event::{Event, KeyCode, KeyModifiers};
use std::io::Stdout;
use std::iter::Iterator;
use std::time::Duration;
use std::{collections::VecDeque, sync::mpsc::Receiver};
use tui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    style::Style,
    text::{Span, Spans, Text},
    widgets::{Block, Borders, Paragraph},
    Frame, Terminal as TuiTerminal,
};

use crate::{
    cursor::{Cursor, CursorMovement},
    interpreter::ReplInterpreter,
    Action,
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

pub struct Terminal<'a, I>
where
    I: ReplInterpreter,
{
    interpreter: I,
    state: AppState,
    cursor: Cursor<Stdout>,
    events: Receiver<Event>,
    clipboard: ClipboardContext,
    action_queue: Vec<Action<'a>>,
}

impl<'a, I> Terminal<'a, I>
where
    I: ReplInterpreter,
{
    pub fn new(interpreter: I) -> Self {
        // set event handler thread
        let (tx, events) = std::sync::mpsc::channel();
        let _ = {
            let tx = tx.clone();
            std::thread::spawn(move || -> crossterm::Result<()> {
                loop {
                    match crossterm::event::read()? {
                        Event::Key(event) => {
                            tx.send(Event::Key(event));
                            break;
                        }
                        Event::Mouse(event) => {
                            tx.send(Event::Mouse(event));
                        }
                        Event::Resize(width, height) => {
                            println!("New size {}x{}", width, height);
                        }
                    }
                }
                Ok(())
            })
        };

        // write!(
        //     stdout,
        //     "{}{}",
        //     termion::clear::All,
        //     termion::cursor::Goto(1, 1)
        // )
        // .unwrap();

        Terminal {
            interpreter,
            state: AppState::new(),
            events,
            cursor: Cursor::new(),
            clipboard: ClipboardContext::new().unwrap(),
            action_queue: Vec::new(),
        }
    }

    fn draw_frame(_f: &mut Frame<CrosstermBackend<Stdout>>) {
        // self.side_effects();
    }

    pub fn start_event_loop(&mut self) -> std::io::Result<()> {
        let stdout = std::io::stdout();
        let backend = CrosstermBackend::new(stdout);
        let mut terminal = TuiTerminal::new(backend).unwrap();

        // Set up the initial layout and perform any required initialization actions
        // terminal.draw(|f| {
        //     let chunks = Layout::default()
        //         .direction(Direction::Vertical)
        //         .margin(2)
        //         .constraints(
        //             [
        //                 Constraint::Length(1),
        //                 Constraint::Length(3),
        //                 Constraint::Min(1),
        //             ]
        //             .as_ref(),
        //         )
        //         .split(f.size());

        //     self.action_queue.reverse();
        //     while let Some(action) = self.action_queue.pop() {
        //         match action {
        //             Action::Write((val, new_line)) => {}
        //             Action::WriteStr((val, new_line)) => {}
        //             Action::WriteMulti((val, new_line)) => {}
        //             Action::WriteMultiStr((val, new_line)) => {
        //                 let text = Text::from(Span::from(val));
        //                 let help_paragraph = Paragraph::new(text);
        //                 f.render_widget(help_paragraph, chunks[0]);
        //             }
        //             _ => {}
        //         }
        //     }
        // })?;
        terminal.clear()?;

        loop {
            terminal.draw(|f| {
                let chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .margin(2)
                    .constraints(
                        [
                            Constraint::Length(2),
                            Constraint::Length(100),
                            Constraint::Min(1),
                        ]
                        .as_ref(),
                    )
                    .split(f.size());

                let mut text = Text::from(Spans::from("HELLO WORLD!"));
                text.patch_style(Style::default());
                let help_message = Paragraph::new(text);
                f.render_widget(help_message, chunks[0]);

                let input = Paragraph::new(">>> ")
                    .style(Style::default())
                    .block(Block::default().borders(Borders::ALL).title("Console"));
                f.render_widget(input, chunks[1]);

                let output_msg = format!("{:?}", chunks);
                let output = Paragraph::new(output_msg)
                    .style(Style::default())
                    .block(Block::default().borders(Borders::ALL).title("Output"));
                f.render_widget(output, chunks[2]);

                let f = f;
                match self.events.recv_timeout(Duration::from_millis(15)) {
                    Ok(event) => {
                        let action = self
                            .process_event(event)
                            .map_or_else(|| Action::None, |x| x);
                        match self.exec_action(action) {
                            Some(Action::Exit) => self.state.terminate = true,
                            Some(Action::Chain(chain)) => {
                                self.exec_or_break(chain);
                            }
                            Some(Action::None) | None => {}
                            _ => self.report_error("Cannot perform that action in this context."),
                        }
                    }
                    Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {}
                    Err(_) => {
                        self.state.terminate = true;
                    }
                }
                Self::draw_frame(f);
            })?;
            if self.state.terminate {
                break;
            }
        }

        terminal.clear()?;
        terminal.show_cursor()?;

        // Clean up application activity
        // write!(
        //     self.stdout,
        //     "{}{}{}{}",
        //     termion::style::Reset,
        //     termion::clear::All,
        //     termion::cursor::Goto(1, 1),
        //     termion::cursor::Show
        // )
        // .unwrap();
        // self.flush();
        Ok(())
    }

    fn exec_action<'b>(&'b mut self, action: Action<'a>) -> Option<Action<'a>> {
        match action {
            Action::Continue => {
                // let stdout = &mut self.stdout;
                self.cursor.effect_on = false;
                // self.cursor.show(stdout);
            }
            Action::Read => {
                self.state.reading = true;
                self.newline();
            }
            Action::StopReading => {
                self.state.reading = false;
                self.newline();
            }
            Action::Discard => {
                // self.cursor
                //     .action(&mut self.stdout, CursorMovement::MoveRight(1));
                self.newline();
            }
            Action::Command(cmd) => {
                self.state.reset_call_stack();
                self.state.push_in_call(cmd.clone());
                if let Some(cmd) = self.built_cmd_exec(cmd) {
                    // self.cursor
                    //     .action(&mut self.stdout, CursorMovement::MoveDown(1));
                    if let Some(action) = self.interpreter.cmd_executor(cmd) {
                        return self.exec_action(action);
                    }
                }
                self.newline();
            }

            Action::Chain(chain) => return self.exec_or_break(chain),
            Action::Newline => {
                self.check_reading_status();
                self.newline();
            }
            Action::Sleep(val) => std::thread::sleep(Duration::from_millis(val)),
            Action::Exit => return Some(Action::Exit),
            _ => {}
        }
        None
    }

    /// Returns the input command only if it's not a builtin command,
    /// produces a side-effect otherwise.
    fn built_cmd_exec(&mut self, cmd: String) -> Option<String> {
        match cmd.as_str() {
            "clear" => {
                self.clear_terminal();
                None
            }
            "shortcuts" => {
                self.print_multiline(SHORTCUTS_HELP, false);
                None
            }
            _non_executable_cmd => Some(cmd),
        }
    }

    fn exec_or_break(&mut self, mut chain: Vec<Action<'a>>) -> Option<Action<'a>> {
        while !chain.is_empty() {
            let mut chained = Vec::new();
            for action in chain.into_iter().filter_map(|a| self.exec_action(a)) {
                if let Action::Chain(new_chain) = action {
                    chained.extend(new_chain);
                } else if action.exit() {
                    return Some(Action::Exit);
                }
            }
            chain = Vec::with_capacity(chained.len());
            chain.extend(chained);
        }
        None
    }

    fn side_effects(&mut self) {
        if self.cursor.effect_on {
            // let stdout = &mut self.stdout;
            // self.cursor.side_effects(stdout);
        }
    }

    fn process_event(&mut self, event: Event) -> Option<Action<'a>> {
        if let Event::Key(key) = event {
            let action = match key.code {
                KeyCode::Char(c) if key.modifiers == KeyModifiers::NONE => {
                    if c != '\n' {
                        self.print_char(c);
                    }
                    self.interpreter.digest(c)
                }
                KeyCode::Backspace => {
                    if !self.cursor.at_start_of_the_line() {
                        self.delete();
                        match self.interpreter.delete_last() {
                            Some(Action::Discard) => {
                                self.cursor.effect_on = true;
                                self.state.reading = false;
                                return Some(Action::None);
                            }
                            Some(other) => return Some(other),
                            None => {}
                        }
                    }
                    Action::Continue
                }
                KeyCode::Esc => Action::Exit,
                KeyCode::Char(c) if key.modifiers == KeyModifiers::CONTROL => self.ctrl_action(c),
                KeyCode::Up => {
                    return self.print_previous_command();
                }
                KeyCode::Down => {
                    return self.print_following_command();
                }
                _ => Action::Continue,
            };
            self.exec_action(action)
        } else {
            None
        }
    }

    fn ctrl_action(&mut self, key: char) -> Action<'a> {
        match key {
            'd' => Action::Exit,
            'c' => Action::Discard,
            'v' => {
                // copy from clipboard
                self.clipboard
                    .get_contents()
                    .ok()
                    .map(|string| Action::WriteInputText(Text::from(string)))
                    .unwrap_or_else(|| Action::None)
            }
            _ => Action::None,
        }
    }

    fn print_previous_command(&mut self) -> Option<Action<'a>> {
        if self.state.reading {
            None
        } else if let Some(prev_cmd) = self.state.get_previous_call() {
            self.print_command(prev_cmd);
            Some(Action::Continue)
        } else {
            None
        }
    }

    fn print_following_command(&mut self) -> Option<Action<'a>> {
        if self.state.reading {
            None
        } else if let Some(following_cmd) = self.state.get_following_call() {
            self.print_command(following_cmd);
            Some(Action::Continue)
        } else {
            self.clear_line();
            self.interpreter.drop_command();
            self.state.reset_call_stack();
            None
        }
    }

    fn print_command(&mut self, cmd: String) {
        self.interpreter.drop_command();
        self.clear_line();
        self.print_str(&cmd, false);
        for c in cmd.chars() {
            self.interpreter.digest(c);
        }
    }

    fn clear_terminal(&mut self) {
        // write!(
        //     self.stdout,
        //     "{}{}",
        //     termion::clear::BeforeCursor,
        //     termion::cursor::Goto(1, 1)
        // )
        // .unwrap();
        self.cursor.row = 1;
        self.cursor.column = 1;
    }

    pub fn clear_line(&mut self) {
        // write!(
        //     self.stdout,
        //     "{}{}{}>>> {}",
        //     termion::cursor::Goto(1, self.cursor.row),
        //     termion::style::Bold,
        //     termion::clear::AfterCursor,
        //     termion::style::Reset
        // )
        // .unwrap();
        self.flush();
        self.cursor.column = 5;
    }

    fn check_reading_status(&mut self) {
        if self.interpreter.is_reading() {
            self.state.reading = true;
        } else {
            self.state.reading = false;
        }
    }

    fn print_char(&mut self, output: char) {
        // write!(self.stdout, "{}", output).unwrap();
        // if let CursorMovement::Newline = self
        //     .cursor
        //     .action(&mut self.stdout, CursorMovement::MoveRight(1))
        // {
        //     self.cursor
        //         .action(&mut self.stdout, CursorMovement::MoveDown(1));
        //     self.print_char(output);
        //     return;
        // }
        // self.flush();
    }

    fn print_str(&mut self, output: &str, new_line: bool) {
        // self.state.reading = false;

        // write!(self.stdout, "{}", output).unwrap();
        // if let CursorMovement::Newline = self.cursor.action(
        //     &mut self.stdout,
        //     CursorMovement::MoveRight(output.len() as u16),
        // ) {
        //     self.cursor
        //         .action(&mut self.stdout, CursorMovement::MoveDown(1));
        //     self.print_str(output, new_line);
        //     return;
        // }
        // self.flush();
        // if new_line {
        //     self.newline();
        // };
    }

    /// Prints a multiline text in the terminal.
    pub fn print_multiline<'b>(&'b mut self, output: &'a str, info_box: bool) {
        self.state.reading = false;
        let action = if info_box {
            Action::WriteInfoText(Text::from(output))
        } else {
            Action::WriteInputText(Text::from(output))
        };
        self.action_queue.push(action);

        // for (i, line) in output.lines().enumerate() {
        //     if i != 0 {
        //         self.cursor
        //             .action(&mut self.stdout, CursorMovement::MoveDown(1));
        //     }
        //     write!(self.stdout, "{}", line).unwrap();
        // }
        // self.flush();
        // if print_newline {
        //     self.newline();
        // }
    }

    fn newline(&mut self) {
        // self.cursor
        //     .action(&mut self.stdout, CursorMovement::MoveDown(1));
        // self.cursor.command_line_start();

        // if self.state.reading {
        //     write!(
        //         self.stdout,
        //         "{}... {}",
        //         termion::style::Bold,
        //         termion::style::Reset
        //     )
        //     .unwrap();
        // } else {
        //     write!(
        //         self.stdout,
        //         "{}>>> {}",
        //         termion::style::Bold,
        //         termion::style::Reset
        //     )
        //     .unwrap();
        // }
        // self.flush();
        // self.cursor.effect_on = true;
    }

    fn delete(&mut self) {
        // self.cursor
        //     .action(&mut self.stdout, CursorMovement::MoveLeft(1));
        // write!(self.stdout, "{}", termion::clear::AfterCursor).unwrap();
    }

    fn flush(&mut self) {
        // self.stdout.flush().unwrap();
    }

    fn report_error(&mut self, msg: &str) {
        self.print_str(msg, true);
    }
}

#[derive(Default)]
struct AppState {
    /// in reading mode flag
    reading: bool,
    /// terminate the application flag
    terminate: bool,
    call_stack: VecDeque<String>,
    shifted_backward: usize,
    shifted_forward: usize,
}

impl AppState {
    fn new() -> Self {
        AppState {
            reading: false,
            terminate: false,
            call_stack: VecDeque::with_capacity(100),
            shifted_backward: 0,
            shifted_forward: 0,
        }
    }

    fn push_in_call(&mut self, call: String) {
        if self.call_stack.len() == 100 {
            self.call_stack.pop_front();
        }
        self.call_stack.push_back(call);
    }

    fn get_previous_call(&mut self) -> Option<String> {
        if self.call_stack.is_empty() || self.shifted_backward == self.call_stack.len() {
            return None;
        }
        self.call_stack.rotate_right(1);
        self.shifted_backward += 1;
        if self.shifted_forward > 0 {
            self.shifted_forward -= 1;
        }
        self.call_stack.front().cloned()
    }

    fn get_following_call(&mut self) -> Option<String> {
        if self.call_stack.is_empty()
            || self.shifted_forward == self.call_stack.len() - 1
            || self.shifted_backward == 0
        {
            return None;
        }
        self.call_stack.rotate_left(1);
        self.shifted_forward += 1;
        self.shifted_backward -= 1;
        self.call_stack.front().cloned()
    }

    fn reset_call_stack(&mut self) {
        if self.shifted_backward > 0 {
            self.call_stack.rotate_left(self.shifted_backward);
            if (self.shifted_forward + 1) > 2 {
                self.shifted_forward -= self.shifted_backward;
            }
        }
        if (self.shifted_forward + 1) > 2 {
            self.call_stack.rotate_right(self.shifted_forward + 2);
        }
        self.shifted_backward = 0;
        self.shifted_forward = 0;
    }
}

#[test]
fn call_stack_rotation() {
    use std::iter::FromIterator;

    let front_sample = "baz".to_owned();
    let back_sample = "bar".to_owned();
    let mut state = AppState::new();
    state.call_stack = VecDeque::from_iter(vec!["foo".to_owned(); 2]);
    state.call_stack.push_front(front_sample);
    state.push_in_call(back_sample);

    for i in 0..5 {
        let r = state.get_previous_call();
        if i == 4 {
            assert!(r.is_none());
        } else {
            assert!(r.is_some());
            if i == 3 {
                assert_eq!("baz", r.unwrap());
            }
        }
    }

    for i in 0..4 {
        let r = state.get_following_call();
        if i == 3 {
            assert!(r.is_none());
        } else {
            assert!(r.is_some());
            if i == 2 {
                assert_eq!("bar", r.unwrap());
            }
        }
    }

    state.reset_call_stack();
    assert_eq!(
        Vec::from(state.call_stack.clone()),
        vec!["baz", "foo", "foo", "bar"]
    );

    state.push_in_call("stuff".to_owned());
    assert_eq!(5, state.call_stack.len());
    assert_eq!("stuff", state.get_previous_call().unwrap().as_str());
}
