//! Terminal that controls the main application flow, includes the main event loop.

use crate::{cursor::Cursor, interpreter::ReplInterpreter, Action};
use copypasta::{ClipboardContext, ClipboardProvider};
use crossterm::event::{Event, KeyCode, KeyModifiers};
use std::iter::Iterator;
use std::time::Duration;
use std::{collections::VecDeque, sync::mpsc::Receiver};
use std::{io::Stdout, time::Instant};
use tui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::Style,
    text::{Span, Spans, Text},
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
    events: Receiver<Event>,
}

impl<'a, I> Application<'a, I>
where
    I: ReplInterpreter,
{
    pub fn new(interpreter: I) -> Self {
        // set event handler thread
        let (tx, events) = std::sync::mpsc::channel();
        std::thread::spawn(move || -> crossterm::Result<()> {
            loop {
                let event = crossterm::event::read()?;
                if is_exit_event(&event) {
                    tx.send(event);
                    break;
                }
                match event {
                    Event::Key(event) => {
                        tx.send(Event::Key(event));
                    }
                    Event::Mouse(event) => {
                        tx.send(Event::Mouse(event));
                    }
                    Event::Resize(width, height) => {
                        eprintln!("New size {}x{}", width, height);
                    }
                }
            }
            Ok(())
        });

        Application {
            interpreter,
            state: AppState::new(),
            events,
        }
    }

    pub fn start_event_loop(&mut self) -> std::io::Result<()> {
        let stdout = std::io::stdout();
        let backend = CrosstermBackend::new(stdout);
        let mut terminal = TuiTerminal::new(backend).unwrap();

        terminal.clear()?;
        self.state.newline();
        terminal.flush()?;

        if cfg!(debug_assertions) {
            self.state
                .output_box
                .push(Spans::from(format!("tick: {:?}", Instant::now())));
        }

        loop {
            terminal.draw(|f| {
                match self.events.recv_timeout(Duration::from_millis(15)) {
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
                            Some(Action::None) | None => {}
                            _ => self
                                .state
                                .report_error("Cannot perform that action in this context."),
                        }
                    }
                    Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {}
                    Err(_) => {
                        self.state.terminate = true;
                    }
                };

                let chunks = Self::layout_for_frame(f.size());
                if cfg!(debug_assertions) {
                    self.state.output_box.pop();
                    self.state
                        .output_box
                        .push(Spans::from(format!("tick: {:?}", Instant::now())));
                }

                self.state.input_box_size = chunks[0];
                let input = Self::draw_input_box(self.state.get_current_input_box());
                f.render_widget(input, chunks[0]);

                self.state.output_box_size = chunks[1];
                let output = Self::draw_output_box(self.state.get_current_output_box());
                f.render_widget(output, chunks[1]);
            })?;
            if self.state.terminate {
                break;
            }
        }
        terminal.clear()?;
        terminal.show_cursor()?;
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
            .block(Block::default().borders(Borders::ALL).title(" Inspect "))
    }

    fn exec_action<'b>(&'b mut self, action: Action<'a>) -> Option<Action<'a>> {
        match action {
            Action::Continue => {
                // let stdout = &mut self.stdout;
                self.state.cursor.effect_on = false;
                // self.cursor.show(stdout);
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
                // self.cursor
                //     .action(&mut self.stdout, CursorMovement::MoveRight(1));
                self.state.newline();
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
                self.state.newline();
            }

            Action::Chain(chain) => return self.exec_or_break(chain),
            Action::Newline => {
                self.state.set_reading_status(self.interpreter.is_reading());
                self.state.newline();
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
                self.state.clear_terminal();
                None
            }
            "shortcuts" => {
                self.print_text(SHORTCUTS_HELP);
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

    fn process_event(&mut self, event: Event) -> Option<Action<'a>> {
        if let Event::Key(key) = event {
            let action = match key.code {
                KeyCode::Char(c) if key.modifiers == KeyModifiers::NONE => {
                    self.state.input_char(c);
                    self.interpreter.digest(c)
                }
                KeyCode::Backspace => {
                    if !self.state.cursor.at_start_of_the_line() {
                        self.state.delete();
                        match self.interpreter.delete_last() {
                            Some(Action::Discard) => {
                                self.state.cursor.effect_on = true;
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
                    return self.show_previous_command();
                }
                KeyCode::Down => {
                    return self.show_following_command();
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
                self.state
                    .clipboard
                    .get_contents()
                    .ok()
                    .map(|string| Action::WriteInputText(Text::from(string)))
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
        } else if let Some(prev_cmd) = self.state.get_previous_call() {
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
        } else if let Some(following_cmd) = self.state.get_following_call() {
            self.show_input_command(following_cmd);
            Some(Action::Continue)
        } else {
            self.state.clear_line();
            self.interpreter.drop_command();
            self.state.reset_call_stack();
            None
        }
    }

    fn show_input_command(&mut self, cmd: String) {
        self.state.clear_line();
        self.interpreter.drop_command();
        cmd.chars().for_each(|c| self.state.input_char(c));
        cmd.chars().for_each(|c| {
            self.interpreter.digest(c);
        })
    }
}

fn is_exit_event(ev: &Event) -> bool {
    match ev {
        Event::Key(key) if key.modifiers == KeyModifiers::CONTROL => match key.code {
            KeyCode::Char('c') => true,
            _ => false,
        },
        _ => false,
    }
}

struct AppState<'a> {
    /// in reading mode flag
    reading: bool,
    /// terminate the application flag
    terminate: bool,
    call_stack: VecDeque<String>,
    shifted_backward: usize,
    shifted_forward: usize,
    cursor: Cursor<Stdout>,
    /// each string belongs to a single line in the box
    input_box: Vec<String>,
    input_box_size: Rect,
    /// each group of spans belongs to a single line in the box
    output_box: Vec<Spans<'a>>,
    output_box_size: Rect,
    clipboard: ClipboardContext,
}

impl<'a> AppState<'a> {
    fn new() -> Self {
        AppState {
            reading: false,
            terminate: false,
            call_stack: VecDeque::with_capacity(100),
            shifted_backward: 0,
            shifted_forward: 0,
            cursor: Cursor::new(),
            input_box: Vec::new(),
            input_box_size: Rect::default(),
            output_box: Vec::new(),
            output_box_size: Rect::default(),
            clipboard: ClipboardContext::new().unwrap(),
        }
    }

    fn get_current_input_box(&self) -> Text {
        Text::from(Spans::from(
            self.input_box
                .iter()
                .map(|s| Span::from(s.as_str()))
                .into_iter()
                .collect::<Vec<_>>(),
        ))
    }

    fn get_current_output_box(&mut self) -> Text {
        let spans_to_print: Vec<_> = if self.output_box.len() > self.output_box_size.height as usize
        {
            self.output_box[self.output_box_size.height as usize..]
                .iter()
                .cloned()
                .collect()
        } else {
            self.output_box.clone()
        };
        Text::from(spans_to_print)
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

    fn print_text<'b>(&'b mut self, text: Text<'a>) {
        self.output_box.extend(text.into_iter())
    }

    fn clear_terminal(&mut self) {
        // write!(
        //     self.stdout,
        //     "{}{}",
        //     termion::clear::BeforeCursor,
        //     termion::cursor::Goto(1, 1)
        // )
        // .unwrap();
        // self.cursor.row = 1;
        // self.cursor.column = 1;
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
        // self.flush();
        // self.cursor.column = 5;
    }

    fn set_reading_status(&mut self, status: bool) {
        self.reading = status;
    }

    /// Append a character to the current input box line.
    fn input_char(&mut self, output: char) {
        if let Some(line) = self.input_box.last_mut() {
            line.push(output);
        } else {
            self.input_box.push(String::from(output));
        };

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

    // fn print_str(&mut self, output: &str, new_line: bool) {
    //     self.state.reading = false;
    //     write!(self.stdout, "{}", output).unwrap();
    //     if let CursorMovement::Newline = self.cursor.action(
    //         &mut self.stdout,
    //         CursorMovement::MoveRight(output.len() as u16),
    //     ) {
    //         self.cursor
    //             .action(&mut self.stdout, CursorMovement::MoveDown(1));
    //         self.print_str(output, new_line);
    //         return;
    //     }
    //     self.flush();
    //     if new_line {
    //         self.newline();
    //     };
    // }

    /// Make a new line in the input box.
    fn newline(&mut self) {
        self.input_box.push(String::from(">>> "));

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

    // Delete previous character from the input box.
    fn delete(&mut self) {
        // self.cursor
        //     .action(&mut self.stdout, CursorMovement::MoveLeft(1));
        // write!(self.stdout, "{}", termion::clear::AfterCursor).unwrap();
    }

    // Report an error in the output box.
    fn report_error(&mut self, msg: &str) {}

    fn side_effects(&mut self) {
        if self.cursor.effect_on {
            // let stdout = &mut self.stdout;
            // self.cursor.side_effects(stdout);
        }
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
