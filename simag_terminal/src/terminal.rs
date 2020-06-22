//! Terminal that controls the main application flow, includes the main event loop.

use std::collections::VecDeque;
use std::io::{stdout, Bytes, Read, Stdout, StdoutLock, Write};
use std::iter::Iterator;
use std::time::{Duration, Instant};

use copypasta::{ClipboardContext, ClipboardProvider};
use once_cell::sync::Lazy;
use termion::event::{parse_event, Event, Key};
use termion::raw::{IntoRawMode, RawTerminal};
use termion::{async_stdin, AsyncReader};

use crate::{
    cursor::{Cursor, CursorMovement},
    interpreter::ReplInterpreter,
    Action,
};

fn get_raw_stdout() -> RawTerminal<StdoutLock<'static>> {
    static STDOUT: Lazy<Stdout> = Lazy::new(stdout);

    STDOUT.lock().into_raw_mode().unwrap()
}

fn get_stdin() -> Bytes<AsyncReader> {
    async_stdin().bytes()
}

const ESCAPE_SEQ: u8 = b'\x1B';

const SHORTCUTS_HELP: &str = "\
Shortcut help:
* CTRL-C > cancel the current command
* CTRL-D > close down the program (non graceful)
* UP > Scroll commands backwards
* DOWN > Scroll commands forwards

Built-in commands:
* clear > clears the terminal screen
";

pub struct Terminal<I>
where
    I: ReplInterpreter,
{
    interpreter: I,
    state: TerminalState,
    cursor: Cursor<RawTerminal<StdoutLock<'static>>>,
    stdin: Bytes<AsyncReader>,
    stdout: RawTerminal<StdoutLock<'static>>,
    clipboard: ClipboardContext,
    clipped_content: Option<Vec<u8>>,
}

impl<'a, I> Terminal<I>
where
    I: ReplInterpreter,
{
    pub fn new(interpreter: I) -> Self {
        let mut stdout = get_raw_stdout();

        write!(
            stdout,
            "{}{}",
            termion::clear::All,
            termion::cursor::Goto(1, 1)
        )
        .unwrap();

        Terminal {
            interpreter,
            state: TerminalState::new(),
            cursor: Cursor::new(),
            stdout,
            stdin: get_stdin(),
            clipboard: ClipboardContext::new().unwrap(),
            clipped_content: None,
        }
    }

    pub fn start_event_loop(&mut self) {
        loop {
            self.side_effects();
            match self.next_input() {
                Some(Ok(c)) => {
                    if c == ESCAPE_SEQ {
                        // Escape character, means this is a control sequence
                        match self.sequence() {
                            Ok(action) => match self.exec_action(action) {
                                Some(Action::Exit) => break,
                                Some(Action::Chain(chain)) => {
                                    self.exec_or_break(chain);
                                }
                                Some(Action::None) | None => {}
                                _ => {
                                    self.report_error("Cannot perform that action in this context.")
                                }
                            },
                            Err(err) => {
                                self.report_error(&format!(
                                    "Error reading input: {}",
                                    err.to_string()
                                ));
                            }
                        }
                    } else if let Ok(event) = parse_event(c, &mut self.stdin) {
                        if let Some(Action::Exit) = self.parse_event(&event) {
                            break;
                        }
                    }
                }
                Some(Err(_)) => break,
                None => {}
            }
        }

        // Clean up application activity
        write!(
            self.stdout,
            "{}{}{}{}",
            termion::style::Reset,
            termion::clear::All,
            termion::cursor::Goto(1, 1),
            termion::cursor::Show
        )
        .unwrap();

        self.flush();
    }

    fn next_input(&mut self) -> Option<Result<u8, std::io::Error>> {
        if self.clipped_content.is_some() {
            if let Some(c) = self.clipped_content.as_mut().unwrap().pop() {
                Some(Ok(c))
            } else {
                self.clipped_content = None;
                None
            }
        } else {
            self.stdin.next()
        }
    }

    fn sequence(&mut self) -> Result<Action<'a>, std::io::Error> {
        // block a for a bit so we catch up all the input from the reading thread
        const WAIT: Duration = Duration::from_nanos(10);
        let wait_time = Instant::now() + WAIT;
        while wait_time < Instant::now() {}

        match parse_event(ESCAPE_SEQ, &mut self.stdin) {
            Ok(event) => Ok(self.parse_event(&event).map_or_else(|| Action::None, |x| x)),
            _ => Ok(Action::None),
        }
    }

    fn exec_action(&mut self, action: Action<'a>) -> Option<Action<'a>> {
        match action {
            Action::Continue => {
                let stdout = &mut self.stdout;
                self.cursor.effect_on = false;
                self.cursor.show(stdout);
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
                self.cursor
                    .action(&mut self.stdout, CursorMovement::MoveRight(1));
                self.newline();
            }
            Action::Command(cmd) => {
                self.state.reset_call_stack();
                self.state.push_in_call(cmd.clone());
                if let Some(cmd) = self.built_cmd_exec(cmd) {
                    self.cursor
                        .action(&mut self.stdout, CursorMovement::MoveDown(1));
                    if let Some(action) = self.interpreter.cmd_executor(cmd) {
                        return self.exec_action(action);
                    }
                }
                self.newline();
            }
            Action::Write((val, new_line)) => self.print_str(&val, new_line),
            Action::WriteStr((val, new_line)) => self.print_str(val, new_line),
            Action::WriteMulti((val, new_line)) => self.print_multiline(&val, new_line),
            Action::WriteMultiStr((val, new_line)) => self.print_multiline(val, new_line),
            Action::Chain(chain) => return self.exec_or_break(chain),
            Action::Newline => {
                self.check_reading_status();
                self.newline();
            }
            Action::Sleep(val) => std::thread::sleep(Duration::from_millis(val)),
            Action::None => {}
            Action::Exit => return Some(Action::Exit),
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
            let stdout = &mut self.stdout;
            self.cursor.side_effects(stdout);
        }
    }

    fn parse_event(&mut self, event: &Event) -> Option<Action<'a>> {
        if let Event::Key(key) = event {
            let action = match key {
                Key::Char(c) => {
                    if c != &'\n' {
                        self.print_char(*c);
                    }
                    self.interpreter.digest(*c)
                }
                Key::Backspace => {
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
                Key::Esc => Action::Exit,
                Key::Ctrl(key) => self.ctrl_action(*key),
                Key::Up => {
                    return self.print_previous_command();
                }
                Key::Down => {
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
                self.clipped_content = self.clipboard.get_contents().ok().map(|string| {
                    let mut bytes: Vec<u8> = string.into();
                    bytes.reverse();
                    bytes
                });
                Action::None
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
        write!(
            self.stdout,
            "{}{}",
            termion::clear::BeforeCursor,
            termion::cursor::Goto(1, 1)
        )
        .unwrap();
        self.cursor.row = 1;
        self.cursor.column = 1;
    }

    pub fn clear_line(&mut self) {
        write!(
            self.stdout,
            "{}{}{}>>> {}",
            termion::cursor::Goto(1, self.cursor.row),
            termion::style::Bold,
            termion::clear::AfterCursor,
            termion::style::Reset
        )
        .unwrap();
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
        write!(self.stdout, "{}", output).unwrap();
        if let CursorMovement::Newline = self
            .cursor
            .action(&mut self.stdout, CursorMovement::MoveRight(1))
        {
            self.cursor
                .action(&mut self.stdout, CursorMovement::MoveDown(1));
            self.print_char(output);
            return;
        }
        self.flush();
    }

    fn print_str(&mut self, output: &str, new_line: bool) {
        self.state.reading = false;

        write!(self.stdout, "{}", output).unwrap();
        if let CursorMovement::Newline = self.cursor.action(
            &mut self.stdout,
            CursorMovement::MoveRight(output.len() as u16),
        ) {
            self.cursor
                .action(&mut self.stdout, CursorMovement::MoveDown(1));
            self.print_str(output, new_line);
            return;
        }
        self.flush();
        if new_line {
            self.newline();
        };
    }

    /// Prints a multiline text in the terminal.
    pub fn print_multiline(&mut self, output: &str, print_newline: bool) {
        self.state.reading = false;

        for (i, line) in output.lines().enumerate() {
            if i != 0 {
                self.cursor
                    .action(&mut self.stdout, CursorMovement::MoveDown(1));
            }
            write!(self.stdout, "{}", line).unwrap();
        }
        self.flush();
        if print_newline {
            self.newline();
        }
    }

    fn newline(&mut self) {
        self.cursor
            .action(&mut self.stdout, CursorMovement::MoveDown(1));
        self.cursor.command_line_start();

        if self.state.reading {
            write!(
                self.stdout,
                "{}... {}",
                termion::style::Bold,
                termion::style::Reset
            )
            .unwrap();
        } else {
            write!(
                self.stdout,
                "{}>>> {}",
                termion::style::Bold,
                termion::style::Reset
            )
            .unwrap();
        }
        self.flush();
        self.cursor.effect_on = true;
    }

    fn delete(&mut self) {
        self.cursor
            .action(&mut self.stdout, CursorMovement::MoveLeft(1));
        write!(self.stdout, "{}", termion::clear::AfterCursor).unwrap();
    }

    fn flush(&mut self) {
        self.stdout.flush().unwrap();
    }

    fn report_error(&mut self, msg: &str) {
        self.print_str(msg, true);
    }
}

#[derive(Default)]
struct TerminalState {
    reading: bool,
    call_stack: VecDeque<String>,
    shifted_backward: usize,
    shifted_forward: usize,
}

impl TerminalState {
    fn new() -> Self {
        TerminalState {
            reading: false,
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
    let mut state = TerminalState::new();
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
