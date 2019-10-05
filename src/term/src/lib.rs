mod action;
mod cursor;

use std::io::{stdout, Bytes, Read, Stdout, StdoutLock, Write};
use std::iter::Iterator;
use std::time::Duration;

use copypasta::{ClipboardContext, ClipboardProvider};
use once_cell::sync::Lazy;
use termion::event::{parse_event, Event, Key};
use termion::raw::{IntoRawMode, RawTerminal};
use termion::{async_stdin, AsyncReader};

pub use action::Action;
use cursor::{Cursor, CursorMovement};

fn get_raw_stdout() -> RawTerminal<StdoutLock<'static>> {
    static STDOUT: Lazy<Stdout> = Lazy::new(stdout);

    STDOUT.lock().into_raw_mode().unwrap()
}

fn get_stdin() -> Bytes<AsyncReader> {
    async_stdin().bytes()
}

const ESCAPE: u8 = b'\x1B';

pub struct Terminal<I>
where
    I: Interpreter,
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
    I: Interpreter,
{
    pub fn new(interpreter: I) -> Self {
        let mut stdout = get_raw_stdout();

        write!(
            stdout,
            "{}{}",
            termion::clear::BeforeCursor,
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

    pub fn start_event_loop(&mut self) {
        loop {
            self.side_effects();
            let input = self.next_input();
            match input {
                Some(Ok(c)) => {
                    if c == ESCAPE {
                        match self.sequence() {
                            Ok(action) => match self.exec_action(action) {
                                Some(Action::Exit) => break,
                                Some(Action::Chain(chain)) => {
                                    self.exec_or_break(chain);
                                }
                                _ => {}
                            },
                            Err(val) => {
                                if let Ok(event) = parse_event(val, &mut self.stdin) {
                                    if let Some(Action::Exit) = self.parse_event(&event) {
                                        break;
                                    }
                                }
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

    fn side_effects(&mut self) {
        if self.cursor.effect_on {
            let stdout = &mut self.stdout;
            self.cursor.cursor_effect(stdout);
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
                    if self.cursor.at_start_of_the_line() {
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

    fn sequence(&mut self) -> Result<Action<'a>, u8> {
        let combo = &mut Combo { buffered: vec![] };
        let mut end = false;
        let mut num = false;

        loop {
            let next = self.stdin.next();
            match next {
                Some(Ok(b'O')) => {
                    combo.buffered.push(b'O');
                    end = true;
                }
                Some(Ok(b'[')) => {
                    combo.buffered.push(b'[');
                    end = true;
                }
                Some(Ok(c)) if (c < 64 || c > 126) && num => {
                    if combo.buffered.len() >= 4 {
                        return Err(c);
                    }
                    combo.buffered.push(c);
                }
                Some(Ok(c @ b'0'..=b'9')) if end => {
                    combo.buffered.push(c);
                    num = true;
                }
                Some(Ok(c)) if end => {
                    combo.buffered.push(c);
                    break;
                }
                Some(Err(_)) | None => break,
                Some(Ok(val)) => {
                    unreachable!("byte seq {} is not parseable under this context", val);
                }
            }
        }

        Ok(if let Ok(event) = parse_event(b'\x1B', combo) {
            self.parse_event(&event).map_or_else(|| Action::None, |x| x)
        } else {
            Action::None
        })
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

    fn built_cmd_exec(&mut self, cmd: String) -> Option<String> {
        match cmd.as_str() {
            "clear" => {
                self.clear();
                None
            }
            _non_executable_cmd => Some(cmd),
        }
    }

    fn clear(&mut self) {
        write!(
            self.stdout,
            "{}{}",
            termion::clear::BeforeCursor,
            termion::cursor::Goto(1, 1)
        )
        .unwrap();

        self.cursor.row = 0;
        self.cursor.column = 0;
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
    }

    fn flush(&mut self) {
        self.stdout.flush().unwrap();
    }
}

#[derive(Default)]
struct TerminalState {
    reading: bool,
}

impl TerminalState {
    pub fn new() -> Self {
        TerminalState { reading: false }
    }
}

struct Combo {
    buffered: Vec<u8>,
}

impl Iterator for Combo {
    type Item = Result<u8, ::std::io::Error>;
    fn next(&mut self) -> Option<Self::Item> {
        if !self.buffered.is_empty() {
            Some(Ok(self.buffered.pop().unwrap()))
        } else {
            None
        }
    }
}

pub trait Interpreter {
    /// Last source code input char that was received by the interpreter.
    fn last_source_input(&self) -> Option<char>;

    /// Returns if the interpreter is currently receiving source stream
    /// or is parsing commmand.
    fn is_reading(&self) -> bool;

    /// Feed new input characters to the interpreter and return
    /// a new action after digestion.
    fn digest<'b, 'a: 'b>(&'b mut self, input: char) -> Action<'a>;

    /// Execute a command and return the continuation action if there
    /// is one.
    fn cmd_executor<'b, 'a: 'b>(&'b mut self, command: String) -> Option<Action<'a>>;

    /// Drop last change of the input stream and return an optional action
    /// to be performed after rollingback last change.
    fn delete_last<'b, 'a: 'b>(&'b mut self) -> Option<Action<'a>>;

    /// Set current reading source disposition.
    fn set_reading(&mut self, currently_reading: bool);

    /// Evaluate the currently ingested source.
    fn evaluate<'b, 'a: 'b>(&'b mut self) -> Result<Action<'a>, String>;

    /// Return any queued command if there is one.
    fn queued_command(&mut self) -> Option<String>;

    /// Evaluate the special newline eval character
    fn newline_eval<'b, 'a: 'b>(&'b mut self) -> Action<'a> {
        let currently_reading = self.is_reading();
        let last_input = self.last_source_input();
        if currently_reading && last_input.is_some() && last_input.unwrap() != '\n' {
            Action::Read
        } else if currently_reading {
            self.set_reading(false);
            match self.evaluate() {
                Ok(Action::Write(p)) => Action::Write(p),
                Ok(Action::WriteStr(p)) => Action::WriteStr(p),
                Ok(Action::WriteMulti(p)) => Action::WriteMulti(p),
                Ok(Action::WriteMultiStr(p)) => Action::WriteMultiStr(p),
                Err(msg) => Action::WriteMulti((msg, true)),
                _ => Action::StopReading,
            }
        } else if let Some(command) = self.queued_command() {
            Action::Command(command)
        } else if last_input.is_some() && last_input.unwrap() == '\n' {
            Action::StopReading
        } else {
            Action::Newline
        }
    }
}
