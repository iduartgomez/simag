mod cursor;

use std::io::{stdout, Bytes, Read, Stdout, StdoutLock, Write};
use std::iter::Iterator;

use once_cell::sync::Lazy;
use termion::event::{parse_event, Event, Key};
use termion::raw::{IntoRawMode, RawTerminal};
use termion::{async_stdin, AsyncReader};

use cursor::{Cursor, CursorMovement};

fn get_raw_stdout() -> RawTerminal<StdoutLock<'static>> {
    static STDOUT: Lazy<Stdout> = Lazy::new(stdout);

    STDOUT.lock().into_raw_mode().unwrap()
}

fn get_stdin() -> Bytes<AsyncReader> {
    async_stdin().bytes()
}

pub struct Terminal<I>
where
    I: Interpreter,
{
    interpreter: I,
    state: TerminalState,
    cursor: Cursor<RawTerminal<StdoutLock<'static>>>,
    stdin: Bytes<AsyncReader>,
    stdout: RawTerminal<StdoutLock<'static>>,
}

impl<I> Terminal<I>
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
        }
    }

    pub fn start_event_loop(&mut self) {
        loop {
            if self.cursor.effect_on {
                let stdout = &mut self.stdout;
                self.cursor.cursor_effect(stdout);
            }
            if let Some(Ok(c)) = self.stdin.next() {
                if c == b'\x1B' {
                    match self.sequence() {
                        Ok(action) => {
                            if let Some(Action::Exit) = self.exec_action(action) {
                                break;
                            }
                        }
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

    fn parse_event(&mut self, event: &Event) -> Option<Action> {
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
                Key::Ctrl('d') | Key::Esc => Action::Exit,
                Key::Ctrl('c') => Action::Discard,
                _ => Action::Continue,
            };
            self.exec_action(action)
        } else {
            None
        }
    }

    fn sequence(&mut self) -> Result<Action, u8> {
        let combo = &mut Combo { buffered: vec![] };
        let mut end = false;
        let mut num = false;

        loop {
            match self.stdin.next() {
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
                None if num => break,
                Some(Err(_)) => break,
                _ => {}
            }
        }

        Ok(if let Ok(event) = parse_event(b'\x1B', combo) {
            self.parse_event(&event).map_or_else(|| Action::None, |x| x)
        } else {
            Action::None
        })
    }

    fn exec_action(&mut self, action: Action) -> Option<Action> {
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
                self.cursor
                    .action(&mut self.stdout, CursorMovement::MoveRight(1));
                if let Some(action) = self.interpreter.cmd_executor(cmd) {
                    match action {
                        Action::WriteStr(val) => self.print_str(val),
                        Action::Write(val) => self.print_str(&val),
                        Action::WriteMulti(val) => self.print_multiline(&val),
                        Action::WriteMultiStr(val) => self.print_multiline(&val),
                        Action::Command(cmd) => return self.interpreter.cmd_executor(cmd),
                        _ => return Some(action),
                    }
                }
                self.newline();
            }
            Action::Write(msg) => {
                self.print_multiline(msg.as_str());
                self.state.reading = false;
                self.newline();
            }
            Action::Exit => return Some(Action::Exit),
            _ => {}
        }
        None
    }

    fn print_str(&mut self, output: &str) {
        write!(self.stdout, "{}", output).unwrap();
        if let CursorMovement::Newline = self.cursor.action(
            &mut self.stdout,
            CursorMovement::MoveRight(output.len() as u16),
        ) {
            self.cursor
                .action(&mut self.stdout, CursorMovement::MoveDown(1));
            self.print_str(output);
            return;
        }
        self.flush();
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

    /// Prints a multiline text in the terminal.
    pub fn print_multiline(&mut self, output: &str) {
        for (i, line) in output.lines().enumerate() {
            if i != 0 {
                self.cursor
                    .action(&mut self.stdout, CursorMovement::MoveDown(1));
            }
            write!(self.stdout, "{}", line).unwrap();
        }
        self.flush()
    }

    pub fn newline(&mut self) {
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

pub enum Action {
    /// Signal to the terminal that the interpreter is currently reading
    /// source interpretable instructions.
    Read,
    /// Signal to the terminal that the interpreter is done ingesting
    /// source of interpretable instructions.
    StopReading,
    /// Output a single line message to the terminal.  
    Write(String),
    /// Output a single line message to the terminal.
    WriteStr(&'static str),
    /// Output multiple lines message to the terminal.
    WriteMulti(String),
    /// Output multiple lines message to the terminal.
    WriteMultiStr(&'static str),
    /// Signal an interpretable command to the terminal main event loop
    /// for the interpreter.
    Command(String),
    /// Continue digesting input
    Continue,
    /// Exit the program
    Exit,
    /// Signal cancelation of any ongoing evaluation
    Discard,
    /// Do nothing
    None,
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
    /// Last input char that was received by the interpreter.
    fn last_input(&self) -> Option<char>;

    /// Returns if the interpreter is currently receiving source stream
    /// or is parsing commmand.
    fn is_reading(&self) -> bool;

    /// Feed new input characters to the interpreter and return
    /// a new action after digestion.
    fn digest(&mut self, input: char) -> Action;

    /// Execute a command and return the continuation action if there
    /// is one.
    fn cmd_executor(&mut self, command: String) -> Option<Action>;

    /// Drop last change of the input stream and return an optional action
    /// to be performed after rollingback last change.
    fn delete_last(&mut self) -> Option<Action>;

    /// Set current reading source disposition.
    fn set_reading(&mut self, currently_reading: bool);

    /// Evaluate the currently ingested source.
    fn evaluate(&mut self) -> Result<Action, String>;

    /// Return any queued command if there is one.
    fn queued_command(&mut self) -> Option<String>;

    /// Evaluate the special newline eval character
    fn newline_eval(&mut self) -> Action {
        let currently_reading = self.is_reading();
        let last_input = self.last_input();
        if currently_reading && last_input.is_some() && last_input.unwrap() != '\n' {
            self.digest('\n');
            Action::Read
        } else if currently_reading {
            self.set_reading(false);
            match self.evaluate() {
                Ok(Action::Write(p)) => Action::Write(p),
                Err(msg) => Action::Write(msg),
                _ => Action::None,
            }
        } else if let Some(command) = self.queued_command() {
            Action::Command(command)
        } else {
            Action::None
        }
    }
}
