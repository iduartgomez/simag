use std::io::{stdout, Bytes, Read, Stdout, StdoutLock, Write};
use std::iter::Iterator;
use std::time;

use once_cell::sync::Lazy;
use termion::event::{parse_event, Event, Key};
use termion::raw::{IntoRawMode, RawTerminal};
use termion::{async_stdin, AsyncReader};

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
    cursor: Cursor,
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
                    self.interpreter.read(*c)
                }
                Key::Backspace => {
                    if self.cursor.column > 5 {
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
            Action::Newline => {
                self.state.reading = false;
                self.newline();
            }
            Action::Discard => {
                let stdout = &mut self.stdout;
                self.cursor.move_down(stdout, 1);
                self.newline();
            }
            Action::Command(cmd) => {
                let stdout = &mut self.stdout;
                self.cursor.move_down(stdout, 1);
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
        if let Action::Newline = self
            .cursor
            .move_right(&mut self.stdout, output.len() as u16)
        {
            self.cursor.move_down(&mut self.stdout, 1);
            self.print_str(output);
            return;
        }
        self.flush();
    }

    fn print_char(&mut self, output: char) {
        write!(self.stdout, "{}", output).unwrap();
        if let Action::Newline = self.cursor.move_right(&mut self.stdout, 1) {
            self.cursor.move_down(&mut self.stdout, 1);
            self.print_char(output);
            return;
        }
        self.flush();
    }

    /// Prints a multiline text in the terminal.
    pub fn print_multiline(&mut self, output: &str) {
        for (i, line) in output.lines().enumerate() {
            if i != 0 {
                self.cursor.move_down(&mut self.stdout, 1);
            }
            write!(self.stdout, "{}", line).unwrap();
        }
        self.flush()
    }

    pub fn newline(&mut self) {
        self.cursor.move_down(&mut self.stdout, 1);
        self.cursor.column = 5;
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
        self.cursor.column -= 1;
        write!(
            self.stdout,
            "{} {}",
            termion::cursor::Goto(self.cursor.column, self.cursor.row),
            termion::cursor::Goto(self.cursor.column, self.cursor.row)
        )
        .unwrap();
        self.flush();
    }

    fn flush(&mut self) {
        self.stdout.flush().unwrap();
    }
}

pub enum Action {
    Read,
    Write(String),
    WriteStr(&'static str),
    WriteMulti(String),
    WriteMultiStr(&'static str),
    Command(String),
    Continue,
    Exit,
    Discard,
    Newline,
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

struct Cursor {
    column: u16,
    row: u16,
    space: (u16, u16),
    time: time::Instant,
    show: bool,
    effect_on: bool,
}

impl Default for Cursor {
    fn default() -> Self {
        Self::new()
    }
}

impl Cursor {
    pub fn new() -> Cursor {
        Cursor {
            row: 1,
            column: 1,
            space: termion::terminal_size().unwrap(),
            time: time::Instant::now(),
            show: true,
            effect_on: true,
        }
    }

    pub fn move_down<O: Write>(&mut self, stdout: &mut O, pos: u16) {
        if self.row + pos > self.space.1 {
            write!(stdout, "{}", termion::scroll::Up(pos)).unwrap();
        }
        self.row += pos;
        write!(stdout, "{}", termion::cursor::Goto(1, self.row)).unwrap();
        stdout.flush().unwrap();
    }

    pub fn move_right<O: Write>(&mut self, stdout: &mut O, pos: u16) -> Action {
        if self.column + pos > self.space.0 {
            return Action::Newline;
        } else {
            self.column += pos;
        }
        write!(stdout, "{}", termion::cursor::Goto(self.column, self.row)).unwrap();
        stdout.flush().unwrap();
        Action::None
    }

    pub fn cursor_effect<O: Write>(&mut self, stdout: &mut O) {
        if !self.effect_on {
            return;
        }
        let nt = time::Instant::now();
        if nt.duration_since(self.time) >= time::Duration::new(0, 500_000_000) {
            if self.show {
                self.show = false;
                self.hide(stdout);
            } else {
                self.show = true;
                self.show(stdout);
            }
            self.time = nt;
        }
    }

    pub fn show<O: Write>(&self, stdout: &mut O) {
        write!(stdout, "{}", termion::cursor::Show).unwrap();
        stdout.flush().unwrap();
    }

    pub fn hide<O: Write>(&self, stdout: &mut O) {
        write!(stdout, "{}", termion::cursor::Hide).unwrap();
        stdout.flush().unwrap();
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
    fn read(&mut self, input: char) -> Action;
    fn cmd_executor(&mut self, command: String) -> Option<Action>;
    fn delete_last(&mut self) -> Option<Action>;
}
