//! A REPL (Read-Eval-Print-Loop) terminal interface for the language

extern crate termion;
extern crate simag_core;

use std::io::{Read, Write, Bytes, StdoutLock, StdinLock, stdout, stdin};
use std::mem;
use simag_core::Agent;
use termion::event::{Event, Key, parse_event};
use termion::raw::{IntoRawMode, RawTerminal};

const INFO: &'static str = "Simag Logic Lang 0.0.1 Interpreter";

struct TermInterface<'a> {
    cursor: Cursor,
    stdout: RawTerminal<StdoutLock<'a>>,
    stdin: Bytes<StdinLock<'a>>,
    interpreter: Agent,
    parse_input: String,
    command: String,
    reading: bool,
    ask: bool,
}

impl<'a> TermInterface<'a> {
    fn new(stdout: RawTerminal<StdoutLock<'a>>, stdin: StdinLock<'a>) -> TermInterface<'a> {
        TermInterface {
            cursor: Cursor { row: 1, column: 1, size: termion::terminal_size().unwrap()},
            stdout: stdout,
            stdin: stdin.bytes(),
            interpreter: Agent::default(),
            parse_input: String::new(),
            command: String::new(),
            reading: false,
            ask: false,
        }
    }

    fn read(&mut self) {
        loop {
            match self.stdin.next() {
                Some(Ok(b'\n')) | Some(Ok(b'\r')) => {
                    if self.reading && !self.parse_input.ends_with("\n") {
                        self.print_newline();
                        self.parse_input.push('\n');
                    } else if self.reading {
                        self.reading = false;
                        self.eval();
                        self.ask = false;
                        self.print_newline();
                    } else if !self.command.is_empty() {
                        self.cursor.column = 5;
                        match Command::from(self.command.as_str()) {
                            Command::Help => {}
                            Command::Err => {
                                self.cursor.move_down(&mut self.stdout, 2);
                                write!(self.stdout, "Unknown command").unwrap();
                                self.cursor.move_down(&mut self.stdout, 1);
                                self.flush();
                            }
                        }
                        self.command = String::new();
                        self.print_newline();
                    } else {
                        self.print_newline();
                    }
                }
                Some(Ok(b'(')) if !self.reading => {
                    self.parse_input.push('(');
                    self.print_str("(");
                    self.reading = true;
                }
                Some(Ok(b'?')) if !self.reading => {
                    self.print_str("?");
                    self.ask = true;
                }
                Some(Ok(c)) => {
                    match parse_event(c, &mut self.stdin) {
                        Ok(Event::Key(key)) => {
                            match self.parse_input(key) {
                                Action::Continue => {}
                                Action::Discard => {
                                    self.parse_input = String::new();
                                } 
                                Action::Break => break,
                                _ => {}
                            }
                        }
                        Err(error) => {
                            self.cursor.move_down(&mut self.stdout, 2);
                            write!(self.stdout, "{}", error).unwrap();
                            self.flush();
                            break;
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }
    }

    fn print_str(&mut self, output: &str) {
        write!(self.stdout,
               "{}",
               output)
                .unwrap();
        if let Action::Newline = self.cursor.move_right(&mut self.stdout, output.len() as u16) {
            self.print_newline();
        }
        self.flush();
    }

    fn print_newline(&mut self) {
        self.cursor.move_down(&mut self.stdout, 1);
        self.cursor.column = 5;
        if self.reading {
            write!(self.stdout,
                   "{}... {}",
                   termion::style::Bold,
                   termion::style::Reset)
                    .unwrap();
        } else {
            write!(self.stdout,
                   "{}>>> {}",
                   termion::style::Bold,
                   termion::style::Reset)
                    .unwrap();
        }
        self.flush();
    }

    fn delete(&mut self) {
        self.cursor.column -= 1;
        write!(self.stdout,
               "{} ",
               termion::cursor::Goto(self.cursor.column, self.cursor.row))
                .unwrap();
        self.flush();
        write!(self.stdout,
               "{}",
              termion::cursor::Goto(self.cursor.column, self.cursor.row))
                .unwrap();
        self.flush();
    }

    fn parse_input(&mut self, input: Key) -> Action {
        match input {
            Key::Char(c) => {
                if self.reading {
                    self.parse_input.push(c);
                } else {
                    self.command.push(c);
                }
                self.print_str(&c.to_string());
                Action::Continue
            }
            Key::Backspace | Key::Delete => {
                if self.cursor.column > 5 && self.reading {
                    self.parse_input.pop();
                    self.delete();
                } else if self.cursor.column > 5 {
                    self.delete();
                }
                Action::Continue
            }
            Key::Ctrl('d') | Key::Esc => Action::Break,
            Key::Ctrl('c') => Action::Discard,
            _ => Action::Continue,
        }
    }

    fn eval(&mut self) {
        let mut input = String::new();
        mem::swap(&mut self.parse_input, &mut input);
        if !self.ask {
            match self.interpreter.tell(input) {
                Err(errors) => {
                    let err = format!("{:?}", errors.get(0));
                    let mut n = 0;
                    for _ in err.matches('\n') {
                        n += 1;
                    }
                    self.cursor.move_down(&mut self.stdout, 2 + n);
                    write!(self.stdout, "{}", err).unwrap();
                    self.cursor.move_down(&mut self.stdout, 1);
                }
                Ok(_) => {}
            }
        } else {
            let result = self.interpreter.ask(input);
            write!(self.stdout, "{:?}", result).unwrap();
        }
        self.flush();
    }

    fn flush(&mut self) {
        self.stdout.flush().unwrap();
    }
}

struct Cursor {
    column: u16,
    row: u16,
    size: (u16, u16),
}

impl Cursor {
    fn move_down(&mut self, stdout: &mut RawTerminal<StdoutLock>, pos: u16) {
        if self.row + pos > self.size.1 {
            write!(stdout, "{}", termion::scroll::Up(pos)).unwrap();
        }
        self.row += pos;
        write!(stdout, "{}", termion::cursor::Goto(1, self.row)).unwrap();
        stdout.flush().unwrap();
    }

    fn move_right(&mut self, stdout: &mut RawTerminal<StdoutLock>, pos: u16) -> Action {
        if self.column + pos > self.size.0 {
            return Action::Newline
        } else {
            self.column += pos;
        }
        write!(stdout, "{}", termion::cursor::Goto(self.column, self.row)).unwrap();
        stdout.flush().unwrap();
        Action::None
    }
}

enum Command {
    Help,
    Err,
}

impl<'a> From<&'a str> for Command {
    fn from(command: &'a str) -> Command {
        match command {
            "help" => Command::Help,
            _ => Command::Err,
        }
    }
}

enum Action {
    Continue,
    Break,
    Discard,
    Newline,
    None
}

fn main() {
    let stdout = stdout();
    let stdin = stdin();

    let mut term = TermInterface::new(stdout.lock().into_raw_mode().unwrap(), stdin.lock());
    write!(term.stdout,
           "{}{}{}",
           termion::clear::BeforeCursor,
           termion::cursor::Goto(1, 1),
           INFO)
            .unwrap();
    term.print_newline();
    term.read();
    writeln!(term.stdout,
             "{}{}{}",
             termion::style::Reset,
             termion::clear::All,
             termion::cursor::Goto(1, 1))
            .unwrap();
    term.flush();
}
