//! A REPL (Read-Eval-Print-Loop) terminal interface for the language

extern crate termion;
extern crate simag_core;

use std::io::{Read, Write, Bytes, stdout};
use std::mem;
use std::time;

use simag_core::{Agent, Answer, QueryErr};
use termion::event::{Event, Key, parse_event};
use termion::raw::IntoRawMode;

const INFO: &'static str = "Simag Logic Lang 0.0.1 Interpreter";


struct TermInterface<I, O, E>
    where I: Read,
          O: Write,
          E: Interpreter
{
    cursor: Cursor,
    stdout: O,
    stdin: Bytes<I>,
    interpreter: E,
    reading: bool,
}

impl<I, O, E> TermInterface<I, O, E>
    where I: Read,
          O: Write,
          E: Interpreter
{
    fn new(stdin: I, stdout: O, interpreter: E) -> TermInterface<I, O, E> {
        TermInterface {
            cursor: Cursor::new(),
            stdout: stdout,
            stdin: stdin.bytes(),
            interpreter: interpreter,
            reading: false,
        }
    }

    fn read(&mut self) {
        loop {
            if self.cursor.effect_on {
                self.cursor.cursor_effect(&mut self.stdout);
            }
            if let Some(Ok(c)) = self.stdin.next() {
                match parse_event(c, &mut self.stdin) {
                    Ok(Event::Key(key)) => {
                        match self.parse_input(key) {
                            Action::Continue => {
                                self.cursor.effect_on = false;
                                self.cursor.show(&mut self.stdout);
                            }
                            Action::Read => {
                                self.reading = true;
                                self.newline();
                            }
                            Action::Newline => {
                                self.reading = false;
                                self.newline();
                            }
                            Action::Discard => {
                                self.cursor.move_down(&mut self.stdout, 1);
                                self.print_str("Command cancelled");
                                self.newline();
                            }
                            Action::Command(Command::Err) => {
                                self.cursor.move_down(&mut self.stdout, 1);
                                self.print_str("Unknown command");
                                self.newline();
                            }
                            Action::Command(Command::Help) => {
                                self.cursor.move_down(&mut self.stdout, 1);
                                self.print_str("< HELP COMMAND >");
                                self.newline();
                            }
                            Action::WriteLine(msg) => {
                                self.cursor.move_down(&mut self.stdout, 1);
                                self.print_multiline(msg.as_str());
                                self.cursor.move_down(&mut self.stdout, 1);
                                self.reading = false;
                                self.newline();
                            }
                            Action::Exit |
                            Action::Command(Command::Exit) => break,
                            _ => {}
                        }
                    }
                    Err(err) => {
                        self.print_multiline(format!("{:?}", err).as_str());
                        break;
                    }
                    _ => {}
                }
            }
        }
        write!(self.stdout,
               "{}{}{}{}",
               termion::style::Reset,
               termion::clear::All,
               termion::cursor::Goto(1, 1),
               termion::cursor::Show)
                .unwrap();
        self.flush();
    }

    fn print_str(&mut self, output: &str) {
        write!(self.stdout, "{}", output).unwrap();
        if let Action::Newline = self.cursor
               .move_right(&mut self.stdout, output.len() as u16) {
            self.newline();
        }
        self.flush();
    }

    fn print_char(&mut self, output: char) {
        write!(self.stdout, "{}", output).unwrap();
        if let Action::Newline = self.cursor.move_right(&mut self.stdout, 1) {
            self.newline();
        }
        self.flush();
    }

    /// take a multiline str and output it in the terminal properly
    fn print_multiline(&mut self, output: &str) {
        for line in output.lines() {
            self.cursor.move_down(&mut self.stdout, 1);
            write!(self.stdout, "{}", line).unwrap();
        }
    }

    fn newline(&mut self) {
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
        self.cursor.effect_on = true;
    }

    fn delete(&mut self) {
        self.cursor.column -= 1;
        write!(self.stdout,
               "{} {}",
               termion::cursor::Goto(self.cursor.column, self.cursor.row),
               termion::cursor::Goto(self.cursor.column, self.cursor.row))
                .unwrap();
        self.flush();
    }

    fn parse_input(&mut self, input: Key) -> Action {
        match input {
            Key::Char(c) => {
                if c != '\n' || c != '\r' {
                    self.print_char(c);
                }
                self.interpreter.read(c)
            }
            Key::Backspace => {
                if self.cursor.column > 5 {
                    self.delete();
                    self.interpreter.del();
                }
                Action::Continue
            }
            Key::Ctrl('d') | Key::Esc => Action::Exit,
            Key::Ctrl('c') => Action::Discard,
            _ => Action::Continue,
        }
    }

    fn flush(&mut self) {
        self.stdout.flush().unwrap();
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

impl Cursor {
    fn new() -> Cursor {
        Cursor {
            row: 1,
            column: 1,
            space: termion::terminal_size().unwrap(),
            time: time::Instant::now(),
            show: true,
            effect_on: true,
        }
    }

    fn move_down<O: Write>(&mut self, stdout: &mut O, pos: u16) {
        if self.row + pos > self.space.1 {
            //print!("(MOVED:{})", pos);
            write!(stdout, "{}", termion::scroll::Up(pos)).unwrap();
        }
        self.row += pos;
        write!(stdout, "{}", termion::cursor::Goto(1, self.row)).unwrap();
        stdout.flush().unwrap();
    }

    fn move_right<O: Write>(&mut self, stdout: &mut O, pos: u16) -> Action {
        if self.column + pos > self.space.0 {
            return Action::Newline;
        } else {
            self.column += pos;
        }
        write!(stdout, "{}", termion::cursor::Goto(self.column, self.row)).unwrap();
        stdout.flush().unwrap();
        Action::None
    }

    fn cursor_effect<O: Write>(&mut self, stdout: &mut O) {
        if !self.effect_on {
            return;
        }
        let nt = time::Instant::now();
        if nt.duration_since(self.time.clone()) >= time::Duration::new(0, 500000000) {
            match self.show {
                true => {
                    self.show = false;
                    self.hide(stdout);
                }
                false => {
                    self.show = true;
                    self.show(stdout);
                }
            }
            self.time = nt;
        }
    }

    fn show<O: Write>(&self, stdout: &mut O) {
        write!(stdout, "{}", termion::cursor::Show).unwrap();
        stdout.flush().unwrap();
    }

    fn hide<O: Write>(&self, stdout: &mut O) {
        write!(stdout, "{}", termion::cursor::Hide).unwrap();
        stdout.flush().unwrap();
    }
}

enum Command {
    Help,
    Exit,
    Err,
}

impl<'a> From<&'a str> for Command {
    fn from(command: &'a str) -> Command {
        match command {
            "help" => Command::Help,
            "quit" => Command::Exit,
            _ => Command::Err,
        }
    }
}

enum Action {
    Read,
    WriteLine(String),
    Continue,
    Exit,
    Discard,
    Newline,
    Command(Command),
    None,
}

struct SimagInter<'a> {
    state: Agent,
    result: Option<Answer<'a>>,
    reading: bool,
    ask: bool,
    code_input: String,
    command: String,
}

impl<'a> SimagInter<'a> {
    fn new() -> SimagInter<'a> {
        SimagInter {
            state: Agent::default(),
            result: None,
            reading: false,
            code_input: String::new(),
            command: String::new(),
            ask: false,
        }
    }

    fn eval(&mut self) -> Result<Action, String> {
        let mut input = String::new();
        mem::swap(&mut self.code_input, &mut input);
        if !self.ask {
            match self.state.tell(input) {
                Err(errors) => Err(format!("{}", errors.get(0).unwrap())),
                Ok(_) => Ok(Action::Continue),
            }
        } else {
            if let Some(r) = match self.state.ask(input) {
                   Err(QueryErr::ParseErr(_)) |
                   Err(QueryErr::QueryErr) => None,
                   Ok(result) => unsafe {
                let answ = mem::transmute::<Answer, Answer<'a>>(result);
                Some(answ)
            },
               } {
                self.result = Some(r);
                Ok(Action::Continue)
            } else {
                self.result = None;
                Err("Incorrect query".to_string())
            }
        }
    }
}

impl<'a> Interpreter for SimagInter<'a> {
    fn read(&mut self, input: char) -> Action {
        match input {
            '\n' | '\r' => {
                if self.reading && !self.code_input.ends_with("\n") {
                    self.code_input.push('\n');
                    Action::Read
                } else if self.reading {
                    self.reading = false;
                    let action = if let Err(msg) = self.eval() {
                        Action::WriteLine(msg)
                    } else {
                        Action::Newline
                    };
                    self.ask = false;
                    action
                } else if !self.command.trim().is_empty() {
                    let cmd = Action::Command(Command::from(self.command.as_str()));
                    self.command = String::new();
                    cmd
                } else {
                    Action::Newline
                }
            }
            '(' if !self.reading => {
                self.code_input.push('(');
                self.reading = true;
                Action::Continue
            }
            '?' if !self.reading => {
                self.ask = true;
                self.reading = true;
                Action::Continue
            }
            c => {
                if self.reading {
                    self.code_input.push(c);
                } else {
                    self.command.push(c);
                }
                Action::Continue
            }
        }
    }

    fn del(&mut self) {
        if self.reading {
            self.code_input.pop();
        } else {
            self.command.pop();
        }
    }
}

trait Interpreter {
    fn read(&mut self, input: char) -> Action;
    fn del(&mut self);
}

fn main() {
    let stdout = stdout();
    let stdout = stdout.lock().into_raw_mode().unwrap();
    let stdin = termion::async_stdin();
    let interpreter = SimagInter::new();
    let mut term = TermInterface::new(stdin, stdout, interpreter);
    write!(term.stdout,
           "{}{}{}",
           termion::clear::BeforeCursor,
           termion::cursor::Goto(1, 1),
           INFO)
            .unwrap();
    term.newline();
    term.read();
}

