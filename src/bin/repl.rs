//! A REPL (Read-Eval-Print-Loop) terminal interface for the language

extern crate termion;
extern crate simag_core;

use std::io::{Read, Write, Bytes, stdout};
use std::time;

use simag_core::utils::{Interpreter, SimagInterpreter, Action};
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
                if c == b'\x1B' {
                    match self.sequence() {
                        Ok(action) => {
                            if let Some(Action::Exit) = self.exec_action(action) {
                                break;
                            }
                        }
                        Err(val) => {
                            if let Ok(event) = parse_event(val, &mut self.stdin) {
                                if let Some(Action::Exit) = self.parse_event(event) {
                                    break;
                                }
                            }
                        }
                    }
                } else {
                    if let Ok(event) = parse_event(c, &mut self.stdin) {
                        if let Some(Action::Exit) = self.parse_event(event) {
                            break;
                        }
                    }
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

    fn parse_event(&mut self, event: Event) -> Option<Action> {
        if let Event::Key(key) = event {
            let action = match key {
                Key::Char(c) => {
                    if c != '\n' {
                        self.print_char(c);
                    }
                    self.interpreter.read(c)
                }
                Key::Backspace => {
                    if self.cursor.column > 5 {
                        self.delete();
                        match self.interpreter.delete_last() {
                            Some(Action::Discard) => {
                                self.cursor.effect_on = true;
                                self.reading = false;
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
        let mut combo = &mut Combo { buffered: vec![] };
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
                Some(Ok(c @ b'0'...b'9')) if end => {
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
               self.parse_event(event)
                   .map_or_else(|| Action::None, |x| x)
           } else {
               Action::None
           })
    }

    fn exec_action(&mut self, action: Action) -> Option<Action> {
        match action {
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
            Action::Command(cmd) => {
                self.cursor.move_down(&mut self.stdout, 1);
                match Command::from(cmd.as_str()) {
                    Command::Err => self.print_str("Unknown command"),
                    Command::Help => self.print_str("< HELP COMMAND >"),
                    Command::Exit => return Some(Action::Exit),
                }
                self.newline();
            }
            Action::Write(msg) => {
                self.cursor.move_down(&mut self.stdout, 1);
                self.print_multiline(msg.as_str());
                self.cursor.move_down(&mut self.stdout, 1);
                self.reading = false;
                self.newline();
            }
            Action::Exit => return Some(Action::Exit),
            _ => {}
        }
        None
    }

    fn print_str(&mut self, output: &str) {
        write!(self.stdout, "{}", output).unwrap();
        if let Action::Newline = self.cursor
               .move_right(&mut self.stdout, output.len() as u16) {
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

    fn flush(&mut self) {
        self.stdout.flush().unwrap();
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

fn main() {
    let stdout = stdout();
    let stdout = stdout.lock().into_raw_mode().unwrap();
    let stdin = termion::async_stdin();
    let interpreter = SimagInterpreter::new();
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

