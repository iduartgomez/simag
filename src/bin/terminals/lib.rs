use once_cell::sync::Lazy;
use std::io::{stdout, Bytes, Error, Read, Stdout, StdoutLock, Write};
use std::iter::Iterator;
use std::time;
use termion::{async_stdin, AsyncReader};

pub mod commands;

use termion::event::{parse_event, Event, Key};
use termion::raw::{IntoRawMode, RawTerminal};

fn get_raw_stdout() -> RawTerminal<StdoutLock<'static>> {
    static STDOUT: Lazy<Stdout> = Lazy::new(stdout);

    STDOUT.lock().into_raw_mode().unwrap()
}

fn get_stdin() -> Bytes<AsyncReader> {
    async_stdin().bytes()
}

#[derive(Default)]
pub struct TerminalState {
    reading: bool,
}

impl TerminalState {
    pub fn new() -> Self {
        TerminalState { reading: false }
    }
}

pub trait Terminal<I>
where
    I: Iterator<Item = Result<u8, Error>>,
{
    fn set_up(&self) {
        write!(
            get_raw_stdout(),
            "{}{}",
            termion::clear::BeforeCursor,
            termion::cursor::Goto(1, 1)
        )
        .unwrap();
    }

    fn read(&mut self) {
        loop {
            if self.get_cursor().effect_on {
                let stdout = &mut get_raw_stdout();
                self.get_cursor().cursor_effect(stdout);
            }
            if let Some(Ok(c)) = get_stdin().next() {
                if c == b'\x1B' {
                    match self.sequence() {
                        Ok(action) => {
                            if let Some(Action::Exit) = self.exec_action(action) {
                                break;
                            }
                        }
                        Err(val) => {
                            if let Ok(event) = parse_event(val, &mut get_stdin()) {
                                if let Some(Action::Exit) = self.parse_event(&event) {
                                    break;
                                }
                            }
                        }
                    }
                } else if let Ok(event) = parse_event(c, &mut get_stdin()) {
                    if let Some(Action::Exit) = self.parse_event(&event) {
                        break;
                    }
                }
            }
        }
        write!(
            get_raw_stdout(),
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
                    // FIXME: Actionable interpreter; check core/utils
                    //self.interpreter.read(*c)
                    Action::None
                }
                Key::Backspace => {
                    if self.get_cursor().column > 5 {
                        self.delete();
                        // FIXME: Actionable interpreter; check core/utils
                        /*
                        match self.interpreter.delete_last() {
                            Some(Action::Discard) => {
                                self.cursor.effect_on = true;
                                self.reading = false;
                                return Some(Action::None);
                            }
                            Some(other) => return Some(other),
                            None => {}
                        }
                        */
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
            match get_stdin().next() {
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
                let stdout = &mut get_raw_stdout();
                let cursor = self.get_cursor();
                cursor.effect_on = false;
                cursor.show(stdout);
            }
            Action::Read => {
                self.get_state_mut().reading = true;
                self.newline();
            }
            Action::Newline => {
                self.get_state_mut().reading = false;
                self.newline();
            }
            Action::Discard => {
                let stdout = &mut get_raw_stdout();
                self.get_cursor().move_down(stdout, 1);
                self.newline();
            }
            Action::Command(cmd) => {
                let stdout = &mut get_raw_stdout();
                self.get_cursor().move_down(stdout, 1);
                // FIXME: Actionable interpreter; check core/utils
                /*
                match Command::from(cmd.as_str()) {
                    Command::Err => self.print_str("Unknown command"),
                    Command::Help => self.print_multiline(HELP_COMMAND),
                    Command::HelpCommands => self.print_multiline(HELP_COMMANDS),
                    Command::HelpQuerying => self.print_multiline(HELP_QUERYING),
                    Command::Query(ResultQuery::Single) => {
                        match self.interpreter.query_result(ResultQuery::Single) {
                            Ok(Action::Write(result)) => {
                                self.print_multiline(result.as_str());
                            }
                            Ok(action) => return self.exec_action(action),
                            Err(msg) => {
                                self.print_multiline(msg.as_str());
                            }
                        }
                        self.newline();
                        return Some(Action::Continue);
                    }
                    Command::Query(ResultQuery::Multiple) => unimplemented!(),
                    Command::Exit => return Some(Action::Exit),
                }
                */
                self.newline();
            }
            Action::Write(msg) => {
                self.print_multiline(msg.as_str());
                self.get_state_mut().reading = false;
                self.newline();
            }
            Action::Exit => return Some(Action::Exit),
            _ => {}
        }
        None
    }

    fn print_str(&mut self, output: &str) {
        let mut stdout = get_raw_stdout();
        let cursor = self.get_cursor();

        write!(stdout, "{}", output).unwrap();
        if let Action::Newline = cursor.move_right(&mut stdout, output.len() as u16) {
            cursor.move_down(&mut stdout, 1);
            self.print_str(output);
            return;
        }
        self.flush();
    }

    fn print_char(&mut self, output: char) {
        let mut stdout = get_raw_stdout();
        let cursor = self.get_cursor();

        write!(stdout, "{}", output).unwrap();
        if let Action::Newline = cursor.move_right(&mut stdout, 1) {
            cursor.move_down(&mut stdout, 1);
            self.print_char(output);
            return;
        }
        self.flush();
    }

    /// take a multiline str and output it in the terminal properly
    fn print_multiline(&mut self, output: &str) {
        let mut stdout = get_raw_stdout();
        let cursor = self.get_cursor();

        for (i, line) in output.lines().enumerate() {
            if i != 0 {
                cursor.move_down(&mut stdout, 1);
            }
            write!(stdout, "{}", line).unwrap();
        }
        self.flush()
    }

    fn newline(&mut self) {
        let mut stdout = get_raw_stdout();
        {
            let mut cursor = self.get_cursor();
            cursor.move_down(&mut stdout, 1);
            cursor.column = 5;
        }
        if self.get_state().reading {
            write!(
                stdout,
                "{}... {}",
                termion::style::Bold,
                termion::style::Reset
            )
            .unwrap();
        } else {
            write!(
                stdout,
                "{}>>> {}",
                termion::style::Bold,
                termion::style::Reset
            )
            .unwrap();
        }
        self.flush();
        self.get_cursor().effect_on = true;
    }

    fn delete(&mut self) {
        let mut stdout = get_raw_stdout();
        let mut cursor = self.get_cursor();

        cursor.column -= 1;
        write!(
            stdout,
            "{} {}",
            termion::cursor::Goto(cursor.column, cursor.row),
            termion::cursor::Goto(cursor.column, cursor.row)
        )
        .unwrap();
        self.flush();
    }

    fn flush(&mut self) {
        get_raw_stdout().flush().unwrap();
    }

    fn get_state_mut(&mut self) -> &mut TerminalState;
    fn get_cursor(&mut self) -> &mut Cursor;
    fn get_state(&self) -> &TerminalState;
}

pub struct Cursor {
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

pub enum Action {
    Read,
    Write(String),
    Continue,
    Exit,
    Discard,
    Newline,
    Command(String),
    None,
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
