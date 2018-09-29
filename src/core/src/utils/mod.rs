use std::mem;

use agent::{Answer, QueryErr, Representation};

#[derive(Default)]
pub struct SimagInterpreter<'a> {
    state: Representation,
    result: Option<Answer<'a>>,
    reading: bool,
    ask: bool,
    source: String,
    command: String,
}

impl<'a> SimagInterpreter<'a> {
    pub fn new() -> SimagInterpreter<'a> {
        SimagInterpreter {
            state: Representation::default(),
            result: None,
            reading: false,
            source: String::new(),
            command: String::new(),
            ask: false,
        }
    }

    fn eval(&mut self) -> Result<Action, String> {
        let mut input = String::new();
        mem::swap(&mut self.source, &mut input);
        if !self.ask {
            match self.state.tell(input) {
                Err(errors) => Err(format!("{}", &errors[0])),
                Ok(_) => Ok(Action::Continue),
            }
        } else {
            input.remove(0);
            if let Some(r) = match self.state.ask(input) {
                Err(QueryErr::ParseErr(_)) | Err(QueryErr::QueryErr) => None,
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

impl<'a> Interpreter for SimagInterpreter<'a> {
    fn read(&mut self, input: char) -> Action {
        match input {
            '\n' => {
                if self.reading && !self.source.ends_with('\n') {
                    self.source.push('\n');
                    Action::Read
                } else if self.reading {
                    self.reading = false;
                    let action = if let Err(msg) = self.eval() {
                        Action::Write(msg)
                    } else {
                        Action::Newline
                    };
                    self.ask = false;
                    action
                } else if !self.command.trim().is_empty() {
                    let mut command = String::new();
                    mem::swap(&mut self.command, &mut command);
                    Action::Command(command)
                } else {
                    Action::Newline
                }
            }
            '(' if !self.reading => {
                self.source.push('(');
                self.reading = true;
                Action::Continue
            }
            '?' if !self.reading => {
                self.source.push('?');
                self.ask = true;
                self.reading = true;
                Action::Continue
            }
            c => {
                if self.reading {
                    self.source.push(c);
                } else {
                    self.command.push(c);
                }
                Action::Continue
            }
        }
    }

    fn delete_last(&mut self) -> Option<Action> {
        if self.reading {
            self.source.pop();
            if self.source.is_empty() {
                self.ask = false;
                self.reading = false;
                Some(Action::Discard)
            } else {
                None
            }
        } else {
            self.command.pop();
            if self.command.is_empty() {
                Some(Action::Discard)
            } else {
                None
            }
        }
    }
}

pub trait Interpreter {
    fn read(&mut self, input: char) -> Action;
    fn delete_last(&mut self) -> Option<Action>;
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
