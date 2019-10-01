use std::mem;

use crate::agent::{Answer, QueryErr, Representation};

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

    fn eval(&mut self) -> Result<InterpreterAction, String> {
        let mut input = String::new();
        mem::swap(&mut self.source, &mut input);
        if !self.ask {
            match self.state.tell(&input) {
                Err(errors) => Err(format!("{}", &errors[0])),
                Ok(_) => Ok(InterpreterAction::Continue),
            }
        } else {
            input.remove(0);
            if let Some(r) = match self.state.ask(&input) {
                Err(QueryErr::ParseErr(_)) | Err(QueryErr::QueryErr) => None,
                Ok(result) => unsafe {
                    let answ = mem::transmute::<Answer, Answer<'a>>(result);
                    Some(answ)
                },
            } {
                self.result = Some(r);
                Ok(InterpreterAction::Write("Query executed succesfully".to_owned()))
            } else {
                self.result = None;
                Err("Incorrect query".to_string())
            }
        }
    }
}

impl<'a> Interpreter for SimagInterpreter<'a> {
    fn read(&mut self, input: char) -> InterpreterAction {
        match input {
            '\n' => {
                if self.reading && !self.source.ends_with('\n') {
                    self.source.push('\n');
                    InterpreterAction::Read
                } else if self.reading {
                    self.reading = false;
                    let action = match self.eval() {
                        Ok(InterpreterAction::Write(p)) => InterpreterAction::Write(p),
                        Err(msg) => InterpreterAction::Write(msg),
                        _ => InterpreterAction::Newline,
                    };
                    self.ask = false;
                    action
                } else if !self.command.trim().is_empty() {
                    let mut command = String::new();
                    mem::swap(&mut self.command, &mut command);
                    InterpreterAction::Command(command)
                } else {
                    InterpreterAction::Newline
                }
            }
            '(' if !self.reading => {
                self.source.push('(');
                self.reading = true;
                InterpreterAction::Continue
            }
            '?' if !self.reading => {
                self.source.push('?');
                self.ask = true;
                self.reading = true;
                InterpreterAction::Continue
            }
            '?' if self.reading && self.ask => {
                // Query the answer
                self.reading = false;
                self.ask = false;
                InterpreterAction::Continue
            }
            c => {
                if self.reading {
                    self.source.push(c);
                } else {
                    self.command.push(c);
                }
                InterpreterAction::Continue
            }
        }
    }

    fn delete_last(&mut self) -> Option<InterpreterAction> {
        if self.reading {
            self.source.pop();
            if self.source.is_empty() {
                self.ask = false;
                self.reading = false;
                Some(InterpreterAction::Discard)
            } else {
                None
            }
        } else {
            self.command.pop();
            if self.command.is_empty() {
                Some(InterpreterAction::Discard)
            } else {
                None
            }
        }
    }

    fn query_result(&self, _query: ResultQuery) -> Result<InterpreterAction, String> {
        if let Some(ref result) = self.result {
            if let Some(r) = result.get_results_single() {
                Ok(InterpreterAction::Write(r.to_string()))
            } else {
                Err("Query didn't return any results".to_owned())
            }
        } else {
            Err("Result wasn't found".to_owned())
        }
    }
}

pub trait Interpreter {
    fn read(&mut self, input: char) -> InterpreterAction;
    fn delete_last(&mut self) -> Option<InterpreterAction>;
    fn query_result(&self, query: ResultQuery) -> Result<InterpreterAction, String>;
}

pub enum InterpreterAction {
    Read,
    Write(String),
    Continue,
    Exit,
    Discard,
    Newline,
    Command(String),
    None,
}

pub enum ResultQuery {
    Single,
    Multiple,
}
