use std::mem;

use super::*;
use crate::agent::{Answer, QueryErr, Representation};
use simag_terminal::{Action, ReplInterpreter};

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

    fn query_result(&self, _query: ResultQuery) -> Result<String, String> {
        if let Some(ref result) = self.result {
            if let Some(r) = result.get_results_single() {
                Ok(r.to_string())
            } else {
                Err("Query didn't return any results".to_owned())
            }
        } else {
            Err("Result wasn't found".to_owned())
        }
    }
}

impl<'a> ReplInterpreter for SimagInterpreter<'a> {
    fn digest<'b, 'c: 'b>(&'b mut self, input: char) -> Action<'c> {
        match input {
            '\n' => {
                let action = self.newline_eval();
                let last_source = self.last_source_input();
                if self.reading && last_source.is_some() && last_source.unwrap() == '\n' {
                    self.reading = false;
                } else if self.reading {
                    self.source.push('\n');
                }
                action
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
            '?' if self.reading && self.ask => {
                // Query the answer
                self.reading = false;
                self.ask = false;
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

    fn is_reading(&self) -> bool {
        self.reading
    }

    fn delete_last<'b, 'c: 'b>(&'b mut self) -> Option<Action<'c>> {
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

    fn drop_command(&mut self) {
        self.command.truncate(0);
    }

    fn cmd_executor<'b, 'c: 'b>(&'b mut self, command: String) -> Option<Action<'c>> {
        match Command::from(command.as_str()) {
            Command::Err => Some(Action::WriteStr(("Unknown command", true))),
            Command::Help => Some(Action::WriteMultiStr((HELP_COMMAND, true))),
            Command::HelpCommands => Some(Action::WriteMultiStr((HELP_COMMANDS, true))),
            Command::HelpQuerying => Some(Action::WriteMultiStr((HELP_QUERYING, true))),
            Command::Query(ResultQuery::Single) => match self.query_result(ResultQuery::Single) {
                Ok(result) => Some(Action::WriteMulti((result, true))),
                Err(msg) => Some(Action::WriteMulti((msg, true))),
            },
            Command::Query(ResultQuery::Multiple) => todo!(),
            Command::Exit => Some(Action::Exit),
        }
    }

    fn queued_command(&mut self) -> Option<String> {
        if !self.command.trim().is_empty() {
            let mut command = String::new();
            mem::swap(&mut self.command, &mut command);
            Some(command)
        } else {
            None
        }
    }

    fn set_reading(&mut self, currently_reading: bool) {
        self.reading = currently_reading;
    }

    fn last_source_input(&self) -> Option<char> {
        self.source.chars().last()
    }

    fn evaluate<'b, 'c: 'b>(&'b mut self) -> Result<Action<'c>, String> {
        let mut input = String::new();
        mem::swap(&mut self.source, &mut input);
        if !self.ask {
            match self.state.tell(&input) {
                Err(errors) => Err(format!("{}", &errors[0])),
                Ok(_) => Ok(Action::Continue),
            }
        } else {
            input.remove(0);
            if let Some(r) = match self.state.ask(&input) {
                Err(QueryErr::ParseErr(_)) | Err(QueryErr::QueryErr) => None,
                Ok(result) => unsafe {
                    // Safety: lives as long as self really because the references come from self.state
                    let answ = mem::transmute::<Answer, Answer<'a>>(result);
                    Some(answ)
                },
            } {
                self.result = Some(r);
                Ok(Action::WriteStr(("Query executed succesfully", true)))
            } else {
                self.result = None;
                Err("Incorrect query".to_string())
            }
        }
    }
}

enum Command {
    Help,
    HelpCommands,
    HelpQuerying,
    Exit,
    Query(ResultQuery),
    Err,
}

impl<'a> From<&'a str> for Command {
    fn from(command: &'a str) -> Command {
        match command {
            "help" => Command::Help,
            "help commands" => Command::HelpCommands,
            "help queries" => Command::HelpQuerying,
            "quit" | "exit" => Command::Exit,
            "single" => Command::Query(ResultQuery::Single),
            "multi" => Command::Query(ResultQuery::Multiple),
            _ => Command::Err,
        }
    }
}

enum ResultQuery {
    Single,
    Multiple,
}
