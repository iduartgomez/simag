use simag_term::Action;
use std::mem;

use crate::agent::{Answer, QueryErr, Representation};

use simag_term::Interpreter;

const HELP_COMMAND: &str = "\
Welcome to the interactive Simag 0.0.1 interpreter!

You can start feeding information for the interpreter by writing syntactically valid expressions.
For querying the interpreter just preceed your expression query with a ?. For more info on 
the query expression operator write \"help queries\".

To quit this utility just write \"quit\" or \"exit\". For a complete list of commands write 
\"help commands\".
";

const HELP_COMMANDS: &str = "\
List of valid commands:

* help > the help command
* help commands > this command, prints info about commands
";

const HELP_QUERYING: &str = "\
For querying just preceed your query with ?. If the query is valid you can explore the results 
using ??<expr>, substitue <expr> for a valid expression for the ?? operator:

> ??single > return the global result for the query
> ??multi <expr> > return the result for this part of the query
";

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
            match self.state.tell(&input) {
                Err(errors) => Err(format!("{}", &errors[0])),
                Ok(_) => Ok(Action::Continue),
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
                Ok(Action::Write("Query executed succesfully".to_owned()))
            } else {
                self.result = None;
                Err("Incorrect query".to_string())
            }
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

impl<'a> Interpreter for SimagInterpreter<'a> {
    fn read(&mut self, input: char) -> Action {
        match input {
            '\n' => {
                if self.reading && !self.source.ends_with('\n') {
                    self.source.push('\n');
                    Action::Read
                } else if self.reading {
                    self.reading = false;
                    let action = match self.eval() {
                        Ok(Action::Write(p)) => Action::Write(p),
                        Err(msg) => Action::Write(msg),
                        _ => Action::Newline,
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

    fn cmd_executor(&mut self, command: String) -> Option<Action> {
        match Command::from(command.as_str()) {
            Command::Err => Some(Action::WriteStr("Unknown command")),
            Command::Help => Some(Action::WriteMultiStr(HELP_COMMAND)),
            Command::HelpCommands => Some(Action::WriteMultiStr(HELP_COMMANDS)),
            Command::HelpQuerying => Some(Action::WriteMultiStr(HELP_QUERYING)),
            Command::Query(ResultQuery::Single) => match self.query_result(ResultQuery::Single) {
                Ok(result) => Some(Action::WriteMulti(result)),
                Err(msg) => Some(Action::WriteMulti(msg)),
            },
            Command::Query(ResultQuery::Multiple) => unimplemented!(),
            Command::Exit => Some(Action::Exit),
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
