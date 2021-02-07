//! A type that implements the behaviour of the input stream interpreter.
//!
//! There are two kind of evaluation dispositions for the interpreter:
//! * Command interpreter
//! * Source reading mode
//!
//! The second mode is optional and is set manually by the interpreter itself.
//! Eventually during the evaluation of newline context the default behaviour will check
//! its own disposition and evaluate the source if in reading mode. This allows to feed
//! the interpreter new source input as long as it wants (ie. until it finds some scape sequence)
//! and only then evalute the result of the source code.
//!
//! Command line interpreter must be implemented mandatorily, in the default implementation
//! the interpreter will be queued for new commands and then will require the interpreter
//! to execute the command.
use crate::Action;
use simag_core::{Agent, Answer, QueryErr};
use tui::text::Text;

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
* help queries > how to query the engine
* quit | exit > close down the program
* shortcuts > show available keyboard shortcuts
";

const HELP_QUERYING: &str = "\
For querying just preceed your query with ?. If the query is valid you can explore the results 
using ??<expr>, substitue <expr> for a valid expression for the ?? operator:

> ??single > return the global result for the query
> ??multi <expr> > return the result for this part of the query
";

pub trait ReplInterpreter {
    /// Last source code input char that was received by the interpreter.
    fn last_source_input(&self) -> Option<char>;

    /// Returns if the interpreter is currently receiving source stream
    /// or is parsing commmand.
    fn is_reading(&self) -> bool;

    /// Feed new input characters to the interpreter and return
    /// a new action after digestion.
    fn digest<'b, 'a: 'b>(&'b mut self, input: char) -> Action<'a>;

    /// Execute a command and return the continuation action if there
    /// is one.
    fn cmd_executor<'b, 'a: 'b>(&'b mut self, command: &str) -> Option<Action<'a>>;

    /// Drop last change of the input stream and return an optional action
    /// to be performed after rollingback last change.
    fn delete_last<'b, 'a: 'b>(&'b mut self) -> Option<Action<'a>>;

    /// Drops any undergoing write on the interpreter command buffer.
    fn drop_command(&mut self);

    /// Set current reading source disposition.
    fn set_reading(&mut self, currently_reading: bool);

    /// Evaluate the currently ingested source.
    fn evaluate<'b, 'a: 'b>(&'b mut self) -> Result<Action<'a>, String>;

    /// Return any queued command if there is one.
    fn queued_command(&mut self) -> Option<String>;

    /// Evaluate the special newline eval character.
    ///
    /// The default implementation will check the reading dispossition of the interpreter
    /// and issue an evaluation of the source if corresponding. Check the module details
    /// for more information.
    fn newline_eval<'b, 'a: 'b>(&'b mut self) -> Action<'a> {
        let currently_reading = self.is_reading();
        let last_input = self.last_source_input();
        if currently_reading && last_input.is_some() && last_input.unwrap() != '\n' {
            Action::Read
        } else if currently_reading {
            self.set_reading(false);
            match self.evaluate() {
                Ok(Action::WriteInputText(p)) => {
                    Action::WriteInputText(p).compose(Action::StopReading)
                }
                Err(msg) => Action::WriteInfoText(Text::from(msg)).compose(Action::StopReading),
                _ => Action::StopReading,
            }
        } else if let Some(command) = self.queued_command() {
            Action::Command(command)
        } else if last_input.is_some() && last_input.unwrap() == '\n' {
            Action::StopReading
        } else {
            Action::Newline
        }
    }
}

#[derive(Default)]
pub struct SimagInterpreter<'a> {
    state: Agent,
    result: Option<Answer<'a>>,
    reading: bool,
    ask: bool,
    source: String,
    command: String,
}

impl<'a> SimagInterpreter<'a> {
    pub fn new() -> SimagInterpreter<'a> {
        SimagInterpreter {
            state: Agent::default(),
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

    fn cmd_executor<'b, 'c: 'b>(&'b mut self, command: &str) -> Option<Action<'c>> {
        match Command::from(command) {
            Command::Err => Some(Action::WriteInfoText(Text::from("Unknown command"))),
            Command::Help => Some(Action::WriteInfoText(Text::from(HELP_COMMAND))),
            Command::HelpCommands => Some(Action::WriteInfoText(Text::from(HELP_COMMANDS))),
            Command::HelpQuerying => Some(Action::WriteInfoText(Text::from(HELP_QUERYING))),
            Command::Query(ResultQuery::Single) => match self.query_result(ResultQuery::Single) {
                Ok(result) => Some(Action::WriteInfoText(Text::from(result))),
                Err(msg) => Some(Action::WriteInfoText(Text::from(msg))),
            },
            Command::Query(ResultQuery::Multiple) => todo!(),
            Command::Exit => Some(Action::Exit),
        }
    }

    fn queued_command(&mut self) -> Option<String> {
        if !self.command.trim().is_empty() {
            let mut command = String::new();
            std::mem::swap(&mut self.command, &mut command);
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
        std::mem::swap(&mut self.source, &mut input);
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
                    // SAFETY: lives as long as self really because the references come from self.state
                    let answ = std::mem::transmute::<Answer, Answer<'a>>(result);
                    Some(answ)
                },
            } {
                self.result = Some(r);
                Ok(Action::WriteInfoText(Text::from(
                    "Query executed succesfully",
                )))
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
