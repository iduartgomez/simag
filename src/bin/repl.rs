//! A REPL (Read-Eval-Print-Loop) terminal interface for the language

extern crate simag_core;
extern crate termion;

use std::io::Bytes;
use termion::AsyncReader;

use simag_core::utils::{Interpreter, InterpreterAction, ResultQuery, SimagInterpreter};
use simag_terminals::{Cursor, Terminal, TerminalState};

const INFO: &str = "Simag Logic Lang 0.0.1 Interpreter\nType \"help\" for more information";

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

struct TermInterface<E>
where
    E: Interpreter,
{
    interpreter: E,
    cursor: Cursor,
    state: TerminalState,
}

impl<E> TermInterface<E>
where
    E: Interpreter,
{
    fn new(interpreter: E) -> TermInterface<E> {
        TermInterface {
            cursor: Cursor::new(),
            interpreter,
            state: TerminalState::new(),
        }
    }
}

impl<E> Terminal<Bytes<AsyncReader>> for TermInterface<E>
where
    E: Interpreter,
{
    fn get_state_mut(&mut self) -> &mut TerminalState {
        &mut self.state
    }

    fn get_cursor(&mut self) -> &mut Cursor {
        &mut self.cursor
    }

    fn get_state(&self) -> &TerminalState {
        &self.state
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

fn main() {
    let interpreter = SimagInterpreter::new();
    let mut term = TermInterface::new(interpreter);
    term.set_up();
    term.print_multiline(INFO);
    term.newline();
    term.read();
}
