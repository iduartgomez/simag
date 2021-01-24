//! A REPL (Read-Eval-Print-Loop) terminal interface for the language
use simag_term_utils::{Application, SimagInterpreter};

const INFO: &str = "Simag Logic Lang 0.0.1 Interpreter\nType \"help\" for more information";

struct SimagRepl<'a> {
    terminal: Application<'a, SimagInterpreter<'a>>,
}

impl<'a> SimagRepl<'_> {
    fn new(interpreter: SimagInterpreter<'a>) -> SimagRepl<'a> {
        SimagRepl {
            terminal: Application::new(interpreter),
        }
    }
}

pub fn init_app() {
    let app = SimagInterpreter::new();
    let mut repl = SimagRepl::new(app);
    repl.terminal.print_text(INFO);
    repl.terminal.start_event_loop().unwrap();
}

fn main() {
    init_app()
}
