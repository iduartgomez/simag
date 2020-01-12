//! A REPL (Read-Eval-Print-Loop) terminal interface for the language

use simag_core::SimagInterpreter;
use simag_term::Terminal;

const INFO: &str = "Simag Logic Lang 0.0.1 Interpreter\nType \"help\" for more information";

struct SimagRepl<'a> {
    terminal: Terminal<SimagInterpreter<'a>>,
}

impl<'a> SimagRepl<'_> {
    fn new(interpreter: SimagInterpreter<'a>) -> SimagRepl<'a> {
        SimagRepl {
            terminal: Terminal::new(interpreter),
        }
    }
}

fn main() {
    let interpreter = SimagInterpreter::new();
    let mut repl = SimagRepl::new(interpreter);
    repl.terminal.print_multiline(INFO, true);
    repl.terminal.start_event_loop();
}
