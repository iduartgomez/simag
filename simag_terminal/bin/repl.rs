//! A REPL (Read-Eval-Print-Loop) terminal interface for the language
use simag_term_utils::SimagInterpreter;

const INFO: &str = "Simag Logic Lang 0.0.1 Interpreter\nType \"help\" for more information";

fn main() {
    if cfg!(target_family = "unix") {
        repl_unix::init_unix()
    } else {
        todo!("no repl supported yet for non-unix platforms!")
    }
}

#[cfg(all(target_family = "unix"))]
mod repl_unix {
    use super::*;
    use simag_term_utils::Terminal;

    struct SimagRepl<'a> {
        terminal: Terminal<'a, SimagInterpreter<'a>>,
    }

    impl<'a> SimagRepl<'_> {
        fn new(interpreter: SimagInterpreter<'a>) -> SimagRepl<'a> {
            SimagRepl {
                terminal: Terminal::new(interpreter),
            }
        }
    }

    pub fn init_unix() {
        let interpreter = SimagInterpreter::new();
        let mut repl = SimagRepl::new(interpreter);
        repl.terminal.print_multiline(INFO, true);
        repl.terminal.start_event_loop().unwrap();
    }
}
