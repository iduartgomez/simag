pub(self) mod conf;
mod kb;
mod lang;
pub(self) mod time_semantics;

pub(crate) use self::kb::repr::Representation;
pub use self::kb::{
    repl::SimagInterpreter,
    repr::{Answer, QueryErr},
};
pub(self) use self::lang::ParseErrF;

/// Represent an alive object which can interact with environment.
///
/// Is the core construct of the simAG framework.
pub struct Agent {
    /// available threads for this agent
    thread_manager: ThreadManager,
    representation: kb::repr::Representation,
}

impl Agent {
    pub fn new(threads: usize) -> Agent {
        let representation = kb::repr::Representation::new(threads);
        Agent {
            thread_manager: ThreadManager { threads },
            representation,
        }
    }

    pub fn ask(&mut self, source: &str) -> Result<kb::repr::Answer, kb::repr::QueryErr> {
        self.representation.ask(source)
    }

    pub fn tell(&mut self, source: &str) -> Result<(), Vec<ParseErrF>> {
        self.representation.tell(source)
    }

    pub fn set_thread_pool(&mut self, threads: usize) {
        self.thread_manager.threads = threads;
        self.representation.set_threads(threads);
    }
}

impl Default for Agent {
    fn default() -> Agent {
        Agent::new(num_cpus::get())
    }
}

#[derive(Debug)]
struct ThreadManager {
    threads: usize,
}
