mod bms;
mod kb;

pub(crate) use self::bms::BmsWrapper;
pub(crate) use self::kb::{Representation, VarAssignment};

pub use self::kb::{Answer, QueryErr};
pub use crate::lang::ParseErrF;

/// Represent an alive object which can interact with environment.
///
/// Is the core construct of the simAG framework.
#[derive(Debug)]
pub struct Agent {
    /// available threads for this agent
    thread_manager: ThreadManager,
    representation: kb::Representation,
}

impl Agent {
    pub fn new(threads: usize) -> Agent {
        Agent {
            thread_manager: ThreadManager { threads },
            representation: kb::Representation::new().with_threads(threads),
        }
    }

    pub fn ask(&self, source: &str) -> Result<kb::Answer, kb::QueryErr> {
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
        use num_cpus;
        Agent::new(num_cpus::get())
    }
}

#[derive(Debug)]
struct ThreadManager {
    threads: usize,
}
