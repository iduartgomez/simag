mod bms;
mod kb;

pub(crate) use self::bms::BmsWrapper;
pub(crate) use self::kb::{Representation, VarAssignment};

pub use self::kb::{Answer, QueryErr};
pub use lang::ParseErrF;

/// Represent an alive object which can interact with environment.
///
/// Is the core construct of the simAG framework.
#[derive(Debug)]
pub struct Agent {
    /// available threads for this agent
    threads: usize,
    representation: kb::Representation,
}

impl Agent {
    pub fn new(threads: usize) -> Agent {
        Agent {
            threads,
            representation: kb::Representation::new(),
        }
    }

    pub fn ask(&self, source: String) -> Result<kb::Answer, kb::QueryErr> {
        self.representation.ask(source)
    }

    pub fn tell(&mut self, source: String) -> Result<(), Vec<ParseErrF>> {
        self.representation.tell(source)
    }
}

impl Default for Agent {
    fn default() -> Agent {
        use num_cpus;
        Agent::new(num_cpus::get())
    }
}
