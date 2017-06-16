mod kb;
mod bms;

pub(crate) use self::kb::{VarAssignment, Representation};
pub(crate) use self::bms::{BmsWrapper};

pub use self::kb::{Answer, QueryErr};
pub use lang::ParseErrF;

/// Represent an alive object which can interact with environment.
///
/// Is the core construct of the simAG framework.
#[derive(Debug)]
pub struct Agent {
    representation: kb::Representation
}

impl Agent {
    pub fn new() -> Agent {
        Agent { representation: kb::Representation::new() }
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
        Agent::new()
    }
}
