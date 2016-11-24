use super::kb;
use lang::ParseErrF;

pub struct Agent {
    representation: kb::Representation
}

impl Agent {
    pub fn new() -> Agent {
        Agent { representation: kb::Representation::new() }
    }

    pub fn ask(&self, source: String) -> kb::Answer {
        self.representation.ask(source)
    }

    pub fn tell(&self, source: String) -> Result<(), Vec<ParseErrF>> {
        self.representation.tell(source)
    }
}
