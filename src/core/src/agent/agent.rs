use super::kb;
use lang::ParseErrF;

pub struct Agent<'a> {
    representation: kb::Representation<'a>
}

impl<'a> Agent<'a> {
    pub fn new() -> Agent<'a> {
        Agent { representation: kb::Representation::new() }
    }

    pub fn ask(&'a self, source: String, single_answer: bool) -> kb::Answer {
        self.representation.ask(source, single_answer)
    }

    pub fn tell(&'a self, source: String) -> Result<(), Vec<ParseErrF>> {
        self.representation.tell(source)
    }
}
