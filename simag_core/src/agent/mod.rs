pub(self) mod config;
mod kb;
mod lang;

pub use self::kb::repl::SimagInterpreter;
pub(crate) use self::kb::repr::Representation;
pub use self::kb::repr::{Answer, QueryErr};
pub(self) use self::lang::ParseErrF;

/// Represent an alive object which can interact with environment.
///
/// Is the core construct of the simAG framework.
pub struct Agent {
    id: String,
    /// available threads for this agent
    thread_manager: ThreadManager,
    representation: kb::repr::Representation,
}

impl Agent {
    pub fn new(id: String) -> Agent {
        let threads = num_cpus::get();
        let representation = kb::repr::Representation::new(threads);
        Agent {
            id,
            thread_manager: ThreadManager { threads },
            representation,
        }
    }

    pub fn ask(&self, source: &str) -> Result<kb::repr::Answer, kb::repr::QueryErr> {
        self.representation.ask(source)
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn tell(&self, source: &str) -> Result<(), Vec<ParseErrF>> {
        self.representation.tell(source)
    }

    /// Change the current thread pool size. Default size is the number of logical CPUs.
    pub fn thread_pool_size(&mut self, threads: usize) {
        self.thread_manager.threads = threads;
        self.representation.set_threads(threads);
    }

    pub fn with_threads(mut self, threads: usize) -> Self {
        self.thread_pool_size(threads);
        self
    }

    /// Clean up all the knowledge of the agent.
    pub fn clear(&mut self) {
        self.representation.clear()
    }
}

impl Default for Agent {
    fn default() -> Agent {
        let id = uuid::Uuid::new_v4();
        Agent::new(id.to_string())
    }
}

struct ThreadManager {
    threads: usize,
}
