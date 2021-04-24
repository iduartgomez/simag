use std::path::Path;

pub(self) mod config;
mod kb;
mod lang;
#[cfg(feature = "persistence")]
mod storage;

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
    /// # Panic
    /// If the `persistence` cfg feature is enable will panic if there are any failures
    /// while openning files.
    pub fn new(id: String) -> Agent {
        let threads = num_cpus::get();
        #[cfg(feature = "persistence")]
        let representation = kb::repr::Representation::new(threads).unwrap();
        #[cfg(not(feature = "persistence"))]
        let representation = kb::repr::Representation::new(threads);

        Agent {
            id,
            thread_manager: ThreadManager { threads },
            representation,
        }
    }

    #[cfg(feature = "persistence")]
    pub fn load_from_disc(id: String, path: &Path) -> std::io::Result<Agent> {
        let threads = num_cpus::get();
        let representation = kb::repr::Representation::load_from_disc(Some(threads), path)?;
        Ok(Agent {
            id,
            thread_manager: ThreadManager { threads },
            representation,
        })
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

    /// Persistance enables eventual consistency between in-memory state and
    /// the physical storage persisted state.
    ///
    /// Agents are memory-first data structures, and in-memory ops take preference over
    /// secondary on-disk persistence. This means that in the event of crashes there
    /// could be an inconsistency introduced if an agent is recreated from any persisted
    /// data.
    #[cfg(feature = "persistence")]
    pub fn with_persistance(mut self) -> Self {
        self.representation.enable_persistence();
        self
    }

    #[cfg(feature = "persistence")]
    pub fn enable_persistance(&mut self) -> &mut Self {
        self.representation.enable_persistence();
        self
    }

    #[cfg(feature = "persistence")]
    pub fn disable_persistance(&mut self) {
        self.representation.disable_persistence();
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
