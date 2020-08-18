//! Support networking library for the simAG framework
mod config;
mod handle;
mod network;

pub use network::{NetworkBuilder, Provider};

pub mod prelude {
    pub use super::handle::NetworkHandle;
    pub use super::network::{AgentKey, NetworkBuilder, Provider};
    pub use libp2p::identity::ed25519::Keypair;
}
