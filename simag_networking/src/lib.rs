//! Support networking library for the simAG framework
#![allow(clippy::type_complexity)]

mod config;
mod handle;
mod network;
mod stream;

pub use network::{NetworkBuilder, Provider};

pub mod prelude {
    pub use super::handle::NetworkHandle;
    pub use super::network::{AgentKey, NetworkBuilder, Provider};
    pub use libp2p::identity::ed25519::Keypair;
}
