//! Support networking library for the simAG framework
mod config;
mod network;

pub use network::{Network, Provider};

pub mod prelude {
    pub use super::{Network, Provider};
    pub use libp2p::identity::ed25519::Keypair;
}
