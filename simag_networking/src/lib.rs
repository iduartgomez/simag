//! Support networking library for the simAG framework
mod config;
mod network;

pub use network::{BootstrapListener, Network};

pub mod prelude {
    pub use super::{BootstrapListener, Network};
    pub use libp2p::identity::ed25519::Keypair;
}
