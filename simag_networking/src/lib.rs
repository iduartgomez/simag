//! Support networking library for the simAG framework
#![allow(clippy::type_complexity)]

mod channel;
mod config;
mod handle;
mod message;
mod network;

pub use network::{NetworkBuilder, Provider};

pub mod prelude {
    pub use super::handle::NetworkHandle;
    pub use super::network::{NetworkBuilder, Provider, Resource, ResourceIdentifier};
    pub use libp2p::{identity::ed25519::Keypair, PeerId};
}
