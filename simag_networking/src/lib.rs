//! Support networking library for the simAG framework
#![allow(clippy::type_complexity)]
use handle::HandleError;

mod channel;
mod config;
mod handle;
mod message;
mod network;
mod rpc;

pub use network::{NetworkBuilder, Provider};

pub mod prelude {
    pub use super::handle::{HandleError, NetworkHandle};
    pub use super::network::{NetworkBuilder, Provider};
    pub use super::rpc::Resource;
    pub use super::Error;
    pub use libp2p::{identity::ed25519::Keypair, PeerId};
}

pub type Result<R> = std::result::Result<R, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    OpError(#[from] HandleError),
}
