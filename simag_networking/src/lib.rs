//! Support networking library for the simAG framework
#![allow(clippy::type_complexity)]
use handle::HandleError;

mod channel;
mod config;
pub(crate) mod group;
mod handle;
mod message;
mod network;
mod rpc;

pub use network::{NetworkBuilder, Provider};
use uuid::Uuid;

pub mod prelude {
    pub use super::group::GroupSettings;
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
    #[error("failed to join group `{group_id}` because: {reason}")]
    GroupError {
        group_id: Uuid,
        reason: group::GroupError,
    },
}
