//! Support networking library for the simAG framework

mod channel;
mod config;
pub(crate) mod group;
mod handle;
mod message;
mod network;
mod rpc;

use network::NetworkError;

pub use group::GroupSettings;
pub use handle::{HandleError, NetworkHandle};
pub use network::{NetworkBuilder, Provider};
pub use rpc::Resource;

pub use libp2p::{identity::ed25519::Keypair, PeerId};
use uuid::Uuid;

pub type Result<R> = std::result::Result<R, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("failed to join group `{group_id}` because: {reason}")]
    GroupError {
        group_id: Uuid,
        reason: group::GroupError,
    },
    #[error(transparent)]
    NetworkError(#[from] NetworkError),
    #[error(transparent)]
    OpError(#[from] HandleError),
}
