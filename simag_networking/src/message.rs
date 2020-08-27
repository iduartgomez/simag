use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub(crate) enum MessageType {
    StreamRcp,
    Message,
}

#[derive(Serialize, Deserialize)]
pub(crate) struct Message {
    header: MessageType,
    payload: Vec<u8>,
}
