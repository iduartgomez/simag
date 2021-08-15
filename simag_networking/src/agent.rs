//! An agent

use libp2p::{Multiaddr, PeerId};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::rpc::{agent_id_from_str, ResourceIdentifier};

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Agent {
    pub addr: Vec<Multiaddr>,
    #[serde(serialize_with = "custom_ser::ser_peer")]
    #[serde(deserialize_with = "custom_ser::de_peer")]
    pub peer: Option<PeerId>,
    pub agent_id: Uuid,
    // pub policy: AgentPolicy,
}

impl Agent {
    pub fn as_peer(&mut self, peer: PeerId) {
        self.peer = Some(peer);
    }

    pub fn with_address(&mut self, addr: Multiaddr) -> &mut Self {
        self.addr.push(addr);
        self
    }

    pub fn identifier(&self) -> ResourceIdentifier {
        ResourceIdentifier::unique(&self.agent_id)
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize)]
pub struct AgentId(pub(crate) Uuid);

impl<T> From<T> for AgentId
where
    T: AsRef<str>,
{
    fn from(id: T) -> Self {
        AgentId(agent_id_from_str(id))
    }
}

#[derive(Default, Clone, Debug, Serialize, Deserialize)]
/// The network policy configuration for an agent.
///
/// Agents have permits set for both reading and writing.
/// - A 'writer' in this context is allowed to send an agent new information which may modify behaviour.
/// - A 'reader' is allowed to query the agent for information.
///
/// Default policy is to allow both for writing and reading.
///
/// Once a reader or a writer is white listed it's assumed that any other agent or group not listed won't
/// be allowed to read or write.
pub struct AgentPolicy {
    readers_white_list: Vec<ResourceIdentifier>,
    readers_black_list: Vec<ResourceIdentifier>,
    writers_white_list: Vec<ResourceIdentifier>,
    writers_black_list: Vec<ResourceIdentifier>,
}

impl AgentPolicy {
    fn allowed_to_read(&self, other: &ResourceIdentifier) -> bool {
        if self.readers_black_list.contains(other) {
            false
        } else if self.readers_white_list.is_empty() || self.readers_white_list.contains(other) {
            true
        } else {
            false
        }
    }

    fn allowed_to_write(&self, other: &ResourceIdentifier) -> bool {
        if self.writers_black_list.contains(other) {
            false
        } else if self.writers_white_list.is_empty() || self.writers_white_list.contains(other) {
            true
        } else {
            false
        }
    }

    pub fn block_reader(&mut self, reader: ResourceIdentifier) {
        self.readers_black_list.push(reader);
    }

    pub fn block_writer(&mut self, writer: ResourceIdentifier) {
        self.writers_black_list.push(writer);
    }

    pub fn allow_reader(&mut self, reader: ResourceIdentifier) {
        self.readers_white_list.push(reader);
    }

    pub fn allow_writer(&mut self, writer: ResourceIdentifier) {
        self.writers_white_list.push(writer);
    }
}

mod custom_ser {
    use super::*;
    use serde::{Deserializer, Serializer};

    pub(super) fn ser_peer<S: Serializer>(
        peer: &Option<PeerId>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        let as_bytes = if let Some(peer) = peer {
            Some(peer.to_bytes())
        } else {
            None
        };
        as_bytes.serialize(serializer)
    }

    pub(super) fn de_peer<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Option<PeerId>, D::Error> {
        let peer: Option<Vec<u8>> = Deserialize::deserialize(deserializer)?;
        Ok(peer.map(|data| PeerId::from_bytes(&data).unwrap()))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn check_ag_policies_whitelist() {
        let id1 = ResourceIdentifier::unique(&uuid::Uuid::new_v4());
        let id2 = ResourceIdentifier::unique(&uuid::Uuid::new_v4());
        let id3 = ResourceIdentifier::unique(&uuid::Uuid::new_v4());

        let mut pol1 = AgentPolicy::default();
        assert!(pol1.allowed_to_read(&id1));
        assert!(pol1.allowed_to_write(&id1));

        pol1.allow_reader(id2);
        assert!(!pol1.allowed_to_read(&id1) && !pol1.allowed_to_read(&id3));
        assert!(pol1.allowed_to_write(&id1));
        assert!(pol1.allowed_to_read(&id2));

        pol1.allow_reader(id3);
        assert!(pol1.allowed_to_read(&id2) && pol1.allowed_to_read(&id3));
        assert!(!pol1.allowed_to_read(&id1));

        pol1.allow_writer(id2);
        assert!(!pol1.allowed_to_write(&id1) && !pol1.allowed_to_write(&id3));
        assert!(!pol1.allowed_to_read(&id1));
        assert!(pol1.allowed_to_write(&id2));
        assert!(pol1.allowed_to_read(&id2));

        pol1.allow_writer(id3);
        assert!(pol1.allowed_to_write(&id2) && pol1.allowed_to_write(&id3));
        assert!(!pol1.allowed_to_write(&id1));
    }

    #[test]
    fn check_ag_policies_blacklist() {
        let id1 = ResourceIdentifier::unique(&uuid::Uuid::new_v4());
        let id2 = ResourceIdentifier::unique(&uuid::Uuid::new_v4());

        let mut pol1 = AgentPolicy::default();
        assert!(pol1.allowed_to_read(&id1));
        assert!(pol1.allowed_to_write(&id1));

        pol1.block_reader(id1);
        assert!(!pol1.allowed_to_read(&id1));
        assert!(pol1.allowed_to_write(&id1));
        assert!(pol1.allowed_to_read(&id2));

        pol1.block_writer(id1);
        assert!(!pol1.allowed_to_write(&id1));
        assert!(!pol1.allowed_to_read(&id1));
        assert!(pol1.allowed_to_write(&id2));
        assert!(pol1.allowed_to_read(&id2));
    }
}
