use libp2p::{Multiaddr, PeerId};
use serde::{Deserialize, Serialize};
use std::{
    borrow::Borrow,
    collections::BTreeSet,
    convert::{TryFrom, TryInto},
    fmt::Debug,
    iter::FromIterator,
};
use uuid::Uuid;

#[derive(Clone, Debug)]
#[doc(hidden)]
pub enum AgentRpc {
    RegisterAgent {
        agent_id: String,
    },
    JoinGroup {
        resource_key: ResourceIdentifier,
        agent_id: Uuid,
        group: Resource,
    },
}

pub(crate) fn agent_id_from_str<ID: AsRef<str>>(id: ID) -> Uuid {
    Uuid::new_v5(
        &Uuid::NAMESPACE_URL,
        ["simag://agent.", id.as_ref()].concat().as_bytes(),
    )
}

/// A `Resource` in a simag network is ...
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Resource(AgOrGroup);

impl Resource {
    pub(crate) fn new_agent(agent: AgentRes) -> Resource {
        Resource(AgOrGroup::Ag(agent))
    }

    pub(crate) fn new_group(group: GroupRes) -> Resource {
        Resource(AgOrGroup::Gr(group))
    }

    /// Create a resource of agent kind. Returns both the identifier (key) and the resource
    /// handle.
    pub(crate) fn agent<ID: AsRef<str>>(agent_id: ID) -> (ResourceIdentifier, AgentRes) {
        let uid = agent_id_from_str(agent_id);
        let key = ResourceIdentifier::unique(&uid);
        let res = AgentRes {
            agent_id: uid,
            peer: None,
            addr: Vec::with_capacity(1),
        };
        (key, res)
    }

    /// Create a resource of group kind, optionally provide a list of owners of the group.
    /// Returns both the identifier (key) and the resource handle.
    ///
    pub(crate) fn group<ID: AsRef<str>>(
        owners: impl IntoIterator<Item = ID>,
        group_id: &str,
    ) -> (ResourceIdentifier, GroupRes) {
        let uid = agent_id_from_str(group_id);
        let key = ResourceIdentifier::group(&uid);
        let res = GroupRes {
            id: uid,
            topic_span: vec![],
            members: BTreeSet::new(),
            owners: BTreeSet::from_iter(owners.into_iter().map(agent_id_from_str)),
            permissions: GroupPermission::new(),
        };
        (key, res)
    }
}

/// A `ResourceIdentifier` is the default simag network resource identifier.
///
/// The key has two different variants that represent two different kinds of resources:
/// - A unique agent identifier, a self identifier for a given agent. Since agents own
///   their own process and are binded to a single peer (independently of multiple agents being
///   ran in a single host), each peer should have at most one (permanent) identifier binded
///   to a single agent at creation time. This id must be unique across the whole network.
/// - A group identifier, an agent can pertain to a number of groups, this can be determined by
///   a given peer owning this resource (key) in the Kadmelia DHT.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct ResourceIdentifier {
    key: [u8; ResourceIdentifier::KIND_SIZE + ResourceIdentifier::KEY_SIZE],
}

impl Debug for ResourceIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AgentKey")
            .field("kind", &AgentKeyKind::try_from(self.key[0]).unwrap())
            .field(
                "id",
                &Uuid::from_u128(u128::from_be_bytes(self.key[1..].try_into().unwrap())),
            )
            .finish()
    }
}

impl ResourceIdentifier {
    const KIND_SIZE: usize = 1;
    const KEY_SIZE: usize = std::mem::size_of::<u128>();

    /// This key represents a group resource kind.
    fn group(id: &Uuid) -> ResourceIdentifier {
        let id = id.as_u128().to_be_bytes();
        let kind = AgentKeyKind::GroupId as u8;
        let mut key = [0; Self::KEY_SIZE + Self::KIND_SIZE];
        (&mut key[1..]).copy_from_slice(&id);
        key[0] = kind;
        ResourceIdentifier { key }
    }

    fn unique(id: &Uuid) -> ResourceIdentifier {
        let id = id.as_u128().to_be_bytes();
        let kind = AgentKeyKind::UniqueId as u8;
        let mut key = [0; Self::KEY_SIZE + Self::KIND_SIZE];
        (&mut key[1..]).copy_from_slice(&id);
        key[0] = kind;
        ResourceIdentifier { key }
    }

    /// Returns the key of the agent as a byte array.
    pub fn as_encoded_key(
        &self,
    ) -> [u8; ResourceIdentifier::KIND_SIZE + ResourceIdentifier::KEY_SIZE] {
        self.key
    }
}

impl Borrow<[u8]> for ResourceIdentifier {
    fn borrow(&self) -> &[u8] {
        &self.key
    }
}

impl TryFrom<&[u8]> for ResourceIdentifier {
    type Error = std::io::Error;
    fn try_from(slice: &[u8]) -> std::io::Result<Self> {
        if Self::KEY_SIZE + Self::KIND_SIZE != slice.len() {
            return Err(std::io::ErrorKind::InvalidInput.into());
        }
        // validate the kind byte
        let _kind = AgentKeyKind::try_from(slice[0])?;
        // validate that is a valid uuid
        let _uuid = Uuid::from_u128(u128::from_be_bytes(
            slice[1..]
                .try_into()
                .map_err(|_| std::io::ErrorKind::InvalidInput)?,
        ));

        let mut key = [0; Self::KEY_SIZE + Self::KIND_SIZE];
        key.copy_from_slice(&slice);
        Ok(ResourceIdentifier { key })
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
#[repr(u8)]
enum AgentKeyKind {
    UniqueId = 0,
    GroupId = 1,
}

impl TryFrom<u8> for AgentKeyKind {
    type Error = std::io::Error;
    fn try_from(variant: u8) -> std::io::Result<AgentKeyKind> {
        match variant {
            0 => Ok(AgentKeyKind::UniqueId),
            1 => Ok(AgentKeyKind::GroupId),
            _ => Err(std::io::ErrorKind::InvalidInput.into()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum AgOrGroup {
    Ag(AgentRes),
    Gr(GroupRes),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct AgentRes {
    addr: Vec<Multiaddr>,
    #[serde(serialize_with = "custom_ser::ser_peer")]
    #[serde(deserialize_with = "custom_ser::de_peer")]
    peer: Option<PeerId>,
    agent_id: Uuid,
}

impl AgentRes {
    pub fn as_peer(&mut self, peer: PeerId) {
        self.peer = Some(peer);
    }

    pub fn with_address(&mut self, addr: Multiaddr) {
        self.addr.push(addr);
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct GroupRes {
    id: Uuid,
    /// All the topics peers connected to the group are automatically subscribed to
    topic_span: Vec<String>,
    /// Eventually consistent list of peers; a message directed to this group will be propagated
    /// to all the members of the group.
    members: BTreeSet<Uuid>,
    /// Owners of the group, passed at group creation time or updated by one of the original owners.
    owners: BTreeSet<Uuid>,
    /// Permissions to write/read to a group.
    permissions: GroupPermission,
}

impl GroupRes {
    pub fn with_permissions(&mut self, permission: GroupPermission) -> &mut Self {
        self.permissions = permission;
        self
    }
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct GroupPermission {
    read: BTreeSet<Uuid>,
    write: BTreeSet<Uuid>,
}

impl GroupPermission {
    pub fn new() -> Self {
        GroupPermission {
            read: BTreeSet::new(),
            write: BTreeSet::new(),
        }
    }

    pub fn for_agent(agent: &str, read: bool, write: bool) -> Self {
        let mut permits = GroupPermission::new();
        if read {
            permits.read(agent);
        }
        if write {
            permits.write(agent);
        }
        permits
    }

    /// Grant/request read permit for this agent.
    pub fn read(&mut self, agent: &str) -> &mut Self {
        let uid = agent_id_from_str(agent);
        self.read.insert(uid);
        self
    }

    /// Grant/request write permit for this agent.
    pub fn write(&mut self, agent: &str) -> &mut Self {
        let uid = agent_id_from_str(agent);
        self.write.insert(uid);
        self
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
            Some(peer.as_bytes().to_vec())
        } else {
            None
        };
        as_bytes.serialize(serializer)
    }

    pub(super) fn de_peer<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Option<PeerId>, D::Error> {
        let peer: Option<Vec<u8>> = Deserialize::deserialize(deserializer)?;
        Ok(peer.map(|data| PeerId::from_bytes(data).unwrap()))
    }
}
