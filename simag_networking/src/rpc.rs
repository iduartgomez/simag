use crate::{
    agent::Agent,
    group::{Group, GroupError, GroupSettings},
    handle::OpId,
};
use libp2p::PeerId;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Borrow,
    convert::{TryFrom, TryInto},
    fmt::{Debug, Display},
};
use uuid::Uuid;

#[derive(Debug, Serialize, Deserialize)]
#[allow(clippy::enum_variant_names)]
pub(crate) enum AgentRpc {
    /// request joining a group to one of the owners
    ReqGroupJoin {
        op_id: OpId,
        group_id: Uuid,
        agent_id: Uuid,
        /// (read, write)
        permits: (bool, bool),
        /// the settings of the petitioner
        settings: Box<dyn GroupSettings>,
    },
    ReqGroupJoinAccepted {
        op_id: OpId,
        group_id: Uuid,
    },
    ReqGroupJoinDenied {
        op_id: OpId,
        group_id: Uuid,
        reason: GroupError,
    },
}

pub(crate) fn agent_id_from_str<ID: AsRef<str>>(id: ID) -> Uuid {
    Uuid::new_v5(
        &Uuid::NAMESPACE_URL,
        ["simag://agent.", id.as_ref()].concat().as_bytes(),
    )
}

/// A `Resource` in a simag network is ...
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Resource(pub(crate) ResourceKind);

impl Resource {
    pub(crate) fn new_agent(agent: Agent) -> Resource {
        Resource(ResourceKind::Agent(agent))
    }

    pub(crate) fn new_group(group: Group) -> Resource {
        Resource(ResourceKind::Group(group))
    }

    /// Create a resource of agent kind. Returns both the identifier (key) and the resource
    /// handle.
    pub(crate) fn agent<ID: AsRef<str>>(
        agent_id: ID,
        peer_id: PeerId,
    ) -> (ResourceIdentifier, Agent) {
        let uid = agent_id_from_str(agent_id);
        let key = ResourceIdentifier::unique(&uid);
        let res = Agent {
            agent_id: uid,
            peer: Some(peer_id),
            addr: Vec::with_capacity(1),
            ..Default::default()
        };
        (key, res)
    }

    /// Create a resource of group kind, optionally provide a list of owners of the group.
    /// Returns both the identifier (key) and the resource handle.
    ///
    pub(crate) fn group<C, ID>(
        owners: impl IntoIterator<Item = ID>,
        group_id: &str,
        settings: C,
    ) -> (ResourceIdentifier, Group)
    where
        C: GroupSettings,
        ID: AsRef<str>,
    {
        let uid = agent_id_from_str(group_id);
        let key = ResourceIdentifier::group(&uid);
        let res = Group::from_owners(uid, owners, settings);
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
#[derive(PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub struct ResourceIdentifier {
    key: [u8; ResourceIdentifier::KIND_SIZE + ResourceIdentifier::KEY_SIZE],
}

impl Debug for ResourceIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AgentKey")
            .field("kind", &ResourceKeyKind::try_from(self.key[0]).unwrap())
            .field(
                "id",
                &Uuid::from_u128(u128::from_be_bytes(self.key[1..].try_into().unwrap())),
            )
            .finish()
    }
}

impl Display for ResourceIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        let kind =
            if let ResourceKeyKind::UniqueId = ResourceKeyKind::try_from(self.key[0]).unwrap() {
                "Agent"
            } else {
                "Group"
            };
        write!(
            f,
            "{}(\"{}\")",
            kind,
            &Uuid::from_u128(u128::from_be_bytes(self.key[1..].try_into().unwrap()))
        )?;
        Ok(())
    }
}

impl ResourceIdentifier {
    const KIND_SIZE: usize = 1;
    const KEY_SIZE: usize = std::mem::size_of::<u128>();

    /// This key represents a group resource kind.
    pub(crate) fn group(id: &Uuid) -> ResourceIdentifier {
        let id = id.as_u128().to_be_bytes();
        let kind = ResourceKeyKind::GroupId as u8;
        let mut key = [0; Self::KEY_SIZE + Self::KIND_SIZE];
        (&mut key[1..]).copy_from_slice(&id);
        key[0] = kind;
        ResourceIdentifier { key }
    }

    /// This key represents a unique agent.
    pub(crate) fn unique(id: &Uuid) -> ResourceIdentifier {
        let id = id.as_u128().to_be_bytes();
        let kind = ResourceKeyKind::UniqueId as u8;
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

    pub(crate) fn get_group_id(&self) -> Result<Uuid, ()> {
        todo!()
    }

    pub fn is_agent(&self) -> bool {
        self.key[0] == (ResourceKeyKind::UniqueId as u8)
    }
}

impl Borrow<[u8]> for ResourceIdentifier {
    fn borrow(&self) -> &[u8] {
        &self.key
    }
}

impl AsRef<[u8]> for ResourceIdentifier {
    fn as_ref(&self) -> &[u8] {
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
        let _kind = ResourceKeyKind::try_from(slice[0])?;
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
enum ResourceKeyKind {
    UniqueId = 0,
    GroupId = 1,
}

impl TryFrom<u8> for ResourceKeyKind {
    type Error = std::io::Error;
    fn try_from(variant: u8) -> std::io::Result<ResourceKeyKind> {
        match variant {
            0 => Ok(ResourceKeyKind::UniqueId),
            1 => Ok(ResourceKeyKind::GroupId),
            _ => Err(std::io::ErrorKind::InvalidInput.into()),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) enum ResourceKind {
    Agent(Agent),
    Group(Group),
}
