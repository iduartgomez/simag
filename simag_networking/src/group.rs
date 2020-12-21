use serde::{Deserialize, Serialize};
use std::{collections::BTreeSet, collections::HashMap, fmt::Debug, iter::FromIterator};
use uuid::Uuid;

use crate::rpc::agent_id_from_str;

pub(crate) struct GroupManager {
    groups: HashMap<Uuid, Group>,
}

impl GroupManager {
    pub fn new() -> Self {
        GroupManager {
            groups: HashMap::new(),
        }
    }

    pub fn request_joining(
        &mut self,
        agent_id: Uuid,
        group_id: Uuid,
        petitioner_settings: &dyn GroupSettings,
        permits: GroupPermits,
    ) -> Result<(), GroupError> {
        let group = self
            .groups
            .get_mut(&group_id)
            .ok_or_else(|| GroupError::NotExisting)?;
        if !group
            .settings
            .is_allowed_to_join(agent_id, petitioner_settings)
        {
            return Err(GroupError::NotAllowedToJoin);
        }
        if !group.permits.are_compatible(&permits) {
            return Err(GroupError::IncompatiblePermits);
        }
        group.permits.append(permits);
        Ok(())
    }
}

/// A series of characteristics that define a group and which set who can or cannot join.
#[typetag::serde]
pub trait GroupSettings: Debug + Send + Sync + 'static {
    /// Takes an inbound instance of settings and evaluates if are compatible with this set of
    /// settings.
    fn is_allowed_to_join(&self, agent_id: Uuid, petitioner_settings: &dyn GroupSettings) -> bool;
    fn box_cloned(&self) -> Box<dyn GroupSettings>;
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct Group {
    pub id: Uuid,
    /// All the topics peers connected to the group are automatically subscribed to
    topic_span: Vec<String>,
    /// Owners of the group, passed at group creation time or updated by one of the original owners.
    pub owners: BTreeSet<Uuid>,
    /// Members and their permits to write/read to/from a group. T
    /// This should be eventually consitent across all nodes.
    permits: GroupPermits,
    /// Settings for this group.
    pub settings: Box<dyn GroupSettings>,
}

impl Clone for Group {
    fn clone(&self) -> Self {
        Group {
            id: self.id.clone(),
            topic_span: self.topic_span.clone(),
            owners: self.owners.clone(),
            permits: self.permits.clone(),
            settings: self.settings.box_cloned(),
        }
    }
}

impl Group {
    pub fn from_owners<ID, C>(id: Uuid, owners: impl IntoIterator<Item = ID>, settings: C) -> Group
    where
        ID: AsRef<str>,
        C: GroupSettings,
    {
        let settings = Box::new(settings);
        Group {
            id,
            topic_span: vec![],
            owners: BTreeSet::from_iter(owners.into_iter().map(agent_id_from_str)),
            permits: GroupPermits::new(),
            settings,
        }
    }

    pub fn with_permits(&mut self, permits: GroupPermits) -> &mut Self {
        self.permits = permits;
        self
    }
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct GroupPermits {
    pub(crate) read: BTreeSet<Uuid>,
    pub(crate) write: BTreeSet<Uuid>,
}

impl GroupPermits {
    pub fn new() -> Self {
        GroupPermits {
            read: BTreeSet::new(),
            write: BTreeSet::new(),
        }
    }

    pub fn for_agent(agent: &str, read: bool, write: bool) -> Self {
        let mut permits = GroupPermits::new();
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

    pub(crate) fn are_compatible(&self, other: &Self) -> bool {
        todo!()
    }

    /// Apends a new set of permits to this group if permits.
    pub(crate) fn append(&mut self, other: Self) -> bool {
        todo!()
    }
}

#[derive(thiserror::Error, Debug, Serialize, Deserialize, Clone)]
pub enum GroupError {
    #[error("requested permits are not allowed")]
    IncompatiblePermits,
    #[error("group does not exist")]
    NotExisting,
    #[error("request to join denied")]
    NotAllowedToJoin,
    #[error("managers for this group not found")]
    ManagerNotFound,
}
