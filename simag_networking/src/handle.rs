use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    fs::File,
    hash::Hash,
    io::Write,
    result::Result as StdResult,
    sync::{Arc, Mutex},
};

use dashmap::DashMap;
use libp2p::{identity, kad::QueryId, PeerId};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use tokio::sync::mpsc::{error::TrySendError, Receiver, Sender};
use uuid::Uuid;

use crate::{
    agent::AgentPolicy,
    config::GlobalExecutor,
    group::GroupSettings,
    group::{Group, GroupPermits},
    network::NetworkError,
    rpc::{self, Resource, ResourceIdentifier},
    Error, Result,
};

/// A handle to a running network connection.
pub struct NetworkHandle<M>
where
    M: Serialize + DeserializeOwned,
{
    pub stats: NetworkStats,
    sender: Sender<HandleCmd<M>>,
    local_key: identity::ed25519::Keypair,
    pending: Arc<DashMap<OpId, PendingCmd<M>>>,
    next_msg_id: usize,
    /// a flag attribute to indicate in the background network is alive or not
    network_is_dead: bool,
    msg_buf: Arc<DashMap<PeerId, Vec<M>>>,
}

impl<M> NetworkHandle<M>
where
    M: Serialize + DeserializeOwned + Debug + Send + Sync + 'static,
{
    /// Only should be instanced throught the network builder.
    pub(crate) fn new(
        sender: Sender<HandleCmd<M>>,
        rcv: Receiver<StdResult<HandleAnsw<M>, NetworkError>>,
        local_key: identity::ed25519::Keypair,
    ) -> Self {
        let pending = Arc::new(DashMap::new());
        let msg_buf = Arc::new(DashMap::new());
        let net_stats = Arc::new(Mutex::new(Vec::new()));
        GlobalExecutor::spawn(Self::process_answer_buffer(
            rcv,
            pending.clone(),
            msg_buf.clone(),
            net_stats.clone(),
        ));
        NetworkHandle {
            sender,
            local_key,
            pending,
            next_msg_id: 1, // 0 value is reserved
            network_is_dead: false,
            stats: NetworkStats::new(net_stats),
            msg_buf,
        }
    }

    /// Returns whether an asynchrnous operation executed succesfully or not.
    /// If the operation still is running it will return a `waiting` error type.
    /// Returns the resource identifier in case the operation returns one:
    /// - create_group
    /// - join_group
    /// - register_agent
    /// - find_agent.
    pub fn op_result(&mut self, op_id: OpId) -> Result<Option<ResourceIdentifier>> {
        let answ = match self.pending.get_mut(&op_id) {
            None => Err(HandleError::OpNotFound(op_id).into()),
            Some(mut pend_cmd) => match pend_cmd.value_mut().answ.as_mut() {
                None => return Err(HandleError::AwaitingResponse(op_id).into()),
                Some(HandleAnsw::ReqJoinGroupAccepted { group_id, .. }) => {
                    Ok(Some(ResourceIdentifier::group(&group_id)))
                }
                Some(HandleAnsw::ReqJoinGroupDenied {
                    group_id, reason, ..
                }) => Err(Error::GroupError {
                    group_id: *group_id,
                    reason: reason.clone(),
                }),
                Some(HandleAnsw::AgentRegistered { rss_key, .. }) => Ok(Some(*rss_key)),
                Some(HandleAnsw::AgentFound { rss_key, .. }) => {
                    log::info!("Found agent {}", rss_key);
                    Ok(Some(*rss_key))
                }
                Some(HandleAnsw::HasShutdown { .. })
                | Some(HandleAnsw::PropagateGroupChange { .. }) => Ok(None),
                Some(HandleAnsw::AwaitingRegistration { .. }) => Ok(None),
                Some(HandleAnsw::RcvMsg { .. }) => unreachable!(),
            },
        };
        self.pending.remove(&op_id);
        answ
    }

    /// Get all the received messages from a given peer.
    pub fn received_messages(&mut self, peer: PeerId) -> Vec<M> {
        let mut new_msgs = vec![];
        if let Some(mut msgs) = self.msg_buf.get_mut(&peer) {
            std::mem::swap(&mut *msgs, &mut new_msgs);
        }
        new_msgs
    }

    /// Register an agent with this node. From this point on the node will act as the owner of this
    /// agent and handle any communication from/to this agent. Operation is asynchronous.
    ///
    /// This will block the handle until the agent has been correctly registered or an error happens.
    pub fn register_agent(&mut self, agent: &simag_core::Agent, config: AgentPolicy) -> OpId {
        // TODO: An agent can only be registered with a single node. If you try to register the same agent in
        // the same network more than once it will be an error.

        let agent_id = agent.id().to_string();
        let id = self.next_id();
        let msg = HandleCmd::RegisterAgent {
            op_id: id,
            agent_id,
            config,
        };
        let sender = self.sender.clone();
        self.pending.insert(
            id,
            PendingCmd {
                cmd: SentCmd::RegisterAgent,
                answ: None,
            },
        );
        GlobalExecutor::spawn(async move {
            sender.send(msg).await.map_err(|_| ())?;
            Ok::<_, ()>(())
        });
        id
    }

    /// Find an agent in the network and try to open a connection with it. Operation is asynchronous.
    pub fn find_agent<ID: ToString>(&mut self, agent_id: ID) -> OpId {
        let op_id = self.next_id();
        let agent_id = agent_id.to_string();
        let sender = self.sender.clone();
        self.pending.insert(
            op_id,
            PendingCmd {
                cmd: SentCmd::FindAgent,
                answ: None,
            },
        );
        GlobalExecutor::spawn(async move {
            let msg = HandleCmd::ConnectToAgent { agent_id, op_id };
            sender.send(msg).await.map_err(|_| ())?;
            Ok::<_, ()>(())
        });
        op_id
    }

    /// Create a resource of the group kind that belongs to the given agent, which will then be the original
    /// owner of the group. Operation is asynchronous.
    ///
    /// Returns the network identifier (key) to the group in case the group has not already been
    /// registered by an other agent/peer or error otherwise.
    pub fn create_group<C, ID>(
        &mut self,
        group_id: &str,
        owners: impl IntoIterator<Item = ID> + Clone,
        permits: Option<GroupPermits>,
        settings: C,
    ) -> OpId
    where
        ID: AsRef<str>,
        C: GroupSettings,
    {
        let op_id = self.next_id();
        let (key, mut group) = Resource::group(owners.clone(), group_id, settings);
        if let Some(mut permits) = permits {
            // ensure that owners are added as readers/writers
            owners.into_iter().for_each(|owner| {
                permits.write(owner.as_ref());
                permits.read(owner.as_ref());
            });
            group.with_permits(permits);
        }
        let sender = self.sender.clone();
        // FIXME: register op as pending
        GlobalExecutor::spawn(async move {
            let msg = HandleCmd::RegisterGroup {
                op_id,
                group_key: key,
                group,
            };
            sender.send(msg).await.map_err(|_| ())?;
            Ok::<_, ()>(())
        });
        op_id
    }

    /// Request joining to a given group, will return the network identifier of the group in case
    /// this agent is allowed to join the group. Operation is asynchronous.
    ///
    /// The petitioner must pass the settings used to evaluate if joining is possible when comparing
    /// with the settings set by the owners of the group.
    ///
    /// Optionally request a set of group permits, otherwise read permits will be requested.
    /// The agent only will be allowed to join if the permits are legal for this agent.
    ///
    /// ## Arguments
    /// - agent_id: identifier of the agent making the request.
    pub fn join_group<C>(
        &mut self,
        group_id: &str,
        agent_id: &str,
        permits: Option<GroupPermits>,
        settings: C,
    ) -> OpId
    where
        C: GroupSettings,
    {
        let op_id = self.next_id();
        let agent_id = rpc::agent_id_from_str(agent_id);
        // unknown owners, this information is to be completed after fecthing the initial info
        let (group_key, mut group) = Resource::group(Vec::<String>::new(), group_id, settings);
        if let Some(permits) = permits {
            group.with_permits(permits);
        }
        let sender = self.sender.clone();
        // FIXME: register op as pending
        GlobalExecutor::spawn(async move {
            let msg = HandleCmd::ReqJoinGroup {
                op_id,
                group_key,
                agent_id,
                group,
            };
            sender.send(msg).await.map_err(|_| ())?;
            Ok::<_, ()>(())
        });
        op_id
    }

    /// Operation is asynchronous.
    pub fn leave_group(&mut self, group_id: &str, agent_id: &str) -> OpId {
        todo!()
    }

    /// Send a message to the given peer directly. Operation is asynchronous.
    pub fn send_message(&mut self, value: M, peer: PeerId) -> OpId {
        let op_id = self.next_id();
        self.pending.insert(
            op_id,
            PendingCmd {
                cmd: SentCmd::SentMsg,
                answ: None,
            },
        );
        let sender = self.sender.clone();
        GlobalExecutor::spawn(async move {
            let msg = HandleCmd::<M>::SendMessage { op_id, value, peer };
            sender.send(msg).await.map_err(|_| ())?;
            Ok::<_, ()>(())
        });
        op_id
    }

    /// Get this peer id encoded as a Base58 string.
    pub fn get_peer_id(&self) -> String {
        Self::peer_id_from_ed25519(self.local_key.public()).to_base58()
    }

    /// Saves this peer secret key to a file in bytes. This file should be kept in a secure location.
    pub fn save_secret_key<T: AsRef<std::path::Path>>(&self, path: T) -> Result<()> {
        let enconded_key = self.local_key.encode().to_vec();
        let mut file = File::create(path.as_ref())
            .map_err(|err| crate::Error::from(HandleError::FailedSaving(err)))?;
        file.write_all(enconded_key.as_slice())
            .map_err(|err| HandleError::FailedSaving(err).into())
    }

    /// Returns whether the network connection is running or has shutdown.
    pub fn running(&mut self) -> bool {
        if self.network_is_dead {
            return false;
        }

        self.sender
            .try_send(HandleCmd::IsRunning)
            .map(|_| true)
            .unwrap_or_else(|err| match err {
                TrySendError::Full(_) => true,
                TrySendError::Closed(_) => {
                    self.network_is_dead = true;
                    false
                }
            })
    }

    /// Commands the network to shutdown, returns inmediately if the network has shutdown
    /// or if it already had disconnected for any reason or blocking otherwise.
    ///
    /// The network may not shutdown inmediately if still is processing any commands.
    pub fn shutdown(&mut self) -> Result<bool> {
        if self.network_is_dead {
            // was previously marked as dead
            return Err(HandleError::Disconnected.into());
        }

        if self.has_shutdown() {
            return Ok(true);
        }

        let msg_id = self.next_id();
        let msg = HandleCmd::Shutdown(msg_id);
        match self.sender.blocking_send(msg) {
            Ok(()) => {}
            Err(_err) => {
                self.network_is_dead = true;
                return Err(HandleError::Disconnected.into());
            }
        }

        Ok(self.has_shutdown())
    }

    fn has_shutdown(&mut self) -> bool {
        let shutdown_cmds: Vec<_> = self
            .pending
            .iter()
            .filter_map(|p| {
                if (*p).shutdown() {
                    Some(*p.key())
                } else {
                    None
                }
            })
            .collect();
        let mut has_shutdown = false;
        for k in shutdown_cmds {
            if let Some((
                _,
                PendingCmd {
                    cmd: SentCmd::ShutDown,
                    answ: Some(HandleAnsw::HasShutdown { answ, .. }),
                },
            )) = self.pending.remove(&k)
            {
                if answ {
                    // one of all the sent shutdown commands was successful, not the conn is dead
                    self.network_is_dead = true;
                    has_shutdown = true;
                }
            }
        }
        has_shutdown
    }

    async fn process_answer_buffer(
        mut rcv: Receiver<StdResult<HandleAnsw<M>, NetworkError>>,
        pending: Arc<DashMap<OpId, PendingCmd<M>>>,
        msg_buf: Arc<DashMap<PeerId, Vec<M>>>,
        net_stats: Arc<Mutex<Vec<(PeerId, usize)>>>,
    ) -> Result<()> {
        while let Some(answ_res) = rcv.recv().await {
            let msg = answ_res?;
            match msg {
                HandleAnsw::HasShutdown { op_id: id, answ } => {
                    pending.entry(id).and_modify(|e| {
                        e.answ = Some(HandleAnsw::HasShutdown { op_id: id, answ });
                    });
                }
                HandleAnsw::RcvMsg { peer, msg } => {
                    log::debug!("Received streaming msg from {}: {:?}", peer, msg);
                    msg_buf.entry(peer).or_default().push(msg);
                    let stats = &mut *net_stats.lock().unwrap();
                    if let Ok(idx) = stats.binary_search_by_key(&peer, |&(p, _)| p) {
                        stats[idx].1 += 1;
                    } else {
                        stats.push((peer, 1));
                        stats.sort_by_key(|e| e.0);
                    }
                }
                HandleAnsw::AgentRegistered { op_id, rss_key } => {
                    log::info!("Registered {} with {:?}", rss_key, op_id);
                    pending.entry(op_id).and_modify(|e| {
                        e.answ = Some(HandleAnsw::AgentRegistered { op_id, rss_key });
                    });
                }
                HandleAnsw::PropagateGroupChange { op_id, group_id } => todo!(),
                HandleAnsw::ReqJoinGroupAccepted { op_id, group_id } => {
                    pending.entry(op_id).and_modify(|e| {
                        e.answ = Some(HandleAnsw::ReqJoinGroupAccepted { op_id, group_id });
                    });
                }
                HandleAnsw::ReqJoinGroupDenied {
                    op_id,
                    group_id,
                    reason,
                } => {
                    pending.entry(op_id).and_modify(|e| {
                        e.answ = Some(HandleAnsw::ReqJoinGroupDenied {
                            op_id,
                            group_id,
                            reason,
                        });
                    });
                }
                HandleAnsw::AgentFound {
                    op_id,
                    rss_key: key,
                } => {
                    pending.alter(&op_id, |_, mut e| {
                        e.answ = Some(HandleAnsw::AgentFound {
                            op_id,
                            rss_key: key,
                        });
                        e
                    });
                }
                HandleAnsw::AwaitingRegistration { .. } => {}
            }
        }

        // all sending halves were closed, meaning that the network connection has been dropped
        // communicate back to the main thread
        pending.insert(
            OpId(usize::MAX),
            PendingCmd {
                cmd: SentCmd::ShutDown,
                answ: Some(HandleAnsw::HasShutdown {
                    op_id: OpId(usize::MAX),
                    answ: true,
                }),
            },
        );
        Err(HandleError::Disconnected.into())
    }

    fn next_id(&mut self) -> OpId {
        let msg_id = self.next_msg_id;
        self.next_msg_id += 1;
        OpId(msg_id)
    }

    fn peer_id_from_ed25519(key: identity::ed25519::PublicKey) -> PeerId {
        PeerId::from_public_key(identity::PublicKey::Ed25519(key))
    }
}

pub struct NetworkStats {
    key_stats: HashMap<ResourceIdentifier, KeyStats>,
    msg_stats: Arc<Mutex<Vec<(PeerId, usize)>>>,
}

impl NetworkStats {
    fn new(msg_stats: Arc<Mutex<Vec<(PeerId, usize)>>>) -> Self {
        NetworkStats {
            key_stats: HashMap::new(),
            msg_stats,
        }
    }

    pub fn for_key(&self, key: &ResourceIdentifier) -> Option<&KeyStats> {
        self.key_stats.get(key)
    }

    pub fn received_messages(&self) -> Vec<(PeerId, usize)> {
        let stats = self.msg_stats.lock().unwrap();
        stats.clone()
    }
}

#[derive(Default)]
pub struct KeyStats {
    pub times_served: usize,
    pub times_received: usize,
}

struct PendingCmd<M>
where
    M: DeserializeOwned,
{
    cmd: SentCmd,
    answ: Option<HandleAnsw<M>>,
}

impl<M: DeserializeOwned> PendingCmd<M> {
    fn shutdown(&self) -> bool {
        match self.cmd {
            SentCmd::ShutDown => true,
            _ => false,
        }
    }
}

enum SentCmd {
    AddedKey(Vec<u8>),
    PullContent(Vec<u8>),
    ShutDown,
    SentMsg,
    RegisterAgent,
    FindAgent,
}

#[derive(Debug)]
pub(crate) enum HandleCmd<M> {
    /// query the network about current status
    IsRunning,
    /// put a resource in this node
    RegisterGroup {
        op_id: OpId,
        group_key: ResourceIdentifier,
        group: Group,
    },
    /// issue a shutdown command
    Shutdown(OpId),
    /// send a serialized message
    /// M should be heap allocated ideally to avoid large enum sizes
    SendMessage { op_id: OpId, value: M, peer: PeerId },
    /// register a peer as the manager of an agent
    RegisterAgent {
        op_id: OpId,
        agent_id: String,
        config: AgentPolicy,
    },
    /// try to open a channel to the specified agent
    ConnectToAgent { agent_id: String, op_id: OpId },
    /// instruct the network handler to send a request
    /// for an agent joining a group
    ReqJoinGroup {
        op_id: OpId,
        group_key: ResourceIdentifier,
        agent_id: Uuid,
        group: Group,
        // state: RequestState,
    },
    /// No manager was found in the local cache. Awaiting peer information for managers
    /// so a connection attempt can be initialized.
    FindGroupManager {
        op_id: OpId,
        group_key: ResourceIdentifier,
        agent_id: Uuid,
        group: Group,
    },
    /// Awaiting publish confirmation by the kad DHT of a rss.
    AwaitingRegistration {
        op_id: OpId,
        rss_key: ResourceIdentifier,
        resource: Resource,
    },
}

impl<M: DeserializeOwned> HandleCmd<M> {
    fn get_op_id(&self) -> OpId {
        use self::HandleCmd::*;
        match self {
            IsRunning => OpId::IS_RUNNING,
            RegisterGroup { op_id, .. } => *op_id,
            Shutdown(op_id) => *op_id,
            SendMessage { op_id, .. } => *op_id,
            RegisterAgent { op_id, .. } => *op_id,
            ConnectToAgent { op_id, .. } => *op_id,
            ReqJoinGroup { op_id, .. } => *op_id,
            FindGroupManager { op_id, .. } => *op_id,
            AwaitingRegistration { op_id, .. } => *op_id,
        }
    }
}

impl<M: DeserializeOwned> PartialEq for HandleCmd<M> {
    fn eq(&self, other: &Self) -> bool {
        self.get_op_id() == other.get_op_id()
    }
}

impl<M: DeserializeOwned> Eq for HandleCmd<M> {}

impl<M: DeserializeOwned> PartialOrd for HandleCmd<M> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.get_op_id().partial_cmp(&other.get_op_id())
    }
}

impl<M: DeserializeOwned> Ord for HandleCmd<M> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_op_id().cmp(&other.get_op_id())
    }
}

/// The answer of an operation.
#[derive(Clone, Debug)]
pub(crate) enum HandleAnsw<M>
where
    M: DeserializeOwned,
{
    AgentRegistered {
        op_id: OpId,
        rss_key: ResourceIdentifier,
    },
    HasShutdown {
        op_id: OpId,
        answ: bool,
    },
    /// Propagate a change in the configuration or composition of a group.
    PropagateGroupChange {
        op_id: OpId,
        group_id: Uuid,
    },
    ReqJoinGroupAccepted {
        op_id: OpId,
        group_id: Uuid,
    },
    ReqJoinGroupDenied {
        op_id: OpId,
        group_id: Uuid,
        reason: crate::group::GroupError,
    },
    /// The operation id of received messages is always considered OpId::RCV_MSG.
    RcvMsg {
        peer: PeerId,
        msg: M,
    },
    AgentFound {
        op_id: OpId,
        rss_key: ResourceIdentifier,
    },
    AwaitingRegistration {
        op_id: OpId,
        qid: QueryId,
    },
}

impl<M: DeserializeOwned> HandleAnsw<M> {
    fn get_op_id(&self) -> OpId {
        use self::HandleAnsw::*;
        match self {
            AgentRegistered { op_id, .. } => *op_id,
            HasShutdown { op_id, .. } => *op_id,
            PropagateGroupChange { op_id, .. } => *op_id,
            ReqJoinGroupAccepted { op_id, .. } => *op_id,
            ReqJoinGroupDenied { op_id, .. } => *op_id,
            RcvMsg { .. } => OpId::RCV_MSG,
            AgentFound { op_id, .. } => *op_id,
            AwaitingRegistration { op_id, .. } => *op_id,
        }
    }
}

impl<M: DeserializeOwned> PartialEq for HandleAnsw<M> {
    fn eq(&self, other: &Self) -> bool {
        self.get_op_id() == other.get_op_id()
    }
}

impl<M: DeserializeOwned> Eq for HandleAnsw<M> {}

impl<M: DeserializeOwned> PartialOrd for HandleAnsw<M> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.get_op_id().partial_cmp(&other.get_op_id())
    }
}

impl<M: DeserializeOwned> Ord for HandleAnsw<M> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_op_id().cmp(&other.get_op_id())
    }
}

#[derive(thiserror::Error, Debug)]
pub enum HandleError {
    #[error("awaiting a response for op `{0}`")]
    AwaitingResponse(OpId),
    #[error("unexpected disconnect")]
    Disconnected,
    #[error("failed saving secret key")]
    FailedSaving(#[from] std::io::Error),
    #[error("handle not available")]
    HandleNotResponding,
    #[error("irrecoverable error in the network")]
    IrrecoverableError,
    #[error("network op execution failed")]
    OpFailed,
    #[error("operation `{0}` not found")]
    OpNotFound(OpId),
    #[error("manager not found for op `{0}`")]
    ManagerNotFound(OpId),
    #[error("unexpected response")]
    UnexpectedResponse(OpId),
}

/// An identifier for an operation executed asynchronously. Used to fetch the results
/// of such operation in an asynchronous fashion.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct OpId(usize);

impl OpId {
    const RCV_MSG: OpId = OpId(0);
    const IS_RUNNING: OpId = OpId(0);
}

impl Display for OpId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        Ok(())
    }
}
