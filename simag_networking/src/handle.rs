use crate::{
    config::GlobalExecutor,
    group::GroupSettings,
    group::{Group, GroupPermits},
    network::NetworkError,
    rpc::{self, Resource, ResourceIdentifier},
    Error, Result,
};
use libp2p::{identity, PeerId};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::{
    borrow::Borrow,
    collections::HashMap,
    convert::TryFrom,
    fmt::{Debug, Display},
    fs::File,
    hash::Hash,
    io::Write,
    result::Result as StdResult,
};
use tokio::sync::mpsc::{
    error::{TryRecvError, TrySendError},
    Receiver, Sender,
};
use uuid::Uuid;

/// A handle to a running network connection.
pub struct NetworkHandle<M>
where
    M: Serialize + DeserializeOwned,
{
    pub stats: NetworkStats,
    sender: Sender<HandleCmd<M>>,
    rcv: Receiver<StdResult<HandleAnsw<M>, NetworkError>>,
    local_key: identity::ed25519::Keypair,
    pending: HashMap<OpId, PendingCmd<M>>,
    next_msg_id: usize,
    is_dead: bool,
    rcv_msg: HashMap<PeerId, Vec<M>>,
}

impl<M> NetworkHandle<M>
where
    M: Serialize + DeserializeOwned + Debug + Send + 'static,
{
    /// Returns whether an asynchrnous operation executed succesfully or not.
    /// If the operation still is running it will return a `waiting` error type.
    /// Returns the resource identifier in case the operation returns one:
    /// - create_group
    /// - join_group
    /// - register_agent
    /// - find_agent.
    pub fn op_result(&mut self, op_id: OpId) -> Result<Option<ResourceIdentifier>> {
        match self.pending.remove(&op_id) {
            None => Err(HandleError::OpNotFound(op_id).into()),
            Some(PendingCmd { cmd, answ: None }) => {
                // no answer received, insert back into the pending cmd map.
                self.pending.insert(op_id, PendingCmd { cmd, answ: None });
                Err(HandleError::AwaitingResponse(op_id).into())
            }
            Some(PendingCmd {
                answ: Some(answ), ..
            }) => match answ {
                HandleAnsw::ReqJoinGroupAccepted { group_id, .. } => {
                    Ok(Some(ResourceIdentifier::group(&group_id)))
                }
                HandleAnsw::ReqJoinGroupDenied {
                    group_id, reason, ..
                } => Err(Error::GroupError { group_id, reason }),
                HandleAnsw::AgentRegistered { key, .. } | HandleAnsw::GotRecord { key, .. } => {
                    Ok(Some(key))
                }
                HandleAnsw::HasShutdown { .. }
                | HandleAnsw::KeyAdded { .. }
                | HandleAnsw::PropagateGroupChange { .. } => Ok(None),
                HandleAnsw::RcvMsg { peer, msg } => Ok(None),
            },
        }
    }

    /// Register an agent with this node. From this point on the node will act as the owner of this
    /// agent and handle any communication from/to this agent. Operation is asynchronous.
    ///
    /// This will block the handle until the agent has been correctly registered or an error happens.
    pub fn register_agent(&mut self, agent: &simag_core::Agent) -> OpId {
        // TODO: An agent can only be registered with a single node. If you try to register the same agent in
        // the same network more than once it will be an error.

        let agent_id = agent.id().to_string();
        let id = self.next_id();
        let msg = HandleCmd::RegisterAgent {
            op_id: id,
            agent_id,
        };
        let sender = self.sender.clone();
        GlobalExecutor::spawn(async move {
            sender.send(msg).await.map_err(|_| ())?;
            Ok::<_, ()>(())
        });
        id
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
        let msg = HandleCmd::ProvideResource {
            op_id,
            key,
            value: Resource::new_group(group),
        };
        let sender = self.sender.clone();
        GlobalExecutor::spawn(async move {
            sender.send(msg).await.map_err(|_| ())?;
            Ok::<_, ()>(())
        });
        op_id
    }

    /// Request joining to a given group, will return the network identifier of the group in case
    /// this agent is allowed to join the group. Operation is asynchronous.
    ///
    /// The petitioner must pass the settings used to evaluate if are compatible with the settings
    /// set by the owners of the group.
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
        let msg = HandleCmd::ReqJoinGroup {
            op_id,
            group_key,
            agent_id,
            group,
        };
        let sender = self.sender.clone();
        GlobalExecutor::spawn(async move {
            sender.send(msg).await.map_err(|_| ())?;
            Ok::<_, ()>(())
        });
        op_id
    }

    /// Operation is asynchronous.
    pub fn leave_group(&mut self, group_id: &str, agent_id: &str) -> OpId {
        todo!()
    }

    /// Find an agent in the network and try to open a connection with it. Operation is asynchronous.
    pub fn find_agent(&mut self, agent_id: &str) -> OpId {
        todo!()
    }

    /// Send a message to the given peer. Operation is asynchronous.
    pub fn send_message(&mut self, value: M, peer: PeerId) -> OpId {
        let id = self.next_id();
        self.pending.insert(
            id,
            PendingCmd {
                cmd: SentCmd::SentMsg,
                answ: None,
            },
        );
        let sender = self.sender.clone();
        GlobalExecutor::spawn(async move {
            let msg = HandleCmd::<M>::SendMessage {
                op_id: id,
                value,
                peer,
            };
            sender.send(msg).await.map_err(|_| ())?;
            Ok::<_, ()>(())
        });
        id
    }

    /// Request a key to the network. The result can be fetched asynchronously. Operation is asynchronous.
    pub fn get(&mut self, key: ResourceIdentifier) -> OpId {
        let id = self.next_id();
        let key_bytes: &[u8] = &key.borrow();
        self.pending.insert(
            id,
            PendingCmd {
                cmd: SentCmd::PullContent(key_bytes.to_owned()),
                answ: None,
            },
        );
        let msg = HandleCmd::PullResource { op_id: id, key };
        let sender = self.sender.clone();
        GlobalExecutor::spawn(async move {
            sender.send(msg).await.map_err(|_| ())?;
            Ok::<_, ()>(())
        });
        id
    }

    /// Set this peer as a provider for a certain key and value pair. Operation is asynchronous.
    pub fn put(&mut self, key: ResourceIdentifier, value: Resource) -> OpId {
        let id = self.next_id();
        let key_bytes: &[u8] = &key.borrow();
        self.pending.insert(
            id,
            PendingCmd {
                cmd: SentCmd::AddedKey(key_bytes.to_owned()),
                answ: None,
            },
        );
        let msg = HandleCmd::ProvideResource {
            op_id: id,
            key,
            value,
        };
        let sender = self.sender.clone();
        GlobalExecutor::spawn(async move {
            sender.send(msg).await.map_err(|_| ())?;
            Ok::<_, ()>(())
        });
        id
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
    pub fn is_running(&mut self) -> bool {
        if self.is_dead || self.process_answer_buffer().is_err() {
            return false;
        }

        self.sender
            .try_send(HandleCmd::IsRunning)
            .map(|_| true)
            .unwrap_or_else(|err| match err {
                TrySendError::Full(_) => true,
                TrySendError::Closed(_) => {
                    self.is_dead = true;
                    false
                }
            })
    }

    /// Commands the network to shutdown asynchronously, returns inmediately if the network has shutdown
    /// or if it already had disconnected for any reason.
    ///
    /// The network may not shutdown inmediately if still is listening and waiting for any events.
    pub fn shutdown(&mut self) -> Result<bool> {
        if self.is_dead {
            return Err(HandleError::Disconnected.into());
        }
        self.process_answer_buffer()?;
        if let Ok(answ) = self.try_getting_shutdown_answ() {
            return Ok(answ);
        }

        let msg_id = self.next_id();
        let msg = HandleCmd::Shutdown(msg_id);
        match self.sender.try_send(msg) {
            Ok(()) => {}
            Err(TrySendError::Full(_msg)) => {
                self.pending.insert(
                    msg_id,
                    PendingCmd {
                        cmd: SentCmd::ShutDown,
                        answ: None,
                    },
                );
            }
            Err(TrySendError::Closed(_)) => {
                self.is_dead = true;
                return Err(HandleError::Disconnected.into());
            }
        }

        self.process_answer_buffer()?;
        Ok(self.try_getting_shutdown_answ().unwrap_or(false))
    }

    /// Only should be instanced throught the network builder.
    pub(crate) fn new(
        sender: Sender<HandleCmd<M>>,
        rcv: Receiver<StdResult<HandleAnsw<M>, NetworkError>>,
        local_key: identity::ed25519::Keypair,
    ) -> Self {
        NetworkHandle {
            sender,
            rcv,
            local_key,
            pending: HashMap::new(),
            next_msg_id: 0,
            is_dead: false,
            stats: NetworkStats::default(),
            rcv_msg: HashMap::new(),
        }
    }

    fn try_getting_shutdown_answ(&mut self) -> Result<bool> {
        match self.pending.values().find(|p| p.shutdown()) {
            Some(PendingCmd {
                cmd: SentCmd::ShutDown,
                answ: Some(HandleAnsw::HasShutdown { op_id: id, answ }),
            }) => {
                let answ = *answ;
                let id = *id;
                self.pending.remove(&id.clone());
                Ok(answ)
            }
            Some(_) => Ok(false),
            _ => Err(HandleError::Disconnected.into()),
        }
    }

    fn process_answer_buffer(&mut self) -> Result<()> {
        loop {
            let msg = match self.rcv.try_recv() {
                Err(err) => {
                    self.received_error(err)?;
                    break; // queue is empty so finish
                }
                Ok(answ_res) => answ_res?, // disregard the result
            };
            match msg {
                HandleAnsw::HasShutdown { op_id: id, answ } => {
                    self.pending.entry(id).and_modify(|e| {
                        e.answ = Some(HandleAnsw::HasShutdown { op_id: id, answ });
                    });
                }
                HandleAnsw::KeyAdded { op_id: id } => {
                    if let Some(PendingCmd {
                        cmd: SentCmd::AddedKey(key),
                        ..
                    }) = self.pending.remove(&id)
                    {
                        log::debug!(
                            "Added key: {:?}",
                            ResourceIdentifier::try_from(&*key).unwrap()
                        );
                    }
                }
                HandleAnsw::GotRecord { key, .. } => {
                    self.stats.key_stats.entry(key).or_default().times_received += 1;
                }
                HandleAnsw::RcvMsg { peer, msg } => {
                    log::debug!("Received streaming msg from {}: {:?}", peer, msg);
                    if let Some(stats) = self.stats.rcv_msgs.iter_mut().find(|p| p.0 == peer) {
                        stats.1 += 1;
                    } else {
                        self.stats.rcv_msgs.push((peer.clone(), 1));
                    }
                    self.rcv_msg.entry(peer).or_default().push(msg);
                }
                HandleAnsw::AgentRegistered { op_id: id, key } => {
                    self.pending.entry(id).and_modify(|e| {
                        e.answ = Some(HandleAnsw::AgentRegistered { op_id: id, key });
                    });
                }
                HandleAnsw::PropagateGroupChange { group_id } => todo!(),
                HandleAnsw::ReqJoinGroupAccepted { op_id, group_id } => {
                    self.pending.entry(op_id).and_modify(|e| {
                        e.answ = Some(HandleAnsw::ReqJoinGroupAccepted { op_id, group_id });
                    });
                }
                HandleAnsw::ReqJoinGroupDenied {
                    op_id,
                    group_id,
                    reason,
                } => {
                    self.pending.entry(op_id).and_modify(|e| {
                        e.answ = Some(HandleAnsw::ReqJoinGroupDenied {
                            op_id,
                            group_id,
                            reason,
                        });
                    });
                }
            }
        }
        Ok(())
    }

    /// Called when an error is received from the network message queue.
    /// Sets a flag if the thread died.
    fn received_error(&mut self, err: TryRecvError) -> Result<()> {
        match err {
            TryRecvError::Closed => {
                self.is_dead = true;
                Err(HandleError::Disconnected.into())
            }
            TryRecvError::Empty => Ok(()),
        }
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
    rcv_msgs: Vec<(PeerId, usize)>,
}

impl Default for NetworkStats {
    fn default() -> Self {
        NetworkStats {
            key_stats: HashMap::new(),
            rcv_msgs: Vec::new(),
        }
    }
}

impl NetworkStats {
    pub fn for_key(&self, key: &ResourceIdentifier) -> Option<&KeyStats> {
        self.key_stats.get(key)
    }

    pub fn received_messages(&self) -> &[(PeerId, usize)] {
        &self.rcv_msgs
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
}

#[derive(Debug)]
pub(crate) enum HandleCmd<M> {
    /// query the network about current status
    IsRunning,
    /// put a resource in this node
    ProvideResource {
        op_id: OpId,
        key: ResourceIdentifier,
        value: Resource,
    },
    /// pull a resource in this node
    PullResource {
        op_id: OpId,
        key: ResourceIdentifier,
    },
    /// issue a shutdown command
    Shutdown(OpId),
    /// send a serialized message
    SendMessage { op_id: OpId, value: M, peer: PeerId },
    /// register a peer as the manager of an agent
    RegisterAgent { op_id: OpId, agent_id: String },
    /// instruct the network handler to send a request
    /// for an agent joining a group
    ReqJoinGroup {
        op_id: OpId,
        group_key: ResourceIdentifier,
        agent_id: Uuid,
        group: Group,
        // state: RequestState,
    },
    /// awaiting peer information to try connect
    AwaitingReqJoinGroup {
        op_id: OpId,
        group_key: ResourceIdentifier,
        agent_id: Uuid,
        group: Group,
    },
}

/// The answer of an operation.
#[derive(Clone, Debug)]
pub(crate) enum HandleAnsw<M>
where
    M: DeserializeOwned,
{
    AgentRegistered {
        op_id: OpId,
        key: ResourceIdentifier,
    },
    GotRecord {
        op_id: OpId,
        key: ResourceIdentifier,
    },
    HasShutdown {
        op_id: OpId,
        answ: bool,
    },
    KeyAdded {
        op_id: OpId,
    },
    /// Propagate a change in the configuration or composition of a group.
    PropagateGroupChange {
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
    RcvMsg {
        peer: PeerId,
        msg: M,
    },
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
    #[error("network op still running")]
    OpRunning,
    #[error("manager not found for op `{0}`")]
    ManagerNotFound(OpId),
    #[error("unexpected response")]
    UnexpectedResponse(OpId),
}

/// An identifier for an operation executed asynchronously. Used to fetch the results
/// of such operation in an asynchronous fashion.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct OpId(usize);

impl Display for OpId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        Ok(())
    }
}
