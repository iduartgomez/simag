use std::{
    borrow::Borrow,
    convert::TryFrom,
    fmt::Debug,
    net::IpAddr,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::Duration,
};

use dashmap::DashMap;
use libp2p::{
    core::{muxing, transport, upgrade},
    dns::TokioDnsConfig,
    futures::StreamExt,
    identify, identity,
    kad::{self, record::Key, QueryId},
    mplex,
    multiaddr::Protocol,
    noise,
    swarm::{SwarmBuilder, SwarmEvent},
    tcp::TokioTcpConfig,
    yamux, Multiaddr, PeerId, Swarm, Transport,
};
use serde::{de::DeserializeOwned, Serialize};
use tokio::sync::{
    mpsc::{channel, Receiver, Sender},
    Mutex,
};
use uuid::Uuid;

use crate::{
    agent::{AgentId, AgentPolicy},
    channel::{self, CHANNEL_PROTOCOL},
    config::{GlobalExecutor, CONF},
    group::{Group, GroupError, GroupManager, GroupPermits, GroupSettings},
    handle::{HandleAnsw, HandleCmd, NetworkHandle, OpId},
    message::Message,
    rpc::{agent_id_from_str, AgentRpc, Resource, ResourceIdentifier, ResourceKind},
};

const CURRENT_AGENT_VER: &str = "simag/0.1.0";
const CURRENT_IDENTIFY_PROTOC_VER: &str = "id/0.1.0";
const CURRENT_KAD_PROTOC_VER: &str = "/ipfs/kad/1.0.0";
const HANDLE_TIMEOUT_SECS: u64 = 5;
const PEER_DISCONNECT_SECS: u64 = 300;
/// Seconds to wait for an RPC response.
const RPC_RESPONSE_TIME_OUT_SECS: u16 = 60;

static KILL_HANDLER: AtomicBool = AtomicBool::new(false);

/// A prepared network connection not yet running.
/// This can be either the preparation of a network bootstrap node or a new peer
/// connecting to an existing network.
pub struct Network<M>
where
    M: DeserializeOwned + Serialize + Send + Sync,
{
    id: PeerId,
    key: identity::Keypair,
    listen_on: Option<(IpAddr, u16)>,
    swarm: Mutex<Swarm<NetBehaviour>>,
    /// Pending queries made to the DHT awaiting for response. In case they have been executed
    /// due to commands from the handle the value will be set, otherwise it will be none.
    sent_kad_queries: DashMap<kad::QueryId, Option<HandleCmd<M>>>,
    /// Ops which returned an unexpected result query result but may return at a later time successfully.
    dangling_ops: Mutex<Vec<HandleCmd<M>>>,
    /// A map from agent identifiers to peers which own that agent.
    agent_owners: DashMap<uuid::Uuid, PeerId>,
    /// Local storage of agent configurations set on this peer. Used to check against connection petition.
    agent_configurations: DashMap<ResourceIdentifier, AgentPolicy>,
    /// The queue for sending the results of the handle commands.
    handle_answ_queue: Sender<Result<HandleAnsw<M>, NetworkError>>,
    /// A copy of the sending end of the channel to be used internally to make callbacks
    /// to it's own command queue.
    cmd_callback_queue: Sender<HandleCmd<M>>,
    /// Some RPC require maintaining a state while ongoing.
    rpc_state: DashMap<OpId, OpState>,
}

enum OpState {
    ReqJoinGroup(JoinReq),
}

/*
TODO: profile which is better: have two different threads handling the event rcv
and the cmd hanling or leave it all under one single thread and avoid sync friction
by having the swarm behind a mutex.
*/

impl<M> Network<M>
where
    M: DeserializeOwned + Serialize + Debug + Send + Sync + 'static,
{
    /// ## Arguments
    /// - query_rcv: The receiving end of a channel used to received commands from the network handle.
    fn run_event_loop(self, mut query_rcv: Receiver<HandleCmd<M>>) {
        let shared_handler = Arc::new(self);

        let sh = shared_handler.clone();
        GlobalExecutor::spawn(async move {
            // If both an IP and listening port are set listen on that socket.
            if let Some((ip, port)) = sh.listen_on {
                let mut swarm = sh.swarm.lock().await;
                let mut bootstrap_addr = Multiaddr::with_capacity(2);
                bootstrap_addr.push(Protocol::from(ip));
                bootstrap_addr.push(Protocol::Tcp(port));
                swarm
                    .listen_on(bootstrap_addr)
                    .map_err(|_| NetworkError::IrrecoverableError)?;
            }

            // set the main inbound connection event loop
            while !KILL_HANDLER.load(Ordering::Relaxed) {
                let mut swarm = sh.swarm.lock().await;
                let get_next =
                    tokio::time::timeout(Duration::from_nanos(100), swarm.select_next_some());
                // unlock the swarm to allow the thread handling commands to send messages, time to time
                if let Ok(SwarmEvent::Behaviour(event)) = get_next.await {
                    match sh.process_event(event, &mut *swarm).await {
                        Ok(_) => {}
                        Err(err) => {
                            log::error!("Error ocurred while processing event: {}", err);
                            // attempt to send back to the handle, optimistically locking
                            Self::return_answ(&sh, Err(NetworkError::IrrecoverableError)).await?;
                            break;
                        }
                    }
                }
            }
            Ok::<_, NetworkError>(())
        });

        GlobalExecutor::spawn(async move {
            // set the command handling event loop
            while !KILL_HANDLER.load(Ordering::Relaxed) {
                let f = tokio::time::timeout(Duration::from_millis(100), query_rcv.recv());
                // timeout time to time to check if the handle still is alive as well to see
                // if from an other thread a kill handle signal was sent
                match f.await {
                    Ok(Some(cmd)) => {
                        let sh = shared_handler.clone();
                        tokio::spawn(sh.process_handle_cmd(cmd));
                    }
                    Ok(None) => break,
                    Err(_) => {}
                }
            }
        });
    }

    async fn process_handle_cmd(self: Arc<Self>, cmd: HandleCmd<M>) -> Result<(), NetworkError> {
        let swarm = &mut *self.swarm.lock().await;
        match cmd {
            HandleCmd::RegisterGroup {
                op_id,
                group_key: key,
                group,
            } => {
                let qid = self
                    .provide_value(swarm, key, Resource::new_group(group), op_id)
                    .await?;
                self.return_answ(Ok(HandleAnsw::AwaitingRegistration { op_id, qid }))
                    .await?;
            }
            HandleCmd::RegisterAgent {
                op_id,
                agent_id,
                config,
            } => {
                let (key, mut res) = Resource::agent(agent_id, self.id.clone());
                res.as_peer(self.id.clone());
                self.agent_configurations.insert(key, config);
                Swarm::external_addresses(&*swarm).for_each(|a| {
                    res.with_address(a.addr.clone());
                });
                let qid = self
                    .provide_value(swarm, key, Resource::new_agent(res), op_id)
                    .await?;
                self.return_answ(Ok(HandleAnsw::AwaitingRegistration { op_id, qid }))
                    .await?;
            }
            HandleCmd::SendMessage { value, peer, .. } => {
                let msg = tokio::task::block_in_place(|| Message::build(&value, &self.key, true));
                match msg {
                    Ok(msg) => {
                        swarm.behaviour_mut().send_message(&peer, msg);
                    }
                    Err(_) => {
                        log::error!("Failed building a message from data: {:?}", value);
                    }
                }
            }
            HandleCmd::AwaitingRegistration { .. } => {}
            HandleCmd::ReqJoinGroup {
                op_id,
                group_key,
                agent_id,
                group,
            } => {
                // get the group resource from network
                let key_borrowed: &&[u8] = &group_key.borrow();
                let qid = swarm
                    .behaviour_mut()
                    .get_record(&Key::new(key_borrowed), kad::Quorum::Majority);
                self.sent_kad_queries.insert(
                    qid,
                    Some(HandleCmd::ReqJoinGroup {
                        op_id,
                        group_key,
                        agent_id,
                        group,
                    }),
                );
            }
            HandleCmd::FindGroupManager { .. } => {
                // TODO: check if there is a manager registered for this group in this host
                log::error!("FindGroupManager cannot be sent by the handle!");
            }
            HandleCmd::ConnectToAgent { agent_id, op_id } => {
                let ag_id = ResourceIdentifier::unique(&agent_id.0);
                let key_borrowed: &&[u8] = &ag_id.borrow();
                let qid = swarm
                    .behaviour_mut()
                    .get_record(&Key::new(key_borrowed), kad::Quorum::One);
                self.sent_kad_queries
                    .insert(qid, Some(HandleCmd::ConnectToAgent { op_id, agent_id }));
            }
            HandleCmd::SendMessageToAg {
                agent_id,
                value,
                op_id,
            } => {
                todo!()
            }
            HandleCmd::IsRunning => {} // will get an answer from the channel, so is active
            HandleCmd::Shutdown(id) => {
                KILL_HANDLER.store(true, Ordering::Relaxed);
                let answ = HandleAnsw::HasShutdown {
                    op_id: id,
                    answ: true,
                };
                self.return_answ(Ok(answ)).await?;
            }
        }
        Ok(())
    }

    async fn process_event(
        self: &Arc<Self>,
        event: NetEvent,
        swarm: &mut Swarm<NetBehaviour>,
    ) -> Result<(), NetworkError> {
        match event {
            NetEvent::KademliaEvent(event) => match event {
                kad::KademliaEvent::OutboundQueryCompleted { result, id, .. } => {
                    process_kad_msg(self, swarm, id, result).await?;
                }
                _ => {}
            },
            NetEvent::Identify(identify::IdentifyEvent::Received { peer_id, mut info }) => {
                if info.protocol_version == CURRENT_IDENTIFY_PROTOC_VER
                    && info.agent_version == CURRENT_AGENT_VER
                    && info.protocols.iter().any(|p| p == CURRENT_KAD_PROTOC_VER)
                    && info.protocols.iter().any(|p| p == CHANNEL_PROTOCOL)
                {
                    info.observed_addr
                        .push(Protocol::P2p(peer_id.clone().into()));
                    swarm
                        .behaviour_mut()
                        .add_address(&peer_id, info.observed_addr);
                }
            }
            NetEvent::Stream(channel::ChannelEvent::MessageReceived { msg, peer }) => {
                if let Some(data) = msg.data {
                    let msg: M =
                        tokio::task::spawn_blocking(|| -> Result<M, Box<bincode::ErrorKind>> {
                            bincode::deserialize_from(std::io::Cursor::new(data))
                        })
                        .await??;
                    self.return_answ(Ok(HandleAnsw::RcvMsg {
                        msg,
                        peer: peer.clone(),
                    }))
                    .await?;
                }

                match msg.rpc {
                    Some(msg) => {
                        process_rpc_msg(self, swarm, msg, &peer).await?;
                    }
                    None => {}
                }
            }
            NetEvent::Stream(channel::ChannelEvent::ConnectionError { peer, err }) => {
                log::error!("Connection error with peer: {}:\n{}", peer, err);
            }
            NetEvent::Identify(_) => {}
        }
        Ok(())
    }

    async fn provide_value(
        self: &Arc<Self>,
        swarm: &mut Swarm<NetBehaviour>,
        key: ResourceIdentifier,
        resource: Resource,
        op_id: OpId,
    ) -> Result<QueryId, NetworkError> {
        let key_borrow: &&[u8] = &key.borrow();
        let value = bincode::serialize(&resource)?;
        let record = kad::Record {
            key: Key::new(key_borrow),
            value,
            publisher: Some(self.id.clone()),
            expires: None,
        };

        let qid = swarm
            .behaviour_mut()
            .start_providing(Key::new(key_borrow))
            .unwrap();
        self.sent_kad_queries.insert(qid, None);
        let qid = swarm
            .behaviour_mut()
            .put_record(record, kad::Quorum::One)
            .unwrap();
        self.sent_kad_queries.insert(
            qid,
            Some(HandleCmd::AwaitingRegistration {
                op_id,
                rss_key: key,
                resource,
            }),
        );

        Ok(qid)
    }

    /// Returns an answer to the handle.
    async fn return_answ(
        self: &Arc<Self>,
        answ: Result<HandleAnsw<M>, NetworkError>,
    ) -> Result<(), NetworkError> {
        let answ = self
            .handle_answ_queue
            .send_timeout(answ, Duration::from_secs(HANDLE_TIMEOUT_SECS))
            .await;
        if answ.is_err() {
            log::debug!("Network handle dropped");
            Err(NetworkError::HandleDropped)
        } else {
            Ok(())
        }
    }

    async fn find_dangling_op(
        self: &Arc<Self>,
        key: Key,
    ) -> Result<Option<HandleCmd<M>>, NetworkError> {
        let mut ops = self.dangling_ops.lock().await;
        if let Some(idx) = ops.iter().position(|cmd| {
            if let HandleCmd::AwaitingRegistration { rss_key, .. } = cmd {
                key.as_ref() == rss_key.as_ref()
            } else {
                false
            }
        }) {
            Ok(Some(ops.remove(idx)))
        } else {
            Ok(None)
        }
    }
}

async fn process_kad_msg<M>(
    nt: &Arc<Network<M>>,
    swarm: &mut Swarm<NetBehaviour>,
    query_id: kad::QueryId,
    result: kad::QueryResult,
) -> Result<(), NetworkError>
where
    M: DeserializeOwned + Serialize + Send + Sync + Debug + 'static,
{
    let ev_reg_cmd = { nt.sent_kad_queries.remove(&query_id).map(|r| r.1) };
    match ev_reg_cmd {
        Some(Some(HandleCmd::ReqJoinGroup {
            op_id,
            group_key,
            agent_id,
            group,
        })) => {
            if let kad::QueryResult::GetRecord(Ok(kad::GetRecordOk { mut records, .. })) = result {
                // TODO: this can be retrieved from local storage if already exists
                // TODO: should ensure consistency across the network!
                if let Some(record) = records.pop() {
                    init_req_to_join_group(
                        &nt,
                        &mut *swarm,
                        op_id,
                        agent_id,
                        record,
                        group_key,
                        group,
                    )
                    .await?;
                }
            } else {
                nt.return_answ(Err(NetworkError::ManagerNotFound(op_id)))
                    .await?;
                return Ok(());
            }
        }
        Some(Some(HandleCmd::FindGroupManager {
            op_id,
            group_key,
            group,
            agent_id,
        })) => {
            if let kad::QueryResult::GetRecord(Ok(kad::GetRecordOk { mut records, .. })) = result {
                if records.is_empty() {
                    nt.return_answ(Err(NetworkError::ManagerNotFound(op_id)))
                        .await?;
                    return Ok(());
                }

                // the answer should consist of a single record from the peer owning the agent which manages the group
                if let Some(rec) = records.pop() {
                    let kad::PeerRecord {
                        record: kad::Record { value, .. },
                        ..
                    } = rec;
                    let msg: Resource = tokio::task::spawn_blocking(|| -> Result<Resource, _> {
                        bincode::deserialize_from(std::io::Cursor::new(value))
                    })
                    .await??;
                    if let ResourceKind::Agent(ag) = msg.0 {
                        match ag.peer {
                            Some(peer) => {
                                nt.agent_owners.insert(agent_id.0, peer);
                            }
                            None => {
                                nt.return_answ(Err(NetworkError::ManagerNotFound(op_id)))
                                    .await?;
                                return Ok(());
                            }
                        }
                    } else {
                        log::error!("Received unexpected response while retrieving the managers for {} from agent `{}`", group_key, agent_id.0);
                        nt.return_answ(Err(NetworkError::UnexpectedResponse(op_id)))
                            .await?;
                        return Ok(());
                    }

                    // Found at least one agent which manages that group; request joining again.
                    let cmd = HandleCmd::ReqJoinGroup {
                        op_id,
                        group_key,
                        agent_id,
                        group,
                    };
                    nt.cmd_callback_queue
                        .send(cmd)
                        .await
                        .map_err(|_| NetworkError::CallbackError)?;
                }
            }
        }
        Some(Some(HandleCmd::ConnectToAgent { op_id, .. })) => {
            match result {
                kad::QueryResult::GetRecord(Ok(kad::GetRecordOk { records, .. })) => {
                    for rec in records {
                        let kad::PeerRecord {
                            record: kad::Record { value, .. },
                            ..
                        } = rec;
                        if let Resource(ResourceKind::Agent(agent)) = bincode::deserialize(&value)?
                        {
                            let key = agent.identifier();
                            nt.return_answ(Ok(HandleAnsw::AgentFound {
                                op_id,
                                rss_key: key,
                            }))
                            .await?;

                            // register the agent owner
                            nt.agent_owners.insert(
                                agent.agent_id,
                                agent
                                    .peer
                                    .ok_or_else(|| NetworkError::UnexpectedResponse(op_id))?,
                            );

                            // propagate this agent in the network
                            let key: &&[u8] = &key.borrow();
                            let record = kad::Record {
                                key: Key::new(key),
                                value,
                                publisher: Some(nt.id.clone()),
                                expires: None,
                            };
                            swarm
                                .behaviour_mut()
                                .put_record(record, kad::Quorum::One)
                                .unwrap();

                            // TODO: attempt an initial handshake by sending a RPC to the agent and check
                            // if connection is allowed at all
                        } else {
                            todo!("return an error here");
                        }
                    }
                }
                _other => {}
            }
        }
        Some(Some(HandleCmd::AwaitingRegistration { op_id, rss_key, .. })) => match result {
            kad::QueryResult::PutRecord(Ok(kad::PutRecordOk { key })) => {
                log::info!("Succesfully published rec {}", rss_key);
                if let Some(HandleCmd::AwaitingRegistration { op_id, rss_key, .. }) =
                    nt.find_dangling_op(key).await?
                {
                    if rss_key.is_agent() {
                        nt.return_answ(Ok(HandleAnsw::AgentRegistered { op_id, rss_key }))
                            .await?;
                    }
                } else {
                    if let Some(_cmd) = nt.sent_kad_queries.remove(&query_id) {
                        if rss_key.is_agent() {
                            nt.return_answ(Ok(HandleAnsw::AgentRegistered { op_id, rss_key }))
                                .await?;
                        }
                    }
                }
            }
            kad::QueryResult::PutRecord(Err(kad::PutRecordError::QuorumFailed { .. })) => {
                log::error!(
                    "Failed publishing {}, didn't reach quorum approval",
                    rss_key
                );
                nt.return_answ(Ok(HandleAnsw::AgentRegistered { op_id, rss_key }))
                    .await?;
            }
            kad::QueryResult::PutRecord(Err(kad::PutRecordError::Timeout { key, .. })) => {
                log::error!("Timed out while publishing rec {:?}", key);
            }
            kad::QueryResult::RepublishRecord(Ok(kad::PutRecordOk { key })) => {
                log::info!("Successfully republished rec {:?}", key);
            }
            kad::QueryResult::RepublishRecord(Err(kad::PutRecordError::QuorumFailed {
                key,
                ..
            })) => {
                log::error!(
                    "Failed republishing rec {:?}, didn't reach quorum approval",
                    key
                );
            }
            kad::QueryResult::RepublishRecord(Err(kad::PutRecordError::Timeout {
                key, ..
            })) => {
                log::error!("Timed out while republishing rec {:?}", key);
            }
            _ => {}
        },
        Some(_) | None => match result {
            kad::QueryResult::StartProviding(Err(kad::AddProviderError::Timeout { key })) => {
                log::error!("Timed out while trying to provide rec {:?}", key);
            }
            kad::QueryResult::RepublishProvider(Err(kad::AddProviderError::Timeout { key })) => {
                log::error!("Timed out while republishing provider rec {:?}", key);
            }
            kad::QueryResult::RepublishProvider(Ok(kad::AddProviderOk { key })) => {
                log::info!("Republished providing rec {:?}", key);
            }
            kad::QueryResult::PutRecord(Ok(kad::PutRecordOk { key })) => {
                log::info!("Successfully put rec {:?} with {:?}", key, &query_id);
                if let Some(HandleCmd::AwaitingRegistration { op_id, rss_key, .. }) =
                    nt.find_dangling_op(key).await?
                {
                    if rss_key.is_agent() {
                        nt.return_answ(Ok(HandleAnsw::AgentRegistered { op_id, rss_key }))
                            .await?;
                    }
                } else {
                    if let Some(Some(HandleCmd::AwaitingRegistration { op_id, rss_key, .. })) =
                        nt.sent_kad_queries.remove(&query_id).map(|r| r.1)
                    {
                        if rss_key.is_agent() {
                            nt.return_answ(Ok(HandleAnsw::AgentRegistered { op_id, rss_key }))
                                .await?;
                        }
                    }
                };
            }
            kad::QueryResult::PutRecord(Err(kad::PutRecordError::Timeout { .. })) => {
                log::info!("put err t/o");
            }
            kad::QueryResult::PutRecord(Err(kad::PutRecordError::QuorumFailed { key, .. })) => {
                log::error!("failed putting key {:?}", key);
            }
            _ => {}
        },
    }

    Ok(())
}

/// Processes an RPC message and returns the appropiate return message to the network handle
/// depending on the king of message received.
///
/// ## Arguments
/// - peer: the peer which sent the message peer: &PeerId,
async fn process_rpc_msg<M>(
    nt: &Arc<Network<M>>,
    swarm: &mut Swarm<NetBehaviour>,
    rpc: AgentRpc,
    peer: &PeerId,
) -> Result<(), NetworkError>
where
    M: DeserializeOwned + Serialize + Send + Sync + Debug + 'static,
{
    match rpc {
        AgentRpc::ReqGroupJoin {
            op_id,
            group_id,
            agent_id,
            permits: (read, write),
            settings,
        } => {
            let mut permits = GroupPermits::new();
            if read {
                permits.read.insert(agent_id);
            }
            if write {
                permits.write.insert(agent_id);
            }
            let mut manager = GroupManager::new();
            match manager.request_joining(agent_id, group_id, &*settings, permits) {
                Ok(_) => {
                    nt.return_answ(Ok(HandleAnsw::PropagateGroupChange { op_id, group_id }))
                        .await?;
                    let msg =
                        Message::rpc(AgentRpc::ReqGroupJoinAccepted { op_id, group_id }, None);
                    swarm.behaviour_mut().send_message(peer, msg);
                }
                Err(reason) => {
                    let msg = Message::rpc(
                        AgentRpc::ReqGroupJoinDenied {
                            op_id,
                            group_id,
                            reason,
                        },
                        None,
                    );
                    swarm.behaviour_mut().send_message(peer, msg);
                }
            }
        }
        AgentRpc::ReqGroupJoinAccepted { op_id, group_id } => {
            // if the state was already dropped it means an other manager answered first
            if let Some(_) = nt.rpc_state.remove(&op_id) {
                // TODO: set open channel for pub/sub within the group topics
                nt.return_answ(Ok(HandleAnsw::ReqJoinGroupAccepted { op_id, group_id }))
                    .await?;
            }
        }
        AgentRpc::ReqGroupJoinDenied {
            op_id,
            group_id,
            reason,
        } => {
            // if the state was already dropped it means an other manager answered first
            if let Some(_) = nt.rpc_state.remove(&op_id) {
                nt.return_answ(Ok(HandleAnsw::ReqJoinGroupDenied {
                    op_id,
                    group_id,
                    reason,
                }))
                .await?;
            }
        }
    }

    Ok(())
}

struct JoinReq {
    op_id: OpId,
    /// Identifier of the agent making the request.
    agent_id: AgentId,
    group_key: ResourceIdentifier,
    group_managers: Vec<PeerId>,
    /// Number of managers contacted so far to request access.
    managers_requested: usize,
    /// Number of managers to contact before giving up.
    managers_total: usize,
    settings: Box<dyn GroupSettings>,
}

impl Into<OpState> for JoinReq {
    fn into(self) -> OpState {
        OpState::ReqJoinGroup(self)
    }
}

/// Will send a request to join if managing peers have been locally cached.
/// Otherwise will query the network to find any group managing peers.
///
/// The following paths can happen when performing a request:
/// 1. Receive a response from the first batch of managers contacted,
///   then the next ones should be discarded.
/// 2. None are found, then retry with an other batch, until exhausted
///   (this means this function will be called a 2nd, 3rd, N times).
/// 3. Once exhausted return an error if not found to the handle.
async fn init_req_to_join_group<M>(
    network: &Arc<Network<M>>,
    swarm: &mut Swarm<NetBehaviour>,
    op_id: OpId,
    agent_id: AgentId,
    record: kad::PeerRecord,
    expected_key: ResourceIdentifier,
    ori_group: Group,
) -> Result<(), NetworkError>
where
    M: Serialize + DeserializeOwned + Send + Sync + Debug + 'static,
{
    let kad::PeerRecord {
        record: kad::Record { key, value, .. },
        ..
    } = record;
    let group_key = ResourceIdentifier::try_from(key.borrow()).unwrap();
    let group: Group =
        tokio::task::spawn_blocking(|| bincode::deserialize_from(std::io::Cursor::new(value)))
            .await??;

    if group_key != expected_key || ori_group.id != group.id {
        return Err(NetworkError::UnexpectedResponse(op_id));
    }

    // request joining to owners, owners will propagate the change in membership,
    // if accepted, across nodes which belong to the group
    let mut group_managers = Vec::with_capacity(group.owners.len());
    for owner in group.owners.iter() {
        if let Some(peer) = network.agent_owners.get(owner) {
            group_managers.push(peer.value().clone());
        }
    }

    if group_managers.is_empty() {
        // try getting at least one of the peers managing one of the owners
        let owner = group
            .owners
            .iter()
            .next()
            .expect("there should be at least one always");
        let owner_key = ResourceIdentifier::unique(owner);
        let owner_key: &&[u8] = &owner_key.borrow();
        let qid = swarm
            .behaviour_mut()
            .get_record(&Key::new(owner_key), kad::Quorum::One);
        // wait until getting a response to continue
        network.sent_kad_queries.insert(
            qid,
            Some(HandleCmd::FindGroupManager {
                op_id,
                group_key,
                agent_id,
                group: group.clone(),
            }),
        );
        let state = JoinReq {
            op_id,
            agent_id,
            group_key,
            group_managers,
            managers_requested: 0,
            managers_total: 0,
            settings: group.settings.box_cloned(),
        };
        network.rpc_state.insert(op_id, state.into());
    } else {
        let mut managers_requested = 0;
        // TODO: only requests the first 10, if nothing returns will fail,
        // should keep trying until all are exhausted
        for peer in group_managers.iter().take(10) {
            let rpc = AgentRpc::ReqGroupJoin {
                op_id,
                agent_id,
                group_id: group.id,
                permits: (true, true),
                settings: ori_group.settings.box_cloned(),
            };
            managers_requested += 1;
            let msg = Message::rpc(rpc, Some(RPC_RESPONSE_TIME_OUT_SECS));
            swarm.behaviour_mut().send_message(peer, msg);
        }

        log::debug!("Requested {} group managers to join", managers_requested);
        let managers_total = group_managers.len();
        let state = JoinReq {
            op_id,
            agent_id,
            group_key,
            group_managers,
            managers_requested,
            managers_total,
            settings: group.settings.box_cloned(),
        };
        network.rpc_state.insert(op_id, state.into());
    }

    Ok(())
}

async fn retry_req_join_group<M>(
    network: &Arc<Network<M>>,
    swarm: &mut Swarm<NetBehaviour>,
    op_id: OpId,
) -> Result<(), NetworkError>
where
    M: Serialize + DeserializeOwned + Send + Sync + Debug + 'static,
{
    let states = &network.rpc_state;
    let JoinReq {
        op_id,
        agent_id,
        group_key,
        group_managers,
        mut managers_requested,
        managers_total,
        settings,
    } = match states.remove(&op_id).map(|r| r.1) {
        Some(OpState::ReqJoinGroup(req)) => req,
        None => {
            // this means that a response was already received from an other request
            return Ok(());
        }
    };

    let group_id = group_key.get_group_id().unwrap();
    if managers_total == managers_requested {
        // failed to get any response whatsoever, return an error
        let msg = HandleAnsw::ReqJoinGroupDenied {
            op_id,
            group_id,
            reason: GroupError::ManagerNotFound,
        };
        network.return_answ(Ok(msg)).await?;
        return Err(NetworkError::UnexpectedResponse(op_id));
    }

    for peer in group_managers.iter().skip(managers_requested).take(10) {
        let rpc = AgentRpc::ReqGroupJoin {
            op_id,
            agent_id,
            group_id,
            permits: (true, true),
            settings: settings.box_cloned(),
        };
        managers_requested += 1;
        let msg = Message::rpc(rpc, Some(RPC_RESPONSE_TIME_OUT_SECS));
        swarm.behaviour_mut().send_message(peer, msg);
    }

    let state = JoinReq {
        op_id,
        agent_id,
        group_key,
        group_managers,
        managers_requested,
        managers_total,
        settings,
    };
    states.insert(op_id, state.into());

    Ok(())
}

#[derive(thiserror::Error, Debug)]
pub enum NetworkError {
    #[error("callback command queue failed")]
    CallbackError,
    #[error(transparent)]
    ExecTaskFailed(#[from] tokio::task::JoinError),
    #[error("the handle droppped")]
    HandleDropped,
    #[error("handle not available")]
    HandleNotResponding,
    #[error("irrecoverable error in the network")]
    IrrecoverableError,
    #[error("manager not found for op `{0}`")]
    ManagerNotFound(OpId),
    #[error(transparent)]
    SerializationError(#[from] Box<bincode::ErrorKind>),
    #[error("unexpected response")]
    UnexpectedResponse(OpId),
    #[error("received a shutdown signal from the handle")]
    ShutdownSignal,
}

#[derive(libp2p::NetworkBehaviour)]
#[behaviour(event_process = false)]
#[behaviour(out_event = "NetEvent")]
pub(crate) struct NetBehaviour {
    kad: kad::Kademlia<kad::store::MemoryStore>,
    identify: identify::Identify,
    streams: channel::Channel,
}

impl NetBehaviour {
    fn bootstrap(&mut self) -> kad::QueryId {
        self.kad
            .bootstrap()
            .expect("At least one peer is required when bootstrapping.")
    }

    fn send_message(&mut self, peer: &PeerId, msg: Message) {
        self.streams.send_message(peer, msg);
    }

    fn add_address(&mut self, peer: &PeerId, address: Multiaddr) {
        self.kad.add_address(peer, address.clone());
        self.streams.add_address(peer, address);
    }
}

impl std::ops::DerefMut for NetBehaviour {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.kad
    }
}

impl std::ops::Deref for NetBehaviour {
    type Target = kad::Kademlia<kad::store::MemoryStore>;

    fn deref(&self) -> &Self::Target {
        &self.kad
    }
}

pub(crate) enum NetEvent {
    KademliaEvent(kad::KademliaEvent),
    Identify(identify::IdentifyEvent),
    Stream(channel::ChannelEvent),
}

impl From<kad::KademliaEvent> for NetEvent {
    fn from(event: kad::KademliaEvent) -> NetEvent {
        Self::KademliaEvent(event)
    }
}

impl From<identify::IdentifyEvent> for NetEvent {
    fn from(event: identify::IdentifyEvent) -> NetEvent {
        Self::Identify(event)
    }
}

impl From<channel::ChannelEvent> for NetEvent {
    fn from(event: channel::ChannelEvent) -> NetEvent {
        Self::Stream(event)
    }
}

/// Initial listening provider peer to bootstrap the network.
#[derive(Clone)]
pub struct Provider {
    addr: Option<Multiaddr>,
    identifier: Option<PeerId>,
}

impl Provider {
    pub fn new() -> Provider {
        Provider {
            addr: None,
            identifier: None,
        }
    }

    pub fn decode_peer_id<T: AsMut<[u8]>>(mut bytes: T) -> PeerId {
        PeerId::from_public_key(
            identity::Keypair::Ed25519(identity::ed25519::Keypair::decode(bytes.as_mut()).unwrap())
                .public(),
        )
    }

    /// IP which will be assigned to this node.
    pub fn listening_ip<T: Into<IpAddr>>(mut self, ip: T) -> Self {
        if let Some(addr) = &mut self.addr {
            addr.push(Protocol::from(ip.into()));
        } else {
            self.addr = Some(Multiaddr::from(ip.into()));
        }
        self
    }

    /// TCP listening port (only required in case of using TCP as transport).
    /// If not specified port 7800 will be used as default.
    pub fn listening_port(mut self, port: u16) -> Self {
        if let Some(addr) = &mut self.addr {
            addr.push(Protocol::Tcp(port));
        } else {
            self.addr = Some(Multiaddr::from(Protocol::Tcp(port)));
        }
        self
    }

    pub fn with_identifier(mut self, id: PeerId) -> Self {
        self.identifier = Some(id);
        self
    }
}

impl std::default::Default for Provider {
    fn default() -> Self {
        let conf = &CONF;
        let mut multi_addr = Multiaddr::with_capacity(2);
        let identifier = conf.bootstrap_id.clone()
            .expect("At least one public identifier is required to bootstrap the connection to the network.");
        multi_addr.push(Protocol::from(conf.bootstrap_ip));
        multi_addr.push(Protocol::Tcp(conf.bootstrap_port));
        Provider {
            addr: Some(multi_addr),
            identifier: Some(identifier),
        }
    }
}

pub struct NetworkBuilder {
    /// ED25519 local peer private key.
    local_ed25519_key: identity::ed25519::Keypair,
    /// ED25519 local peer private key in generic format.
    local_key: identity::Keypair,
    /// The peer ID of this machine.
    local_peer_id: PeerId,

    // optional local info, in case this is an initial provider
    /// IP to bind to the listener
    local_ip: Option<IpAddr>,
    /// socket port to bind to the listener
    local_port: Option<u16>,

    /// At least an other running listener node is required for joining the network.
    /// Not necessary if this is an initial provider.
    remote_providers: Vec<Provider>,
}

impl NetworkBuilder {
    /// When instancing a network you can either join an existing one or bootstrap a new network with a listener
    /// which will act as the initial provider. This initial peer will be listening at the provided port and assigned IP.
    /// If those are not free the instancing process will return an error.
    ///
    /// In other to bootstrap a new network the following arguments are required to be provided to the builder:
    /// - ip: IP associated to the initial node.
    /// - port: listening port of the initial node.
    ///
    /// If both are provided but also additional peers are added via the [add_provider] method, this node will
    /// be listening but also try to connect to an existing peer.
    pub fn configure_network() -> NetworkBuilder {
        let local_ed25519_key = if let Some(key) = &CONF.local_peer_keypair {
            key.clone()
        } else {
            identity::ed25519::Keypair::generate()
        };
        let local_key = identity::Keypair::Ed25519(local_ed25519_key.clone());
        let local_peer_id = PeerId::from(local_key.public());
        NetworkBuilder {
            local_ed25519_key,
            local_key,
            local_peer_id,
            remote_providers: Vec::with_capacity(1),
            local_ip: None,
            local_port: None,
        }
    }

    pub fn with_port(mut self, port: u16) -> Self {
        self.local_port = Some(port);
        self
    }

    pub fn with_ip<T: Into<IpAddr>>(mut self, ip: T) -> Self {
        self.local_ip = Some(ip.into());
        self
    }

    /// Optional identity key of this node.
    /// If not provided it will be either obtained from the configuration or freshly generated.
    pub fn with_key(mut self, key: identity::ed25519::Keypair) -> Self {
        self.local_key = identity::Keypair::Ed25519(key.clone());
        self.local_ed25519_key = key;
        self.local_peer_id = PeerId::from(self.local_key.public());
        self
    }

    /// Connection info for an already existing peer. Required in case this is not a bootstrapping node.
    pub fn add_provider(mut self, peer: Provider) -> Self {
        self.remote_providers.push(peer);
        self
    }

    /// Builds the default implementation of network with a custom message kind.
    pub fn build<Message>(self) -> std::io::Result<NetworkHandle<Message>>
    where
        Message: Serialize + DeserializeOwned + Debug + Send + Sync + 'static,
    {
        if (self.local_ip.is_none() || self.local_port.is_none())
            && self.remote_providers.is_empty()
        {
            // This is not an initial provider. At least one remote provider is required to join an existing network.
            // return Err();
            todo!()
        }

        let transport = self.config_transport()?;
        let behav = self.config_behaviour();

        let mut swarm = {
            let builder = SwarmBuilder::new(transport, behav, self.local_peer_id.clone())
                .executor(Box::new(GlobalExecutor::new()));
            builder.build()
        };

        let sent_queries = DashMap::new();
        if self
            .remote_providers
            .iter()
            .filter(|p| Some(&self.local_peer_id) != p.identifier.as_ref())
            .next()
            .is_some()
        {
            let bootstrap_query = swarm.behaviour_mut().bootstrap();
            sent_queries.insert(bootstrap_query, None);
        }

        let (query_send, query_rcv) = channel::<HandleCmd<Message>>(100);
        let (answ_send, answ_rcv) = channel(100);

        let network: Network<Message> = Network {
            listen_on: self.local_ip.zip(self.local_port),
            key: self.local_key,
            swarm: Mutex::new(swarm),
            sent_kad_queries: sent_queries,
            dangling_ops: Mutex::new(Vec::new()),
            id: self.local_peer_id,
            handle_answ_queue: answ_send,
            agent_owners: DashMap::new(),
            agent_configurations: DashMap::new(),
            cmd_callback_queue: query_send.clone(),
            rpc_state: DashMap::new(),
        };
        network.run_event_loop(query_rcv);
        Ok(NetworkHandle::new(
            query_send,
            answ_rcv,
            self.local_ed25519_key,
        ))
    }

    fn config_transport(
        &self,
    ) -> std::io::Result<transport::Boxed<(PeerId, muxing::StreamMuxerBox)>> {
        let noise_keys = noise::Keypair::<noise::X25519Spec>::new()
            .into_authentic(&self.local_key)
            .expect("Signing libp2p-noise static DH keypair failed.");

        let tcp = TokioTcpConfig::new()
            .nodelay(true)
            // FIXME: there seems to be a problem with the deflate upgrade 
            // that repeteadly allocates more space on the heap until OOM
            // .and_then(|conn, endpoint| {
            //     upgrade::apply(
            //         conn,
            //         DeflateConfig::default(),
            //         endpoint,
            //         upgrade::Version::V1,
            //     )
            // });
            ;
        Ok(TokioDnsConfig::system(tcp)?
            .upgrade(upgrade::Version::V1)
            .authenticate(noise::NoiseConfig::xx(noise_keys).into_authenticated())
            .multiplex(upgrade::SelectUpgrade::new(
                yamux::YamuxConfig::default(),
                mplex::MplexConfig::default(),
            ))
            .timeout(std::time::Duration::from_secs(PEER_DISCONNECT_SECS))
            .map(|(peer, muxer), _| (peer, muxing::StreamMuxerBox::new(muxer)))
            .boxed())
    }

    fn config_behaviour(&self) -> NetBehaviour {
        let store = kad::store::MemoryStore::new(self.local_peer_id.clone());
        let mut config = kad::KademliaConfig::default();
        config.set_connection_idle_timeout(Duration::from_secs(PEER_DISCONNECT_SECS));
        let mut kad = kad::Kademlia::with_config(self.local_peer_id.clone(), store, config);

        for remote_bootstrap in &self.remote_providers {
            // There is already at least one existing node in the network. Add it to the table and query for bootstrapping.
            kad.add_address(
                remote_bootstrap.identifier.as_ref().unwrap(),
                remote_bootstrap.addr.clone().unwrap(),
            );
        }

        let ident_config = identify::IdentifyConfig::new(
            CURRENT_IDENTIFY_PROTOC_VER.to_string(),
            self.local_key.public(),
        )
        .with_agent_version(CURRENT_AGENT_VER.to_string());

        NetBehaviour {
            kad,
            identify: identify::Identify::new(ident_config),
            streams: channel::Channel::new(),
        }
    }
}
