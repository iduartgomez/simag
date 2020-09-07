use crate::{
    channel::{self, CHANNEL_PROTOCOL},
    config::{GlobalExecutor, CONF, PEER_TIMEOUT_SECS},
    handle::{HandleAnsw, HandleCmd, NetworkHandle},
    message::Message,
    rpc::{AgentRpc, GroupRes, Resource, ResourceIdentifier},
};
use kad::record::Key;
use libp2p::{
    core::{muxing, upgrade},
    deflate::DeflateConfig,
    dns::DnsConfig,
    identify, identity, kad, mplex,
    multiaddr::Protocol,
    noise,
    swarm::SwarmBuilder,
    tcp::TokioTcpConfig,
    yamux, Multiaddr, NetworkBehaviour, PeerId, Swarm, Transport,
};
use serde::{de::DeserializeOwned, Serialize};
use std::{
    borrow::Borrow, collections::HashMap, convert::TryFrom, fmt::Debug, hash::Hash, net::IpAddr,
    sync::Arc, time::Duration,
};
use tokio::sync::{
    mpsc::{channel, Receiver, Sender},
    Mutex,
};

const CURRENT_AGENT_VERSION: &str = "simag/0.1.0";
const CURRENT_IDENTIFY_PROTO_VERSION: &str = "ipfs/0.1.0";
const HANDLE_TIMEOUT_SECS: u64 = 5;

/// A prepared network connection not yet running.
/// This can be either the preparation of a network bootstrap node or a new peer
/// connecting to an existing network.
pub struct Network<M>
where
    M: DeserializeOwned + Serialize + Send + Sync,
{
    pub(crate) id: PeerId,
    pub(crate) key: identity::Keypair,
    pub(crate) swarm: Mutex<Swarm<NetBehaviour>>,
    pub(crate) sent_kad_queries: Mutex<HashMap<kad::QueryId, Option<HandleCmd<M>>>>,
    pub(crate) answ_send: Mutex<Sender<HandleAnsw<M>>>,
}

// pub(crate) type DefaultHandleCmd<M> = HandleCmd<ResourceIdentifier, Resource, M, AgentRpc>;
// pub(crate) type DefaultHandledAnsw<M> = HandleAnsw<ResourceIdentifier, M>;

impl<M> Network<M>
where
    M: DeserializeOwned + Serialize + Debug + Send + Sync + 'static,
{
    /// specialized version of the event loop
    fn run_event_loop(self, mut query_rcv: Receiver<HandleCmd<M>>) {
        let shared_handler = Arc::new(self);

        // set the main event loop
        let sh = shared_handler.clone();
        GlobalExecutor::spawn(sh.process_events());

        // set the command handling thread
        GlobalExecutor::spawn(async move {
            loop {
                if let Some(cmd) = query_rcv.recv().await {
                    let sh = shared_handler.clone();
                    tokio::spawn(sh.process_handle_cmd(cmd));
                }
            }
        });
    }

    async fn process_handle_cmd(self: Arc<Self>, cmd: HandleCmd<M>) -> Result<(), ()> {
        let mut swarm = self.swarm.lock().await;
        match cmd {
            HandleCmd::ProvideResource { id, key, value } => {
                self.provide_value(key, value).await?;
                let answ = HandleAnsw::KeyAdded { id };
                self.return_answ(answ).await?;
            }
            HandleCmd::PullResource { id, key } => {
                let key_borrowed: &&[u8] = &key.borrow();
                let qid = swarm.get_record(&Key::new(key_borrowed), kad::Quorum::One);
                self.sent_kad_queries
                    .lock()
                    .await
                    .insert(qid, Some(HandleCmd::PullResource { id, key }));
            }
            HandleCmd::SendMessage { value, peer, .. } => {
                let msg = tokio::task::block_in_place(|| Message::build(&value, &self.key, true));
                match msg {
                    Ok(msg) => {
                        swarm.send_message(&peer, msg);
                    }
                    Err(_) => {
                        log::error!("Failed building a message from data: {:?}", value);
                    }
                }
            }
            HandleCmd::Shutdown(id) => {
                let answ = HandleAnsw::HasShutdown { id, answ: true };
                self.return_answ(answ).await?;
            }
            HandleCmd::Rpc(rpc) => {
                match rpc {
                    AgentRpc::RegisterAgent { agent_id } => {
                        let (key, mut res) = Resource::agent(agent_id);
                        res.as_peer(self.id.clone());
                        Swarm::external_addresses(&*swarm)
                            .for_each(|a| res.with_address(a.clone()));
                        self.provide_value(key, Resource::new_agent(res)).await?;
                        let answ = HandleAnsw::AgentRegistered { key };
                        self.return_answ(answ).await?;
                    }
                    AgentRpc::JoinGroup {
                        resource_key,
                        agent_id,
                        group,
                    } => {
                        // get the group resource from network
                        let key_borrowed: &&[u8] = &resource_key.borrow();
                        let qid = swarm.get_record(&Key::new(key_borrowed), kad::Quorum::Majority);

                        self.sent_kad_queries.lock().await.insert(
                            qid,
                            Some(HandleCmd::Rpc(AgentRpc::JoinGroup {
                                resource_key,
                                agent_id,
                                group,
                            })),
                        );
                    }
                }
            }
            HandleCmd::IsRunning => {}
        }
        Ok(())
    }

    async fn process_events(self: Arc<Self>) {
        loop {
            let mut swarm = self.swarm.lock().await;
            let get_next = tokio::time::timeout(Duration::from_nanos(10), swarm.next());
            if let Ok(event) = get_next.await {
                match event {
                    NetEvent::KademliaEvent(event) => {
                        // log::debug!("\nPeer #{} received event:\n {:?}", self.id, event);
                        if let kad::KademliaEvent::QueryResult { result, id, .. } = event {
                            match self.sent_kad_queries.lock().await.remove(&id) {
                                Some(Some(HandleCmd::PullResource { id, key })) => {
                                    if let kad::QueryResult::GetRecord(Ok(kad::GetRecordOk {
                                        records,
                                        ..
                                    })) = result
                                    {
                                        for rec in records {
                                            let kad::PeerRecord {
                                                record: kad::Record { value, .. },
                                                ..
                                            } = rec;
                                            let msg: Resource = tokio::task::spawn_blocking(|| {
                                                bincode::deserialize_from(std::io::Cursor::new(
                                                    value,
                                                ))
                                                .unwrap()
                                            })
                                            .await
                                            .unwrap();
                                            log::debug!("Received kademlia msg: {:?}", msg);
                                            self.return_answ(HandleAnsw::GotRecord { id, key })
                                                .await
                                                .unwrap();
                                        }
                                    } else {
                                        unreachable!()
                                    }
                                }
                                Some(Some(HandleCmd::Rpc(rpc))) => {
                                    if let kad::QueryResult::GetRecord(Ok(kad::GetRecordOk {
                                        records,
                                        ..
                                    })) = result
                                    {
                                        if let AgentRpc::JoinGroup {
                                            resource_key: expected_key,
                                            agent_id,
                                            group,
                                        } = rpc
                                        {
                                            for record in records {
                                                let kad::PeerRecord {
                                                    peer,
                                                    record: kad::Record { key, value, .. },
                                                } = record;
                                                let group_key =
                                                    ResourceIdentifier::try_from(key.borrow())
                                                        .unwrap();
                                                let group: GroupRes =
                                                    bincode::deserialize(&*value).unwrap();

                                                if group_key != expected_key {
                                                    panic!()
                                                }

                                                // request joining to owners, owners will propagate the change
                                                // in membership across nodes which belong to the group
                                                // receive confirmation that the agent has been added to the group
                                            }
                                        } else {
                                            unreachable!()
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    NetEvent::Identify(identify::IdentifyEvent::Received {
                        peer_id,
                        mut observed_addr,
                        info,
                    }) => {
                        if info.protocol_version == CURRENT_IDENTIFY_PROTO_VERSION
                            && info.agent_version == CURRENT_AGENT_VERSION
                            && info.protocols.iter().any(|p| p == "/ipfs/kad/1.0.0")
                            && info.protocols.iter().any(|p| p == CHANNEL_PROTOCOL)
                        {
                            observed_addr.push(Protocol::P2p(peer_id.clone().into()));
                            swarm.add_address(&peer_id, observed_addr);
                        }
                    }
                    NetEvent::Stream(channel::ChannelEvent::MessageReceived { msg, peer }) => {
                        let msg: M = tokio::task::spawn_blocking(|| {
                            bincode::deserialize_from(std::io::Cursor::new(msg)).unwrap()
                        })
                        .await
                        .unwrap();
                        self.return_answ(HandleAnsw::RcvMsg { msg, peer })
                            .await
                            .unwrap();
                    }
                    NetEvent::Stream(channel::ChannelEvent::ConnectionError { peer, err }) => {
                        log::debug!("Connection error with peer: {}:\n{}", peer, err);
                    }
                    NetEvent::Identify(_) => {}
                }
            }
            // drop the swarm lock after work or timeout so work can be done in other thread
            std::mem::drop(swarm);
        }
    }

    pub(crate) async fn provide_value(
        self: &Arc<Self>,
        key: ResourceIdentifier,
        value: Resource,
    ) -> Result<(), ()> {
        let key: &&[u8] = &key.borrow();
        let value = tokio::task::spawn_blocking(move || -> Result<Vec<u8>, ()> {
            bincode::serialize(&value).map_err(|_| ())
        })
        .await
        .unwrap()?;
        let record = kad::Record {
            key: Key::new(key),
            value,
            publisher: Some(self.id.clone()),
            expires: None,
        };

        let mut swarm = self.swarm.lock().await;
        let mut sent_kad_queries = self.sent_kad_queries.lock().await;

        let qid = swarm.put_record(record, kad::Quorum::All).map_err(|_| ())?;
        sent_kad_queries.insert(qid, None);
        let qid = swarm.start_providing(Key::new(key)).map_err(|_| ())?;
        sent_kad_queries.insert(qid, None);

        Ok(())
    }

    pub(crate) async fn return_answ(self: &Arc<Self>, answ: HandleAnsw<M>) -> Result<(), ()> {
        let mut sender = self.answ_send.lock().await;
        let answ = sender
            .send_timeout(answ, Duration::from_secs(HANDLE_TIMEOUT_SECS))
            .await;
        if answ.is_err() {
            log::debug!("Network handle dropped!");
            Err(())
        } else {
            Ok(())
        }
    }
}

#[derive(NetworkBehaviour)]
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

pub trait KadKey:
    Borrow<[u8]>
    + Eq
    + Hash
    + Debug
    + Clone
    + Send
    + Sync
    + for<'a> TryFrom<&'a [u8], Error = std::io::Error>
    + 'static
{
}

impl<T> KadKey for T where
    T: Borrow<[u8]>
        + Eq
        + Hash
        + Debug
        + Clone
        + Send
        + Sync
        + for<'a> TryFrom<&'a [u8], Error = std::io::Error>
        + 'static
{
}

pub trait KadValue: Serialize + DeserializeOwned + Debug + Send + Sync + 'static {}

impl<T> KadValue for T where T: Serialize + DeserializeOwned + Debug + Send + Sync + 'static {}

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

        // If both an IP and listening port are set listen on that socket.
        if let (Some(ip), Some(port)) = (self.local_ip, self.local_port) {
            let mut bootstrap_addr = Multiaddr::with_capacity(2);
            bootstrap_addr.push(Protocol::from(ip));
            bootstrap_addr.push(Protocol::Tcp(port));
            Swarm::listen_on(&mut swarm, bootstrap_addr).unwrap();
        }

        let mut sent_queries = HashMap::new();
        if !self.remote_providers.is_empty() {
            let bootstrap_query = swarm.bootstrap();
            sent_queries.insert(bootstrap_query, None);
        }

        let (query_send, query_rcv) = channel::<HandleCmd<Message>>(10);
        let (answ_send, answ_rcv) = channel::<HandleAnsw<Message>>(10);

        let network: Network<Message> = Network {
            key: self.local_key,
            swarm: Mutex::new(swarm),
            sent_kad_queries: Mutex::new(sent_queries),
            id: self.local_peer_id,
            answ_send: Mutex::new(answ_send),
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
    ) -> std::io::Result<
        impl Transport<
                Output = (
                    PeerId,
                    impl muxing::StreamMuxer<
                            OutboundSubstream = impl Send,
                            Substream = impl Send,
                            Error = impl Into<std::io::Error>,
                        > + Send
                        + Sync,
                ),
                Listener = impl Send,
                ListenerUpgrade = impl Send,
                Dial = impl Send,
                Error = impl std::error::Error + Send,
            > + Clone,
    > {
        let noise_keys = noise::Keypair::<noise::X25519Spec>::new()
            .into_authentic(&self.local_key)
            .expect("Signing libp2p-noise static DH keypair failed.");

        let tcp = TokioTcpConfig::new()
            .nodelay(true)
            .and_then(|conn, endpoint| {
                upgrade::apply(
                    conn,
                    DeflateConfig::default(),
                    endpoint,
                    upgrade::Version::V1,
                )
            });
        Ok(DnsConfig::new(tcp)?
            .upgrade(upgrade::Version::V1)
            .authenticate(noise::NoiseConfig::xx(noise_keys).into_authenticated())
            .multiplex(upgrade::SelectUpgrade::new(
                yamux::Config::default(),
                mplex::MplexConfig::default(),
            ))
            .map(|(peer, muxer), _| (peer, muxing::StreamMuxerBox::new(muxer)))
            .timeout(std::time::Duration::from_secs(PEER_TIMEOUT_SECS)))
    }

    fn config_behaviour(&self) -> NetBehaviour {
        let store = kad::store::MemoryStore::new(self.local_peer_id.clone());
        let mut kad = kad::Kademlia::new(self.local_peer_id.clone(), store);

        for remote_bootstrap in &self.remote_providers {
            // There is already at least one existing node in the network. Add it to the table and query for bootstrapping.
            kad.add_address(
                remote_bootstrap.identifier.as_ref().unwrap(),
                remote_bootstrap.addr.clone().unwrap(),
            );
        }

        NetBehaviour {
            kad,
            identify: identify::Identify::new(
                CURRENT_IDENTIFY_PROTO_VERSION.to_owned(),
                CURRENT_AGENT_VERSION.to_owned(),
                self.local_key.public(),
            ),
            streams: channel::Channel::new(),
        }
    }
}
