use crate::{
    config::CONF,
    handle::{NetHandleAnsw, NetHandleCmd, NetworkHandle},
    stream::{self, STREAM_PROTOCOL},
};
use kad::record::Key;
use libp2p::{
    core::{muxing, upgrade},
    dns::DnsConfig,
    identify, identity, kad, mplex,
    multiaddr::Protocol,
    noise,
    tcp::TcpConfig,
    yamux, Multiaddr, NetworkBehaviour, PeerId, Swarm, Transport,
};
use std::{borrow::Borrow, collections::HashMap, net::IpAddr, time::Duration};
use tokio::sync::mpsc::{channel, Receiver, Sender};

const CURRENT_AGENT_VERSION: &str = "simag/0.1.0";
const CURRENT_IDENTIFY_PROTO_VERSION: &str = "ipfs/0.1.0";
const TIMEOUT: u64 = 10;

/// A prepared network connection not yet running.
/// This can be either the preparation of a network bootstrap node or a new peer
/// connecting to an existing network.
pub(crate) struct Network<K, V>
where
    K: Borrow<[u8]> + Copy + Clone,
    V: Into<Vec<u8>>,
{
    id: PeerId,
    swarm: Swarm<NetBehaviour>,
    sent_kad_queries: HashMap<kad::QueryId, Option<NetHandleCmd<K, V>>>,
    answ_queue: Vec<NetHandleAnsw<K>>,
}

impl<K, V> Network<K, V>
where
    K: Borrow<[u8]> + Send + Clone + Copy + 'static,
    V: Into<Vec<u8>> + Send + 'static,
{
    fn run_event_loop(mut self) -> (Sender<NetHandleCmd<K, V>>, Receiver<NetHandleAnsw<K>>) {
        let (query_send, mut query_rcv) = channel::<NetHandleCmd<K, V>>(10);
        let (mut answ_send, answ_rcv) = channel::<NetHandleAnsw<K>>(10);
        std::thread::spawn(|| {
            smol::run(async move {
                eprintln!("Setting #{} event loop", &self.id);
                let mut cmd_queue = Vec::with_capacity(1);
                'main: loop {
                    tokio::select! {
                        answ = Network::push_msg_into_cmd_buffer(&mut cmd_queue, &mut query_rcv) => {
                            if let Err(()) = answ {
                                break 'main;
                            }
                        }
                        event = self.swarm.next() => self.process_event(event).await,
                    }
                    while let Some(cmd) = cmd_queue.pop() {
                        match cmd {
                            NetHandleCmd::ProvideResource { id, key, value } => {
                                let key = &key.borrow();
                                let record = kad::Record {
                                    key: Key::new(key),
                                    value: value.into(),
                                    publisher: Some(self.id.clone()),
                                    expires: None,
                                };
                                let qid = self.swarm.put_record(record, kad::Quorum::One).unwrap();
                                self.sent_kad_queries.insert(qid, None);
                                let qid = self.swarm.start_providing(Key::new(key)).unwrap();
                                self.sent_kad_queries.insert(qid, None);
                                let answ = NetHandleAnsw::KeyAdded { id };
                                self.answ_queue.push(answ);
                            }
                            NetHandleCmd::PullResource { id, key } => {
                                let qid = self
                                    .swarm
                                    .get_record(&Key::new(&key.borrow()), kad::Quorum::One);
                                self.sent_kad_queries
                                    .insert(qid, Some(NetHandleCmd::PullResource { id, key }));
                            }
                            NetHandleCmd::SendMessage { value, peer, .. } => {
                                self.swarm.as_mut().send_message(&peer, value);
                            }
                            NetHandleCmd::Shutdown(id) => {
                                let answ = NetHandleAnsw::HasShutdown { id, answ: true };
                                Network::<_, V>::return_and_finish(answ_send, answ).await;
                                break 'main;
                            }
                            NetHandleCmd::IsRunning(_id) => {}
                        }
                    }
                    while let Some(answ) = self.answ_queue.pop() {
                        if let Err(()) = Network::<_, V>::return_answ(&mut answ_send, answ).await {
                            break 'main;
                        }
                    }
                }
            })
        });
        (query_send, answ_rcv)
    }

    async fn push_msg_into_cmd_buffer(
        cmd_queue: &mut Vec<NetHandleCmd<K, V>>,
        query_rcv: &mut Receiver<NetHandleCmd<K, V>>,
    ) -> Result<(), ()> {
        match query_rcv.recv().await {
            Some(cmd) => {
                cmd_queue.push(cmd);
                Ok(())
            }
            None => Err(()),
        }
    }

    async fn return_answ(
        sender: &mut Sender<NetHandleAnsw<K>>,
        answ: NetHandleAnsw<K>,
    ) -> Result<(), ()> {
        if sender
            .send_timeout(answ, Duration::from_secs(TIMEOUT))
            .await
            .is_err()
        {
            eprintln!("Network handle dropped!");
            Err(())
        } else {
            Ok(())
        }
    }

    async fn return_and_finish(mut sender: Sender<NetHandleAnsw<K>>, answ: NetHandleAnsw<K>) {
        if sender
            .send_timeout(answ, Duration::from_secs(TIMEOUT))
            .await
            .is_err()
        {
            eprintln!("Network handle dropped!");
        }
    }

    async fn process_event(&mut self, event: NetEvent) {
        match event {
            NetEvent::KademliaEvent(event) => {
                eprintln!("\nPeer #{} received event:\n {:?}", self.id, event);
                if let kad::KademliaEvent::QueryResult { result, id, .. } = event {
                    match self.sent_kad_queries.remove(&id) {
                        Some(Some(NetHandleCmd::PullResource { id, key })) => {
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
                                    eprintln!(
                                        "\nRECEIVED KAD MSG: {}",
                                        String::from_utf8(value).unwrap()
                                    );
                                    self.answ_queue.push(NetHandleAnsw::GotRecord { id, key });
                                }
                            }
                        }
                        Some(None) => {
                            eprintln!("Got result from query id: {:?}", id);
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
                eprintln!(
                    "Received info from {} with following addr: {}",
                    peer_id, observed_addr
                );
                observed_addr.push(Protocol::P2p(peer_id.clone().into()));
                if info.protocol_version == CURRENT_IDENTIFY_PROTO_VERSION
                    && info.agent_version == CURRENT_AGENT_VERSION
                    && info.protocols.iter().any(|p| p == "/ipfs/kad/1.0.0")
                    && info.protocols.iter().any(|p| p == STREAM_PROTOCOL)
                {
                    self.swarm.add_address(&peer_id, observed_addr.clone());
                    self.swarm.as_mut().add_address(&peer_id, observed_addr);
                }
            }
            NetEvent::Stream(stream::StreamEvent::MessageReceived { msg, peer }) => {
                eprintln!(
                    "RECEIVED STREAMING MSG: {}",
                    String::from_utf8(msg.clone()).unwrap()
                );
                self.answ_queue.push(NetHandleAnsw::RcvMsg { msg, peer });
            }
            NetEvent::Stream(stream::StreamEvent::ConnectionError { peer, err }) => {
                eprintln!("Connection error with peer: {}:\n{}", peer, err);
            }
            NetEvent::Identify(_) => {}
        }
    }
}

#[derive(NetworkBehaviour)]
#[behaviour(event_process = false)]
#[behaviour(out_event = "NetEvent")]
struct NetBehaviour {
    kad: kad::Kademlia<kad::store::MemoryStore>,
    identify: identify::Identify,
    streams: stream::Stream,
}

impl NetBehaviour {
    fn bootstrap(&mut self) -> kad::QueryId {
        self.kad
            .bootstrap()
            .expect("At least one peer is required when bootstrapping.")
    }
}

impl AsMut<stream::Stream> for NetBehaviour {
    fn as_mut(&mut self) -> &mut stream::Stream {
        &mut self.streams
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

enum NetEvent {
    KademliaEvent(kad::KademliaEvent),
    Identify(identify::IdentifyEvent),
    Stream(stream::StreamEvent),
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

impl From<stream::StreamEvent> for NetEvent {
    fn from(event: stream::StreamEvent) -> NetEvent {
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

    pub fn build(self) -> std::io::Result<NetworkHandle<AgentKey, Vec<u8>>> {
        if (self.local_ip.is_none() || self.local_port.is_none())
            && self.remote_providers.is_empty()
        {
            // This is not an initial provider. At least one remote provider is required to join an existing network.
            // return Err();
            todo!()
        }

        let transport = self.config_transport()?;
        let behav = self.config_behaviour();

        let mut swarm = Swarm::new(transport, behav, self.local_peer_id.clone());

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

        let network: Network<AgentKey, Vec<u8>> = Network {
            swarm,
            sent_kad_queries: sent_queries,
            id: self.local_peer_id,
            answ_queue: Vec::with_capacity(1),
        };
        let (sender, rcv) = network.run_event_loop();
        Ok(NetworkHandle::new(sender, rcv, self.local_ed25519_key))
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

        let tcp = TcpConfig::new().nodelay(true);
        Ok(DnsConfig::new(tcp)?
            .upgrade(upgrade::Version::V1)
            .authenticate(noise::NoiseConfig::xx(noise_keys).into_authenticated())
            .multiplex(upgrade::SelectUpgrade::new(
                yamux::Config::default(),
                mplex::MplexConfig::default(),
            ))
            .map(|(peer, muxer), _| (peer, muxing::StreamMuxerBox::new(muxer)))
            .timeout(std::time::Duration::from_secs(TIMEOUT)))
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
            streams: stream::Stream::new(),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct AgentKey {}

impl Borrow<[u8]> for AgentKey {
    fn borrow(&self) -> &[u8] {
        &[0, 1, 2, 3]
    }
}

impl From<&[u8]> for AgentKey {
    fn from(_slice: &[u8]) -> Self {
        AgentKey {}
    }
}

impl std::fmt::Display for AgentKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Key!")
    }
}
