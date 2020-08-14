use crate::config::CONF;
use crossbeam::{channel::bounded, Sender};
use libp2p::{
    core::{muxing, upgrade, PublicKey},
    dns::DnsConfig,
    identity,
    kad::{store::MemoryStore, Kademlia, KademliaEvent, QueryId},
    mplex,
    multiaddr::Protocol,
    noise,
    tcp::TcpConfig,
    yamux, Multiaddr, PeerId, Swarm, Transport,
};
use std::{collections::HashSet, io::Write, net::IpAddr};

/// A prepared network connection not yet running.
/// This can be either the preparation of a network bootstrap node or a new peer
/// connecting to an existing network.
pub struct Network {
    id: PeerId,
    swarm: Swarm<Kademlia<MemoryStore>>,
    sent_queries: HashSet<QueryId>,
}

/// A handle to a running network connection.
pub struct NetworkHandle {
    sender: Sender<String>,
    local_key: identity::ed25519::Keypair,
}

impl NetworkHandle {
    /// Get this peer id encoded as a Base58 string.
    pub fn get_peer_id(&self) -> String {
        peer_id_from_ed25519(self.local_key.public()).to_base58()
    }

    /// Saves this peer secret key to a file in bytes. This file should be kept in a secure location.
    pub fn save_secret_key<T: AsRef<std::path::Path>>(&self, path: T) -> std::io::Result<()> {
        let enconded_key = self.local_key.encode().to_vec();
        use std::fs::File;
        let mut file = File::create(path.as_ref())?;
        file.write_all(enconded_key.as_slice())
    }

    /// Returns Ok while the connection to the network still is running or Err if
    /// it shutdowns.
    pub fn is_running(&self) -> Result<(), ()> {
        Ok(())
    }
}

struct NetworkBuilder {
    /// ED25519 local peer private key.
    ed25519_key: identity::ed25519::Keypair,
    /// ED25519 local peer private key in generic format.
    local_key: identity::Keypair,
    /// The peer ID of this machine
    local_peer_id: PeerId,
    /// If this is a bootstrapping node the bootstrap info is required.
    bootstrap: Option<BootstrapListener>,
}

impl NetworkBuilder {
    fn set_transport(
        &self,
    ) -> std::io::Result<
        impl Transport<
                Output = (
                    PeerId,
                    impl libp2p::core::muxing::StreamMuxer<
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
                mplex::MplexConfig::new(),
            ))
            .map(|(peer, muxer), _| (peer, muxing::StreamMuxerBox::new(muxer)))
            .timeout(std::time::Duration::from_secs(20)))
    }

    fn set_kademlia_dht(&self) -> (Option<QueryId>, Kademlia<MemoryStore>) {
        let store = MemoryStore::new(self.local_peer_id.clone());
        let mut kdm = Kademlia::new(self.local_peer_id.clone(), store);
        let mut qid = None;
        if let Some(bootstrap_info) = &self.bootstrap {
            // There is already at least one existing node in the network. Add it to the table and query for bootstrapping.
            kdm.add_address(
                &bootstrap_info.identifier.clone().unwrap(),
                bootstrap_info.addr.clone().unwrap(),
            );
            qid = Some(kdm.bootstrap().expect("Bootstrapping peer not found"));
        }
        (qid, kdm)
    }
}

impl Network {
    /// Bootstrap the network with a listener which will act as the initial provider. This initial peer
    /// will be listening at the provided port and assigned IP. If those are not free it will error.
    pub fn bootstrap<T: Into<IpAddr>>(
        ip: T,
        port: u16,
        key: Option<identity::ed25519::Keypair>,
    ) -> std::io::Result<NetworkHandle> {
        let builder = Network::connection_info(None, key);
        let swarm = {
            let transport = builder.set_transport()?;
            let (_, kdm) = builder.set_kademlia_dht();
            Swarm::new(transport, kdm, builder.local_peer_id.clone())
        };

        let mut multi_addr = Multiaddr::with_capacity(2);
        multi_addr.push(Protocol::from(ip.into()));
        multi_addr.push(Protocol::Tcp(port));
        let mut network = Network {
            swarm,
            sent_queries: HashSet::new(),
            id: builder.local_peer_id,
        };
        let _listener_id = Swarm::listen_on(&mut network.swarm, multi_addr).unwrap();
        let sender = network.run_event_loop();

        Ok(NetworkHandle {
            sender,
            local_key: builder.ed25519_key,
        })
    }

    /// Join an existing network as a new or old node (if the identity was already registered).
    pub fn join_network(bootstrap_info: BootstrapListener) -> std::io::Result<NetworkHandle> {
        let builder = Network::connection_info(Some(bootstrap_info), None);
        let mut sent_queries = HashSet::new();
        let swarm = {
            let transport = builder.set_transport()?;
            let (qid, kdm) = builder.set_kademlia_dht();
            sent_queries.insert(qid.unwrap());
            Swarm::new(transport, kdm, builder.local_peer_id.clone())
        };

        let network = Network {
            swarm,
            sent_queries,
            id: builder.local_peer_id,
        };
        let sender = network.run_event_loop();

        Ok(NetworkHandle {
            sender,
            local_key: builder.ed25519_key,
        })
    }

    fn run_event_loop(mut self) -> Sender<String> {
        let (sender, receiver) = bounded::<String>(100);
        std::thread::spawn(|| {
            smol::run(async move {
                eprintln!("Setting #{} event loop", &self.id);
                loop {
                    if let Ok(msg) = receiver.try_recv() {
                        eprintln!("Received: {}", msg);
                    }
                    let event = self.swarm.next().await;
                    self.process_event(event);
                }
            })
        });
        sender
    }

    fn process_event(&mut self, event: KademliaEvent) {
        eprintln!("Peer #{} received event:\n {:?}", self.id, event);
        match event {
            KademliaEvent::QueryResult { result, id, .. } => {
                if self.sent_queries.remove(&id) {
                    eprintln!("Peer #{} got result from query id: {:?}", self.id, id);
                }
            }
            KademliaEvent::RoutablePeer { .. } => {}
            KademliaEvent::RoutingUpdated { .. } => {}
            KademliaEvent::UnroutablePeer { .. } => {}
            KademliaEvent::PendingRoutablePeer { .. } => {}
        }
    }

    fn connection_info(
        bootstrap: Option<BootstrapListener>,
        key: Option<identity::ed25519::Keypair>,
    ) -> NetworkBuilder {
        let ed25519_key = {
            if let Some(key) = key {
                key
            } else if let Some(key) = &CONF.local_peer_keypair {
                key.clone()
            } else {
                identity::ed25519::Keypair::generate()
            }
        };
        let local_key = identity::Keypair::Ed25519(ed25519_key.clone());
        let local_peer_id = PeerId::from(local_key.public());
        NetworkBuilder {
            ed25519_key,
            local_key,
            local_peer_id,
            bootstrap,
        }
    }
}

#[derive(Clone)]
pub struct BootstrapListener {
    addr: Option<Multiaddr>,
    identifier: Option<PeerId>,
}

impl BootstrapListener {
    pub fn new() -> BootstrapListener {
        BootstrapListener {
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

impl From<(Multiaddr, PeerId)> for BootstrapListener {
    fn from(info: (Multiaddr, PeerId)) -> Self {
        let (addr, identifier) = info;
        BootstrapListener {
            addr: Some(addr),
            identifier: Some(identifier),
        }
    }
}

impl std::default::Default for BootstrapListener {
    fn default() -> Self {
        let conf = &CONF;
        let mut multi_addr = Multiaddr::with_capacity(2);
        multi_addr.push(Protocol::from(conf.bootstrap_ip));
        multi_addr.push(Protocol::Tcp(conf.bootstrap_port));
        let identifier = conf.bootstrap_id.clone()
            .expect("At least one public identifier is required to bootstrap the connection to the network.");
        BootstrapListener {
            addr: Some(multi_addr),
            identifier: Some(identifier),
        }
    }
}

fn peer_id_from_ed25519(key: identity::ed25519::PublicKey) -> PeerId {
    PeerId::from_public_key(PublicKey::Ed25519(key))
}
