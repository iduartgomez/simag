use crate::{
    config::CONF,
    handle::{NetHandleAnsw, NetHandleCmd, NetworkHandle},
};
use crossbeam::{channel::bounded, Receiver, Sender};
use libp2p::{
    core::{muxing, upgrade},
    dns::DnsConfig,
    identify, identity, kad, mplex,
    multiaddr::Protocol,
    noise,
    tcp::TcpConfig,
    yamux, Multiaddr, NetworkBehaviour, PeerId, Swarm, Transport,
};
use std::{collections::HashSet, net::IpAddr};

const CURRENT_AGENT_VERSION: &str = "simag/0.1.0";
const CURRENT_IDENTIFY_PROTO_VERSION: &str = "ipfs/0.1.0";

/// A prepared network connection not yet running.
/// This can be either the preparation of a network bootstrap node or a new peer
/// connecting to an existing network.
pub struct Network {
    id: PeerId,
    sent_queries: HashSet<kad::QueryId>,
    swarm: Swarm<NetBehaviour>,
}

impl Network {
    /// Bootstrap the network with a listener which will act as the initial provider. This initial peer
    /// will be listening at the provided port and assigned IP. If those are not free it will error.
    ///
    /// ## Arguments
    /// - ip: IP associated to this node.
    /// - port: listening port.
    /// - peer: Optional connection to an already existing listener.
    /// - key: Optional identity key of this node; if it's not provided it will be either obtained
    ///        from the configuration or generated.
    pub fn bootstrap<T: Into<IpAddr>>(
        ip: T,
        port: u16,
        peer: Option<Provider>,
        key: Option<identity::ed25519::Keypair>,
    ) -> std::io::Result<NetworkHandle> {
        let peer_set = peer.is_some();
        let builder = network_builder::NetworkBuilder::new(peer, key);
        let local_key = builder.local_ed25519_key.clone();
        let mut network = builder.configure_network()?;

        let mut bootstrap_addr = Multiaddr::with_capacity(2);
        bootstrap_addr.push(Protocol::from(ip.into()));
        bootstrap_addr.push(Protocol::Tcp(port));
        let _listener_id = Swarm::listen_on(&mut network.swarm, bootstrap_addr).unwrap();
        if peer_set {
            let bootstrap_query = network.swarm.bootstrap();
            network.sent_queries.insert(bootstrap_query);
        }

        let (sender, rcv) = network.run_event_loop();
        Ok(NetworkHandle::new(sender, rcv, local_key))
    }

    /// Join an existing network as a new or old node (if the identity was already registered).
    pub fn join_network(bootstrap_info: Provider) -> std::io::Result<NetworkHandle> {
        let builder = network_builder::NetworkBuilder::new(Some(bootstrap_info), None);
        let local_key = builder.local_ed25519_key.clone();
        let mut network = builder.configure_network()?;

        let bootstrap_query = network.swarm.bootstrap();
        network.sent_queries.insert(bootstrap_query);

        let (sender, rcv) = network.run_event_loop();
        Ok(NetworkHandle::new(sender, rcv, local_key))
    }

    fn run_event_loop(mut self) -> (Sender<NetHandleCmd>, Receiver<NetHandleAnsw>) {
        let (query_send, query_rcv) = bounded::<NetHandleCmd>(10);
        let (answ_send, answ_rcv) = bounded::<NetHandleAnsw>(10);
        std::thread::spawn(|| {
            smol::run(async move {
                eprintln!("Setting #{} event loop", &self.id);
                loop {
                    while let Ok(msg) = query_rcv.try_recv() {
                        // Clean up the message buffer
                        match msg {
                            NetHandleCmd::IsRunning(_id) => {}
                            NetHandleCmd::Shutdown(id) => {
                                answ_send
                                    .send(NetHandleAnsw::HasShutdown { id, answ: true })
                                    .expect("SIMAG: parent main thread may be dead");
                                break;
                            }
                        }
                    }
                    match self.swarm.next().await {
                        NetEvent::KademliaEvent(event) => self.process_event(event),
                        NetEvent::Identify(identify::IdentifyEvent::Received {
                            peer_id,
                            observed_addr,
                            info,
                        }) => {
                            eprintln!(
                                "Received info from {} with following addr: {}",
                                peer_id, observed_addr
                            );
                            if info.protocol_version == CURRENT_IDENTIFY_PROTO_VERSION
                                && info.agent_version == CURRENT_AGENT_VERSION
                                && info.protocols.iter().any(|p| p == "/ipfs/kad/1.0.0")
                            {
                                self.swarm.add_address(&peer_id, observed_addr);
                            }
                        }
                        _ => {}
                    }
                }
            })
        });
        (query_send, answ_rcv)
    }

    fn process_event(&mut self, event: kad::KademliaEvent) {
        eprintln!("\nPeer #{} received event:\n {:?}", self.id, event);
        match event {
            kad::KademliaEvent::QueryResult { result, id, .. } => {
                if self.sent_queries.remove(&id) {
                    eprintln!("Got result from query id: {:?}", id);
                }
            }
            kad::KademliaEvent::RoutablePeer { .. } => {}
            kad::KademliaEvent::RoutingUpdated { .. } => {}
            kad::KademliaEvent::UnroutablePeer { .. } => {}
            kad::KademliaEvent::PendingRoutablePeer { .. } => {}
        }
    }
}

#[derive(NetworkBehaviour)]
#[behaviour(event_process = false)]
#[behaviour(out_event = "NetEvent")]
struct NetBehaviour {
    kad: kad::Kademlia<kad::store::MemoryStore>,
    identify: identify::Identify,
}

impl NetBehaviour {
    fn bootstrap(&mut self) -> kad::QueryId {
        self.kad
            .bootstrap()
            .expect("At least one peer is required when bootstrapping.")
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

impl From<(Multiaddr, PeerId)> for Provider {
    fn from(info: (Multiaddr, PeerId)) -> Self {
        let (addr, identifier) = info;
        Provider {
            addr: Some(addr),
            identifier: Some(identifier),
        }
    }
}

impl std::default::Default for Provider {
    fn default() -> Self {
        let conf = &CONF;
        let mut multi_addr = Multiaddr::with_capacity(2);
        multi_addr.push(Protocol::from(conf.bootstrap_ip));
        multi_addr.push(Protocol::Tcp(conf.bootstrap_port));
        let identifier = conf.bootstrap_id.clone()
            .expect("At least one public identifier is required to bootstrap the connection to the network.");
        Provider {
            addr: Some(multi_addr),
            identifier: Some(identifier),
        }
    }
}

mod network_builder {
    use super::*;

    pub(super) struct NetworkBuilder {
        /// ED25519 local peer private key.
        pub local_ed25519_key: identity::ed25519::Keypair,
        /// ED25519 local peer private key in generic format.
        local_key: identity::Keypair,
        /// The peer ID of this machine.
        local_peer_id: PeerId,
        /// At least an other running listener node is required for joining the network.
        /// Not necessary if this is an initial provider.
        remote_provider: Option<Provider>,
    }

    impl NetworkBuilder {
        pub fn new(
            remote_provider: Option<Provider>,
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
                local_ed25519_key: ed25519_key,
                local_key,
                local_peer_id,
                remote_provider,
            }
        }

        pub fn configure_network(self) -> std::io::Result<Network> {
            let transport = self.config_transport()?;
            let behav = NetBehaviour {
                kad: self.config_kademlia_dht(),
                identify: identify::Identify::new(
                    CURRENT_IDENTIFY_PROTO_VERSION.to_owned(),
                    CURRENT_AGENT_VERSION.to_owned(),
                    self.local_key.public(),
                ),
            };
            let swarm = Swarm::new(transport, behav, self.local_peer_id.clone());

            Ok(Network {
                swarm,
                sent_queries: HashSet::new(),
                id: self.local_peer_id,
            })
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
                .timeout(std::time::Duration::from_secs(20)))
        }

        fn config_kademlia_dht(&self) -> kad::Kademlia<kad::store::MemoryStore> {
            let store = kad::store::MemoryStore::new(self.local_peer_id.clone());
            let mut kdm = kad::Kademlia::new(self.local_peer_id.clone(), store);

            if let Some(remote_bootstrap) = &self.remote_provider {
                // There is already at least one existing node in the network. Add it to the table and query for bootstrapping.
                kdm.add_address(
                    remote_bootstrap.identifier.as_ref().unwrap(),
                    remote_bootstrap.addr.clone().unwrap(),
                );
            }
            kdm
        }
    }
}
