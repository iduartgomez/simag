use crate::config::CONF;
use libp2p::{
    core::{muxing, upgrade, PublicKey},
    dns::DnsConfig,
    identity,
    kad::{store::MemoryStore, Kademlia},
    mplex,
    multiaddr::Protocol,
    noise,
    tcp::TcpConfig,
    yamux, Multiaddr, PeerId, Swarm, Transport,
};
use std::{io::Write, net::IpAddr};

pub struct Network {
    swarm: Swarm<Kademlia<MemoryStore>>,
    pub local_key: identity::ed25519::Keypair,
}

struct NetworkBuilder {
    local_key: identity::Keypair,
    /// The peer ID of this machine
    local_peer_id: PeerId,
    /// If this is a bootstrapping node the bootstrap info is required.
    bootstrap: Option<BootstrapPeer>,
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

    fn set_kademlia_dht(&self) -> Kademlia<MemoryStore> {
        let store = MemoryStore::new(self.local_peer_id.clone());
        let mut kdm = Kademlia::new(self.local_peer_id.clone(), store);
        if let Some(bootstrap_info) = &self.bootstrap {
            // There is already at least one existing node in the network. Add it to the table and query for bootstrapping.
            kdm.add_address(
                &bootstrap_info.identifier.clone().unwrap(),
                bootstrap_info.addr.clone().unwrap(),
            );
            kdm.bootstrap().expect("Bootstrapping peer not found");
        }
        kdm
    }
}

impl Default for Network {
    fn default() -> Self {
        let bootstrap_info = BootstrapPeer::default();
        Network::join_network(bootstrap_info).expect("Couldn't initialize the network.")
    }
}

impl Network {
    /// Bootstrap the network with a peer which will act as the initial provider. This initial peer
    /// will be listening at the provided port and assigned IP. If those are not free it will error.
    pub fn bootstrap<T: Into<IpAddr>>(_ip: T, _port: u16) -> std::io::Result<Network> {
        let ed25519_key = Network::get_key();
        let builder = Network::connection_info(None);
        let swarm = {
            let transport = builder.set_transport()?;
            let kdm = builder.set_kademlia_dht();
            Swarm::new(transport, kdm, builder.local_peer_id)
        };

        Ok(Network {
            swarm,
            local_key: ed25519_key,
        })
    }

    /// Join an existing network as a new or old node (if the identity was already registered).
    pub fn join_network(bootstrap_info: BootstrapPeer) -> std::io::Result<Network> {
        let ed25519_key = Network::get_key();
        let builder = Network::connection_info(Some(bootstrap_info));
        let swarm = {
            let transport = builder.set_transport()?;
            let kdm = builder.set_kademlia_dht();
            Swarm::new(transport, kdm, builder.local_peer_id)
        };

        Ok(Network {
            swarm,
            local_key: ed25519_key,
        })
    }

    /// Saves this peer secret key to a file in bytes. This file should be kept in a secure location.
    pub fn save_secret_key<T: AsRef<std::path::Path>>(&self, path: T) -> std::io::Result<()> {
        let enconded_key = self.local_key.encode().to_vec();
        use std::fs::File;
        let mut file = File::create(path.as_ref())?;
        file.write_all(enconded_key.as_slice())
    }

    /// Get this peer id encoded as a Base58 string.
    pub fn get_peer_id(&self) -> String {
        PeerId::from_public_key(PublicKey::Ed25519(self.local_key.public())).to_base58()
    }

    fn get_key() -> identity::ed25519::Keypair {
        if let Some(key) = &CONF.local_peer_keypair {
            key.clone()
        } else {
            identity::ed25519::Keypair::generate()
        }
    }

    fn connection_info(bootstrap: Option<BootstrapPeer>) -> NetworkBuilder {
        let ed25519_key = if let Some(key) = &CONF.local_peer_keypair {
            key.clone()
        } else {
            identity::ed25519::Keypair::generate()
        };
        let local_key = identity::Keypair::Ed25519(ed25519_key);
        let local_peer_id = PeerId::from(local_key.public());
        NetworkBuilder {
            local_key,
            local_peer_id,
            bootstrap,
        }
    }
}

#[derive(Clone)]
pub struct BootstrapPeer {
    addr: Option<Multiaddr>,
    identifier: Option<PeerId>,
}

impl BootstrapPeer {
    pub fn new() -> BootstrapPeer {
        BootstrapPeer {
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

impl From<(Multiaddr, PeerId)> for BootstrapPeer {
    fn from(info: (Multiaddr, PeerId)) -> Self {
        let (addr, identifier) = info;
        BootstrapPeer {
            addr: Some(addr),
            identifier: Some(identifier),
        }
    }
}

impl std::default::Default for BootstrapPeer {
    fn default() -> Self {
        let conf = &CONF;
        let mut multi_addr = Multiaddr::with_capacity(2);
        multi_addr.push(Protocol::from(conf.bootstrap_ip));
        multi_addr.push(Protocol::Tcp(conf.bootstrap_port));
        let identifier = conf.bootstrap_id.clone()
            .expect("At least one public identifier is required to bootstrap the connection to the network.");
        BootstrapPeer {
            addr: Some(multi_addr),
            identifier: Some(identifier),
        }
    }
}
