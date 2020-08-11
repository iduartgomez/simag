use crate::config::Config;
use libp2p::{
    core::{muxing, upgrade},
    dns::DnsConfig,
    identity,
    kad::{store::MemoryStore, Kademlia},
    mplex,
    multiaddr::Protocol,
    noise,
    tcp::TcpConfig,
    yamux, Multiaddr, PeerId, Swarm, Transport,
};

pub struct Network(Swarm<Kademlia<MemoryStore>>);

struct NetworkBuilder {
    local_key: identity::Keypair,
    /// The peer ID of this machine
    local_peer_id: PeerId,
    bootstrap: BootstrapInfo,
}

impl Default for Network {
    fn default() -> Self {
        let bootstrap_info = BootstrapInfo::default();
        Network::new(bootstrap_info).expect("Couldn't initialize the network.")
    }
}

impl Network {
    pub fn new(bootstrap: BootstrapInfo) -> std::io::Result<Network> {
        let local_key = identity::Keypair::generate_ed25519();
        let local_peer_id = PeerId::from(local_key.public());
        let builder = NetworkBuilder {
            local_key,
            local_peer_id,
            bootstrap,
        };
        let swarm = Network::set_up_network(builder)?;
        Ok(Network(swarm))
    }

    fn set_up_network(builder: NetworkBuilder) -> std::io::Result<Swarm<Kademlia<MemoryStore>>> {
        // Set the transport protocol
        let transport = {
            let noise_keys = noise::Keypair::<noise::X25519Spec>::new()
                .into_authentic(&builder.local_key)
                .expect("Signing libp2p-noise static DH keypair failed.");

            let tcp = TcpConfig::new().nodelay(true);
            DnsConfig::new(tcp)?
                .upgrade(upgrade::Version::V1)
                .authenticate(noise::NoiseConfig::xx(noise_keys).into_authenticated())
                .multiplex(upgrade::SelectUpgrade::new(
                    yamux::Config::default(),
                    mplex::MplexConfig::new(),
                ))
                .map(|(peer, muxer), _| (peer, muxing::StreamMuxerBox::new(muxer)))
                .timeout(std::time::Duration::from_secs(20))
        };

        // Set the Kademlia DHT
        let store = MemoryStore::new(builder.local_peer_id.clone());
        let mut kdm = Kademlia::new(builder.local_peer_id.clone(), store);
        kdm.add_address(&builder.bootstrap.identifier, builder.bootstrap.addr);
        Ok(Swarm::new(transport, kdm, builder.local_peer_id))
    }
}

#[derive(Clone)]
pub struct BootstrapInfo {
    addr: Multiaddr,
    identifier: PeerId,
}

impl BootstrapInfo {
    fn new() -> BootstrapInfo {
        let conf = Config::load_conf().unwrap();
        let mut multi_addr = Multiaddr::with_capacity(2);
        multi_addr.push(Protocol::from(conf.bootstrap_ip));
        multi_addr.push(Protocol::Tcp(conf.bootstrap_port));
        BootstrapInfo {
            addr: multi_addr,
            identifier: conf.bootstrap_id,
        }
    }
}

impl From<(Multiaddr, PeerId)> for BootstrapInfo {
    fn from(info: (Multiaddr, PeerId)) -> Self {
        let (addr, identifier) = info;
        BootstrapInfo { addr, identifier }
    }
}

impl std::default::Default for BootstrapInfo {
    fn default() -> Self {
        BootstrapInfo::new()
    }
}
