use libp2p::{identity, PeerId};
use once_cell::sync::Lazy;
use std::{
    convert::TryFrom,
    fs::File,
    io::Read,
    net::{IpAddr, Ipv4Addr, SocketAddr, TcpListener},
    path::PathBuf,
    str::FromStr,
};

pub(crate) static CONF: Lazy<Config> =
    Lazy::new(|| Config::load_conf().expect("Failed to load configuration"));

const DEFAULT_BOOTSTRAP_PORT: u16 = 7800;

pub(crate) struct Config {
    pub bootstrap_ip: IpAddr,
    pub bootstrap_port: u16,
    pub bootstrap_id: Option<PeerId>,
    pub local_peer_keypair: Option<identity::ed25519::Keypair>,
}

impl Config {
    pub fn load_conf() -> Result<Config, ()> {
        let mut settings = config::Config::new();
        settings
            .merge(config::Environment::with_prefix("SIMAG"))
            .unwrap();

        let local_peer_keypair =
            if let Ok(path_to_key) = settings.get_str("local_peer_key_file").map(PathBuf::from) {
                let mut key_file = File::open(&path_to_key).unwrap_or_else(|_| {
                    panic!(
                        "Failed to open key file: {}",
                        &path_to_key.to_str().unwrap()
                    )
                });
                let mut buf = Vec::new();
                key_file.read_to_end(&mut buf).unwrap();
                Some(identity::ed25519::Keypair::decode(&mut buf).map_err(|_| ())?)
            } else {
                None
            };

        let (bootstrap_ip, bootstrap_port, bootstrap_id) = Config::get_bootstrap_host(&settings)?;
        Ok(Config {
            bootstrap_ip,
            bootstrap_port,
            bootstrap_id,
            local_peer_keypair,
        })
    }

    fn get_bootstrap_host(settings: &config::Config) -> Result<(IpAddr, u16, Option<PeerId>), ()> {
        let bootstrap_ip = IpAddr::from_str(
            &settings
                .get_str("bootstrap_host")
                .unwrap_or_else(|_| format!("{}", Ipv4Addr::LOCALHOST)),
        )
        .map_err(|_err| ())?;

        let bootstrap_port = settings
            .get_int("bootstrap_port")
            .map(u16::try_from)
            .unwrap_or(Ok(DEFAULT_BOOTSTRAP_PORT))
            .map_err(|_err| ())?;

        let id_str = settings
            .get_str("bootstrap_id")
            .ok()
            .map(|id| id.parse().map_err(|_err| ()).ok())
            .flatten();

        Ok((bootstrap_ip, bootstrap_port, id_str))
    }
}

pub(crate) fn get_free_port() -> Result<u16, ()> {
    const FIRST_DYNAMIC_PORT: u16 = 49152;
    const LAST_DYNAMIC_PORT: u16 = 65535;

    for port in FIRST_DYNAMIC_PORT..LAST_DYNAMIC_PORT {
        let bind_addr = SocketAddr::from((Ipv4Addr::LOCALHOST, port));
        if TcpListener::bind(bind_addr).is_ok() {
            return Ok(port);
        }
    }
    Err(())
}
