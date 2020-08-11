use libp2p::PeerId;
use std::{
    convert::TryFrom,
    net::{IpAddr, Ipv4Addr},
    str::FromStr,
};

const DEFAULT_BOOTSTRAP_PORT: u16 = 7800;
#[cfg(debug_assertions)]
const DEFAULT_BOOTSTRAP_ID: &str = "QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ";

pub(crate) struct Config {
    pub bootstrap_ip: IpAddr,
    pub bootstrap_port: u16,
    pub bootstrap_id: PeerId,
}

impl Config {
    pub fn load_conf() -> Result<Config, ()> {
        let mut settings = config::Config::new();
        settings
            .merge(config::Environment::with_prefix("SIMAG"))
            .unwrap();

        let (bootstrap_ip, bootstrap_port, bootstrap_id) = Config::get_bootstrap_host(&settings)?;
        Ok(Config {
            bootstrap_ip,
            bootstrap_port,
            bootstrap_id,
        })
    }

    fn get_bootstrap_host(settings: &config::Config) -> Result<(IpAddr, u16, PeerId), ()> {
        let bootstrap_ip = IpAddr::from_str(
            &settings
                .get_str("BOOTSTRAP_HOST")
                .unwrap_or_else(|_| format!("{}", Ipv4Addr::LOCALHOST)),
        )
        .map_err(|_err| ())?;

        let bootstrap_port = settings
            .get_int("bootstrap_port")
            .map(u16::try_from)
            .unwrap_or(Ok(DEFAULT_BOOTSTRAP_PORT))
            .map_err(|_err| ())?;

        #[cfg(not(debug_assertions))]
        let id_str = {
            settings
                .get_str("BOOTSTRAP_ID")
                .expect("At least one public identifier is required to bootstrap the connection to the network.")
        };

        #[cfg(debug_assertions)]
        let id_str = {
            settings
                .get_str("BOOTSTRAP_ID")
                .unwrap_or_else(|_| DEFAULT_BOOTSTRAP_ID.to_owned())
        };

        Ok((
            bootstrap_ip,
            bootstrap_port,
            id_str.parse().map_err(|_err| ())?,
        ))
    }
}

/*
fn get_free_connection(ip: IpAddr) -> Result<(TcpListener, u16), error::NetworkError> {
    // const FIRST_DYNAMIC_PORT: u16 = 49152;
    // const LAST_DYNAMIC_PORT: u16 = 65535;

    let mut port = 0;
    for n in 0..100 {
        port = FIRST_DYNAMIC_PORT + n;
        let bind_addr = SocketAddr::from((ip, port));
        if let Ok(conn) = TcpListener::bind(bind_addr) {
            return Ok((conn, port));
        }
    }
    Err(error::NetworkError::FreePortNotFound(port, 100))
}
*/
