use std::{
    convert::TryFrom,
    net::{IpAddr, Ipv4Addr},
    str::FromStr,
};

const DEFAULT_BOOTSTRAP_PORT: u16 = 4040;

struct Config {
    bootstrap_ip: IpAddr,
    bootstrap_port: u16,
}

impl Config {
    fn load_conf() -> Result<Config, ()> {
        let mut settings = config::Config::new();
        settings
            .merge(config::Environment::with_prefix("SIMAG"))
            .unwrap();

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

        Ok(Config {
            bootstrap_ip,
            bootstrap_port,
        })
    }
}

/*
fn get_free_connection(ip: IpAddr) -> Result<(TcpListener, u16), error::NetworkError> {
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
