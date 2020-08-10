use std::net::{IpAddr, SocketAddr};

struct BootstrapHost(SocketAddr);

impl BootstrapHost {
    pub fn new<T: Into<IpAddr>>(ip: T, port: u16) -> BootstrapHost {
        BootstrapHost(SocketAddr::from((ip.into(), port)))
    }
}
