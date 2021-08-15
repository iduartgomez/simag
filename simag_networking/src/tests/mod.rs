use std::{
    fmt::Debug,
    net::{Ipv4Addr, SocketAddr, TcpListener},
};

use rand::Rng;
use serde::{de::DeserializeOwned, Serialize};

use super::{Keypair, NetworkBuilder, NetworkHandle, PeerId, Provider};

mod connect_agents;
mod message_passing;

const ENCODED_KEY1: &[u8] = include_bytes!("key1");

/// A Base58 enconded peer ID. Only used for testing pourpouses.
const KEY1_ID: &str = "12D3KooWS5RYnqsFRNrUABJFa3sj9YdYS7KsX8qLY6yrkfSTyZDv";

fn config_dialer<T>(port: u16) -> (NetworkHandle<T>, PeerId)
where
    T: Serialize + DeserializeOwned + Debug + Send + Sync + 'static,
{
    let peer_id: PeerId = KEY1_ID.parse().unwrap();
    let peer = Provider::new()
        .listening_ip(Ipv4Addr::LOCALHOST)
        .listening_port(port)
        .with_identifier(peer_id.clone());
    (
        NetworkBuilder::configure_network()
            .add_provider(peer)
            .build::<T>()
            .unwrap(),
        peer_id,
    )
}

fn config_listener<T>(port: u16) -> NetworkHandle<T>
where
    T: Serialize + DeserializeOwned + Debug + Send + Sync + 'static,
{
    let provider = Provider::new()
        .listening_ip(Ipv4Addr::LOCALHOST)
        .listening_port(port)
        .with_identifier(KEY1_ID.parse().unwrap());
    NetworkBuilder::configure_network()
        .with_key(Keypair::decode(&mut ENCODED_KEY1.to_vec()).unwrap())
        .with_ip(Ipv4Addr::LOCALHOST)
        .with_port(port)
        .add_provider(provider)
        .build::<T>()
        .unwrap()
}

fn get_free_port() -> Result<u16, ()> {
    let mut port;
    for _ in 0..100 {
        port = get_dynamic_port();
        let bind_addr = SocketAddr::from((Ipv4Addr::LOCALHOST, port));
        if let Ok(conn) = TcpListener::bind(bind_addr) {
            std::mem::drop(conn);
            return Ok(port);
        }
    }
    Err(())
}

fn get_dynamic_port() -> u16 {
    const FIRST_DYNAMIC_PORT: u16 = 49152;
    const LAST_DYNAMIC_PORT: u16 = 65535;
    rand::thread_rng().gen_range(FIRST_DYNAMIC_PORT..LAST_DYNAMIC_PORT)
}
