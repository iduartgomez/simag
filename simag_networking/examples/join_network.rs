use libp2p::PeerId;
use simag_networking::prelude::*;
use std::net::Ipv4Addr;

/// A Base58 enconded peer ID. Only used for testing pourpouses. The corresponding secret key can be found in
/// the examples directory and is used in the bootstrap network example.
const DEFAULT_BOOTSTRAP_ID: &str = "12D3KooWS5RYnqsFRNrUABJFa3sj9YdYS7KsX8qLY6yrkfSTyZDv";

fn main() {
    // At least one existing peer id must be provided in order to join the network.
    // The ID can be provided by either setting the environment variable SIMAG_BOOTSTRAP_ID,
    // specifying it into configuration files (TODO), or explicitly through the Provider data.
    /*
    std::env::set_var("SIMAG_BOOTSTRAP_ID", DEFAULT_BOOTSTRAP_ID);
    */
    let peer_id: PeerId = DEFAULT_BOOTSTRAP_ID.parse().unwrap();
    let peer = Provider::new()
        .listening_ip(Ipv4Addr::LOCALHOST)
        .listening_port(7800)
        .with_identifier(peer_id.clone());

    // The listener connection info can be also instanced through the default method
    // from the configuration variables/files:
    // let peer = Provider::default();
    let mut network = NetworkBuilder::configure_network()
        .add_provider(peer)
        .build()
        .unwrap();
    println!("This network encoded peer id is: {}", network.get_peer_id());

    network.get(AgentKey {});
    network.send_message(b"Awesome message!".to_vec(), peer_id);
    let mut received = false;
    while network.is_running() {
        if let Some(stats) = network.stats.for_key(&AgentKey {}) {
            if stats.times_received > 0 && !network.stats.received_messages().is_empty() {
                if !received {
                    println!("Received a resource at least once");
                    received = true;
                }
                network.shutdown().unwrap();
            }
        }
    }
    println!("Shutted down");
}
