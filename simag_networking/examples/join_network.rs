use libp2p::PeerId;
use simag_networking::{BootstrapListener, Network};

/// A Base58 enconded peer ID. Only used for testing pourpouses. The corresponding secret key can be found in
/// the examples directory and is used in the bootstrap network example.
const DEFAULT_BOOTSTRAP_ID: &str = "12D3KooWS5RYnqsFRNrUABJFa3sj9YdYS7KsX8qLY6yrkfSTyZDv";

fn main() {
    // At least one existing peer id must be provided in order to join the network.
    // The ID can be provided by either setting the environment variable SIMAG_BOOTSTRAP_ID,
    // specifying it into configuration files (TODO), or explicitly through the BootstrapListener data.
    /*
    std::env::set_var("SIMAG_BOOTSTRAP_ID", DEFAULT_BOOTSTRAP_ID);
    */
    let peer_id: PeerId = DEFAULT_BOOTSTRAP_ID.parse().unwrap();
    let ip: std::net::Ipv4Addr = "0.0.0.0".parse().unwrap();
    let peer = BootstrapListener::new()
        .listening_ip(ip)
        .listening_port(7800)
        .with_identifier(peer_id);

    // The listener connection info can be also instanced through the default method
    // from the configuration variables/files:
    // let peer = BootstrapListener::default();

    let network = Network::join_network(peer).unwrap();
    println!("This network encoded peer id is: {}", network.get_peer_id());
    while network.is_running().is_ok() {
        // keep running
    }
}
