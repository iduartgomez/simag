use libp2p::PeerId;
use simag_networking::{BootstrapPeer, Network};

/// A Base58 enconded peer ID. Only used for testing pourpouses. The corresponding secret key can be found in
/// the examples directory and is used in the bootstrap network example.
const DEFAULT_BOOTSTRAP_ID: &str = "12D3KooWLNykGoB2XW229U8bA2NofAL1rNvgb5ioCkUkSCBJS8u1";

fn main() {
    // At least one existing peer id must be provided in order to join the network.
    // The ID can be provided by either setting the environment variable SIMAG_BOOTSTRAP_ID,
    // specifying it into configuration files (TODO), or through the BootstrapPeer data.
    /*
    std::env::set_var("SIMAG_BOOTSTRAP_ID", DEFAULT_BOOTSTRAP_ID);
    */
    let peer_id: PeerId = DEFAULT_BOOTSTRAP_ID.parse().unwrap();

    // Optionally provide the secret key file. If it's not provided a new key and identifier
    // will be generated. In such case, you are required to obtain the id in order to join
    // the network from other nodes (see below). See the `join_network` example for more info.
    /*
    let secret_file = std::env::current_dir().unwrap().join("bootstrap_secret");
    std::env::set_var("SIMAG_LOCAL_PEER_KEY_FILE", secret_file.to_str().unwrap());
    */

    let ip: std::net::Ipv4Addr = "0.0.0.0".parse().unwrap();
    let peer = BootstrapPeer::new()
        .listening_ip(ip)
        .listening_port(7800)
        .with_identifier(peer_id);
    let _network = Network::join_network(peer).unwrap();
}
