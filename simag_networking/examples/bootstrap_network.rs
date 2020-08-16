use simag_networking::prelude::*;
use std::net::Ipv4Addr;

const ENCONDED_KEY1: &[u8] = include_bytes!("key1");
const ENCONDED_KEY2: &[u8] = include_bytes!("key2");

/// A Base58 enconded peer ID. Only used for testing pourpouses.
const KEY1_ID: &str = "12D3KooWS5RYnqsFRNrUABJFa3sj9YdYS7KsX8qLY6yrkfSTyZDv";
const KEY2_ID: &str = "12D3KooWSZSxbBFhZoy7butAGwcySE8oKXcvzcTSZYPM2XiPD8yA";

fn main() {
    // Optionally provide the secret key file. If it's not provided a new key and identifier
    // will be generated. In such case, you are required to obtain the id in order to join
    // the network from other nodes (see below #1). See the `join_network` example for an example
    // on how to join the network.

    // You can provide a key either pointing to a file path in the SIMAG_LOCAL_PEER_KEY_FILE env var:
    /*
    let secret_file = std::env::current_dir().unwrap().join("bootstrap_secret");
    std::env::set_var("SIMAG_LOCAL_PEER_KEY_FILE", secret_file.to_str().unwrap());
    */
    // or while bootstrapping the network as an explicit optional parameter like below.
    let (key, peer_id, port, use_provider) = if std::env::args().nth(1).is_none() {
        (
            Keypair::decode(&mut ENCONDED_KEY1.to_vec()).unwrap(),
            KEY2_ID.parse().unwrap(),
            7800,
            false,
        )
    } else {
        (
            Keypair::decode(&mut ENCONDED_KEY2.to_vec()).unwrap(),
            KEY1_ID.parse().unwrap(),
            7801,
            true,
        )
    };
    let peer = if use_provider {
        Some(
            Provider::new()
                .listening_ip(Ipv4Addr::LOCALHOST)
                .listening_port(7800)
                .with_identifier(peer_id),
        )
    } else {
        None
    };
    let mut network = Network::bootstrap(Ipv4Addr::LOCALHOST, port, peer, Some(key)).unwrap();

    // Uncommenting the following file would save the newly generated key to the specified path
    // (or an existing one if provided like in this example):
    // network.save_secret_key(secret_file).unwrap();

    // #1 This is the id that must be provided to other nodes that want to join the network.
    println!("This network encoded peer id is: {}", network.get_peer_id());

    // A working network requires at least two listening nodes which have been bootstrapped.
    while network.is_running() {
        // keep running
    }
    println!("Shutted down");
}
