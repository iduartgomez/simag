use simag_networking::prelude::*;
use std::{collections::HashMap, net::Ipv4Addr};
use uuid::Uuid;

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
        .build::<AgentKey, String, String>()
        .unwrap();
    println!("This network encoded peer id is: {}", network.get_peer_id());
    let key = AgentKey::unique(Uuid::new_v5(
        &Uuid::NAMESPACE_URL,
        b"resource://example".as_ref(),
    ));
    network.get(key);
    network.send_message("Read my awesome message!".to_string(), peer_id);

    let mut cnt: HashMap<_, usize> = HashMap::new();
    let mut served = false;
    while network.is_running() {
        if let Some(stats) = network.stats.for_key(&key) {
            if stats.times_received > 0 && !served {
                println!("Received a resource at least once");
                served = true;
            }
        }

        for (peer, amount) in network.stats.received_messages().to_owned() {
            let current_amount = cnt.entry(peer.clone()).or_default();
            if *current_amount < amount {
                println!("Received {} messages from #{}", amount, peer);
                *current_amount += 1;
                if *current_amount < 10 {
                    network.send_message("Hai back!".to_string(), peer.clone());
                } else {
                    // network.shutdown().unwrap();
                }

                if *current_amount % 2 == 0 {
                    network.get(key);
                }
            }
        }
    }
    println!("Shutted down");
}
