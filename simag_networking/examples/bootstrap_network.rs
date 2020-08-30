use simag_networking::prelude::*;
use std::{collections::HashMap, net::Ipv4Addr};
use uuid::Uuid;

const ENCONDED_KEY1: &[u8] = include_bytes!("key1");
const ENCONDED_KEY2: &[u8] = include_bytes!("key2");

/// A Base58 enconded peer ID. Only used for testing pourpouses.
const KEY1_ID: &str = "12D3KooWS5RYnqsFRNrUABJFa3sj9YdYS7KsX8qLY6yrkfSTyZDv";
const _KEY2_ID: &str = "12D3KooWSZSxbBFhZoy7butAGwcySE8oKXcvzcTSZYPM2XiPD8yA";

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
    let mut network = if std::env::args().nth(1).is_none() {
        NetworkBuilder::configure_network()
            .with_key(Keypair::decode(&mut ENCONDED_KEY1.to_vec()).unwrap())
            .with_ip(Ipv4Addr::LOCALHOST)
            .with_port(7800)
            .build::<AgentKey, String, String>()
            .unwrap()
    } else {
        let provider = Provider::new()
            .listening_ip(Ipv4Addr::LOCALHOST)
            .listening_port(7800)
            .with_identifier(KEY1_ID.parse().unwrap());
        NetworkBuilder::configure_network()
            .with_key(Keypair::decode(&mut ENCONDED_KEY2.to_vec()).unwrap())
            .with_ip(Ipv4Addr::LOCALHOST)
            .with_port(7801)
            .add_provider(provider)
            .build()
            .unwrap()
    };

    // Uncommenting the following file would save the newly generated key to the specified path
    // (or an existing one if provided like in this example):
    // network.save_secret_key(secret_file).unwrap();

    // #1 This is the id that must be provided to other nodes that want to join the network.
    println!("This network encoded peer id is: {}", network.get_peer_id());
    let key = AgentKey::unique(Uuid::new_v5(
        &Uuid::NAMESPACE_URL,
        b"resource://example".as_ref(),
    ));
    network.put(key, "Joined group!".to_string());

    let mut cnt: HashMap<_, usize> = HashMap::new();
    let mut served = false;
    while network.is_running() {
        if let Some(stats) = network.stats.for_key(&key) {
            if stats.times_served > 0 && !served {
                println!("Served a resource at least once");
                served = false;
            }
        }

        for (peer, amount) in network.stats.received_messages().to_owned() {
            let current_amount = cnt.entry(peer.clone()).or_default();
            if *current_amount < amount {
                println!("Received {} messages from #{}", amount, peer);
                if *current_amount < 9 {
                    network.send_message("Hai there!".to_string(), peer.clone());
                } else {
                    std::thread::sleep(std::time::Duration::from_secs(6));
                    network.send_message("Goooodbyyye!".to_string(), peer.clone());
                    println!("Sent goodbye message")
                }
                *current_amount += 1;
            }
        }
    }
    println!("Shutted down");
}
