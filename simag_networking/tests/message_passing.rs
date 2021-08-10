use std::{
    net::Ipv4Addr,
    time::{Duration, Instant},
};

use simag_networking::{Keypair, NetworkBuilder, NetworkHandle, PeerId, Provider};

const ENCODED_KEY1: &[u8] = include_bytes!("key1");

/// A Base58 enconded peer ID. Only used for testing pourpouses.
const KEY1_ID: &str = "12D3KooWS5RYnqsFRNrUABJFa3sj9YdYS7KsX8qLY6yrkfSTyZDv";

fn set_listener(mut network: NetworkHandle<String>) {
    let mut last_amount = 0;
    let mut time_since_last_msg = Duration::new(0, 0);
    'main: while network.running() {
        let t0 = Instant::now();
        for (peer, amount) in network.stats.received_messages() {
            if time_since_last_msg > Duration::from_secs(10) {
                if let Err(_) = network.shutdown() {
                    break 'main;
                } else {
                    continue;
                }
            } else if last_amount == amount {
                time_since_last_msg += Instant::now() - t0;
                continue;
            }
            time_since_last_msg = Duration::new(0, 0);

            last_amount = amount;
            if amount <= 9 {
                network.send_message("Hai there!".to_string(), peer.clone());
            } else {
                network.send_message("Goooodbyyye!".to_string(), peer.clone());
                let _ = network.shutdown();
            }
        }
    }
    assert!(last_amount >= 9);
}

fn set_dialer(mut network: NetworkHandle<String>, lister_id: PeerId) {
    let mut last_amount = 0;
    network.send_message("Read my awesome message!".to_string(), lister_id);
    let mut time_since_last_msg = Duration::new(0, 0);
    'main: while network.running() {
        let t0 = Instant::now();
        for (peer, amount) in network.stats.received_messages() {
            if time_since_last_msg > Duration::from_secs(10) {
                if let Err(_) = network.shutdown() {
                    break 'main;
                } else {
                    continue;
                }
            } else if last_amount == amount {
                time_since_last_msg += Instant::now() - t0;
                continue;
            }
            time_since_last_msg = Duration::new(0, 0);

            last_amount = amount;
            if amount <= 9 {
                network.send_message("Hai back!".to_string(), peer.clone());
            } else {
                network.send_message("Byeeee!".to_string(), peer.clone());
                let _ = network.shutdown();
            }
        }
    }
    assert!(last_amount >= 9);
}

#[test]
fn send_messages() {
    let listener = std::thread::spawn(|| {
        let network = {
            let provider = Provider::new()
                .listening_ip(Ipv4Addr::LOCALHOST)
                .listening_port(7800)
                .with_identifier(KEY1_ID.parse().unwrap());
            NetworkBuilder::configure_network()
                .with_key(Keypair::decode(&mut ENCODED_KEY1.to_vec()).unwrap())
                .with_ip(Ipv4Addr::LOCALHOST)
                .with_port(7800)
                .add_provider(provider)
                .build::<String>()
                .unwrap()
        };
        set_listener(network);
    });

    std::thread::sleep(Duration::from_millis(200));

    let dialer = std::thread::spawn(|| {
        let peer_id: PeerId = KEY1_ID.parse().unwrap();
        let peer = Provider::new()
            .listening_ip(Ipv4Addr::LOCALHOST)
            .listening_port(7800)
            .with_identifier(peer_id.clone());
        let network = NetworkBuilder::configure_network()
            .add_provider(peer)
            .build::<String>()
            .unwrap();
        set_dialer(network, peer_id);
    });

    assert!(dialer.join().is_ok());
    assert!(listener.join().is_ok());
}
