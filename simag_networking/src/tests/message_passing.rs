use std::{
    sync::atomic::{AtomicBool, Ordering},
    time::{Duration, Instant},
};

use crate::{tests::get_free_port, NetworkHandle, PeerId};

static LISTENER_UP: AtomicBool = AtomicBool::new(false);

fn set_listener(mut network: NetworkHandle<String>) {
    let mut last_amount = 0;
    let mut amount = 0;
    let mut last_iter_time = Instant::now();
    let mut initialized = false;
    while network.running() {
        if !initialized {
            LISTENER_UP.store(true, Ordering::SeqCst);
            initialized = true;
        }

        let msg = network.stats.received_messages();
        for (peer, new_amount) in &msg {
            last_amount = amount;
            amount = *new_amount;
            if *new_amount <= 9 {
                network.send_message("Hai there!".to_string(), peer.clone());
            } else {
                network.send_message("Goooodbyyye!".to_string(), peer.clone());
                let _ = network.shutdown();
            }
            last_iter_time = Instant::now();
        }
        if msg.is_empty() || last_amount == amount {
            let diff = Instant::now() - last_iter_time;
            if diff > Duration::from_secs(5) {
                log::error!("listener been idle more than 5 secs");
                break;
            }
        }
    }
    assert!(last_amount > 0);
}

fn set_dialer(mut network: NetworkHandle<String>, lister_id: PeerId) {
    while !LISTENER_UP.load(Ordering::SeqCst) {}
    let mut last_amount = 0;
    let mut amount = 0;
    let mut last_iter_time = Instant::now();
    network.send_message("Read my awesome message!".to_string(), lister_id);
    while network.running() {
        let msg = network.stats.received_messages();
        for (peer, new_amount) in &msg {
            last_amount = amount;
            amount = *new_amount;
            if *new_amount <= 9 {
                network.send_message("Hai back!".to_string(), peer.clone());
            } else {
                network.send_message("Byeeee!".to_string(), peer.clone());
                let _ = network.shutdown();
            }
            last_iter_time = Instant::now();
        }
        if msg.is_empty() || last_amount == amount {
            let diff = Instant::now() - last_iter_time;
            if diff > Duration::from_secs(5) {
                log::error!("dialer been idle more than 5 secs");
                break;
            }
        }
    }
    assert!(last_amount > 0);
}

#[test]
fn send_messages() {
    let port = get_free_port().unwrap();

    let listener = std::thread::spawn(move || {
        let network = super::config_listener(port);
        set_listener(network);
    });

    let dialer = std::thread::spawn(move || {
        let (network, peer_id) = super::config_dialer(port);
        set_dialer(network, peer_id);
    });

    assert!(dialer.join().is_ok());
    assert!(listener.join().is_ok());
}
