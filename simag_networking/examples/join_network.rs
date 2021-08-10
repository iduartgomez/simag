use simag_networking::*;
use std::{
    net::Ipv4Addr,
    time::{Duration, Instant},
};

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
        .build::<String>()
        .unwrap();
    println!("This network encoded peer id is: {}", network.get_peer_id());
    let query = network.find_agent("agent_01");
    // get the async result of the find agent query
    let ag_key = loop {
        match network.op_result(query) {
            Ok(Some(res)) => break res,
            Err(Error::OpError(HandleError::AwaitingResponse(_))) => {}
            Err(err) => panic!("{}", err),
            Ok(_) => unreachable!(),
        }
    };

    let mut served = false;
    let mut last_amount = 0;
    network.send_message("Read my awesome message!".to_string(), peer_id);
    let mut time_since_last_msg = Duration::new(0, 0);
    'main: while network.running() {
        let t0 = Instant::now();
        if let Some(stats) = network.stats.for_key(&ag_key) {
            if stats.times_received > 0 && !served {
                println!("Received a resource at least once");
                served = true;
            }
        }

        for (peer, amount) in network.stats.received_messages().to_owned() {
            if time_since_last_msg > Duration::from_secs(1) {
                if let Err(_) = network.shutdown() {
                    println!("Peer timed out! ({} secs)", time_since_last_msg.as_secs());
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
                println!("Received {} messages from #{}", amount, peer);
            } else {
                network.send_message("Byeeee!".to_string(), peer.clone());
                println!("Received {} messages from #{}", amount, peer);
                let _ = network.shutdown();
            }
        }
    }
    println!("Shutted down");
}
