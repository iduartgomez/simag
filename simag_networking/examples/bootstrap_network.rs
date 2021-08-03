use simag_core::Agent;
use simag_networking::{agent::AgentPolicy, *};
use std::{collections::HashMap, net::Ipv4Addr};

const ENCODED_KEY1: &[u8] = include_bytes!("key1");
const _ENCODED_KEY2: &[u8] = include_bytes!("key2");

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
    let mut network = {
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

    /*
    Uncommenting the following line would save the newly generated key to the specified path
    (or an existing one if provided like in this example):
    */
    // network.save_secret_key(secret_file).unwrap();

    // #1 This is the id that must be provided to other nodes that want to join the network.
    println!("This network encoded peer id is: {}", network.get_peer_id());
    let agent = Agent::new("agent_01".to_owned());
    let op_id = network.register_agent(&agent, AgentPolicy::default());
    let ag_key = loop {
        match network.op_result(op_id) {
            Err(Error::OpError(HandleError::AwaitingResponse(_))) => {}
            Ok(Some(key)) => break key,
            _ => panic!("something failed!"),
        }
    };
    network.create_group("group_01", &["agent_01"], None, Settings {});

    let mut served = false;
    let mut last_amount = 0;
    while network.running() {
        if let Some(stats) = network.stats.for_key(&ag_key) {
            if stats.times_served > 0 && !served {
                println!("Served a resource at least once");
                served = true;
            }
        }

        for (peer, amount) in network.stats.received_messages().to_owned() {
            if last_amount == amount {
                continue;
            }
            last_amount = amount;
            println!("Received {} messages from #{}", amount, peer);
            if amount <= 9 {
                network.send_message("Hai there!".to_string(), peer.clone());
            } else {
                network.send_message("Goooodbyyye!".to_string(), peer.clone());
                println!("Sent goodbye message");
                network.shutdown().unwrap();
            }
        }
    }
    println!("Shutted down");
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
struct Settings;

#[typetag::serde]
impl GroupSettings for Settings {
    fn is_allowed_to_join(
        &self,
        _agent: uuid::Uuid,
        _petitioner_settings: &dyn GroupSettings,
    ) -> bool {
        true
    }

    fn box_cloned(&self) -> Box<dyn GroupSettings> {
        Box::new(Settings)
    }
}
