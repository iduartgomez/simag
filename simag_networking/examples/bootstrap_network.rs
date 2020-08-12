use simag_networking::Network;
use smol::prelude::*;

fn main() {
    // Optionally provide the secret key file. If it's not provided a new key and identifier
    // will be generated. In such case, you are required to obtain the id in order to join
    // the network from other nodes (see below). See the `join_network` example for more info.
    let secret_file = std::env::current_dir().unwrap().join("bootstrap_secret");
    std::env::set_var("SIMAG_LOCAL_PEER_KEY_FILE", secret_file.to_str().unwrap());

    let ip: std::net::Ipv4Addr = "0.0.0.0".parse().unwrap();
    let port = 7800;
    let network = Network::bootstrap(ip, port).unwrap();
    // TODO: start listening

    // Uncommenting the following file would save the secret key to the speicfied path:
    // network.save_secret_key(secret_file).unwrap();

    // This is the id that must be provided to other nodes that want to join the network.
    println!("This network encoded peer id is: {}", network.get_peer_id());
}
