use simag_networking::prelude::*;

const ENCONDED_KEY: &[u8] = include_bytes!("bootstrap_secret");

fn main() {
    // Optionally provide the secret key file. If it's not provided a new key and identifier
    // will be generated. In such case, you are required to obtain the id in order to join
    // the network from other nodes (see below #1). See the `join_network` example for more info.

    // You can provide a key either pointing to a path with the file in the SIMAG_LOCAL_PEER_KEY_FILE
    // env var, or while bootstrapping the network as an optional parameter.
    /*
    let secret_file = std::env::current_dir().unwrap().join("bootstrap_secret");
    std::env::set_var("SIMAG_LOCAL_PEER_KEY_FILE", secret_file.to_str().unwrap());
    */

    let key = Keypair::decode(&mut ENCONDED_KEY.to_vec()).unwrap();
    let ip: std::net::Ipv4Addr = "0.0.0.0".parse().unwrap();
    let port = 7800;
    let network = Network::bootstrap(ip, port, Some(key)).unwrap();

    // Uncommenting the following file would save the newly generated key to the specified path
    // (or an existing one if provided like in this example):
    // network.save_secret_key(secret_file).unwrap();

    // #1 This is the id that must be provided to other nodes that want to join the network.
    println!("This network encoded peer id is: {}", network.get_peer_id());
    while network.is_running().is_ok() {
        // keep running
    }
}
