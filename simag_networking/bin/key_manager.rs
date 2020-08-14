use ed25519_dalek::Keypair;
use rand::rngs::OsRng;
use std::fs::File;
use std::io::Write;

fn main() {
    let mut csprng = OsRng {};
    let keypair: Keypair = Keypair::generate(&mut csprng);

    let secret_file = std::env::current_dir().unwrap().join("bootstrap_secret");
    let mut file = File::create(secret_file).unwrap();
    file.write_all(&keypair.to_bytes()).unwrap();
}
