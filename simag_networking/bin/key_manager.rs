use rand::rngs::OsRng;
use std::fs::File;
use std::io::Write;

#[allow(unreachable_code, unused_variables)]
fn main() {
    let csprng = OsRng {};
    let keypair: &[u8] = { todo!() };

    let secret_file = std::env::current_dir().unwrap().join("bootstrap_secret");
    let mut file = File::create(secret_file).unwrap();
    file.write_all(&keypair).unwrap();
}
