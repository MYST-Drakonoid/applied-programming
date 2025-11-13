mod crypto;
use crate::crypto::CryptoHandler;

fn main() {
    let handler = CryptoHandler { key: [0u8; 32] };
    let data = b"Hello world";
    let encrypted = handler.encrypt(data);
    println!("Encrypted data: {:?}", encrypted);
}