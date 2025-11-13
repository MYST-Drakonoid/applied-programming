use chacha20poly1305::{ChaCha20Poly1305, ChaChaPoly1305, Key, KeyInit, Nonce};
use chacha20poly1305::aead::{Aead, NewAead, Nonce};
use rand_core::OsRng; 
use std::fs;


pub struct CryptoHandler {
    key: [u8; 32],
}

impl CryptoHandler {
    pub fn new() -> Self {
        let mut key = [0u8; 32];
        OsRng.fill_bytes(&mut key)
    }

    pub fn encrypt_file(&self, data: &[u8]) -> (Vec<u8>, [u8;12]) {
        let cipher= ChaChaPoly1305::new(&self.key.into());

        let mut nonce = [0u8; 12];
        OsRng.fill_bytes(&mut nonce);

        let ciphertext = cipher.encrypt(&nonce.into(), data).expect("encryption failed");

        (ciphertext, nonce)

    }
}