use chacha20poly1305::{ChaCha20Poly1305, KeyInit, aead::{Aead, generic_array::GenericArray}};
use rand::{TryRngCore, rngs::OsRng};
use rand::Rng;   
use std::fs;
use std::io::{Read, Write};
use std::path::Path;





/// A struct that holds the encryption key for ChaCha20-Poly1305.
/// The key is private and used internally by the encryption/decryption methods.
pub struct CryptoHandler {
    key: [u8; 32],
}

impl CryptoHandler {
    /// Creates a new CryptoHandler instance with a randomly generated 32-byte key.
    ///
    /// # Returns
    /// * `Self` - A new instance of CryptoHandler with a private key.
    ///
    /// # Example
    /// ```
    /// let handler = CryptoHandler::new();
    /// ```
    pub fn new() -> Self {
        let key_path = Path::new("key.bin");

        let key = if key_path.exists() {
            let mut file = fs::File::open(key_path).expect("failed to open key file");
            let mut key_bytes = [0u8; 32];
            file.read_exact(&mut key_bytes).expect("Failed to read key file");
            key_bytes
        } else {
            let mut key_bytes = [0u8; 32];
            let mut trng = OsRng::default();
            trng.try_fill_bytes(&mut key_bytes);

            let mut file = fs::File::create(key_path).expect("Failed to create key file");
            file.write_all(&key_bytes).expect("Failed to write key file");
            key_bytes
        };
        Self { key }
    }

    /// Encrypts a slice of bytes using the internal key.
    ///
    /// # Parameters
    /// * `&self` - Immutable reference to the CryptoHandler instance.
    /// * `data: &[u8]` - Borrowed slice of bytes to encrypt.
    ///
    /// # Returns
    /// * `(Vec<u8>)` - Tuple containing the ciphertext and the 12-byte nonce used for encryption.
    ///
    /// # Example
    /// ```
    /// let (ciphertext, nonce) = handler.encrypt_file(&plaintext_bytes);
    /// ```
    pub fn encrypt_file(&self, data: &[u8]) -> Vec<u8> {
        let cipher = ChaCha20Poly1305::new(&self.key.into());

        let mut nonce = [0u8; 12];
        let mut trng = OsRng::default();
        trng.try_fill_bytes(&mut nonce);

        let ciphertext = cipher.encrypt(&nonce.into(), data)
            .expect("encryption failed");

        // Build a single Vec<u8> = nonce + ciphertext
        let mut combined = Vec::new();
        combined.extend_from_slice(&nonce);      // first 12 bytes = nonce
        combined.extend_from_slice(&ciphertext); // rest = encrypted data

        combined
    }

    /// Decrypts previously encrypted bytes using the internal key and nonce.
    ///
    /// # Parameters
    /// * `&self` - Immutable reference to the CryptoHandler instance.
    /// * `data: [u8]` - The encrypted byte array to decrypt.
    ///
    /// # Returns
    /// * `Vec<u8>` - The decrypted plaintext bytes.
    ///
    /// # Example
    /// ```
    /// let plaintext = handler.decrypt_file(ciphertext, &nonce);
    /// ```
    pub fn decrypt_file(&self, data: &[u8]) -> Vec<u8> {
        let cipher = ChaCha20Poly1305::new(&self.key.into());

        // First 12 bytes = nonce
        let nonce: [u8; 12] = data[0..12].try_into().unwrap();

        // Remaining bytes = ciphertext
        let ciphertext = &data[12..];

        let nonce_slice =
            GenericArray::from_slice(&nonce);

        cipher.decrypt(nonce_slice, ciphertext)
            .expect("decryption failed")
}
}