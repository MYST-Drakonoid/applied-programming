mod crypto;
mod file_system;

use crate::file_system::{get_files, PutFiles};
use crate::crypto::CryptoHandler;

use std::io;


fn main() {

    // Attempt to create directory structure (input/output folders etc.)
    match file_system::create_file_system() {
        Ok(()) => println!("Folders created successfully!"),
        Err(e) => println!("Error creating folders: {}", e),
    }

    // Wait for user to prepare input files
    println!("Put files into the input folder then Press Enter to continue...");
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer).unwrap();

    // File writing helper (output)
    let writer = PutFiles::new();

    // Crypto handler containing your encryption/decryption logic
    let crypto = CryptoHandler::new();

    // Store encrypted and decrypted results
    let mut result_enc_matrix: Vec<Vec<u8>> = Vec::new();
    let mut result_dec_matrix: Vec<Vec<u8>> = Vec::new();

    // Load input files: those meant for encryption and those meant for decryption
    let (file_enc_matrix, file_dec_matrix) = get_files();

    // Encrypt all files meant for encryption
    for file in file_enc_matrix {
        let encrypted = crypto.encrypt_file(&file);
        result_enc_matrix.push(encrypted);
    }

    // Save all encrypted results
    writer.enc(result_enc_matrix);

    // Decrypt all files meant for decryption
    for file in file_dec_matrix {
        let decrypted = crypto.decrypt_file(&file);
        result_dec_matrix.push(decrypted);
    }

    // Save all decrypted results
    writer.dec(result_dec_matrix);
}
