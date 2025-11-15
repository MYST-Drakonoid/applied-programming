use std::fs;
use std::io;



/// Creates required directories for the program
/// Returns Result<(), std::io::Error>
pub fn create_file_system() -> io::Result<()> {
    // Create input_folder (will return error if it already exists)
    fs::create_dir("input_folder")?;

    // Create output_folder (will return error if it already exists)
    fs::create_dir("output_folder")?;

    println!("Directories created successfully");

    Ok(()) // Return success
}

/// Reads all files from "input_folder" and returns their contents as a 2D vector of bytes.
/// Each inner Vec<u8> is the contents of one file.
/// Returns 2 <Vec<Vec<u8> a 2d vectors of file contents 1 is enc 2 is dec
pub fn get_files() -> (Vec<Vec<u8>>, Vec<Vec<u8>>) {
    // Read the directory entries (files/folders)
    let entries = fs::read_dir("input_folder").expect("Failed to read directory");

    // Initialize an empty vector to hold file contents
    let mut file_enc_matrix: Vec<Vec<u8>> = Vec::new();
    let mut file_dec_matrix: Vec<Vec<u8>> = Vec::new();

    // Iterate through each entry in the directory
    for entry in entries {
        // Unwrap each entry from Result<DirEntry, Error>
        let entry = entry.expect("Failed to get entry");
        let path = entry.path(); // Get full path to the file

        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            if ext == "txt" {
                let file_bytes = fs::read(&path).expect("Failed to read input file");
                file_enc_matrix.push(file_bytes); 
            } else if ext == "enc" {
                let file_bytes = fs::read(&path).expect("Failed to read input file");
                file_dec_matrix.push(file_bytes); 
                
            }
        }
        // Read the file contents into a Vec<u8>
        

        // Add this file's bytes to the outer vector
        
    }

    // Return the 2D vector: each element = one file's bytes
    (file_enc_matrix, file_dec_matrix)
}

/// A utility struct for writing files to an output folder.
/// This struct can write raw byte arrays to files, either
/// as ".enc" (encrypted) or ".txt" (decrypted).
pub struct PutFiles {
    /// The path to the output folder where files will be written
    output_folder: std::path::PathBuf,
}

impl PutFiles {

    /// Creates a new instance of `put_files`.
    /// Initializes the `output_folder` as "output_folder".
    /// Uses `PathBuf::from` to store an owned path in the struct.
    pub fn new() -> Self {
        let output_folder = std::path::PathBuf::from("output_folder"); 
        Self {output_folder}
    }
    
    /// Writes a vector of byte arrays to the output folder as encrypted files.
    /// Each file is named "fileN.enc" where N is a sequential number starting from 1.
    ///
    /// # Parameters
    /// - `data`: Vec<Vec<u8>> containing the bytes for each file to write.
    pub fn enc(&self, data: Vec<Vec<u8>>) -> () {
        let mut count: u32 = 0; // counter to number files sequentially

        // Iterate through each file's byte data
        for file in data {
            count += 1; // increment counter

            // Combine the folder path and filename dynamically
            let file_path = self.output_folder.join(format!("file{}.enc", count));

            // Write the byte data to disk
            // `&file` passes a slice of the Vec<u8>
            // `expect` will panic if writing fails
            std::fs::write(&file_path, &file).expect("Failed to write file");
        }
    }

    /// Writes a vector of byte arrays to the output folder as decrypted files.
    /// Each file is named "fileN.txt" where N is a sequential number starting from 1.
    ///
    /// # Parameters
    /// - `data`: Vec<Vec<u8>> containing the bytes for each file to write.
    pub fn dec(&self, data: Vec<Vec<u8>>) -> () {
        let mut count: u32 = 0; // counter to number files sequentially

        // Iterate through each file's byte data
        for file in data {
            count += 1; // increment counter

            // Combine the folder path and filename dynamically
            let file_path = self.output_folder.join(format!("file{}.txt", count));

            // Write the byte data to disk
            std::fs::write(&file_path, &file).expect("Failed to write file");
        }
    }
}


