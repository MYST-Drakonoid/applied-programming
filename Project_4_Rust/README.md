# Overview

As a software engineer, my goal is to **make people's lives more pleasant**. Whether that is through games people may enjoy or by creating machines or programs to make life easier, it doesn’t matter. I figured that with my love of computers, I could more easily make a difference in the digital space.  

**NOTICE:** To run the program, you can either run `main.rs` or use the `.exe` file in the `release` folder. Both work generally the same.  

My program is an **encryption/decryption tool written in Rust**. The idea is that you can encrypt or decrypt files in the `input` folder. Currently, only `.txt` files work because they handle `Vec<u8>` directly, but this is suitable for this use case. All results are reversible **as long as you have the original key**. Losing the key means losing the file permanently. All necessary folder structures are created automatically—no need to make directories manually.  

I wrote this project because I thought it would be interesting given my training in security. While I wasn’t able to execute it exactly as I envisioned, it works well for this project.  

[Software Demo Video](http://youtube.link.goes.here)

---

# Development Environment

I used **Visual Studio Code** and AI chatbots to assist with learning, debugging, and commenting on the code. All the code, aside from comments, was written by me.  

| **Crate / Library**                             | **Purpose (General Description)**                                                                                                                                                         |
| ----------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **`rand`**                                      | Core random-number generation library for Rust. Provides RNG algorithms (ChaCha, ISAAC, PCG), OS entropy sources, and utilities for generating random values.                             |
| **`rand_core`**                                 | Low-level traits and abstractions for RNG backends. Defines the `RngCore` and `CryptoRng` traits used by RNGs.                                                                            |
| **`chacha20poly1305`**                          | Rust implementation of the ChaCha20-Poly1305 authenticated encryption scheme (AEAD). Includes ChaCha20 stream cipher + Poly1305 MAC. Provides secure symmetric encryption & decryption. |
| **`generic-array`**                             | Provides fixed-size arrays whose size is determined at compile-time. Frequently used in cryptography to enforce correct key/nonce lengths.                                              |
| **`aead`** (from `chacha20poly1305`)           | Defines traits for authenticated encryption with associated data. Standard trait (`Aead`) used by many AEAD algorithms.                                                                   |
| **`std`**                                       | Rust’s standard library. Includes filesystem I/O, collections, traits, OS interaction, threading, path management, and basic types. Used implicitly by all Rust code.                     |
| **`fs` / `std::fs`**                            | Submodule of std for filesystem operations (read, write, create files/directories).                                                                                                        |
| **`io` / `std::io`**                            | Standard module for input/output traits and operations (read, write, stdin, stdout, buffering).                                                                                            |
| **`path` / `std::path`**                        | Standard module for platform-aware file path manipulation (`Path`, `PathBuf`).                                                                                                            |
| **`convert` / `std::convert`**                  | Contains conversion traits like `From`, `Into`, and `TryInto`.                                                                                                                            |

---

# Useful Websites

- [The Rust Documentation](https://doc.rust-lang.org/?utm_source=chatgpt.com)  
- [Rust Programming Language Wikipedia Page](https://en.wikipedia.org/wiki/Rust_%28programming_language%29?utm_source=chatgpt.com)  

---

# Future Work

- Zeroizing information in RAM could be a good idea in the future.  
- Key rotation would make the system more secure and robust against leaks.  
- Metadata handling would be a nice addition if I had more time.  
- Adding the ability to store the original file name so it restores the file completely.  
