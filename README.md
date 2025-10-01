# Frut Library
A no_std library for parsing, analyzing, and executing the Frut programming language.
This library provides the core functionality for lexing, parsing, semantic analysis,
and interpretation/compilation of Frut code.

## Installation
To use `frut_lib`, first add this to your Cargo.toml:
```toml
[dependencies]
frut_lib = "0.0.1"
```
Or run:
```bash
cargo add frut_lib
```

## Usage
```rust
use frut_lib::{parse_files, analyze_project, File as FrutFile};

fn main() {
    let files = vec![FrutFile {
        path: "main.ft".to_string(),
        code: "var x: int = 42;".to_string(),
        ast: None
    }];
    
    let mut project = parse_files(files).project;
    
    if let Err(errors) = analyze_project(&mut project) {
        println!("Found {} errors", errors.len());
    } else {
        println!("Analysis successful");
    }
}
```

## Features
- No-std compatible (uses only core and alloc)
- Modular architecture for extensibility
- Support for both interpretation and compilation workflows in the future