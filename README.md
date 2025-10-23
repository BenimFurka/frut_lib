# Frut Library

[![Crates.io](https://img.shields.io/crates/v/frut_lib.svg)](https://crates.io/crates/frut_lib)
[![Documentation](https://docs.rs/frut_lib/badge.svg)](https://docs.rs/frut_lib)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](https://github.com/BenimFurka/frut/blob/main/LICENSE)

A `no_std` library for parsing, analyzing, and executing the Frut programming language.
This library provides the core functionality for lexing, parsing, semantic analysis,
and interpretation/compilation of Frut code.

## Features
- No-std compatible (uses only core and alloc)
- Modular architecture for extensibility
- Support for both interpretation and compilation workflows in the future

## Installation
To use `frut_lib`, first add this to your Cargo.toml:

```toml
[dependencies]
frut_lib = "0.0.2"
```

For `no_std` environments, disable default features:

```toml
[dependencies]
frut_lib = { version = "0.0.2", default-features = false }
```

## Usage
```rust
use frut_lib::parse_code;

fn main() {
    let result = parse_code("var x: int = 42;", "main.ft".to_string());
    match result.is_success() {
        false => println!("Found {} errors", result.errors.len()),
        true => println!("Analysis successful"),
    }
}
```

## Examples
See the `examples/` directory for more usage examples:

```bash
cargo run --example base
cargo run --example interp
```


## Documentation
For documentation, visit [docs.rs/frut_lib](https://docs.rs/frut_lib).

## License
Licensed under the Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE) or http://www.apache.org/licenses/LICENSE-2.0).