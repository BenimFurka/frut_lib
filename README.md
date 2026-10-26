# Frut Library

[![Crates.io](https://img.shields.io/crates/v/frut_lib.svg)](https://crates.io/crates/frut_lib)
[![Documentation](https://docs.rs/frut_lib/badge.svg)](https://docs.rs/frut_lib)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](https://github.com/BenimFurka/frut/blob/main/LICENSE)

A `no_std` library for parsing, analyzing, and executing the Frut programming language.
It provides the core lexer, parser, AST, semantic analyzer, import resolver, type system,
and runtime primitives used by the Frut toolchain.

## Features
- No-std compatible (uses only `core` and `alloc`; `std` is an opt-in feature)
- Span-aware lexer and parser with aggregated diagnostics
- Semantic analysis with type checking and scopes
- Multi-file modules and `import` resolution
- Structs, enums, interfaces, `ext` blocks, generics, fixed-length arrays, and `match`
- Runtime `Value`/`RuntimeEnvironment` and a native-function registry

## Installation
To use `frut_lib`, add this to your Cargo.toml:

```toml
[dependencies]
frut_lib = "0.1.1"
```

For `no_std` environments, disable default features:

```toml
[dependencies]
frut_lib = { version = "0.1.1", default-features = false }
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

### Multi-file projects
```rust
use frut_lib::{analyze_module, parse_files, File};

fn main() {
    let mut project = parse_files(vec![File {
        name: "main.ft".to_string(),
        code: "var x: int = 42;".to_string(),
        ast: None,
    }])
    .project;

    if let Err(errors) = analyze_module(&mut project) {
        eprintln!("Found {} errors", errors.len());
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
Licensed under the Apache License, Version 2.0 [LICENSE](LICENSE).
