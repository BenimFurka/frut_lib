//! # Frut Library
//!
//! A no_std library for parsing, analyzing, and executing the Frut programming language.
//! This library provides the core functionality for lexing, parsing, semantic analysis,
//! and interpretation/compilation of Frut code.
//!
//! ## Features
//!
//! - No-std compatible (uses only core and alloc)
//! - Modular architecture for extensibility
//! - Support for both interpretation and compilation workflows in the future

#![cfg_attr(not(feature = "std"), no_std)]

#![allow(clippy::result_large_err)]

#[macro_use]
extern crate alloc;

// Re-export main modules for easy access
pub mod ast;
pub mod errors;
pub mod lexer;
pub mod natives;
pub mod parser;
pub mod primitives;
pub mod resolver;
pub mod semantic;
pub mod types;
pub mod value;

// Core types and functions
pub use ast::*;
pub use errors::*;
pub use lexer::*;
pub use natives::*;
pub use parser::*;
pub use primitives::*;
pub use resolver::*;
pub use semantic::*;
pub use types::*;
pub use value::*;

use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::hash::BuildHasherDefault;

#[allow(deprecated)]
pub type HashMap<K, V> = hashbrown::HashMap<K, V, BuildHasherDefault<core::hash::SipHasher>>;

/// Parse multiple files and return a module with per-file ASTs and aggregated errors
pub fn parse_files(mut files: Vec<File>) -> ParseProjectResult {
    let mut all_errors = ErrorCollection::new();

    for file in files.iter_mut() {
        let res = parse_code(&file.code, file.name.clone());
        if res.is_success() {
            file.ast = Some(res.statements);
        } else {
            all_errors.extend(res.errors);
            file.ast = None;
        }
    }

    ParseProjectResult {
        project: Module {
            name: "root".to_string(),
            files,
            submodules: Vec::new(),
        },
        errors: all_errors,
    }
}

/// Run semantic analysis across all files in the module.
pub fn analyze_module(module: &mut Module) -> core::result::Result<(), ErrorCollection> {
    let mut all_errors = ErrorCollection::new();

    use alloc::collections::BTreeMap;
    let mut module_funcs: BTreeMap<String, Vec<(String, Vec<Type>, Type)>> = BTreeMap::new();
    let mut module_vars: BTreeMap<String, Vec<(String, Type)>> = BTreeMap::new();

    for file in module.files.iter() {
        if let Some(ast) = &file.ast {
            let mut sigs: Vec<(String, Vec<Type>, Type)> = Vec::new();
            let mut vars: Vec<(String, Type)> = Vec::new();
            for stmt in ast.iter() {
                if let StatementKind::FunctionDeclaration {
                    name,
                    params,
                    return_type,
                    ..
                } = &stmt.kind
                {
                    let param_types = params
                        .iter()
                        .map(|p| crate::types::type_from_expr(&p.param_type))
                        .collect::<Vec<_>>();
                    let ret = crate::types::type_from_expr(return_type);
                    sigs.push((name.clone(), param_types, ret));
                } else if let StatementKind::VariableDeclaration { name, var_type, .. } = &stmt.kind
                {
                    vars.push((name.clone(), crate::types::type_from_expr(var_type)));
                }
            }
            module_funcs.insert(file.name.clone(), sigs);
            module_vars.insert(file.name.clone(), vars);
        }
    }

    for file in module.files.iter() {
        if let Some(ast) = &file.ast {
            let mut analyzer = SemanticAnalyzer::new(file.name.clone(), file.code.clone());

            if let Err(errors) = analyzer.analyze(ast) {
                all_errors.extend(errors);
            }
        }
    }

    if all_errors.is_empty() {
        Ok(())
    } else {
        Err(all_errors)
    }
}

/// Simple parsing code
pub fn parse_code(input: &str, filename: String) -> ParseResult {
    let mut lexer = Lexer::new(input, filename.clone());
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens, input.to_string(), filename);
    parser.parse()
}

/// Version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
