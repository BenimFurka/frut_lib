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
//! - Support for both interpretation and compilation workflows

#![cfg_attr(not(feature = "std"), no_std)]

#![no_std]
#![no_main]

#[macro_use]
extern crate alloc;

// Re-export main modules for easy access
pub mod ast;
pub mod errors;
pub mod lexer;
pub mod parser;
pub mod semantic;
pub mod types;
pub mod value;

#[cfg(not(feature = "std"))]
use linked_list_allocator::LockedHeap;

use alloc::string::{String, ToString};
use alloc::vec::Vec;

// Core types and functions
pub use ast::*;
pub use errors::*;
pub use lexer::*;
pub use parser::*;
pub use semantic::*;
pub use types::*;
pub use value::*;
use crate::alloc::*;

#[cfg(not(feature = "std"))]
use core::panic::PanicInfo;

#[cfg(not(feature = "std"))]
#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

/// Represents a source file provided to the core (can be in-memory or from disk)
#[derive(Debug, Clone)]
pub struct File {
    pub path: String,
    pub code: String,
    pub ast: Option<Vec<Statement>>, // NOTE: filled after parsing
}

/// A parsed project consisting of multiple files
#[derive(Debug, Clone)]
pub struct Project {
    pub files: Vec<File>,
}

/// Result of parsing multiple files
#[derive(Debug, Clone)]
pub struct ParseProjectResult {
    pub project: Project,
    pub errors: ErrorCollection,
}

impl ParseProjectResult {
    pub fn is_success(&self) -> bool { self.errors.is_empty() }
}

/// Parse multiple files and return a project with per-file ASTs and aggregated errors
pub fn parse_files(mut files: Vec<File>) -> ParseProjectResult {
    let mut all_errors = ErrorCollection::new();

    for file in files.iter_mut() {
        let res = parse_code(&file.code, file.path.clone());
        if res.is_success() {
            file.ast = Some(res.statements);
        } else {
            for e in res.errors.errors.iter() {
                all_errors.add_error(e.clone());
            }
            file.ast = None;
        }
    }

    ParseProjectResult { project: Project { files }, errors: all_errors }
}

/// Run semantic analysis across all files in the project.
pub fn analyze_project(project: &mut Project) -> core::result::Result<(), ErrorCollection> {
    fn extract_code_snippet(code: &str, line: usize) -> String {
        if line == 0 { return String::new(); }
        let lines: Vec<&str> = code.lines().collect();
        if line > 0 && line <= lines.len() { lines[line - 1].to_string() } else { String::new() }
    }

    let mut all_errors = ErrorCollection::new();

    use alloc::collections::BTreeMap;
    let mut module_funcs: BTreeMap<String, Vec<(String, Vec<Type>, Type)>> = BTreeMap::new();
    let mut module_vars: BTreeMap<String, Vec<(String, Type)>> = BTreeMap::new();

    for file in project.files.iter() {
        if let Some(ref ast) = file.ast {
            let mut sigs: Vec<(String, Vec<Type>, Type)> = Vec::new();
            let mut vars: Vec<(String, Type)> = Vec::new();
            for stmt in ast.iter() {
                if let StatementKind::FunctionDeclaration { name, params, return_type, .. } = &stmt.kind {
                    let param_types = params.iter().map(|p| Type::from(p.param_type.as_str())).collect::<Vec<_>>();
                    let ret = Type::from(return_type.as_str());
                    sigs.push((name.clone(), param_types, ret));
                } else if let StatementKind::VariableDeclaration { name, var_type, .. } = &stmt.kind {
                    vars.push((name.clone(), Type::from(var_type.as_str())));
                }
            }
            module_funcs.insert(file.path.clone(), sigs);
            module_vars.insert(file.path.clone(), vars);
        }
    }

    fn resolve_module_file<'a>(project: &'a Project, path_segments: &[String]) -> Option<&'a File> {
        let joined = path_segments.join("/");
        let needle = joined.as_str();
        for f in project.files.iter() {
            if f.path.ends_with(&format!("/{}.ft", joined)) || f.path.ends_with(&format!("{}{}.ft", if joined.is_empty() { "" } else { "/" }, joined)) {
                return Some(f);
            }
            if let Some(file_name) = f.path.rsplit('/').next() {
                let stem = file_name.strip_suffix(".ft").unwrap_or(file_name);
                if stem == needle || stem == path_segments.last().map(|s| s.as_str()).unwrap_or(needle) {
                    return Some(f);
                }
            }
        }
        None
    }

    for file in project.files.iter() {
        if let Some(ref ast) = file.ast {
            let mut analyzer = SemanticAnalyzer::new(file.path.clone(), file.code.clone());

            for stmt in ast.iter() {
                if let StatementKind::Import { path, kind } = &stmt.kind {
                    let target = resolve_module_file(project, path);
                    if target.is_none() {
                        let pos = &stmt.pos;
                        all_errors.add_error(ErrorReport::with_file(
                            ErrorType::SyntaxError,
                            format!("Module not found: {}", path.join(".")),
                            file.path.clone(),
                            pos.line,
                            pos.column,
                            pos.offset,
                            pos.length,
                            extract_code_snippet(&file.code, pos.line),
                        ));
                        continue;
                    }
                    let target = target.unwrap();
                    let funcs_opt = module_funcs.get(&target.path);
                    let vars_opt = module_vars.get(&target.path);
                    match kind {
                        ImportKind::Wildcard => {
                            if let Some(funcs) = funcs_opt {
                                for (fname, params, ret) in funcs.iter() {
                                    let _ = analyzer.predeclare_function(fname.clone(), params.clone(), ret.clone());
                                }
                            }
                            if let Some(vars) = vars_opt {
                                for (vname, vtype) in vars.iter() {
                                    let _ = analyzer.predeclare_initialized_variable(vname.clone(), vtype.clone());
                                }
                            }
                        }
                        ImportKind::Single(name) => {
                            let mut found = false;
                            if let Some(funcs) = funcs_opt {
                                if let Some((_, params, ret)) = funcs.iter().find(|(n, _, _)| n == name) {
                                    let _ = analyzer.predeclare_function(name.clone(), params.clone(), ret.clone());
                                    found = true;
                                }
                            }
                            if !found {
                                if let Some(vars) = vars_opt {
                                    if let Some((_, vtype)) = vars.iter().find(|(n, _)| n == name) {
                                        let _ = analyzer.predeclare_initialized_variable(name.clone(), vtype.clone());
                                        found = true;
                                    }
                                }
                            }
                            if !found {
                                let pos = &stmt.pos;
                                all_errors.add_error(ErrorReport::with_file(
                                    ErrorType::UndefinedVariable(name.clone()),
                                    format!("Symbol '{}' not found in module {}", name, path.join(".")),
                                    file.path.clone(),
                                    pos.line,
                                    pos.column,
                                    pos.offset,
                                    pos.length,
                                    extract_code_snippet(&file.code, pos.line),
                                ));
                            }
                        }
                        ImportKind::Group(names) => {
                            for name in names.iter() {
                                let mut found = false;
                                if let Some(funcs) = funcs_opt {
                                    if let Some((_, params, ret)) = funcs.iter().find(|(n, _, _)| n == name) {
                                        let _ = analyzer.predeclare_function(name.clone(), params.clone(), ret.clone());
                                        found = true;
                                    }
                                }
                                if !found {
                                    if let Some(vars) = vars_opt {
                                        if let Some((_, vtype)) = vars.iter().find(|(n, _)| n == name) {
                                            let _ = analyzer.predeclare_initialized_variable(name.clone(), vtype.clone());
                                            found = true;
                                        }
                                    }
                                }
                                if !found {
                                    let pos = &stmt.pos;
                                    all_errors.add_error(ErrorReport::with_file(
                                        ErrorType::UndefinedVariable(name.clone()),
                                        format!("Symbol '{}' not found in module {}", name, path.join(".")),
                                        file.path.clone(),
                                        pos.line,
                                        pos.column,
                                        pos.offset,
                                        pos.length,
                                        extract_code_snippet(&file.code, pos.line),
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            if let Err(errors) = analyzer.analyze(ast) {
                for e in errors.errors.iter() {
                    all_errors.add_error(e.clone());
                }
            }
        }
    }

    if all_errors.is_empty() { Ok(()) } else { Err(all_errors) }
}

/// Main parsing function - entry point for the library
pub fn parse_code(input: &str, filename: String) -> ParseResult {
    #[cfg(not(feature = "std"))]
    unsafe { ALLOCATOR.lock().init(HEAP_START.as_mut_ptr(), HEAP_SIZE); }

    let mut lexer = Lexer::new(input, filename.clone());
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens, input.to_string(), filename);
    parser.parse()
}

/// Version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// TODO: um i guess check no-std lol
#[cfg(not(feature = "std"))]
const HEAP_SIZE: usize = 16 * 1024;

#[cfg(not(feature = "std"))]
static mut HEAP_START: [u8; HEAP_SIZE] = [0; HEAP_SIZE];

#[cfg(not(feature = "std"))]
#[global_allocator]
static ALLOCATOR: LockedHeap = LockedHeap::empty();
