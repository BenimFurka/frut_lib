//! Unified import resolver for the Frut.

use crate::ast::{ImportItem, Module, Parameter, Position, Statement, StatementKind};
use crate::{ErrorReport, ErrorType};
use alloc::string::{String, ToString};
use alloc::vec::Vec;

/// A resolved import: which symbol was found and where it lives
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedImport {
    Function {
        /// Local name the symbol is imported as (last path component)
        name: String,
        params: Vec<Parameter>,
        return_type: crate::ast::TypeExpr,
        /// Generic type parameters of the declaration (`func f<T>(...)`)
        /// Needed to type `f<int>()` at the import site
        type_params: Vec<String>,
        /// Interface bounds per type parameter (`T: Shape`)
        type_param_bounds: Vec<Option<String>>,
        /// Fully-qualified module path, e.g. "std.io" (dot-joined)
        module: String,
        /// The file's AST (the one containing the function body)
        source_file: String,
        pos: Position,
    },
    ForeignFunction {
        name: String,
        params: Vec<Parameter>,
        return_type: crate::ast::TypeExpr,
        /// Generic type parameters of the `foreign` declaration
        /// (`foreign func native_vec_push<T>(...)`)
        type_params: Vec<String>,
        type_param_bounds: Vec<Option<String>>,
        /// Fully-qualified module path, e.g. "std.native.io"
        module: String,
        pos: Position,
    },
    // Type and Variable imports are reserved for future use.
    Type {
        name: String,
        module: String,
        /// Field declarations `(name, type_expr)`
        fields: Vec<(String, crate::ast::TypeExpr)>,
        /// For enum types: the variant declarations (name + payload types)
        /// Empty for struct types.
        variants: Vec<crate::ast::EnumVariantDecl>,
        /// Generic type parameters, e.g. `type Box<T>` -> `["T"]`,
        /// `enum Option<T>` -> `["T"]`. Empty for non-generic types
        type_params: Vec<String>,
        /// Method signatures `(name, params, return_type)` collected from the
        /// `ext <Type> { ... }` blocks in the same file
        methods: Vec<(String, Vec<Parameter>, crate::ast::TypeExpr)>,
        /// For interface types: the method signatures of the interface
        /// Empty for struct/enum types
        interface_methods: Vec<(String, Vec<Parameter>, crate::ast::TypeExpr)>,
    },
}

/// Unifies resolving `import` statements against a module tree
#[derive(Debug, Clone)]
pub struct ImportResolver<'a> {
    root: &'a Module,
    /// Files already loaded/visited - used by the interpreter to avoid
    /// re-executing a file during import
    loaded_files: Vec<String>,
}

impl<'a> ImportResolver<'a> {
    /// Create a resolver for a module tree root
    pub fn new(root: &'a Module) -> Self {
        Self {
            root,
            loaded_files: Vec::new(),
        }
    }

    /// Create a resolver with a pre-populated set of already-loaded files
    pub fn with_loaded(root: &'a Module, loaded_files: Vec<String>) -> Self {
        Self { root, loaded_files }
    }

    /// The set of files already visited (mutated via [`Self::mark_loaded`])
    pub fn loaded_files(&self) -> &[String] {
        &self.loaded_files
    }

    /// Mark a fully-qualified file path (e.g. "std.io") as loaded/visited
    pub fn mark_loaded(&mut self, file: String) {
        if !self.loaded_files.contains(&file) {
            self.loaded_files.push(file);
        }
    }

    /// Whether a fully-qualified file path has already been loaded
    pub fn is_loaded(&self, file: &str) -> bool {
        self.loaded_files.iter().any(|f| f == file)
    }

    /// Resolve an import.
    ///
    /// - For `Identifier(item)` returns up to one `ResolvedImport`.
    /// - For `Wildcard` returns all function/foreign/type symbols of the target
    ///   file or module.
    pub fn resolve(
        &mut self,
        library_path: &str,
        item: &ImportItem,
        pos: &Position,
    ) -> Result<Vec<ResolvedImport>, ErrorReport> {
        let parts: Vec<&str> = library_path.split('.').filter(|p| !p.is_empty()).collect();
        if parts.is_empty() {
            return Err(self.error(ErrorType::SyntaxError, "Empty import path".to_string(), pos));
        }

        let (target_file, target_module, qualified) = self.navigate(&parts, pos)?;

        let matches = match item {
            ImportItem::Identifier(name) => {
                let name = name.clone();
                let target = target_file.ok_or_else(|| {
                    self.error(
                        ErrorType::UndefinedVariable(name.clone()),
                        format!(
                            "'{qualified}' is a module, not a file - cannot import '{name}' from it"),
                        pos,
                    )
                })?;
                match self.find_in_file(target, &name, &qualified) {
                    Some(found) => vec![found],
                    None => {
                        return Err(self.error(
                            ErrorType::UndefinedVariable(name.clone()),
                            format!("Symbol '{name}' not found in import '{library_path}'"),
                            pos,
                        ));
                    }
                }
            }
            ImportItem::Wildcard => {
                // All symbols in the target file, or the target module's files
                // if the last segment is a module
                let mut out = Vec::new();
                match target_file {
                    Some(file) => {
                        self.collect_from_file(file, &qualified, &mut out);
                    }
                    None => {
                        for file in &target_module.files {
                            let file_qual = if qualified.is_empty() {
                                file.name.clone()
                            } else {
                                format!("{}.{}", qualified, file.name)
                            };
                            self.collect_from_file(file, &file_qual, &mut out);
                        }
                    }
                }
                out
            }
        };

        Ok(matches)
    }

    /// Navigate the module tree following `parts`. Returns the target file
    /// (if the last segment is a file), the target module, and the qualified
    /// path (without the final item).
    fn navigate<'m>(
        &'m self,
        parts: &[&str],
        pos: &Position,
    ) -> Result<(Option<&'m crate::ast::File>, &'m Module, String), ErrorReport> {
        if parts.is_empty() {
            return Err(self.error(ErrorType::SyntaxError, "Empty import path".to_string(), pos));
        }

        let mut current = self.root.find_submodule(parts[0]).ok_or_else(|| {
            self.error(
                ErrorType::UndefinedVariable(parts[0].to_string()),
                format!("Module '{}' not found", parts[0]),
                pos,
            )
        })?;

        let mut qualified = parts[0].to_string();

        // Navigate intermediate segments: submodule, or file.
        for (i, seg) in parts[1..].iter().enumerate() {
            let is_last = i == parts[1..].len() - 1;

            if let Some(sub) = current.find_submodule(seg) {
                if !qualified.is_empty() {
                    qualified.push('.');
                }
                qualified.push_str(seg);
                current = sub;
                continue;
            }

            if let Some(file) = current.find_file(seg) {
                if !qualified.is_empty() {
                    qualified.push('.');
                }
                qualified.push_str(seg);

                if is_last {
                    return Ok((Some(file), current, qualified));
                }

                return Err(self.error(
                    ErrorType::UndefinedVariable(seg.to_string()),
                    format!(
                        "File '{}' cannot contain further modules in '{}'",
                        seg,
                        parts.join(".")
                    ),
                    pos,
                ));
            }

            return Err(self.error(
                ErrorType::UndefinedVariable(seg.to_string()),
                format!(
                    "Module or file '{}' not found in import '{}'",
                    seg,
                    parts.join(".")
                ),
                pos,
            ));
        }

        Ok((None, current, qualified))
    }

    /// Find a symbol in a single file
    fn find_in_file(
        &self,
        file: &crate::ast::File,
        name: &str,
        qualified: &str,
    ) -> Option<ResolvedImport> {
        let ast = file.ast.as_ref()?;
        for statement in ast {
            match &statement.kind {
                StatementKind::FunctionDeclaration {
                    name: fn_name,
                    params,
                    return_type,
                    type_params,
                    type_param_bounds,
                    ..
                } if fn_name == name => {
                    return Some(ResolvedImport::Function {
                        name: fn_name.clone(),
                        params: params.clone(),
                        return_type: return_type.clone(),
                        type_params: type_params.clone(),
                        type_param_bounds: type_param_bounds.clone(),
                        module: qualified.to_string(),
                        source_file: file.name.clone(),
                        pos: statement.pos.clone(),
                    });
                }
                StatementKind::ForeignFunctionDeclaration {
                    name: fn_name,
                    type_params,
                    type_param_bounds,
                    params,
                    return_type,
                } if fn_name == name => {
                    return Some(ResolvedImport::ForeignFunction {
                        name: fn_name.clone(),
                        params: params.clone(),
                        return_type: return_type.clone(),
                        type_params: type_params.clone(),
                        type_param_bounds: type_param_bounds.clone(),
                        module: qualified.to_string(),
                        pos: statement.pos.clone(),
                    });
                }
                StatementKind::TypeDeclaration {
                    name: ty_name,
                    type_params,
                    type_param_bounds: _,
                    fields,
                } if ty_name == name => {
                    let methods = collect_ext_methods(ast, ty_name);
                    return Some(ResolvedImport::Type {
                        name: ty_name.clone(),
                        module: qualified.to_string(),
                        fields: fields
                            .iter()
                            .map(|f| (f.name.clone(), f.field_type.clone()))
                            .collect(),
                        variants: Vec::new(),
                        type_params: type_params.clone(),
                        methods,
                        interface_methods: Vec::new(),
                    });
                }
                StatementKind::EnumDeclaration {
                    name: ty_name,
                    type_params,
                    type_param_bounds: _,
                    variants,
                } if ty_name == name => {
                    let methods = collect_ext_methods(ast, ty_name);
                    return Some(ResolvedImport::Type {
                        name: ty_name.clone(),
                        module: qualified.to_string(),
                        fields: Vec::new(),
                        variants: variants.clone(),
                        type_params: type_params.clone(),
                        methods,
                        interface_methods: Vec::new(),
                    });
                }
                StatementKind::InterfaceDeclaration {
                    name: iface_name,
                    type_params: _,
                    type_param_bounds: _,
                    methods,
                } if iface_name == name => {
                    let interface_methods = methods
                        .iter()
                        .map(|m| (m.name.clone(), m.params.clone(), m.return_type.clone()))
                        .collect();
                    return Some(ResolvedImport::Type {
                        name: iface_name.clone(),
                        module: qualified.to_string(),
                        fields: Vec::new(),
                        variants: Vec::new(),
                        type_params: Vec::new(),
                        methods: Vec::new(),
                        interface_methods,
                    });
                }
                _ => {}
            }
        }
        None
    }

    /// Collect all importable symbols from a file
    fn collect_from_file(
        &self,
        file: &crate::ast::File,
        qualified: &str,
        out: &mut Vec<ResolvedImport>,
    ) {
        let Some(ast) = file.ast.as_ref() else { return };
        for statement in ast {
            match &statement.kind {
                StatementKind::FunctionDeclaration {
                    name,
                    params,
                    return_type,
                    type_params,
                    type_param_bounds,
                    ..
                } => {
                    out.push(ResolvedImport::Function {
                        name: name.clone(),
                        params: params.clone(),
                        return_type: return_type.clone(),
                        type_params: type_params.clone(),
                        type_param_bounds: type_param_bounds.clone(),
                        module: qualified.to_string(),
                        source_file: file.name.clone(),
                        pos: statement.pos.clone(),
                    });
                }
                StatementKind::ForeignFunctionDeclaration {
                    name,
                    type_params,
                    type_param_bounds,
                    params,
                    return_type,
                } => {
                    out.push(ResolvedImport::ForeignFunction {
                        name: name.clone(),
                        params: params.clone(),
                        return_type: return_type.clone(),
                        type_params: type_params.clone(),
                        type_param_bounds: type_param_bounds.clone(),
                        module: qualified.to_string(),
                        pos: statement.pos.clone(),
                    });
                }
                StatementKind::TypeDeclaration {
                    name,
                    type_params,
                    type_param_bounds: _,
                    fields,
                } => {
                    let methods = collect_ext_methods(ast, name);
                    out.push(ResolvedImport::Type {
                        name: name.clone(),
                        module: qualified.to_string(),
                        fields: fields
                            .iter()
                            .map(|f| (f.name.clone(), f.field_type.clone()))
                            .collect(),
                        variants: Vec::new(),
                        type_params: type_params.clone(),
                        methods,
                        interface_methods: Vec::new(),
                    });
                }
                StatementKind::EnumDeclaration {
                    name,
                    type_params,
                    type_param_bounds: _,
                    variants,
                } => {
                    let methods = collect_ext_methods(ast, name);
                    out.push(ResolvedImport::Type {
                        name: name.clone(),
                        module: qualified.to_string(),
                        fields: Vec::new(),
                        variants: variants.clone(),
                        type_params: type_params.clone(),
                        methods,
                        interface_methods: Vec::new(),
                    });
                }
                StatementKind::InterfaceDeclaration {
                    name,
                    type_params: _,
                    type_param_bounds: _,
                    methods,
                } => {
                    let interface_methods = methods
                        .iter()
                        .map(|m| (m.name.clone(), m.params.clone(), m.return_type.clone()))
                        .collect();
                    out.push(ResolvedImport::Type {
                        name: name.clone(),
                        module: qualified.to_string(),
                        fields: Vec::new(),
                        variants: Vec::new(),
                        type_params: Vec::new(),
                        methods: Vec::new(),
                        interface_methods,
                    });
                }
                _ => {}
            }
        }
    }

    /// Collect method signatures from all `ext <type_name> { ... }` blocks in a
    /// file's AST (both `func` and `foreign func` methods)
    fn error(&self, error_type: ErrorType, message: String, pos: &Position) -> ErrorReport {
        ErrorReport::with_file(
            error_type,
            message,
            pos.file.clone(),
            pos.line,
            pos.column,
            pos.offset,
            pos.length,
            String::new(),
        )
    }
}

/// Collect method signatures from all `ext <type_name> { ... }` blocks in a
/// file's AST (both `func` and `foreign func` methods)
fn collect_ext_methods(
    ast: &[Statement],
    type_name: &str,
) -> Vec<(String, Vec<Parameter>, crate::ast::TypeExpr)> {
    let mut methods = Vec::new();
    for stmt in ast {
        if let StatementKind::ExtDeclaration {
            type_name: ext_ty,
            methods: ext_methods,
            ..
        } = &stmt.kind
        {
            if ext_ty != type_name {
                continue;
            }
            for method in ext_methods {
                match &method.kind {
                    StatementKind::FunctionDeclaration {
                        name,
                        params,
                        return_type,
                        ..
                    } => {
                        methods.push((name.clone(), params.clone(), return_type.clone()));
                    }
                    StatementKind::ForeignFunctionDeclaration {
                        name,
                        type_params: _,
                        type_param_bounds: _,
                        params,
                        return_type,
                    } => {
                        methods.push((name.clone(), params.clone(), return_type.clone()));
                    }
                    _ => {}
                }
            }
        }
    }
    methods
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_code;
    use crate::{File, Position};

    /// Build a module tree `root -> { self, std } -> files` from source strings.
    fn build_root(self_module: Module, std_module: Module) -> Module {
        let mut root = Module::new("root".to_string());
        root.add_submodule(self_module);
        root.add_submodule(std_module);
        root
    }

    fn module_with_file(name: &str, file_name: &str, code: &str) -> Module {
        let mut module = Module::new(name.to_string());
        let result = parse_code(code, format!("{file_name}.ft"));
        assert!(
            result.is_success(),
            "parse failed: {:?}",
            result.errors.errors
        );
        module.add_file(File {
            name: file_name.to_string(),
            code: code.to_string(),
            ast: Some(result.statements),
        });
        module
    }

    #[test]
    fn resolves_item_from_file_with_full_qualified_module() {
        let std = module_with_file(
            "std",
            "io",
            "func print(s: string) {}\nforeign func native_write_stdout(d: string): void;",
        );
        let root = build_root(Module::new("self".to_string()), std);

        let mut resolver = ImportResolver::new(&root);
        let pos = Position::new("main.ft".to_string(), 1, 1, 0, 1);
        let resolved = resolver
            .resolve("std.io", &ImportItem::Identifier("print".to_string()), &pos)
            .expect("resolve should succeed");

        assert_eq!(resolved.len(), 1);
        match &resolved[0] {
            ResolvedImport::Function { module, name, .. } => {
                assert_eq!(name, "print");
                // The qualified path must include the top-level segment
                assert_eq!(module, "std.io");
            }
            other => panic!("expected Function, got {other:?}"),
        }
    }

    #[test]
    fn wildcard_import_collects_all_symbols() {
        let std = module_with_file(
            "std",
            "math",
            "foreign func abs(x: int): int;\nforeign func min(a: int, b: int): int;",
        );
        let root = build_root(Module::new("self".to_string()), std);

        let mut resolver = ImportResolver::new(&root);
        let pos = Position::new("main.ft".to_string(), 1, 1, 0, 1);
        let resolved = resolver
            .resolve("std.math", &ImportItem::Wildcard, &pos)
            .expect("resolve should succeed");

        assert_eq!(resolved.len(), 2);
        for import in &resolved {
            match import {
                ResolvedImport::ForeignFunction { module, .. } => assert_eq!(module, "std.math"),
                other => panic!("expected ForeignFunction, got {other:?}"),
            }
        }
    }

    #[test]
    fn missing_symbol_reports_error() {
        let std = module_with_file("std", "io", "func print(s: string) {}");
        let root = build_root(Module::new("self".to_string()), std);

        let mut resolver = ImportResolver::new(&root);
        let pos = Position::new("main.ft".to_string(), 1, 1, 0, 1);
        let err = resolver
            .resolve(
                "std.io",
                &ImportItem::Identifier("missing".to_string()),
                &pos,
            )
            .expect_err("resolve should fail");

        assert!(
            err.message.contains("missing"),
            "message should mention the missing symbol, got: {}",
            err.message
        );
        // The error position must be the real one, not a dummy (1,1) in the wrong file
        assert_eq!(err.position.file, "main.ft");
        assert_eq!(err.position.line, 1);
    }

    #[test]
    fn resolves_nested_submodule_path() {
        let mut std = Module::new("std".to_string());
        let mut native = Module::new("native".to_string());
        native.add_file(File {
            name: "io".to_string(),
            code: String::new(),
            ast: Some(vec![]),
        });
        std.add_submodule(native);
        let root = build_root(Module::new("self".to_string()), std);

        // Even an empty file resolves as a target (no symbols to import)
        let mut resolver = ImportResolver::new(&root);
        let pos = Position::new("main.ft".to_string(), 1, 1, 0, 1);
        let resolved = resolver
            .resolve("std.native.io", &ImportItem::Wildcard, &pos)
            .expect("nested module path should resolve");
        assert!(resolved.is_empty());
    }
}
