//! Semantic analyzer for the Frut
//!
//! Performs semantic analysis including type checking, name resolution,
//! and control flow analysis.

use alloc::{
    boxed::Box,
    string::{String, ToString},
    vec::Vec,
};

use crate::ast::{
    BinaryOperator, Expression, ExpressionKind, Module, Statement, StatementKind, UnaryOperator,
};
use crate::resolver::{ImportResolver, ResolvedImport};
use crate::types::Type;
use crate::{ErrorCollection, ErrorReport, ErrorType, HashMap};

/// Represents a symbol in the environment (either a variable or a function)
#[derive(Debug, Clone)]
enum Symbol {
    Variable(VariableInfo),
    Function(FunctionInfo),
    StructType(alloc::rc::Rc<StructTypeInfo>),
    EnumType(EnumTypeInfo),
    InterfaceType(InterfaceTypeInfo),
}

/// Information about a struct type
#[derive(Debug, Clone)]
struct StructTypeInfo {
    name: String,
    /// Field name -> type. `Rc` so that `Type::Struct` can share this map
    /// instead of deep-cloning it on every type resolution
    fields: alloc::rc::Rc<HashMap<String, Type>>,
    methods: HashMap<String, FunctionInfo>,
    /// Generic type parameter names: `type Box<T>` -> `["T"]`. Empty for
    /// non-generic types
    type_params: Vec<String>,
}

/// Information about an enum type
#[derive(Debug, Clone)]
struct EnumTypeInfo {
    name: String,
    /// Variant name -> payload types
    variants: Vec<crate::types::EnumVariant>,
    /// Ext methods attached to this enum (`ext Name { func ... }`)
    /// Signatures include `self` as the first parameter
    methods: HashMap<String, FunctionInfo>,
    /// Generic type parameter names: `enum Option<T>` -> `["T"]`. Empty for
    /// non-generic types
    type_params: Vec<String>,
}

/// Information about an interface type
#[derive(Debug, Clone)]
struct InterfaceTypeInfo {
    name: String,
    /// Method name -> signature (without `self`)
    methods: HashMap<String, FunctionInfo>,
    /// Generic type parameter names: `interface Container<T>` -> `["T"]`
    type_params: Vec<String>,
}

/// Represents a variable in the environment
#[derive(Debug, Clone)]
struct VariableInfo {
    var_type: Type,
    is_initialized: bool,
}

#[derive(Debug, Clone)]
struct FunctionInfo {
    param_types: Vec<Type>,
    return_type: Type,
    /// Generic type parameter names: `func f<T, U>(...)` -> `["T", "U"]`
    /// Empty for non-generic functions
    type_params: Vec<String>,
    /// Interface bounds per type parameter (parallel to `type_params`):
    /// `func f<T: Shape, U>` -> `[Some("Shape"), None]`
    type_param_bounds: Vec<Option<String>>,
}

/// A single traversal of the module tree: an index mapping "type name -> fields + extension methods"
fn collect_all_types(module: &crate::ast::Module, index: &mut HashMap<String, RawTypeDecl>) {
    for file in &module.files {
        let ast = match &file.ast {
            Some(a) => a,
            None => continue,
        };
        for stmt in ast {
            match &stmt.kind {
                StatementKind::TypeDeclaration {
                    name: ty_name,
                    type_params,
                    type_param_bounds: _,
                    fields,
                } => {
                    index.entry(ty_name.clone()).or_insert_with(|| RawTypeDecl {
                        name: ty_name.clone(),
                        type_params: type_params.clone(),
                        fields: fields
                            .iter()
                            .map(|f| (f.name.clone(), f.field_type.clone()))
                            .collect(),
                        methods: collect_ext_methods(ast, ty_name),
                    });
                }
                StatementKind::ExtDeclaration { type_name, .. } => {
                    index
                        .entry(type_name.clone())
                        .or_insert_with(|| RawTypeDecl {
                            name: type_name.clone(),
                            type_params: Vec::new(),
                            fields: Vec::new(),
                            methods: collect_ext_methods(ast, type_name),
                        });
                }
                _ => {}
            }
        }
    }
    for sub in &module.submodules {
        collect_all_types(sub, index);
    }
}

/// One traversal of the module tree: index `interface I` -> signatures
fn collect_all_interfaces(
    module: &crate::ast::Module,
    index: &mut HashMap<String, RawInterfaceDecl>,
) {
    for file in &module.files {
        let Some(ast) = file.ast.as_ref() else {
            continue;
        };
        for stmt in ast {
            if let StatementKind::InterfaceDeclaration {
                name,
                type_params,
                methods,
                ..
            } = &stmt.kind
            {
                index
                    .entry(name.clone())
                    .or_insert_with(|| RawInterfaceDecl {
                        name: name.clone(),
                        type_params: type_params.clone(),
                        methods: methods
                            .iter()
                            .map(|m| (m.name.clone(), m.params.clone(), m.return_type.clone()))
                            .collect(),
                    });
            }
        }
    }
    for sub in &module.submodules {
        collect_all_interfaces(sub, index);
    }
}

/// Method signatures from `ext <type_name> { ... }` in one file
fn collect_ext_methods(
    ast: &[Statement],
    type_name: &str,
) -> Vec<(String, Vec<crate::ast::Parameter>, crate::ast::TypeExpr)> {
    let mut out = Vec::new();
    for stmt in ast {
        if let StatementKind::ExtDeclaration {
            type_name: ext_type,
            methods,
            ..
        } = &stmt.kind
        {
            if ext_type != type_name {
                continue;
            }
            for m in methods {
                if let StatementKind::FunctionDeclaration {
                    name,
                    params,
                    return_type,
                    ..
                } = &m.kind
                {
                    out.push((name.clone(), params.clone(), return_type.clone()));
                }
            }
        }
    }
    out
}

/// Raw type declaration from the module tree
#[derive(Clone)]
struct RawTypeDecl {
    name: String,
    type_params: Vec<String>,
    fields: Vec<(String, crate::ast::TypeExpr)>,
    methods: Vec<(String, Vec<crate::ast::Parameter>, crate::ast::TypeExpr)>,
}

/// `interface I { ... }` as is: method signatures are resolved
/// lazily, within the context of the module being analyzed
#[derive(Clone)]
struct RawInterfaceDecl {
    name: String,
    type_params: Vec<String>,
    methods: Vec<(String, Vec<crate::ast::Parameter>, crate::ast::TypeExpr)>,
}

/// Environment for tracking variables and their scopes
#[derive(Debug, Clone)]
struct Environment {
    scopes: Vec<HashMap<String, Symbol>>,
    external_types:
        alloc::rc::Rc<core::cell::RefCell<HashMap<String, alloc::rc::Rc<StructTypeInfo>>>>,
    external_interfaces: alloc::rc::Rc<core::cell::RefCell<HashMap<String, InterfaceTypeInfo>>>,
}

impl Environment {
    /// Create a new environment
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::default()],
            external_types: alloc::rc::Rc::new(core::cell::RefCell::new(HashMap::default())),
            external_interfaces: alloc::rc::Rc::new(core::cell::RefCell::new(HashMap::default())),
        }
    }
    /// Enter a new scope
    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::default());
    }

    /// Exit the current scope
    fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Predeclare a variable in the current scope without marking it initialized
    fn predeclare_variable(&mut self, name: String, var_type: Type) {
        let current_scope = self.scopes.last_mut().unwrap();
        if !current_scope.contains_key(&name) {
            current_scope.insert(
                name,
                Symbol::Variable(VariableInfo {
                    var_type,
                    is_initialized: false,
                }),
            );
        }
    }

    /// Define a variable in the current scope
    fn define_variable(&mut self, name: String, var_type: Type) -> Result<(), (ErrorType, String)> {
        let current_scope = self.scopes.last_mut().unwrap();
        if current_scope.contains_key(&name) {
            return Err((
                ErrorType::VariableAlreadyDefined(name.clone()),
                format!("Variable '{name}' is already defined in this scope"),
            ));
        }
        current_scope.insert(
            name,
            Symbol::Variable(VariableInfo {
                var_type,
                is_initialized: false,
            }),
        );
        Ok(())
    }

    /// Define a function in the current scope
    fn define_function(
        &mut self,
        name: String,
        info: FunctionInfo,
    ) -> Result<(), (ErrorType, String)> {
        let current_scope = self.scopes.last_mut().unwrap();
        if current_scope.contains_key(&name) {
            return Err((
                ErrorType::VariableAlreadyDefined(name.clone()),
                format!("Function '{name}' is already defined in this scope"),
            ));
        }
        current_scope.insert(name, Symbol::Function(info));
        Ok(())
    }

    /// Define a struct type in the current scope
    fn define_struct_type(
        &mut self,
        name: String,
        info: StructTypeInfo,
    ) -> Result<(), (ErrorType, String)> {
        let current_scope = self.scopes.last_mut().unwrap();
        if current_scope.contains_key(&name) {
            return Err((
                ErrorType::VariableAlreadyDefined(name.clone()),
                format!("Type '{name}' is already defined in this scope"),
            ));
        }
        current_scope.insert(name, Symbol::StructType(alloc::rc::Rc::new(info)));
        Ok(())
    }

    /// Get a function from any scope
    fn get_function(&self, name: &str) -> Option<&FunctionInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(Symbol::Function(func_info)) = scope.get(name) {
                return Some(func_info);
            }
        }
        None
    }

    /// Get a struct type from any scope, then from the external cache
    fn get_struct_type(&self, name: &str) -> Option<alloc::rc::Rc<StructTypeInfo>> {
        for scope in self.scopes.iter().rev() {
            if let Some(Symbol::StructType(struct_info)) = scope.get(name) {
                return Some(alloc::rc::Rc::clone(struct_info));
            }
        }
        self.external_types.borrow().get(name).cloned()
    }

    /// Mutable access to the struct type cell in the current scope
    fn get_struct_type_mut(&mut self, name: &str) -> Option<&mut alloc::rc::Rc<StructTypeInfo>> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(Symbol::StructType(struct_info)) = scope.get_mut(name) {
                return Some(struct_info);
            }
        }
        None
    }

    /// Define an enum type in the current scope
    fn define_enum_type(
        &mut self,
        name: String,
        info: EnumTypeInfo,
    ) -> Result<(), (ErrorType, String)> {
        let current_scope = self.scopes.last_mut().unwrap();
        if current_scope.contains_key(&name) {
            return Err((
                ErrorType::VariableAlreadyDefined(name.clone()),
                format!("Type '{name}' is already defined in this scope"),
            ));
        }
        current_scope.insert(name, Symbol::EnumType(info));
        Ok(())
    }

    /// Get an enum type from any scope
    fn get_enum_type(&self, name: &str) -> Option<&EnumTypeInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(Symbol::EnumType(enum_info)) = scope.get(name) {
                return Some(enum_info);
            }
        }
        None
    }

    /// Get a mutable reference to an enum type in any scope
    fn get_enum_type_mut(&mut self, name: &str) -> Option<&mut EnumTypeInfo> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(Symbol::EnumType(enum_info)) = scope.get_mut(name) {
                return Some(enum_info);
            }
        }
        None
    }

    /// Define an interface type in the current scope
    fn define_interface_type(
        &mut self,
        name: String,
        info: InterfaceTypeInfo,
    ) -> Result<(), (ErrorType, String)> {
        let current_scope = self.scopes.last_mut().unwrap();
        if current_scope.contains_key(&name) {
            return Err((
                ErrorType::VariableAlreadyDefined(name.clone()),
                format!("Type '{name}' is already defined in this scope"),
            ));
        }
        current_scope.insert(name, Symbol::InterfaceType(info));
        Ok(())
    }

    /// Interface from any scope
    fn get_interface_type(&self, name: &str) -> Option<InterfaceTypeInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(Symbol::InterfaceType(iface_info)) = scope.get(name) {
                return Some(iface_info.clone());
            }
        }
        self.external_interfaces.borrow().get(name).cloned()
    }

    /// Get a variable from any scope
    fn get_variable(&self, name: &str) -> Option<&VariableInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(Symbol::Variable(var_info)) = scope.get(name) {
                return Some(var_info);
            }
        }
        None
    }

    /// Check if a variable is defined in the current scope only
    fn is_defined_in_current_scope(&self, name: &str) -> bool {
        let current_scope = self.scopes.last().unwrap();
        current_scope.contains_key(name)
    }

    /// Update a variable's initialization status
    fn set_variable_initialized(&mut self, name: &str) -> Result<(), (ErrorType, String)> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(Symbol::Variable(var_info)) = scope.get_mut(name) {
                var_info.is_initialized = true;
                return Ok(());
            }
        }
        Err((
            ErrorType::UndefinedVariable(name.to_string()),
            format!("Variable '{name}' is not defined"),
        ))
    }

    /// Update the type of a predeclared (forward-referenced) variable
    fn update_variable_type(
        &mut self,
        name: &str,
        var_type: Type,
    ) -> Result<(), (ErrorType, String)> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(Symbol::Variable(var_info)) = scope.get_mut(name) {
                var_info.var_type = var_type;
                return Ok(());
            }
        }
        Err((
            ErrorType::UndefinedVariable(name.to_string()),
            format!("Variable '{name}' is not defined"),
        ))
    }
}

/// Semantic analyzer for Frut programs
pub struct SemanticAnalyzer<'m> {
    filename: String,
    source: String,
    environment: Environment,
    errors: ErrorCollection,
    current_return_type: Option<Type>,
    /// Generic params of the function/ext-method body being analyzed
    current_type_params: Vec<String>,
    module: Option<ModuleOrRef<'m>>,
    in_loop: bool,
    external_depth: core::cell::RefCell<Vec<String>>,
    tree_index: core::cell::RefCell<Option<HashMap<String, RawTypeDecl>>>,
    interface_index: core::cell::RefCell<Option<HashMap<String, RawInterfaceDecl>>>,
    expected_hint: core::cell::RefCell<Option<Type>>,
}

/// An owned or borrowed module tree (without cloning)
enum ModuleOrRef<'m> {
    Owned(Module),
    Borrowed(&'m Module),
}

impl core::ops::Deref for ModuleOrRef<'_> {
    type Target = Module;

    fn deref(&self) -> &Module {
        match self {
            ModuleOrRef::Owned(m) => m,
            ModuleOrRef::Borrowed(m) => m,
        }
    }
}

impl<'m> SemanticAnalyzer<'m> {
    /// Create a new semantic analyzer
    pub fn new(filename: String, source: String) -> Self {
        let environment = Environment::new();

        Self {
            errors: ErrorCollection::new(),
            environment,
            filename,
            source,
            current_return_type: None,
            current_type_params: Vec::new(),
            expected_hint: core::cell::RefCell::new(None),
            interface_index: core::cell::RefCell::new(None),
            module: None,
            in_loop: false,
            external_depth: core::cell::RefCell::new(Vec::new()),
            tree_index: core::cell::RefCell::new(None),
        }
    }

    /// Create a new semantic analyzer with owned module tree
    pub fn with_module(filename: String, source: String, module: Module) -> Self {
        let environment = Environment::new();

        Self {
            errors: ErrorCollection::new(),
            environment,
            filename,
            source,
            current_return_type: None,
            current_type_params: Vec::new(),
            expected_hint: core::cell::RefCell::new(None),
            interface_index: core::cell::RefCell::new(None),
            module: Some(ModuleOrRef::Owned(module)),
            in_loop: false,
            external_depth: core::cell::RefCell::new(Vec::new()),
            tree_index: core::cell::RefCell::new(None),
        }
    }

    /// Creates a parser with a BORROWED module tree (without a clone)
    pub fn with_module_ref(filename: String, source: String, module: &'m Module) -> Self {
        Self {
            errors: ErrorCollection::new(),
            environment: Environment::new(),
            filename,
            source,
            current_return_type: None,
            current_type_params: Vec::new(),
            expected_hint: core::cell::RefCell::new(None),
            interface_index: core::cell::RefCell::new(None),
            module: Some(ModuleOrRef::Borrowed(module)),
            in_loop: false,
            external_depth: core::cell::RefCell::new(Vec::new()),
            tree_index: core::cell::RefCell::new(None),
        }
    }

    /// Resolve an import using the unified module-tree resolver
    fn resolve_import(
        &mut self,
        import_path: &str,
        item: &crate::ast::ImportItem,
        pos: &crate::Position,
    ) -> Result<(), ErrorReport> {
        let Some(root_module) = self.module.as_deref() else {
            return Err(self.create_error(
                ErrorType::TypeError,
                "Cannot resolve import without a module tree".to_string(),
                pos,
            ));
        };

        let mut resolver = ImportResolver::new(root_module);
        let resolved = resolver.resolve(import_path, item, pos)?;

        for import in resolved {
            self.define_imported(&import)?;
        }
        Ok(())
    }

    /// Define an imported symbol into the semantic environment
    fn define_imported(
        &mut self,
        import: &crate::resolver::ResolvedImport,
    ) -> Result<(), ErrorReport> {
        use crate::resolver::ResolvedImport;

        match import {
            ResolvedImport::Function {
                name,
                params,
                return_type,
                type_params,
                type_param_bounds,
                ..
            }
            | ResolvedImport::ForeignFunction {
                name,
                params,
                return_type,
                type_params,
                type_param_bounds,
                ..
            } => {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| self.get_type_from_expr_with_params(&p.param_type, type_params))
                    .collect();
                let return_type_ty = self.get_type_from_expr_with_params(return_type, type_params);

                if let Err((err, msg)) = self.environment.define_function(
                    name.clone(),
                    FunctionInfo {
                        param_types,
                        return_type: return_type_ty,
                        type_params: type_params.clone(),
                        type_param_bounds: type_param_bounds.clone(),
                    },
                ) {
                    return Err(self.create_error(err, msg, &pos_of_import(import)));
                }
                Ok(())
            }
            ResolvedImport::Type {
                name,
                fields,
                variants,
                type_params,
                methods,
                interface_methods,
                ..
            } => {
                if !interface_methods.is_empty() {
                    let mut method_map = HashMap::default();
                    for (mname, mparams, mret) in interface_methods {
                        let param_types: Vec<Type> = mparams
                            .iter()
                            .map(|p| {
                                self.get_type_from_expr_with_params(&p.param_type, type_params)
                            })
                            .collect();
                        let return_type_ty = self.get_type_from_expr_with_params(mret, type_params);
                        method_map.insert(
                            mname.clone(),
                            FunctionInfo {
                                param_types,
                                return_type: return_type_ty,
                                type_params: Vec::new(),
                                type_param_bounds: Vec::new(),
                            },
                        );
                    }
                    if self.environment.get_interface_type(name).is_none() {
                        if let Err((err, msg)) = self.environment.define_interface_type(
                            name.clone(),
                            InterfaceTypeInfo {
                                name: name.clone(),
                                methods: method_map,
                                type_params: type_params.clone(),
                            },
                        ) {
                            return Err(self.create_error(err, msg, &pos_of_import(import)));
                        }
                    }
                    return Ok(());
                }
                if !variants.is_empty() {
                    let variants_info: Vec<crate::types::EnumVariant> = variants
                        .iter()
                        .map(|v| crate::types::EnumVariant {
                            name: v.name.clone(),
                            payload_types: v
                                .payload_types
                                .iter()
                                .map(|t| self.get_type_from_expr_with_params(t, type_params))
                                .collect(),
                        })
                        .collect();
                    if let Err((err, msg)) = self.environment.define_enum_type(
                        name.clone(),
                        EnumTypeInfo {
                            name: name.clone(),
                            variants: variants_info,
                            methods: HashMap::default(),
                            type_params: type_params.clone(),
                        },
                    ) {
                        return Err(self.create_error(err, msg, &pos_of_import(import)));
                    }
                    return Ok(());
                }

                if let Err((err, msg)) = self.environment.define_struct_type(
                    name.clone(),
                    StructTypeInfo {
                        name: name.clone(),
                        fields: alloc::rc::Rc::new(HashMap::default()),
                        methods: HashMap::default(),
                        type_params: type_params.clone(),
                    },
                ) {
                    return Err(self.create_error(err, msg, &pos_of_import(import)));
                }
                let field_types: Vec<(String, Type)> = fields
                    .iter()
                    .map(|(fname, fty)| (fname.clone(), self.get_type_from_expr(fty)))
                    .collect();
                let method_infos: Vec<(String, FunctionInfo)> = methods
                    .iter()
                    .map(|(mname, mparams, mret)| {
                        let param_types: Vec<Type> = mparams
                            .iter()
                            .map(|p| {
                                self.get_type_from_expr_with_params(&p.param_type, type_params)
                            })
                            .collect();
                        let return_type_ty = self.get_type_from_expr_with_params(mret, type_params);
                        (
                            mname.clone(),
                            FunctionInfo {
                                param_types,
                                return_type: return_type_ty,
                                type_params: Vec::new(),
                                type_param_bounds: Vec::new(),
                            },
                        )
                    })
                    .collect();

                if let Some(cell) = self.environment.get_struct_type_mut(name) {
                    let struct_info = alloc::rc::Rc::make_mut(cell);
                    for (field, field_type) in field_types {
                        alloc::rc::Rc::make_mut(&mut struct_info.fields).insert(field, field_type);
                    }
                    for (method_name, info) in method_infos {
                        struct_info.methods.insert(method_name, info);
                    }
                }
                Ok(())
            }
        }
    }

    /// Predeclare a function into the current environment (for multi-file import resolution)
    pub fn predeclare_function(
        &mut self,
        name: String,
        param_types: Vec<Type>,
        return_type: Type,
    ) -> Result<(), ErrorReport> {
        let func_info = FunctionInfo {
            param_types,
            return_type,
            type_params: Vec::new(),
            type_param_bounds: Vec::new(),
        };

        if let Err((err, msg)) = self.environment.define_function(name, func_info) {
            return Err(self.create_error(
                err,
                msg,
                &crate::Position::new(self.filename.clone(), 1, 1, 0, 1),
            ));
        }
        Ok(())
    }

    /// Predeclare a variable and mark it as initialized
    pub fn predeclare_initialized_variable(
        &mut self,
        name: String,
        var_type: Type,
    ) -> Result<(), ErrorReport> {
        match self.environment.define_variable(name.clone(), var_type) {
            Ok(_) => {
                if let Err((err, msg)) = self.environment.set_variable_initialized(&name) {
                    return Err(self.create_error(
                        err,
                        msg,
                        &crate::Position::new(self.filename.clone(), 1, 1, 0, 1),
                    ));
                }
                Ok(())
            }
            Err((err, msg)) => Err(self.create_error(
                err,
                msg,
                &crate::Position::new(self.filename.clone(), 1, 1, 0, 1),
            )),
        }
    }

    /// Predeclare all types, functions, and methods to allow forward references
    fn predeclare_signatures(&mut self, statements: &[Statement]) {
        for stmt in statements {
            if let StatementKind::TypeDeclaration {
                name,
                type_params,
                type_param_bounds: _,
                fields,
            } = &stmt.kind
            {
                let mut field_map = HashMap::default();
                for field in fields {
                    let field_type =
                        self.get_type_from_expr_with_params(&field.field_type, type_params);
                    field_map.insert(field.name.clone(), field_type);
                }
                if self.environment.get_struct_type(name).is_none() {
                    let _ = self.environment.define_struct_type(
                        name.clone(),
                        StructTypeInfo {
                            name: name.clone(),
                            fields: alloc::rc::Rc::new(field_map),
                            methods: HashMap::default(),
                            type_params: type_params.clone(),
                        },
                    );
                }
            }
        }

        for stmt in statements {
            if let StatementKind::EnumDeclaration {
                name,
                type_params,
                type_param_bounds: _,
                variants,
            } = &stmt.kind
            {
                if self.environment.get_enum_type(name).is_none() {
                    let variants_info: Vec<crate::types::EnumVariant> = variants
                        .iter()
                        .map(|v| crate::types::EnumVariant {
                            name: v.name.clone(),
                            payload_types: v
                                .payload_types
                                .iter()
                                .map(|t| self.get_type_from_expr_with_params(t, type_params))
                                .collect(),
                        })
                        .collect();
                    if let Err((_, msg)) = self.environment.define_enum_type(
                        name.clone(),
                        EnumTypeInfo {
                            name: name.clone(),
                            variants: variants_info,
                            methods: HashMap::default(),
                            type_params: type_params.clone(),
                        },
                    ) {
                        panic!("define_enum_type failed: {msg}");
                    }
                }
            }
        }

        for stmt in statements {
            if let StatementKind::ExtDeclaration { type_name, .. } = &stmt.kind {
                if self.environment.get_struct_type(type_name).is_none()
                    && self.environment.get_enum_type(type_name).is_none()
                {
                    let _ = self.environment.define_struct_type(
                        type_name.clone(),
                        StructTypeInfo {
                            name: type_name.clone(),
                            fields: alloc::rc::Rc::new(HashMap::default()),
                            methods: HashMap::default(),
                            type_params: Vec::new(),
                        },
                    );
                }
            }
        }

        for stmt in statements {
            if let StatementKind::InterfaceDeclaration {
                name,
                type_params,
                type_param_bounds: _,
                methods,
            } = &stmt.kind
            {
                if self.environment.get_interface_type(name).is_none() {
                    let mut method_map = HashMap::default();
                    for m in methods {
                        let param_types: Vec<Type> = m
                            .params
                            .iter()
                            .map(|p| {
                                self.get_type_from_expr_with_params(&p.param_type, type_params)
                            })
                            .collect();
                        let return_type_ty =
                            self.get_type_from_expr_with_params(&m.return_type, type_params);
                        method_map.insert(
                            m.name.clone(),
                            FunctionInfo {
                                param_types,
                                return_type: return_type_ty,
                                type_params: Vec::new(),
                                type_param_bounds: Vec::new(),
                            },
                        );
                    }
                    let _ = self.environment.define_interface_type(
                        name.clone(),
                        InterfaceTypeInfo {
                            name: name.clone(),
                            methods: method_map,
                            type_params: type_params.clone(),
                        },
                    );
                }
            }
        }

        let mut ext_methods_to_insert: Vec<(String, String, FunctionInfo)> = Vec::new();
        for stmt in statements {
            if let StatementKind::ExtDeclaration {
                type_name,
                type_params: ext_type_params,
                methods,
                ..
            } = &stmt.kind
            {
                for method in methods {
                    if let StatementKind::FunctionDeclaration {
                        name,
                        params,
                        return_type,
                        ..
                    } = &method.kind
                    {
                        let param_types: Vec<Type> = params
                            .iter()
                            .map(|p| {
                                self.get_type_from_expr_with_params(&p.param_type, ext_type_params)
                            })
                            .collect();
                        let return_type_ty =
                            self.get_type_from_expr_with_params(return_type, ext_type_params);
                        ext_methods_to_insert.push((
                            type_name.clone(),
                            name.clone(),
                            FunctionInfo {
                                param_types,
                                return_type: return_type_ty,
                                type_params: Vec::new(),
                                type_param_bounds: Vec::new(),
                            },
                        ));
                    }
                }
            }
        }
        for (ty_name, mname, finfo) in ext_methods_to_insert {
            if let Some(cell) = self.environment.get_struct_type_mut(&ty_name) {
                alloc::rc::Rc::make_mut(cell).methods.insert(mname, finfo);
            } else if let Some(enum_info) = self.environment.get_enum_type_mut(&ty_name) {
                enum_info.methods.insert(mname, finfo);
            }
        }

        for stmt in statements {
            if let StatementKind::FunctionDeclaration {
                name,
                type_params,
                type_param_bounds,
                params,
                return_type,
                ..
            } = &stmt.kind
            {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| self.get_type_from_expr_with_params(&p.param_type, type_params))
                    .collect();
                let return_type_ty = self.get_type_from_expr_with_params(return_type, type_params);
                let _ = self.environment.define_function(
                    name.clone(),
                    FunctionInfo {
                        param_types,
                        return_type: return_type_ty,
                        type_params: type_params.clone(),
                        type_param_bounds: type_param_bounds.clone(),
                    },
                );
            }
        }

        for stmt in statements {
            if let StatementKind::VariableDeclaration { name, var_type, .. } = &stmt.kind {
                let ty = self.get_type_from_expr(var_type);
                if ty != Type::Void {
                    self.environment.predeclare_variable(name.clone(), ty);
                }
            }
        }
    }

    /// Extract a code snippet from the source at the given line
    fn extract_code_snippet(&self, line: usize) -> String {
        if line == 0 {
            return "".to_string();
        }

        let lines: Vec<&str> = self.source.lines().collect();
        if line > 0 && line <= lines.len() {
            lines[line - 1].to_string()
        } else {
            "".to_string()
        }
    }

    /// Create an error report with proper position information
    fn create_error(
        &self,
        error_type: ErrorType,
        message: String,
        pos: &crate::Position,
    ) -> ErrorReport {
        ErrorReport::with_file(
            error_type,
            message,
            self.filename.clone(),
            pos.line,
            pos.column,
            pos.offset,
            pos.length,
            self.extract_code_snippet(pos.line),
        )
    }

    /// Analyze a list of statements
    pub fn analyze(&mut self, statements: &[Statement]) -> Result<(), ErrorCollection> {
        self.predeclare_signatures(statements);
        for statement in statements {
            if let Err(error) = self.analyze_statement(statement) {
                self.errors.add_error(error);
            }
        }

        if self.errors.has_errors() {
            Err(self.errors.clone())
        } else {
            Ok(())
        }
    }

    /// Analyze a single statement
    fn analyze_statement(&mut self, statement: &Statement) -> Result<(), ErrorReport> {
        match &statement.kind {
            StatementKind::VariableDeclaration {
                name,
                var_type,
                initializer,
            } => {
                if let Err((err, msg)) = Self::check_no_explicit_any(var_type) {
                    return Err(self.create_error(err, msg, &statement.pos));
                }
                let is_inferred = matches!(var_type, crate::ast::TypeExpr::Inferred);
                let parsed_type = if is_inferred {
                    match self.get_expression_type_with_environment(initializer, &self.environment)
                    {
                        Ok(t) => t,
                        Err((error_type, message, pos)) => {
                            return Err(self.create_error(error_type, message, &pos));
                        }
                    }
                } else {
                    self.get_type_from_expr(var_type)
                };
                if parsed_type == Type::Void {
                    return Err(self.create_error(
                        ErrorType::SyntaxError,
                        if is_inferred {
                            "Cannot infer the variable type: the initializer is 'void'".to_string()
                        } else {
                            "Variable of type 'void' is not allowed".to_string()
                        },
                        &statement.pos,
                    ));
                }

                let predeclared = if self.environment.is_defined_in_current_scope(name) {
                    let info = self.environment.get_variable(name).unwrap();
                    if info.is_initialized {
                        return Err(self.create_error(
                            ErrorType::VariableAlreadyDefined(name.clone()),
                            format!("Variable '{name}' is already defined in this scope"),
                            &statement.pos,
                        ));
                    }
                    info.var_type != parsed_type
                } else {
                    if let Err((error_type, message)) = self
                        .environment
                        .define_variable(name.clone(), parsed_type.clone())
                    {
                        return Err(self.create_error(error_type, message, &statement.pos));
                    }
                    false
                };
                if predeclared {
                    if let Err((error_type, message)) = self
                        .environment
                        .update_variable_type(name, parsed_type.clone())
                    {
                        return Err(self.create_error(error_type, message, &statement.pos));
                    }
                }

                let initializer_type = match self.expression_type_with_hint(
                    initializer,
                    &self.environment,
                    Some(parsed_type.clone()),
                ) {
                    Ok(t) => t,
                    Err((error_type, message, pos)) => {
                        return Err(self.create_error(error_type, message, &pos));
                    }
                };

                let empty_array_ok = matches!((&initializer_type, &parsed_type),
                    (Type::Array { element_type, len: 0 },
                     Type::Array { element_type: expected_el, len: 0 })
                        if matches!(**element_type, Type::Any) && !matches!(**expected_el, Type::Any));
                let iface_ok = if let Type::Interface {
                    name: iface_name, ..
                } = &parsed_type
                {
                    if let Some(iface_info) = self.environment.get_interface_type(iface_name) {
                        self.type_implements_interface(&initializer_type, &iface_info)
                    } else {
                        false
                    }
                } else {
                    false
                };
                let array_iface_ok = if let (
                    Type::Array {
                        element_type: expected_el,
                        len: elen,
                    },
                    Type::Array {
                        element_type: init_el,
                        len: ilen,
                    },
                ) = (&parsed_type, &initializer_type)
                {
                    (elen == ilen || *elen == 0)
                        && if let Type::Interface {
                            name: iface_name, ..
                        } = &**expected_el
                        {
                            if let Some(iface_info) =
                                self.environment.get_interface_type(iface_name)
                            {
                                self.type_implements_interface(init_el, &iface_info)
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                } else {
                    false
                };
                let open_array_ok = matches!(
                    (&parsed_type, &initializer_type),
                    (Type::Array { element_type: e1, len: 0 }, Type::Array { element_type: e2, .. })
                        if e1 == e2
                );
                if !iface_ok
                    && !array_iface_ok
                    && !open_array_ok
                    && !self.types_assignable(&parsed_type, &initializer_type)
                    && !Self::is_open_type(&parsed_type)
                    && !Self::is_open_type(&initializer_type)
                    && !empty_array_ok
                {
                    return Err(self.create_error(
                        ErrorType::TypeMismatch {
                            expected: format!("{parsed_type}"),
                            found: format!("{initializer_type}"),
                        },
                        format!("Type mismatch in variable declaration for '{name}'"),
                        &initializer.pos,
                    ));
                }

                match self.environment.set_variable_initialized(name) {
                    Ok(_) => {}
                    Err((error_type, message)) => {
                        return Err(self.create_error(error_type, message, &statement.pos));
                    }
                }
                Ok(())
            }
            StatementKind::Assignment { target, value } => match &target.kind {
                ExpressionKind::Variable(name) => {
                    if self.environment.get_variable(name).is_none() {
                        return Err(self.create_error(
                            ErrorType::UndefinedVariable(name.clone()),
                            format!("Cannot assign to undefined variable '{name}'"),
                            &statement.pos,
                        ));
                    }
                    let value_type =
                        match self.get_expression_type_with_environment(value, &self.environment) {
                            Ok(t) => t,
                            Err((error_type, message, pos)) => {
                                return Err(self.create_error(error_type, message, &pos));
                            }
                        };
                    let var_info = self.environment.get_variable(name).unwrap();
                    if value_type != var_info.var_type {
                        return Err(self.create_error(
                            ErrorType::TypeMismatch {
                                expected: format!("{}", var_info.var_type),
                                found: format!("{value_type}"),
                            },
                            format!("Type mismatch in assignment to variable '{name}'"),
                            &value.pos,
                        ));
                    }
                    Ok(())
                }
                ExpressionKind::MemberAccess { object, member } => {
                    let obj_type = match self
                        .get_expression_type_with_environment(object, &self.environment)
                    {
                        Ok(t) => t,
                        Err((error_type, message, pos)) => {
                            return Err(self.create_error(error_type, message, &pos));
                        }
                    };
                    if let Type::Struct { name, fields, .. } = obj_type {
                        let field_ty = if let Some(ft) = fields.get(member) {
                            ft.clone()
                        } else if let Some(struct_info) = self.environment.get_struct_type(&name) {
                            if let Some(ft) = struct_info.fields.get(member) {
                                ft.clone()
                            } else {
                                return Err(self.create_error(
                                    ErrorType::TypeError,
                                    format!("Struct '{name}' has no field '{member}'"),
                                    &target.pos,
                                ));
                            }
                        } else {
                            return Err(self.create_error(
                                ErrorType::TypeError,
                                format!("Unknown struct type '{name}'"),
                                &target.pos,
                            ));
                        };
                        let value_type = match self
                            .get_expression_type_with_environment(value, &self.environment)
                        {
                            Ok(t) => t,
                            Err((error_type, message, pos)) => {
                                return Err(self.create_error(error_type, message, &pos));
                            }
                        };
                        if value_type != field_ty {
                            return Err(self.create_error(
                                ErrorType::TypeMismatch {
                                    expected: field_ty.to_string(),
                                    found: value_type.to_string(),
                                },
                                format!("Type mismatch in assignment to field '{name}.{member}'"),
                                &value.pos,
                            ));
                        }
                        Ok(())
                    } else {
                        Err(self.create_error(
                            ErrorType::TypeError,
                            "Cannot assign to member of non-struct".to_string(),
                            &target.pos,
                        ))
                    }
                }
                ExpressionKind::Index { object, index } => {
                    let object_type = match self
                        .get_expression_type_with_environment(object, &self.environment)
                    {
                        Ok(t) => t,
                        Err((error_type, message, pos)) => {
                            return Err(self.create_error(error_type, message, &pos));
                        }
                    };
                    let idx_type =
                        match self.get_expression_type_with_environment(index, &self.environment) {
                            Ok(t) => t,
                            Err((error_type, message, pos)) => {
                                return Err(self.create_error(error_type, message, &pos));
                            }
                        };
                    if idx_type != Type::Int {
                        return Err(self.create_error(
                            ErrorType::TypeMismatch {
                                expected: "int".to_string(),
                                found: idx_type.to_string(),
                            },
                            "Array index must be of type int".to_string(),
                            &index.pos,
                        ));
                    }
                    let element_type = match object_type {
                        Type::Array { element_type, .. } => *element_type,
                        _ => {
                            return Err(self.create_error(
                                ErrorType::TypeError,
                                format!("Cannot index value of type '{object_type}'"),
                                &target.pos,
                            ));
                        }
                    };
                    let value_type =
                        match self.get_expression_type_with_environment(value, &self.environment) {
                            Ok(t) => t,
                            Err((error_type, message, pos)) => {
                                return Err(self.create_error(error_type, message, &pos));
                            }
                        };
                    if value_type != element_type {
                        return Err(self.create_error(
                            ErrorType::TypeMismatch {
                                expected: element_type.to_string(),
                                found: value_type.to_string(),
                            },
                            "Mismatched type in array assignment".to_string(),
                            &value.pos,
                        ));
                    }
                    Ok(())
                }
                _ => Err(self.create_error(
                    ErrorType::SyntaxError,
                    "Invalid assignment target".to_string(),
                    &target.pos,
                )),
            },
            StatementKind::IfStatement {
                condition,
                then_branch,
                elif_branches,
                else_branch,
            } => {
                let condition_type =
                    match self.get_expression_type_with_environment(condition, &self.environment) {
                        Ok(t) => t,
                        Err((error_type, message, pos)) => {
                            return Err(self.create_error(error_type, message, &pos));
                        }
                    };

                if condition_type != Type::Bool {
                    return Err(self.create_error(
                        ErrorType::TypeMismatch {
                            expected: "bool".to_string(),
                            found: format!("{condition_type}"),
                        },
                        "If condition must be of type bool".to_string(),
                        &condition.pos,
                    ));
                }

                self.environment.enter_scope();
                for stmt in then_branch {
                    match self.analyze_statement(stmt) {
                        Ok(_) => {}
                        Err(error) => {
                            self.errors.add_error(error);
                        }
                    }
                }
                self.environment.exit_scope();

                for (elif_condition, elif_branch) in elif_branches {
                    let elif_condition_type = {
                        match self
                            .get_expression_type_with_environment(elif_condition, &self.environment)
                        {
                            Ok(t) => t,
                            Err((error_type, message, pos)) => {
                                return Err(self.create_error(error_type, message, &pos));
                            }
                        }
                    };
                    if elif_condition_type != Type::Bool {
                        return Err(self.create_error(
                            ErrorType::TypeMismatch {
                                expected: "bool".to_string(),
                                found: format!("{elif_condition_type}"),
                            },
                            "Elif condition must be of type bool".to_string(),
                            &elif_condition.pos,
                        ));
                    }

                    self.environment.enter_scope();
                    for stmt in elif_branch {
                        match self.analyze_statement(stmt) {
                            Ok(_) => {}
                            Err(error) => {
                                self.errors.add_error(error);
                            }
                        }
                    }
                    self.environment.exit_scope();
                }

                if let Some(else_branch) = else_branch {
                    self.environment.enter_scope();
                    for stmt in else_branch {
                        match self.analyze_statement(stmt) {
                            Ok(_) => {}
                            Err(error) => {
                                self.errors.add_error(error);
                            }
                        }
                    }
                    self.environment.exit_scope();
                }

                Ok(())
            }
            StatementKind::WhileStatement { condition, body } => {
                let condition_type =
                    match self.get_expression_type_with_environment(condition, &self.environment) {
                        Ok(t) => t,
                        Err((error_type, message, pos)) => {
                            return Err(self.create_error(error_type, message, &pos));
                        }
                    };

                if condition_type != Type::Bool {
                    return Err(self.create_error(
                        ErrorType::TypeMismatch {
                            expected: "bool".to_string(),
                            found: format!("{condition_type}"),
                        },
                        "While loop condition must be of type bool".to_string(),
                        &condition.pos,
                    ));
                }

                self.environment.enter_scope();
                let prev_in_loop = self.in_loop;
                self.in_loop = true;
                for stmt in body {
                    if let Err(error) = self.analyze_statement(stmt) {
                        self.errors.add_error(error);
                    }
                }
                self.in_loop = prev_in_loop;
                self.environment.exit_scope();

                Ok(())
            }
            StatementKind::Break => {
                if !self.in_loop {
                    return Err(self.create_error(
                        ErrorType::SyntaxError,
                        "`break` outside of a loop".to_string(),
                        &statement.pos,
                    ));
                }
                Ok(())
            }
            StatementKind::Block(statements) => {
                self.environment.enter_scope();
                for stmt in statements {
                    match self.analyze_statement(stmt) {
                        Ok(_) => {}
                        Err(error) => {
                            self.errors.add_error(error);
                        }
                    }
                }
                self.environment.exit_scope();
                Ok(())
            }
            StatementKind::FunctionDeclaration {
                name: _,
                type_params,
                type_param_bounds,
                params,
                return_type,
                body,
            } => {
                for param in params {
                    if let Err((err, msg)) = Self::check_no_explicit_any(&param.param_type) {
                        return Err(self.create_error(err, msg, &param.pos));
                    }
                }
                if let Err((err, msg)) = Self::check_no_explicit_any(return_type) {
                    return Err(self.create_error(err, msg, &statement.pos));
                }
                let return_type_ty = self.get_type_from_expr_with_bounds(
                    return_type,
                    type_params,
                    type_param_bounds,
                );

                self.environment.enter_scope();

                for param in params {
                    if let Err((err, msg)) = self.environment.define_variable(
                        param.name.clone(),
                        self.get_type_from_expr_with_bounds(
                            &param.param_type,
                            type_params,
                            type_param_bounds,
                        ),
                    ) {
                        return Err(self.create_error(err, msg, &param.pos));
                    }
                    if let Err((err, msg)) = self.environment.set_variable_initialized(&param.name)
                    {
                        return Err(self.create_error(err, msg, &param.pos));
                    }
                }
                for (param, bound) in type_params.iter().zip(type_param_bounds.iter()) {
                    if let Some(bound_name) = bound {
                        if let Some(iface_info) = self.environment.get_interface_type(bound_name) {
                            let methods = iface_info
                                .methods
                                .iter()
                                .map(|(mname, finfo)| {
                                    (
                                        mname.clone(),
                                        crate::types::InterfaceMethod {
                                            name: mname.clone(),
                                            param_types: finfo.param_types.clone(),
                                            return_type: finfo.return_type.clone(),
                                        },
                                    )
                                })
                                .collect();
                            let _ = self.environment.define_variable(
                                param.clone(),
                                Type::Interface {
                                    name: iface_info.name.clone(),
                                    methods,
                                    type_params: Vec::new(),
                                },
                            );
                            let _ = self.environment.set_variable_initialized(param);
                        }
                    }
                }

                let prev_return_type = self.current_return_type.take();
                self.current_return_type = Some(return_type_ty);
                let prev_type_params =
                    core::mem::replace(&mut self.current_type_params, type_params.clone());

                for stmt in body {
                    if let Err(e) = self.analyze_statement(stmt) {
                        self.errors.add_error(e);
                    }
                }

                self.environment.exit_scope();
                self.current_return_type = prev_return_type;
                self.current_type_params = prev_type_params;

                Ok(())
            }
            StatementKind::Return { value } => {
                let ret_type = match value {
                    Some(expr) => {
                        let expected = self.current_return_type.clone();
                        match self.expression_type_with_hint(expr, &self.environment, expected) {
                            Ok(t) => t,
                            Err((err, msg, pos)) => return Err(self.create_error(err, msg, &pos)),
                        }
                    }
                    None => Type::Void,
                };

                match &self.current_return_type {
                    Some(expected_type) => {
                        if &ret_type != expected_type
                            && !Self::is_open_type(expected_type)
                            && !Self::is_open_type(&ret_type)
                        {
                            return Err(self.create_error(
                                ErrorType::TypeMismatch {
                                    expected: expected_type.to_string(),
                                    found: ret_type.to_string(),
                                },
                                "Mismatched return type".to_string(),
                                &statement.pos,
                            ));
                        }
                    }
                    None => {
                        return Err(self.create_error(
                            ErrorType::SyntaxError,
                            "Return statement outside of function".to_string(),
                            &statement.pos,
                        ));
                    }
                }
                Ok(())
            }
            StatementKind::ExpressionStatement(expr) => {
                if let Err((err, msg, pos)) =
                    self.get_expression_type_with_environment(expr, &self.environment)
                {
                    return Err(self.create_error(err, msg, &pos));
                }
                Ok(())
            }
            StatementKind::TypeDeclaration {
                name,
                type_params,
                type_param_bounds: _,
                fields,
            } => {
                let mut field_map = HashMap::default();
                for field in fields {
                    let field_type =
                        self.get_type_from_expr_with_params(&field.field_type, type_params);
                    field_map.insert(field.name.clone(), field_type);
                }
                if let Some(existing) = self.environment.get_struct_type_mut(name) {
                    alloc::rc::Rc::make_mut(existing).fields = alloc::rc::Rc::new(field_map);
                    Ok(())
                } else {
                    let struct_info = StructTypeInfo {
                        name: name.clone(),
                        fields: alloc::rc::Rc::new(field_map),
                        methods: HashMap::default(),
                        type_params: type_params.clone(),
                    };
                    if let Err((err, msg)) = self
                        .environment
                        .define_struct_type(name.clone(), struct_info)
                    {
                        return Err(self.create_error(err, msg, &statement.pos));
                    }
                    Ok(())
                }
            }
            StatementKind::EnumDeclaration {
                name,
                type_params,
                type_param_bounds: _,
                variants,
            } => {
                let mut variant_infos: Vec<crate::types::EnumVariant> = Vec::new();
                for variant in variants {
                    let payload_types: Vec<Type> = variant
                        .payload_types
                        .iter()
                        .map(|t| self.get_type_from_expr_with_params(t, type_params))
                        .collect();
                    if variant_infos.iter().any(|v| v.name == variant.name) {
                        return Err(self.create_error(
                            ErrorType::VariableAlreadyDefined(variant.name.clone()),
                            format!(
                                "Variant '{}' is already defined in enum '{}'",
                                variant.name, name
                            ),
                            &variant.pos,
                        ));
                    }
                    variant_infos.push(crate::types::EnumVariant {
                        name: variant.name.clone(),
                        payload_types,
                    });
                }
                let enum_info = EnumTypeInfo {
                    name: name.clone(),
                    variants: variant_infos,
                    methods: HashMap::default(),
                    type_params: type_params.clone(),
                };
                if let Some(existing_mut) = self.environment.get_enum_type_mut(name) {
                    existing_mut.variants = enum_info.variants;
                    Ok(())
                } else if let Err((err, msg)) =
                    self.environment.define_enum_type(name.clone(), enum_info)
                {
                    Err(self.create_error(err, msg, &statement.pos))
                } else {
                    Ok(())
                }
            }
            StatementKind::InterfaceDeclaration {
                name,
                type_params,
                type_param_bounds: _,
                methods,
            } => {
                let mut method_map = HashMap::default();
                let mut seen = Vec::new();
                for m in methods {
                    if seen.contains(&m.name) {
                        return Err(self.create_error(
                            ErrorType::VariableAlreadyDefined(m.name.clone()),
                            format!(
                                "Method '{}' is already defined in interface '{}'",
                                m.name, name
                            ),
                            &m.pos,
                        ));
                    }
                    seen.push(m.name.clone());
                    let param_types: Vec<Type> = m
                        .params
                        .iter()
                        .map(|p| self.get_type_from_expr_with_params(&p.param_type, type_params))
                        .collect();
                    let return_type_ty =
                        self.get_type_from_expr_with_params(&m.return_type, type_params);
                    method_map.insert(
                        m.name.clone(),
                        FunctionInfo {
                            param_types,
                            return_type: return_type_ty,
                            type_params: Vec::new(),
                            type_param_bounds: Vec::new(),
                        },
                    );
                }
                if self.environment.get_interface_type(name).is_none() {
                    if let Err((err, msg)) = self.environment.define_interface_type(
                        name.clone(),
                        InterfaceTypeInfo {
                            name: name.clone(),
                            methods: method_map,
                            type_params: type_params.clone(),
                        },
                    ) {
                        return Err(self.create_error(err, msg, &statement.pos));
                    }
                }
                Ok(())
            }
            StatementKind::ExtDeclaration {
                type_name,
                type_params,
                interfaces,
                interface_args,
                methods,
            } => {
                for method in methods {
                    match &method.kind {
                        StatementKind::FunctionDeclaration {
                            name: _,
                            type_params: _,
                            type_param_bounds: _,
                            params,
                            return_type,
                            body,
                        } => {
                            let return_type_ty =
                                self.get_type_from_expr_with_params(return_type, type_params);

                            self.environment.enter_scope();

                            for param in params {
                                if let Err((err, msg)) = self.environment.define_variable(
                                    param.name.clone(),
                                    self.get_type_from_expr_with_params(
                                        &param.param_type,
                                        type_params,
                                    ),
                                ) {
                                    return Err(self.create_error(err, msg, &param.pos));
                                }
                                if let Err((err, msg)) =
                                    self.environment.set_variable_initialized(&param.name)
                                {
                                    return Err(self.create_error(err, msg, &param.pos));
                                }
                            }

                            let prev_return_type = self.current_return_type.take();
                            self.current_return_type = Some(return_type_ty.clone());
                            let prev_type_params = core::mem::replace(
                                &mut self.current_type_params,
                                type_params.clone(),
                            );

                            for stmt in body {
                                if let Err(e) = self.analyze_statement(stmt) {
                                    self.errors.add_error(e);
                                }
                            }

                            self.environment.exit_scope();
                            self.current_return_type = prev_return_type;
                            self.current_type_params = prev_type_params;
                        }
                        StatementKind::ForeignFunctionDeclaration {
                            name,
                            type_params: _,
                            type_param_bounds: _,
                            params,
                            return_type,
                        } => {
                            let type_name = type_name.clone();
                            let param_types: Vec<Type> = params
                                .iter()
                                .map(|p| {
                                    self.get_type_from_expr_with_params(&p.param_type, type_params)
                                })
                                .collect();
                            let return_type_ty =
                                self.get_type_from_expr_with_params(return_type, type_params);
                            if let Some(cell) = self.environment.get_struct_type_mut(&type_name) {
                                alloc::rc::Rc::make_mut(cell).methods.insert(
                                    name.clone(),
                                    FunctionInfo {
                                        param_types,
                                        return_type: return_type_ty,
                                        type_params: Vec::new(),
                                        type_param_bounds: Vec::new(),
                                    },
                                );
                            }
                        }
                        _ => {}
                    }
                }
                for (iface_idx, iface_name) in interfaces.iter().enumerate() {
                    let Some(iface_info) = self.environment.get_interface_type(iface_name) else {
                        return Err(self.create_error(
                            ErrorType::TypeError,
                            format!("Unknown interface '{iface_name}' in ext declaration"),
                            &statement.pos,
                        ));
                    };
                    let iface_arg_exprs =
                        interface_args.get(iface_idx).cloned().unwrap_or_default();
                    let mut iface_subst: HashMap<String, Type> = HashMap::default();
                    for (param, arg_expr) in
                        iface_info.type_params.iter().zip(iface_arg_exprs.iter())
                    {
                        let arg_ty = self.get_type_from_expr_with_params(arg_expr, type_params);
                        iface_subst.insert(param.clone(), arg_ty);
                    }
                    let type_ty =
                        self.get_type_from_expr(&crate::ast::TypeExpr::Named(type_name.clone()));
                    for (mname, mfinfo) in &iface_info.methods {
                        let expected_params: Vec<Type> = mfinfo
                            .param_types
                            .iter()
                            .map(|t| t.substitute_type_vars(&iface_subst))
                            .collect();
                        let expected_ret = mfinfo.return_type.substitute_type_vars(&iface_subst);
                        let implemented =
                            self.method_exists_on(&type_ty, mname, &expected_params, &expected_ret);
                        if !implemented {
                            return Err(self.create_error(
                                ErrorType::TypeError,
                                format!(
                                    "Type '{}' does not implement method '{}' of interface '{}' (expected ({}) -> {})",
                                    type_name,
                                    mname,
                                    iface_name,
                                    expected_params.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "),
                                    expected_ret
                                ),
                                &statement.pos,
                            ));
                        }
                    }
                }
                Ok(())
            }
            StatementKind::ImportStatement(import_stmt) => {
                self.resolve_import(&import_stmt.library_path, &import_stmt.item, &statement.pos)?;
                Ok(())
            }
            StatementKind::ForeignFunctionDeclaration {
                name,
                type_params: _,
                type_param_bounds: _,
                params,
                return_type,
            } => {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| self.get_type_from_expr(&p.param_type))
                    .collect();
                let return_type_ty = self.get_type_from_expr(return_type);

                if let Err((err, msg)) = self.environment.define_function(
                    name.clone(),
                    FunctionInfo {
                        param_types,
                        return_type: return_type_ty,
                        type_params: Vec::new(),
                        type_param_bounds: Vec::new(),
                    },
                ) {
                    return Err(self.create_error(err, msg, &statement.pos));
                }
                Ok(())
            }
            StatementKind::MatchStatement { scrutinee, arms } => {
                self.analyze_match_statement(scrutinee, arms, &statement.pos)
            }
        }
    }

    /// Analyze a match statement: scrutinee type, per-arm pattern checks,
    /// binding declarations, exhaustiveness and unreachable arms
    fn analyze_match_statement(
        &mut self,
        scrutinee: &Expression,
        arms: &[crate::ast::MatchArm],
        pos: &crate::Position,
    ) -> Result<(), ErrorReport> {
        let scrutinee_type = match self
            .get_expression_type_with_environment(scrutinee, &self.environment)
        {
            Ok(t) => t,
            Err((error_type, message, p)) => return Err(self.create_error(error_type, message, &p)),
        };

        let supported = matches!(
            scrutinee_type,
            Type::Enum { .. } | Type::Int | Type::Double | Type::String | Type::Bool
        );
        if !supported {
            return Err(self.create_error(
                ErrorType::TypeError,
                format!("match supports enum and primitive types, got {scrutinee_type}"),
                pos,
            ));
        }

        let mut covered_variants: Vec<String> = Vec::new();
        let mut has_wildcard = false;
        let mut exhaustive = false;
        let mut covered_bool_true = false;
        let mut covered_bool_false = false;

        for arm in arms {
            if exhaustive {
                return Err(self.create_error(
                    ErrorType::SyntaxError,
                    "unreachable pattern".to_string(),
                    &arm.pos,
                ));
            }

            let bindings = self.check_match_pattern(&arm.pattern, &scrutinee_type, &arm.pos)?;

            let (new_covered, new_wildcard) = pattern_coverage(&arm.pattern);
            for v in new_covered {
                if !covered_variants.contains(&v) {
                    covered_variants.push(v);
                }
            }
            has_wildcard = has_wildcard || new_wildcard;
            let (new_true, new_false) = pattern_bool_coverage(&arm.pattern);
            covered_bool_true = covered_bool_true || new_true;
            covered_bool_false = covered_bool_false || new_false;

            exhaustive = if let Type::Enum { variants, .. } = &scrutinee_type {
                has_wildcard || variants.iter().all(|v| covered_variants.contains(&v.name))
            } else if scrutinee_type == Type::Bool {
                has_wildcard || (covered_bool_true && covered_bool_false)
            } else {
                has_wildcard
            };

            self.environment.enter_scope();
            for (name, _payload_index, var_type) in bindings {
                if let Err((err, msg)) = self.environment.define_variable(name, var_type) {
                    self.errors.add_error(self.create_error(err, msg, &arm.pos));
                }
            }
            for stmt in &arm.body {
                if let Err(error) = self.analyze_statement(stmt) {
                    self.errors.add_error(error);
                }
            }
            self.environment.exit_scope();
        }

        if !exhaustive {
            if let Type::Enum { variants, .. } = &scrutinee_type {
                let missing: Vec<String> = variants
                    .iter()
                    .filter(|v| !covered_variants.contains(&v.name))
                    .map(|v| v.name.clone())
                    .collect();
                return Err(self.create_error(
                    ErrorType::SyntaxError,
                    format!(
                        "non-exhaustive match: missing variants {}",
                        missing.join(", ")
                    ),
                    pos,
                ));
            } else {
                return Err(self.create_error(
                    ErrorType::SyntaxError,
                    format!("non-exhaustive match over {scrutinee_type}: add a `_` arm"),
                    pos,
                ));
            }
        }

        Ok(())
    }

    /// Check one match pattern against the scrutinee type; returns the
    /// bindings it introduces: (name, payload position, type)
    fn check_match_pattern(
        &self,
        pattern: &crate::ast::Pattern,
        scrutinee_type: &Type,
        pos: &crate::Position,
    ) -> Result<Vec<(String, usize, Type)>, ErrorReport> {
        use crate::ast::Pattern as P;

        match pattern {
            P::Wildcard => Ok(Vec::new()),
            P::Literal(lit) => {
                let lit_type = pattern_literal_type(lit);
                if scrutinee_type != &lit_type {
                    return Err(self.create_error(
                        ErrorType::TypeMismatch {
                            expected: scrutinee_type.to_string(),
                            found: lit_type.to_string(),
                        },
                        format!(
                            "literal pattern '{lit}' cannot match value of type {scrutinee_type}"
                        ),
                        pos,
                    ));
                }
                Ok(Vec::new())
            }
            P::Binding(_) => Err(self.create_error(
                ErrorType::SyntaxError,
                "binding is only allowed inside variant arguments".to_string(),
                pos,
            )),
            P::EnumVariant {
                enum_name,
                variant,
                args,
            } => {
                let (enum_type_name, variants) = match scrutinee_type {
                    Type::Enum { name, variants, .. } => (name.clone(), variants),
                    other => {
                        return Err(self.create_error(
                            ErrorType::TypeError,
                            format!(
                                "variant pattern '{variant}' cannot match value of type {other}"
                            ),
                            pos,
                        ));
                    }
                };
                if let Some(en) = enum_name {
                    if *en != enum_type_name {
                        return Err(self.create_error(
                            ErrorType::TypeError,
                            format!(
                                "pattern enum '{en}' does not match scrutinee of type '{enum_type_name}'"),
                            pos,
                        ));
                    }
                }
                let variant_info = match variants.iter().find(|v| &v.name == variant) {
                    Some(v) => v,
                    None => {
                        return Err(self.create_error(
                            ErrorType::TypeError,
                            format!("enum '{enum_type_name}' has no variant '{variant}'"),
                            pos,
                        ));
                    }
                };
                if args.len() != variant_info.payload_types.len() {
                    return Err(self.create_error(
                        ErrorType::TypeMismatch {
                            expected: variant_info.payload_types.len().to_string(),
                            found: args.len().to_string(),
                        },
                        format!(
                            "variant '{}' of enum '{}' expects {} payload argument(s), but got {}",
                            variant,
                            enum_type_name,
                            variant_info.payload_types.len(),
                            args.len()
                        ),
                        pos,
                    ));
                }
                let mut bindings: Vec<(String, usize, Type)> = Vec::new();
                for (arg_index, (arg, expected)) in args
                    .iter()
                    .zip(variant_info.payload_types.iter())
                    .enumerate()
                {
                    match arg {
                        P::Wildcard => {}
                        P::Binding(name) => {
                            if bindings.iter().any(|(n, _, _)| n == name) {
                                return Err(self.create_error(
                                    ErrorType::VariableAlreadyDefined(name.clone()),
                                    format!("duplicate binding '{name}' in pattern"),
                                    pos,
                                ));
                            }
                            bindings.push((name.clone(), arg_index, expected.clone()));
                        }
                        P::Literal(lit) => {
                            let lit_type = pattern_literal_type(lit);
                            let compatible = lit_type == *expected
                                || matches!(expected, Type::Any | Type::TypeVar { .. });
                            if !compatible {
                                return Err(self.create_error(
                                    ErrorType::TypeMismatch {
                                        expected: expected.to_string(),
                                        found: lit_type.to_string(),
                                    },
                                    format!(
                                        "literal pattern '{lit}' cannot match payload of type {expected}"),
                                    pos,
                                ));
                            }
                        }
                        P::EnumVariant { .. } | P::Or(_) => {
                            return Err(self.create_error(
                                ErrorType::TypeError,
                                "nested enum patterns are not supported yet".to_string(),
                                pos,
                            ));
                        }
                    }
                }
                Ok(bindings)
            }
            P::Or(alts) => {
                let mut result: Option<Vec<(String, usize, Type)>> = None;
                for alt in alts {
                    let b = self.check_match_pattern(alt, scrutinee_type, pos)?;
                    match &result {
                        None => result = Some(b),
                        Some(prev) => {
                            let same = prev.len() == b.len()
                                && prev.iter().zip(b.iter()).all(|(x, y)| x == y);
                            if !same {
                                return Err(self.create_error(
                                    ErrorType::TypeError,
                                    "alternatives in `|` must bind the same variables at the same positions".to_string(),
                                    pos,
                                ));
                            }
                        }
                    }
                }
                Ok(result.unwrap_or_default())
            }
        }
    }

    /// Resolve a type expression to a concrete `Type`, looking up struct/enum
    /// types in the environment. Unknown names (generic parameters, types from
    /// other modules) resolve to `Type::Any`
    fn get_type_from_expr(&self, expr: &crate::ast::TypeExpr) -> Type {
        match expr {
            crate::ast::TypeExpr::Named(name) => {
                if let Some(iface_info) = self.environment.get_interface_type(name) {
                    let methods = iface_info
                        .methods
                        .iter()
                        .map(|(mname, finfo)| {
                            (
                                mname.clone(),
                                crate::types::InterfaceMethod {
                                    name: mname.clone(),
                                    param_types: finfo.param_types.clone(),
                                    return_type: finfo.return_type.clone(),
                                },
                            )
                        })
                        .collect();
                    return Type::Interface {
                        name: iface_info.name.clone(),
                        methods,
                        type_params: Vec::new(),
                    };
                }
                let base_type = Type::from(name.as_str());
                if !matches!(base_type, Type::Struct { .. }) {
                    if let Some(enum_info) = self.environment.get_enum_type(name) {
                        return Type::Enum {
                            name: enum_info.name.clone(),
                            variants: enum_info.variants.clone(),
                            type_args: Vec::new(),
                        };
                    }
                    return base_type;
                }
                if let Some(struct_info) = self.environment.get_struct_type(name) {
                    Type::Struct {
                        name: struct_info.name.clone(),
                        fields: struct_info.fields.clone(),
                        type_args: Vec::new(),
                    }
                } else if let Some(enum_info) = self.environment.get_enum_type(name) {
                    Type::Enum {
                        name: enum_info.name.clone(),
                        variants: enum_info.variants.clone(),
                        type_args: Vec::new(),
                    }
                } else if let Some(struct_info) = self.tree_struct_info(name) {
                    Type::Struct {
                        name: struct_info.name.clone(),
                        fields: struct_info.fields.clone(),
                        type_args: Vec::new(),
                    }
                } else if let Some(iface_info) = self.tree_interface_info(name) {
                    let methods = iface_info
                        .methods
                        .iter()
                        .map(|(mname, finfo)| {
                            (
                                mname.clone(),
                                crate::types::InterfaceMethod {
                                    name: mname.clone(),
                                    param_types: finfo.param_types.clone(),
                                    return_type: finfo.return_type.clone(),
                                },
                            )
                        })
                        .collect();
                    Type::Interface {
                        name: iface_info.name,
                        methods,
                        type_params: iface_info.type_params,
                    }
                } else {
                    Type::Any
                }
            }
            crate::ast::TypeExpr::Array { element_type, len } => Type::Array {
                element_type: Box::new(self.get_type_from_expr(element_type)),
                len: *len,
            },
            crate::ast::TypeExpr::Generic { name, args } => {
                let base = self.get_type_from_expr(&crate::ast::TypeExpr::Named(name.clone()));
                let type_args: Vec<Type> =
                    args.iter().map(|a| self.get_type_from_expr(a)).collect();
                self.apply_type_args(base, &type_args)
            }
            crate::ast::TypeExpr::Function { params, ret } => Type::FunctionType {
                param_types: params.iter().map(|p| self.get_type_from_expr(p)).collect(),
                return_type: Box::new(self.get_type_from_expr(ret)),
                type_params: Vec::new(),
                type_param_bounds: Vec::new(),
            },
            crate::ast::TypeExpr::Void => Type::Void,
            crate::ast::TypeExpr::Inferred => Type::Any,
        }
    }

    /// Resolve a type expression to a concrete `Type`, treating the given
    /// generic type parameter names (`type_params`, e.g. `["T"]` from
    /// `enum Option<T>`) as `Type::TypeVar`
    fn get_type_from_expr_with_params(
        &self,
        expr: &crate::ast::TypeExpr,
        type_params: &[String],
    ) -> Type {
        self.get_type_from_expr_with_bounds(expr, type_params, &[])
    }

    /// Like [`get_type_from_expr_with_params`], but a bounded generic
    /// parameter (`T: Shape`) resolves to the bound interface instead of a
    /// bare `TypeVar`
    fn get_type_from_expr_with_bounds(
        &self,
        expr: &crate::ast::TypeExpr,
        type_params: &[String],
        type_param_bounds: &[Option<String>],
    ) -> Type {
        match expr {
            crate::ast::TypeExpr::Named(name) => {
                if let Some(idx) = type_params.iter().position(|p| p == name) {
                    let bound = type_param_bounds.get(idx).cloned().flatten();
                    if let Some(bound_name) = &bound {
                        if let Some(iface_info) = self.environment.get_interface_type(bound_name) {
                            let methods = iface_info
                                .methods
                                .iter()
                                .map(|(mname, finfo)| {
                                    (
                                        mname.clone(),
                                        crate::types::InterfaceMethod {
                                            name: mname.clone(),
                                            param_types: finfo.param_types.clone(),
                                            return_type: finfo.return_type.clone(),
                                        },
                                    )
                                })
                                .collect();
                            return Type::Interface {
                                name: iface_info.name.clone(),
                                methods,
                                type_params: Vec::new(),
                            };
                        }
                    }
                    return Type::TypeVar {
                        name: name.clone(),
                        bound,
                    };
                }
                self.get_type_from_expr(expr)
            }
            crate::ast::TypeExpr::Array { element_type, len } => Type::Array {
                element_type: Box::new(
                    self.get_type_from_expr_with_params(element_type, type_params),
                ),
                len: *len,
            },
            crate::ast::TypeExpr::Function { params, ret } => Type::FunctionType {
                param_types: params
                    .iter()
                    .map(|p| self.get_type_from_expr_with_params(p, type_params))
                    .collect(),
                return_type: Box::new(self.get_type_from_expr_with_params(ret, type_params)),
                type_params: Vec::new(),
                type_param_bounds: Vec::new(),
            },
            crate::ast::TypeExpr::Generic { name, args } => {
                let base = self.get_type_from_expr_with_params(
                    &crate::ast::TypeExpr::Named(name.clone()),
                    type_params,
                );
                let type_args: Vec<Type> = args
                    .iter()
                    .map(|a| self.get_type_from_expr_with_params(a, type_params))
                    .collect();
                self.apply_type_args(base, &type_args)
            }
            crate::ast::TypeExpr::Void => Type::Void,
            crate::ast::TypeExpr::Inferred => Type::Any,
        }
    }

    /// Substitute type arguments into a generic type's fields/payloads:
    /// `Box<T>` (schema) + args `[int]` -> `Box<int>` with `value: int`.
    /// Non-generic bases are returned unchanged
    fn apply_type_args(&self, base: Type, type_args: &[Type]) -> Type {
        match base {
            Type::Struct { name, fields, .. } => {
                let mut subst: HashMap<String, Type> = HashMap::default();
                if let Some(struct_info) = self.environment.get_struct_type(&name) {
                    for (param, arg) in struct_info.type_params.iter().zip(type_args.iter()) {
                        subst.insert(param.clone(), arg.clone());
                    }
                }
                Type::Struct {
                    name,
                    fields: alloc::rc::Rc::new(
                        fields
                            .iter()
                            .map(|(k, v)| (k.clone(), v.substitute_type_vars(&subst)))
                            .collect(),
                    ),
                    type_args: type_args.to_vec(),
                }
            }
            Type::Enum { name, variants, .. } => {
                let mut subst: HashMap<String, Type> = HashMap::default();
                if let Some(enum_info) = self.environment.get_enum_type(&name) {
                    for (param, arg) in enum_info.type_params.iter().zip(type_args.iter()) {
                        subst.insert(param.clone(), arg.clone());
                    }
                }
                Type::Enum {
                    name,
                    variants: variants
                        .iter()
                        .map(|v| crate::types::EnumVariant {
                            name: v.name.clone(),
                            payload_types: v
                                .payload_types
                                .iter()
                                .map(|t| t.substitute_type_vars(&subst))
                                .collect(),
                        })
                        .collect(),
                    type_args: type_args.to_vec(),
                }
            }
            other => other,
        }
    }

    /// Reject an explicit `any` in a type annotation. `any` is no longer a
    /// source-level type: use a generic parameter or an interface instead
    /// (`Type::Any` remains an internal fallback)
    fn check_no_explicit_any(expr: &crate::ast::TypeExpr) -> Result<(), (ErrorType, String)> {
        match expr {
            crate::ast::TypeExpr::Named(name) if name == "any" => Err((
                ErrorType::TypeError,
                "type `any` is not allowed in annotations; use a generic parameter or an interface"
                    .to_string(),
            )),
            crate::ast::TypeExpr::Array { element_type, .. } => {
                Self::check_no_explicit_any(element_type)
            }
            crate::ast::TypeExpr::Function { params, ret } => {
                for p in params {
                    Self::check_no_explicit_any(p)?;
                }
                Self::check_no_explicit_any(ret)
            }
            crate::ast::TypeExpr::Generic { args, .. } => {
                for a in args {
                    Self::check_no_explicit_any(a)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// Declare a type by name from the module tree index (builds the index
    /// on the first access - then O(1))
    fn tree_decl(&self, name: &str) -> Option<RawTypeDecl> {
        if self.tree_index.borrow().is_none() {
            let mut index: HashMap<String, RawTypeDecl> = HashMap::default();
            if let Some(root) = self.module.as_ref() {
                collect_all_types(root, &mut index);
            }
            *self.tree_index.borrow_mut() = Some(index);
        }
        match self.tree_index.borrow().as_ref() {
            Some(index) => index.get(name).cloned(),
            None => None,
        }
    }

    /// Find a type declaration by NAME in the module tree and assemble
    /// `StructTypeInfo` (fields + methods from `ext <Name>`)
    fn tree_struct_info(&self, name: &str) -> Option<alloc::rc::Rc<StructTypeInfo>> {
        if let Some(cached) = self.environment.get_struct_type(name) {
            return Some(cached);
        }
        if self.external_depth.borrow().len() > 8 {
            return None;
        }
        let info = self.tree_decl(name)?;
        let mut field_map = HashMap::default();
        self.external_depth.borrow_mut().push(name.to_string());
        for (fname, fty) in &info.fields {
            let t = self.get_type_from_expr_with_params(fty, &info.type_params);
            field_map.insert(fname.clone(), t);
        }
        let mut method_map = HashMap::default();
        for (mname, mparams, mret) in &info.methods {
            let param_types: Vec<Type> = mparams
                .iter()
                .map(|p| self.get_type_from_expr_with_params(&p.param_type, &info.type_params))
                .collect();
            let return_type = self.get_type_from_expr_with_params(mret, &info.type_params);
            method_map.insert(
                mname.clone(),
                FunctionInfo {
                    param_types,
                    return_type,
                    type_params: Vec::new(),
                    type_param_bounds: Vec::new(),
                },
            );
        }
        self.external_depth.borrow_mut().pop();
        let built = StructTypeInfo {
            name: info.name,
            fields: alloc::rc::Rc::new(field_map),
            methods: method_map,
            type_params: info.type_params,
        };
        let built = alloc::rc::Rc::new(built);
        self.environment
            .external_types
            .borrow_mut()
            .insert(name.to_string(), alloc::rc::Rc::clone(&built));
        Some(built)
    }

    /// Interface derived from a module tree by name (without importing).
    fn tree_interface_info(&self, name: &str) -> Option<InterfaceTypeInfo> {
        if let Some(cached) = self.environment.get_interface_type(name) {
            return Some(cached.clone());
        }
        let module = self.module.as_ref()?;
        let index = {
            let map = match self.interface_index.borrow_mut().take() {
                Some(map) => map,
                None => {
                    let mut fresh = HashMap::default();
                    collect_all_interfaces(module, &mut fresh);
                    fresh
                }
            };
            let found = map.get(name).cloned();
            *self.interface_index.borrow_mut() = Some(map);
            found?
        };
        if self.external_depth.borrow().len() > 8 {
            return None;
        }
        let mut methods = HashMap::default();
        self.external_depth.borrow_mut().push(name.to_string());
        for (mname, mparams, mret) in &index.methods {
            let param_types: Vec<Type> = mparams
                .iter()
                .map(|p| self.get_type_from_expr_with_params(&p.param_type, &index.type_params))
                .collect();
            let return_type = self.get_type_from_expr_with_params(mret, &index.type_params);
            methods.insert(
                mname.clone(),
                FunctionInfo {
                    param_types,
                    return_type,
                    type_params: Vec::new(),
                    type_param_bounds: Vec::new(),
                },
            );
        }
        self.external_depth.borrow_mut().pop();
        let built = InterfaceTypeInfo {
            name: index.name.clone(),
            methods,
            type_params: index.type_params.clone(),
        };
        self.environment
            .external_interfaces
            .borrow_mut()
            .insert(name.to_string(), built.clone());
        Some(built)
    }

    /// Whether the type contains any type variables (directly or nested in
    /// arrays/fields/payloads)
    fn type_has_vars(t: &Type) -> bool {
        match t {
            Type::TypeVar { .. } => true,
            Type::Array { element_type, .. } => Self::type_has_vars(element_type),
            Type::Tuple { element_types } => element_types.iter().any(Self::type_has_vars),
            Type::Struct {
                fields, type_args, ..
            } => {
                fields.values().any(Self::type_has_vars)
                    || type_args.iter().any(Self::type_has_vars)
            }
            Type::Enum {
                variants,
                type_args,
                ..
            } => {
                variants
                    .iter()
                    .any(|v| v.payload_types.iter().any(Self::type_has_vars))
                    || type_args.iter().any(Self::type_has_vars)
            }
            Type::FunctionType {
                param_types,
                return_type,
                ..
            } => param_types.iter().any(Self::type_has_vars) || Self::type_has_vars(return_type),
            _ => false,
        }
    }

    /// Unify a declared parameter type with an argument's actual type,
    /// binding generic type variables
    fn unify_param_type(param: &Type, arg: &Type, subst: &mut HashMap<String, Type>) -> bool {
        match (param, arg) {
            (Type::TypeVar { name, .. }, other) => {
                if Self::is_open_type(other) {
                    return true;
                }
                match subst.get(name) {
                    Some(existing) => existing == other,
                    None => {
                        subst.insert(name.clone(), other.clone());
                        true
                    }
                }
            }
            (
                Type::Array {
                    element_type: p1,
                    len: l1,
                },
                Type::Array {
                    element_type: a1,
                    len: l2,
                },
            ) => l1 == l2 && Self::unify_param_type(p1, a1, subst),
            (
                Type::Struct {
                    name: pn,
                    type_args: pa,
                    ..
                },
                Type::Struct {
                    name: an,
                    type_args: aa,
                    ..
                },
            )
            | (
                Type::Enum {
                    name: pn,
                    type_args: pa,
                    ..
                },
                Type::Enum {
                    name: an,
                    type_args: aa,
                    ..
                },
            ) => {
                if pn != an {
                    return false;
                }
                if pa.is_empty() || aa.is_empty() {
                    return true;
                }
                pa.len() == aa.len()
                    && pa
                        .iter()
                        .zip(aa.iter())
                        .all(|(p, a)| Self::unify_param_type(p, a, subst))
            }
            (Type::Tuple { element_types: p1 }, Type::Tuple { element_types: a1 }) => {
                p1.len() == a1.len()
                    && p1
                        .iter()
                        .zip(a1.iter())
                        .all(|(p, a)| Self::unify_param_type(p, a, subst))
            }
            _ => true,
        }
    }

    /// Infer the type arguments of a generic function from the argument types
    /// of a call site.
    fn infer_type_args(func: &FunctionInfo, arg_types: &[Type]) -> Option<Vec<Type>> {
        if func.type_params.is_empty() {
            return Some(Vec::new());
        }
        let mut subst: HashMap<String, Type> = HashMap::default();
        for (param, arg) in func.param_types.iter().zip(arg_types.iter()) {
            if !Self::unify_param_type(param, arg, &mut subst) {
                return None;
            }
        }
        func.type_params
            .iter()
            .map(|p| subst.get(p).cloned())
            .collect()
    }

    /// Inferring type args from the EXPECTED type (bidirectional inference)
    fn infer_type_args_from_hint(func: &FunctionInfo, hint: Option<&Type>) -> Option<Vec<Type>> {
        if func.type_params.is_empty() {
            return Some(Vec::new());
        }
        let hint = hint?;
        if Self::type_has_vars(hint) {
            return None;
        }
        let mut subst: HashMap<String, Type> = HashMap::default();
        if !Self::unify_param_type(&func.return_type, hint, &mut subst) {
            return None;
        }
        func.type_params
            .iter()
            .map(|p| subst.get(p).cloned())
            .collect()
    }

    /// The expression type given the expected type on the left
    fn expression_type_with_hint(
        &self,
        expr: &Expression,
        env: &Environment,
        expected: Option<Type>,
    ) -> Result<Type, (ErrorType, String, crate::Position)> {
        let saved = self.expected_hint.borrow_mut().take();
        *self.expected_hint.borrow_mut() = expected;
        let result = self.get_expression_type_with_environment(expr, env);
        *self.expected_hint.borrow_mut() = saved;
        result
    }

    /// The current expected type hint (see `expected_hint`)
    fn hint(&self) -> Option<Type> {
        self.expected_hint.borrow().clone()
    }

    /// Bind the declared type params of an ext-method's owner type for a
    /// call site
    fn bind_method_subst(
        &self,
        decl_type_params: &[String],
        receiver_args: &[Type],
        explicit: Option<&[Type]>,
        param_types: &[Type],
        return_type: &Type,
        arg_exprs: &[Expression],
    ) -> HashMap<String, Type> {
        let mut subst: HashMap<String, Type> = match explicit {
            Some(args) => decl_type_params
                .iter()
                .cloned()
                .zip(args.iter().cloned())
                .collect(),
            None => decl_type_params
                .iter()
                .cloned()
                .zip(receiver_args.iter().cloned())
                .filter(|(_, t)| !Self::is_open_type(t))
                .collect(),
        };
        for (param, arg) in param_types.iter().zip(arg_exprs.iter()) {
            if let Ok(arg_type) = self.get_expression_type_with_environment(arg, &self.environment)
            {
                Self::unify_param_type(param, &arg_type, &mut subst);
            }
        }
        if subst.len() < decl_type_params.len() {
            if let Some(hint) = self.hint() {
                if !Self::type_has_vars(&hint) && hint != Type::Any {
                    Self::unify_param_type(return_type, &hint, &mut subst);
                }
            }
        }
        subst
    }

    /// Validate explicit type args of a method call against the owner's
    /// declared type params
    fn explicit_method_type_args(
        &self,
        type_args: &[crate::ast::TypeExpr],
        decl_type_params: &[String],
        callee_pos: &crate::Position,
    ) -> Result<Option<Vec<Type>>, (ErrorType, String, crate::Position)> {
        if type_args.is_empty() {
            return Ok(None);
        }
        let explicit: Vec<Type> = type_args
            .iter()
            .map(|t| self.get_type_from_expr_with_params(t, &self.current_type_params))
            .collect();
        if explicit.len() != decl_type_params.len() {
            return Err((
                ErrorType::TypeError,
                format!(
                    "Method expects {} type argument(s), but got {}",
                    decl_type_params.len(),
                    explicit.len()
                ),
                callee_pos.clone(),
            ));
        }
        Ok(Some(explicit))
    }

    /// Whether a value of type `actual` can be assigned to a slot of type
    /// `expected`
    fn types_assignable(&self, expected: &Type, actual: &Type) -> bool {
        if expected == actual {
            return true;
        }
        match (expected, actual) {
            (
                Type::Struct {
                    name: n1,
                    type_args: a1,
                    ..
                },
                Type::Struct {
                    name: n2,
                    type_args: a2,
                    ..
                },
            )
            | (
                Type::Enum {
                    name: n1,
                    type_args: a1,
                    ..
                },
                Type::Enum {
                    name: n2,
                    type_args: a2,
                    ..
                },
            ) => {
                n1 == n2
                    && (a1.is_empty() || a2.is_empty() || self.type_args_pair_assignable(a1, a2))
            }
            _ => false,
        }
    }

    /// Pairwise assignability of generic type arguments
    fn type_args_pair_assignable(&self, a1: &[Type], a2: &[Type]) -> bool {
        a1.len() == a2.len()
            && a1.iter().zip(a2.iter()).all(|(x, y)| {
                x == y
                    || self.is_ctx_open(x)
                    || self.is_ctx_open(y)
                    || self.types_assignable(x, y)
                    || self.types_assignable(y, x)
            })
    }

    /// Whether the type is "open" - accepts any concrete type in checks
    fn is_open_type(t: &Type) -> bool {
        matches!(t, Type::Any | Type::TypeVar { .. })
    }

    /// Like [`Self::is_open_type`], but a `TypeVar` is open only if it is a
    /// generic param of the body being analyzed
    fn is_ctx_open(&self, t: &Type) -> bool {
        match t {
            Type::Any => true,
            Type::TypeVar { name, .. } => self.current_type_params.iter().any(|p| p == name),
            _ => false,
        }
    }

    /// Signature of an ext method as seen from *outside* the receiver
    fn method_signature_without_self(finfo: &FunctionInfo, receiver: &str) -> (Vec<Type>, Type) {
        let has_self = matches!(finfo.param_types.first(), Some(Type::Struct { name, .. })
            | Some(Type::Enum { name, .. }) if name == receiver);
        let param_types = if has_self {
            finfo.param_types.iter().skip(1).cloned().collect()
        } else {
            finfo.param_types.clone()
        };
        (param_types, finfo.return_type.clone())
    }

    /// Whether a method with the given name and signature (without `self`)
    /// exists on the type `ty`
    fn method_exists_on(
        &self,
        ty: &Type,
        name: &str,
        param_types: &[Type],
        return_type: &Type,
    ) -> bool {
        let sig: Option<Vec<crate::primitives::PrimitiveMethodSig>> = match ty {
            Type::Struct { .. } | Type::Enum { .. } => None,
            other => crate::primitives::get_methods_for(other),
        };
        if let Some(sigs) = sig {
            if let Some(s) = sigs.iter().find(|m| m.name == name) {
                if s.param_types.len() != param_types.len() {
                    return false;
                }
                let params_ok = s
                    .param_types
                    .iter()
                    .zip(param_types.iter())
                    .all(|(a, b)| a == b || Self::is_open_type(a) || Self::is_open_type(b));
                let ret_ok = s.return_type == *return_type
                    || Self::is_open_type(&s.return_type)
                    || Self::is_open_type(return_type);
                return params_ok && ret_ok;
            }
        }
        let m = match ty {
            Type::Struct { name, .. } => self
                .environment
                .get_struct_type(name)
                .map(|s| s.methods.clone()),
            Type::Enum { name, .. } => self
                .environment
                .get_enum_type(name)
                .map(|e| e.methods.clone()),
            _ => None,
        };
        if let Some(methods) = m {
            if let Some(finfo) = methods.get(name) {
                let self_ty_name = match ty {
                    Type::Struct { name, .. } | Type::Enum { name, .. } => Some(name.as_str()),
                    _ => None,
                };
                let expected: Vec<&Type> = if finfo
                    .param_types
                    .first()
                    .map(|p| match p {
                        Type::Struct { name: n, .. } | Type::Enum { name: n, .. } => {
                            Some(n.as_str()) == self_ty_name
                        }
                        _ => false,
                    })
                    .unwrap_or(false)
                {
                    finfo.param_types.iter().skip(1).collect()
                } else {
                    finfo.param_types.iter().collect()
                };
                if expected.len() != param_types.len() {
                    return false;
                }
                let params_ok = expected
                    .iter()
                    .zip(param_types.iter())
                    .all(|(a, b)| **a == *b || Self::is_open_type(a) || Self::is_open_type(b));
                let ret_ok = finfo.return_type == *return_type
                    || Self::is_open_type(&finfo.return_type)
                    || Self::is_open_type(return_type);
                return params_ok && ret_ok;
            }
        }
        false
    }

    /// Determines whether the argument `arg_type` matches the parameter `param_type`
    fn argument_matches_param(&self, arg_type: &Type, param_type: &Type) -> bool {
        if arg_type == param_type {
            return true;
        }
        if self.is_ctx_open(arg_type) || self.is_ctx_open(param_type) {
            return true;
        }
        if let Type::Interface {
            name: iface_name, ..
        } = param_type
        {
            if let Some(iface_info) = self.environment.get_interface_type(iface_name) {
                return self.type_implements_interface(arg_type, &iface_info);
            }
        }
        match (param_type, arg_type) {
            (Type::Struct { name: n1, .. }, Type::Struct { name: n2, .. })
            | (Type::Struct { name: n1, .. }, Type::Enum { name: n2, .. })
            | (Type::Enum { name: n1, .. }, Type::Struct { name: n2, .. })
            | (Type::Enum { name: n1, .. }, Type::Enum { name: n2, .. }) => {
                n1 == n2 && self.types_assignable(param_type, arg_type)
            }
            (
                Type::Array {
                    element_type: p1,
                    len: l1,
                },
                Type::Array {
                    element_type: p2,
                    len: l2,
                },
            ) => l1 == l2 && self.argument_matches_param(p2, p1),
            _ => false,
        }
    }

    /// Whether the type `ty` implements the interface `iface`
    fn type_implements_interface(&self, ty: &Type, iface: &InterfaceTypeInfo) -> bool {
        let r = iface.methods.iter().all(|(mname, mfinfo)| {
            self.method_exists_on(ty, mname, &mfinfo.param_types, &mfinfo.return_type)
        });
        r
    }

    /// Find a common interface implemented by all given types
    fn common_interface_of(&self, types: &[Type], environment: &Environment) -> Option<Type> {
        let mut interfaces: Vec<InterfaceTypeInfo> = Vec::new();
        for scope in environment.scopes.iter() {
            for symbol in scope.values() {
                if let Symbol::InterfaceType(info) = symbol {
                    interfaces.push(info.clone());
                }
            }
        }
        let mut common: Vec<&InterfaceTypeInfo> = Vec::new();
        for iface in &interfaces {
            if types
                .iter()
                .all(|t| self.type_implements_interface(t, iface))
            {
                common.push(iface);
            }
        }
        common.sort_by_key(|i| i.methods.len());
        common.first().map(|iface| {
            let methods = iface
                .methods
                .iter()
                .map(|(mname, finfo)| {
                    (
                        mname.clone(),
                        crate::types::InterfaceMethod {
                            name: mname.clone(),
                            param_types: finfo.param_types.clone(),
                            return_type: finfo.return_type.clone(),
                        },
                    )
                })
                .collect();
            Type::Interface {
                name: iface.name.clone(),
                methods,
                type_params: Vec::new(),
            }
        })
    }

    /// Check if a cast from source_type to target_type is valid
    fn is_valid_cast(&self, source_type: &Type, target_type: &Type) -> bool {
        match (source_type, target_type) {
            // Int to Double is always valid
            (Type::Int, Type::Double) => true,
            // Double to Int is valid (truncates decimal part)
            (Type::Double, Type::Int) => true,
            // String to Int/Double if the string is a valid number
            (Type::String, Type::Int) | (Type::String, Type::Double) => true,
            // Any type to String is valid
            (_, Type::String) => true,
            // Bool to Int (true=1, false=0)
            (Type::Bool, Type::Int) => true,
            // Int to Bool (0=false, non-zero=true)
            (Type::Int, Type::Bool) => true,
            // Same type is always valid
            (t1, t2) if t1 == t2 => true,
            // Invalid cast
            _ => false,
        }
    }

    /// Get the type of an expression with a specific environment
    fn get_expression_type_with_environment(
        &self,
        expr: &Expression,
        environment: &Environment,
    ) -> Result<Type, (ErrorType, String, crate::Position)> {
        match &expr.kind {
            ExpressionKind::StringLiteral(_) => Ok(Type::String),
            ExpressionKind::IntLiteral(_) => Ok(Type::Int),
            ExpressionKind::DoubleLiteral(_) => Ok(Type::Double),
            ExpressionKind::BoolLiteral(_) => Ok(Type::Bool),
            ExpressionKind::Variable(name) => {
                for scope in environment.scopes.iter().rev() {
                    if let Some(symbol) = scope.get(name) {
                        match symbol {
                            Symbol::Variable(var_info) => {
                                return Ok(var_info.var_type.clone());
                            }
                            Symbol::Function(func_info) => {
                                return Ok(Type::FunctionType {
                                    param_types: func_info.param_types.clone(),
                                    return_type: Box::new(func_info.return_type.clone()),
                                    type_params: func_info.type_params.clone(),
                                    type_param_bounds: func_info.type_param_bounds.clone(),
                                });
                            }
                            Symbol::InterfaceType(iface_info) => {
                                let methods = iface_info
                                    .methods
                                    .iter()
                                    .map(|(mname, finfo)| {
                                        (
                                            mname.clone(),
                                            crate::types::InterfaceMethod {
                                                name: mname.clone(),
                                                param_types: finfo.param_types.clone(),
                                                return_type: finfo.return_type.clone(),
                                            },
                                        )
                                    })
                                    .collect();
                                return Ok(Type::Interface {
                                    name: iface_info.name.clone(),
                                    methods,
                                    type_params: Vec::new(),
                                });
                            }
                            Symbol::StructType(struct_info) => {
                                return Ok(Type::Struct {
                                    name: struct_info.name.clone(),
                                    fields: struct_info.fields.clone(),
                                    type_args: Vec::new(),
                                });
                            }
                            Symbol::EnumType(enum_info) => {
                                return Ok(Type::Enum {
                                    name: enum_info.name.clone(),
                                    variants: enum_info.variants.clone(),
                                    type_args: Vec::new(),
                                });
                            }
                        }
                    }
                }

                if let Some(struct_info) = environment.get_struct_type(name) {
                    return Ok(Type::Struct {
                        name: struct_info.name.clone(),
                        fields: struct_info.fields.clone(),
                        type_args: Vec::new(),
                    });
                }

                if let Some(enum_info) = environment.get_enum_type(name) {
                    return Ok(Type::Enum {
                        name: enum_info.name.clone(),
                        variants: enum_info.variants.clone(),
                        type_args: Vec::new(),
                    });
                }

                Err((
                    ErrorType::UndefinedVariable(name.to_string()),
                    format!("Variable, function, or type '{name}' is not defined"),
                    expr.pos.clone(),
                ))
            }
            ExpressionKind::Unary { operator, operand } => {
                let operand_type =
                    self.get_expression_type_with_environment(operand, &self.environment)?;

                match operator {
                    UnaryOperator::Not => {
                        if operand_type == Type::Bool {
                            Ok(Type::Bool)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: "bool".to_string(),
                                    found: format!("{operand_type}"),
                                },
                                "Cannot apply '!' operator to non-boolean type".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                    UnaryOperator::Plus | UnaryOperator::Minus => {
                        if operand_type == Type::Int || operand_type == Type::Double {
                            Ok(operand_type)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: "int or double".to_string(),
                                    found: format!("{operand_type}"),
                                },
                                "Cannot apply unary +/- to non-numeric type".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                }
            }
            ExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                let left_type =
                    self.get_expression_type_with_environment(left, &self.environment)?;
                let right_type =
                    self.get_expression_type_with_environment(right, &self.environment)?;

                match operator {
                    BinaryOperator::Add => {
                        if left_type == right_type {
                            Ok(left_type)
                        } else if left_type == Type::String {
                            Ok(Type::String)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: "matching types".to_string(),
                                    found: format!("{left_type} and {right_type}"),
                                },
                                "Cannot perform addition operation on non-matching or mismatched types".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                    BinaryOperator::Sub
                    | BinaryOperator::Mul
                    | BinaryOperator::Div
                    | BinaryOperator::Mod => {
                        if left_type == Type::Any || right_type == Type::Any {
                            Ok(Type::Any)
                        } else if left_type == right_type
                            && (left_type == Type::Int || left_type == Type::Double)
                        {
                            Ok(left_type)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: "matching numeric types".to_string(),
                                    found: format!("{left_type} and {right_type}"),
                                },
                                "Cannot perform arithmetic operation on non-numeric or mismatched types".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                    BinaryOperator::Less
                    | BinaryOperator::LessEqual
                    | BinaryOperator::Greater
                    | BinaryOperator::GreaterEqual => {
                        if left_type == Type::Any
                            || right_type == Type::Any
                            || (left_type == right_type
                                && (left_type == Type::Int || left_type == Type::Double))
                        {
                            Ok(Type::Bool)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: "matching numeric types".to_string(),
                                    found: format!("{left_type} and {right_type}"),
                                },
                                "Cannot compare non-numeric or mismatched types".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                    BinaryOperator::Equal | BinaryOperator::NotEqual => {
                        if left_type == right_type {
                            Ok(Type::Bool)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: format!("{left_type}"),
                                    found: format!("{right_type}"),
                                },
                                "Cannot compare different types".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                    BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
                        if left_type == Type::Bool && right_type == Type::Bool {
                            Ok(Type::Bool)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: "bool and bool".to_string(),
                                    found: format!("{left_type} and {right_type}"),
                                },
                                "Cannot perform logical operation on non-boolean types".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                }
            }
            ExpressionKind::Cast { expr, target_type } => {
                let expr_type =
                    self.get_expression_type_with_environment(expr, &self.environment)?;
                let target_type = self.get_type_from_expr(target_type);

                if !self.is_valid_cast(&expr_type, &target_type) {
                    return Err((
                        ErrorType::TypeError,
                        format!("Cannot cast from {expr_type} to {target_type}"),
                        expr.pos.clone(),
                    ));
                }

                Ok(target_type)
            }
            ExpressionKind::FunctionCall {
                callee,
                arguments,
                type_args,
            } => {
                if let ExpressionKind::MemberAccess { object, member } = &callee.kind {
                    let object_type =
                        self.get_expression_type_with_environment(object, &self.environment)?;

                    if let Type::Enum { name, variants, .. } = &object_type {
                        if let Some(variant) = variants.iter().find(|v| &v.name == member) {
                            if arguments.len() != variant.payload_types.len() {
                                return Err((
                                    ErrorType::TypeError,
                                    format!(
                                        "Variant '{}' of enum '{}' expects {} payload argument(s), but got {}",
                                        variant.name,
                                        name,
                                        variant.payload_types.len(),
                                        arguments.len()
                                    ),
                                    callee.pos.clone(),
                                ));
                            }
                            let mut subst: HashMap<String, Type> = HashMap::default();
                            for (arg, expected) in
                                arguments.iter().zip(variant.payload_types.iter())
                            {
                                let arg_type = self
                                    .get_expression_type_with_environment(arg, &self.environment)?;
                                if !Self::unify_param_type(expected, &arg_type, &mut subst) {
                                    return Err((
                                        ErrorType::TypeMismatch {
                                            expected: expected.to_string(),
                                            found: arg_type.to_string(),
                                        },
                                        format!("Conflicting types for generic parameter in variant '{}'", variant.name),
                                        arg.pos.clone(),
                                    ));
                                }
                                if &arg_type != expected
                                    && !Self::is_open_type(expected)
                                    && !Self::is_open_type(&arg_type)
                                {
                                    return Err((
                                        ErrorType::TypeMismatch {
                                            expected: expected.to_string(),
                                            found: arg_type.to_string(),
                                        },
                                        format!(
                                            "Mismatched payload type for variant '{}'",
                                            variant.name
                                        ),
                                        arg.pos.clone(),
                                    ));
                                }
                            }

                            if let Some(enum_info) = self.environment.get_enum_type(name) {
                                if !enum_info.type_params.is_empty() {
                                    let type_args: Vec<Type> = enum_info
                                        .type_params
                                        .iter()
                                        .map(|p| {
                                            subst.get(p).cloned().unwrap_or_else(|| {
                                                if self.current_type_params.contains(p) {
                                                    Type::TypeVar {
                                                        name: p.clone(),
                                                        bound: None,
                                                    }
                                                } else {
                                                    Type::Any
                                                }
                                            })
                                        })
                                        .collect();
                                    let applied =
                                        self.apply_type_args(object_type.clone(), &type_args);
                                    return Ok(applied);
                                }
                            }
                            return Ok(object_type.clone());
                        }
                    }

                    if let Type::Interface {
                        name: iface_name,
                        methods,
                        ..
                    } = &object_type
                    {
                        let Some(m) = methods.get(member) else {
                            return Err((
                                ErrorType::TypeError,
                                format!("Interface '{iface_name}' has no method named '{member}'"),
                                callee.pos.clone(),
                            ));
                        };
                        if arguments.len() != m.param_types.len() {
                            return Err((
                                ErrorType::TypeError,
                                format!(
                                    "Expected {} arguments, but got {} for interface method '{}'",
                                    m.param_types.len(),
                                    arguments.len(),
                                    member
                                ),
                                expr.pos.clone(),
                            ));
                        }
                        for (arg, param_ty) in arguments.iter().zip(m.param_types.iter()) {
                            let arg_type =
                                self.get_expression_type_with_environment(arg, &self.environment)?;
                            if arg_type != *param_ty
                                && !Self::is_open_type(param_ty)
                                && !Self::is_open_type(&arg_type)
                            {
                                return Err((
                                    ErrorType::TypeMismatch {
                                        expected: param_ty.to_string(),
                                        found: arg_type.to_string(),
                                    },
                                    "Mismatched argument type".to_string(),
                                    arg.pos.clone(),
                                ));
                            }
                        }
                        return Ok(m.return_type.clone());
                    }

                    let is_static_method_call =
                        if let ExpressionKind::Variable(struct_name) = &object.kind {
                            self.environment.get_struct_type(struct_name).is_some()
                        } else {
                            false
                        };

                    if is_static_method_call {
                        if let ExpressionKind::Variable(struct_name) = &object.kind {
                            if let Some(struct_info) = self.environment.get_struct_type(struct_name)
                            {
                                if let Some(method_info) = struct_info.methods.get(member) {
                                    let has_self_parameter = match method_info.param_types.first() {
                                        Some(Type::Struct { name, .. }) => name == struct_name,
                                        _ => false,
                                    };

                                    let expected_arg_count = if has_self_parameter {
                                        method_info.param_types.len() - 1
                                    } else {
                                        method_info.param_types.len()
                                    };

                                    if arguments.len() != expected_arg_count {
                                        return Err((
                                            ErrorType::TypeError,
                                            format!(
                                                "Expected {} arguments, but got {}. {:?}",
                                                expected_arg_count,
                                                arguments.len(),
                                                arguments
                                            ),
                                            expr.pos.clone(),
                                        ));
                                    }

                                    let param_types_to_check: Vec<&Type> = if has_self_parameter {
                                        method_info.param_types.iter().skip(1).collect()
                                    } else {
                                        method_info.param_types.iter().collect()
                                    };

                                    let explicit = self.explicit_method_type_args(
                                        type_args,
                                        &struct_info.type_params,
                                        &callee.pos,
                                    )?;
                                    let sig_params: Vec<Type> =
                                        param_types_to_check.iter().map(|p| (*p).clone()).collect();
                                    let subst = self.bind_method_subst(
                                        &struct_info.type_params,
                                        &[],
                                        explicit.as_deref(),
                                        &sig_params,
                                        &method_info.return_type,
                                        arguments,
                                    );
                                    let param_types_to_check: Vec<Type> = sig_params
                                        .iter()
                                        .map(|p| p.substitute_type_vars(&subst))
                                        .collect();
                                    let return_type =
                                        method_info.return_type.substitute_type_vars(&subst);

                                    for (arg, param_type) in
                                        arguments.iter().zip(param_types_to_check.iter())
                                    {
                                        let arg_type = self.get_expression_type_with_environment(
                                            arg,
                                            &self.environment,
                                        )?;
                                        if !self.argument_matches_param(&arg_type, param_type) {
                                            return Err((
                                                ErrorType::TypeMismatch {
                                                    expected: param_type.to_string(),
                                                    found: arg_type.to_string(),
                                                },
                                                "Mismatched argument type".to_string(),
                                                arg.pos.clone(),
                                            ));
                                        }
                                    }

                                    Ok(return_type)
                                } else {
                                    Err((
                                        ErrorType::TypeError,
                                        format!(
                                            "Struct '{struct_name}' has no static method named '{member}'"),
                                        callee.pos.clone(),
                                    ))
                                }
                            } else {
                                Err((
                                    ErrorType::TypeError,
                                    format!("Unknown struct type '{struct_name}'"),
                                    callee.pos.clone(),
                                ))
                            }
                        } else {
                            Err((
                                ErrorType::TypeError,
                                "Internal error in static method call handling".to_string(),
                                callee.pos.clone(),
                            ))
                        }
                    } else {
                        let instance_methods = match &object_type {
                            Type::Struct {
                                name,
                                type_args: receiver_args,
                                ..
                            } => self
                                .environment
                                .get_struct_type(name)
                                .map(|s| {
                                    (
                                        name.clone(),
                                        receiver_args.clone(),
                                        s.type_params.clone(),
                                        s.methods.clone(),
                                    )
                                })
                                .filter(|(_, _, _, methods)| methods.contains_key(member)),
                            Type::Enum {
                                name,
                                type_args: receiver_args,
                                ..
                            } => self
                                .environment
                                .get_enum_type(name)
                                .map(|e| {
                                    (
                                        name.clone(),
                                        receiver_args.clone(),
                                        e.type_params.clone(),
                                        e.methods.clone(),
                                    )
                                })
                                .filter(|(_, _, _, methods)| methods.contains_key(member)),
                            _ => None,
                        };
                        if let Some((type_name, receiver_args, decl_params, methods)) =
                            instance_methods
                        {
                            let method_info = methods.get(member).expect("filtered above");
                            let (mut param_types, mut return_type) =
                                Self::method_signature_without_self(method_info, &type_name);
                            if arguments.len() != param_types.len() {
                                return Err((
                                    ErrorType::TypeError,
                                    format!(
                                        "Expected {} arguments, but got {}. {:?}",
                                        param_types.len(),
                                        arguments.len(),
                                        arguments
                                    ),
                                    expr.pos.clone(),
                                ));
                            }

                            let explicit = self.explicit_method_type_args(
                                type_args,
                                &decl_params,
                                &callee.pos,
                            )?;
                            let subst = self.bind_method_subst(
                                &decl_params,
                                &receiver_args,
                                explicit.as_deref(),
                                &param_types,
                                &return_type,
                                arguments,
                            );
                            if !subst.is_empty() {
                                param_types = param_types
                                    .iter()
                                    .map(|p| p.substitute_type_vars(&subst))
                                    .collect();
                                return_type = return_type.substitute_type_vars(&subst);
                            }

                            for (arg, param_type) in arguments.iter().zip(param_types.iter()) {
                                let arg_type = self
                                    .get_expression_type_with_environment(arg, &self.environment)?;
                                if !self.argument_matches_param(&arg_type, param_type) {
                                    return Err((
                                        ErrorType::TypeMismatch {
                                            expected: param_type.to_string(),
                                            found: arg_type.to_string(),
                                        },
                                        "Mismatched argument type".to_string(),
                                        arg.pos.clone(),
                                    ));
                                }
                            }

                            Ok(return_type)
                        } else {
                            if object_type == Type::Any {
                                return Ok(Type::Any);
                            }
                            if let Some(sig) =
                                crate::primitives::find_method_sig(&object_type, member.as_str())
                            {
                                if arguments.len() != sig.param_types.len() {
                                    return Err((
                                        ErrorType::TypeError,
                                        format!(
                                            "Expected {} arguments, but got {}. {:?}",
                                            sig.param_types.len(),
                                            arguments.len(),
                                            arguments
                                        ),
                                        expr.pos.clone(),
                                    ));
                                }
                                for (arg, param_ty) in arguments.iter().zip(sig.param_types.iter())
                                {
                                    let arg_type = self.get_expression_type_with_environment(
                                        arg,
                                        &self.environment,
                                    )?;
                                    if arg_type != *param_ty {
                                        return Err((
                                            ErrorType::TypeMismatch {
                                                expected: param_ty.to_string(),
                                                found: arg_type.to_string(),
                                            },
                                            "Mismatched argument type".to_string(),
                                            arg.pos.clone(),
                                        ));
                                    }
                                }
                                Ok(sig.return_type)
                            } else if let Type::Struct { name, fields, .. } = &object_type {
                                if let Some(Type::FunctionType {
                                    param_types,
                                    return_type,
                                    ..
                                }) = fields.get(member)
                                {
                                    if arguments.len() != param_types.len() {
                                        return Err((
                                            ErrorType::TypeError,
                                            format!(
                                                "Expected {} arguments, but got {}",
                                                param_types.len(),
                                                arguments.len()
                                            ),
                                            callee.pos.clone(),
                                        ));
                                    }
                                    for (arg, param_type) in
                                        arguments.iter().zip(param_types.iter())
                                    {
                                        let arg_type = self.get_expression_type_with_environment(
                                            arg,
                                            &self.environment,
                                        )?;
                                        if arg_type != *param_type {
                                            return Err((
                                                ErrorType::TypeMismatch {
                                                    expected: param_type.to_string(),
                                                    found: arg_type.to_string(),
                                                },
                                                "Mismatched argument type".to_string(),
                                                arg.pos.clone(),
                                            ));
                                        }
                                    }
                                    Ok((**return_type).clone())
                                } else {
                                    Err((
                                        ErrorType::TypeError,
                                        format!("Type '{name}' has no method '{member}'"),
                                        callee.pos.clone(),
                                    ))
                                }
                            } else {
                                Err((
                                    ErrorType::TypeError,
                                    format!(
                                        "Cannot call method '{member}' on non-struct type '{object_type}'"),
                                    callee.pos.clone(),
                                ))
                            }
                        }
                    }
                } else {
                    let callee_type =
                        self.get_expression_type_with_environment(callee, &self.environment)?;
                    if let ExpressionKind::Variable(name) = &callee.kind {
                        if let Some(func_info) = self.environment.get_function(name) {
                            if func_info.param_types.len() != arguments.len() {
                                return Err((
                                    ErrorType::TypeError,
                                    format!(
                                        "Expected {} arguments, but got {}, {:?}",
                                        func_info.param_types.len(),
                                        arguments.len(),
                                        arguments
                                    ),
                                    expr.pos.clone(),
                                ));
                            }
                            if !func_info.type_params.is_empty() {
                                let bound_type_args: Option<Vec<Type>> = if type_args.is_empty() {
                                    let arg_types: Vec<Type> = arguments
                                        .iter()
                                        .map(|a| {
                                            self.get_expression_type_with_environment(
                                                a,
                                                &self.environment,
                                            )
                                        })
                                        .collect::<Result<Vec<_>, _>>()?;
                                    match Self::infer_type_args(func_info, &arg_types) {
                                        Some(args) => Some(args),
                                        None => {
                                            let hint = self.hint();
                                            Self::infer_type_args_from_hint(
                                                func_info,
                                                hint.as_ref(),
                                            )
                                        }
                                    }
                                } else {
                                    let explicit: Vec<Type> = type_args
                                        .iter()
                                        .map(|t| {
                                            self.get_type_from_expr_with_params(
                                                t,
                                                &self.current_type_params,
                                            )
                                        })
                                        .collect();
                                    if explicit.len() != func_info.type_params.len() {
                                        return Err((
                                            ErrorType::TypeError,
                                            format!(
                                                "Generic function '{}' expects {} type argument(s), but got {}",
                                                name,
                                                func_info.type_params.len(),
                                                explicit.len()
                                            ),
                                            callee.pos.clone(),
                                        ));
                                    }
                                    Some(explicit)
                                };
                                if let Some(type_args) = bound_type_args {
                                    for (bound, arg) in
                                        func_info.type_param_bounds.iter().zip(type_args.iter())
                                    {
                                        if let Some(bound_name) = bound {
                                            let bound_ok = match self
                                                .environment
                                                .get_interface_type(bound_name)
                                            {
                                                Some(iface_info) => {
                                                    self.type_implements_interface(arg, &iface_info)
                                                }
                                                None => false,
                                            };
                                            if !bound_ok {
                                                return Err((
                                                    ErrorType::TypeMismatch {
                                                        expected: bound_name.clone(),
                                                        found: arg.to_string(),
                                                    },
                                                    format!(
                                                        "Type '{arg}' does not implement bound '{bound_name}'"),
                                                    callee.pos.clone(),
                                                ));
                                            }
                                        }
                                    }
                                    let subst: HashMap<String, Type> = func_info
                                        .type_params
                                        .iter()
                                        .cloned()
                                        .zip(type_args.iter().cloned())
                                        .collect();
                                    let ret = func_info.return_type.substitute_type_vars(&subst);
                                    for (arg, param_type) in
                                        arguments.iter().zip(func_info.param_types.iter())
                                    {
                                        let param_type = param_type.substitute_type_vars(&subst);
                                        let arg_type = self.get_expression_type_with_environment(
                                            arg,
                                            &self.environment,
                                        )?;
                                        let iface_ok = if let Type::Interface {
                                            name: iface_name,
                                            ..
                                        } = &param_type
                                        {
                                            if let Some(iface_info) =
                                                self.environment.get_interface_type(iface_name)
                                            {
                                                self.type_implements_interface(
                                                    &arg_type,
                                                    &iface_info,
                                                )
                                            } else {
                                                false
                                            }
                                        } else {
                                            false
                                        };
                                        let array_of_any_ok = matches!((&arg_type, &param_type),
                                            (Type::Array { element_type, len: l1 },
                                             Type::Array { element_type: expected_el, len: l2 })
                                                if l1 == l2 && (Self::is_open_type(expected_el) || Self::is_open_type(element_type)));
                                        if !iface_ok
                                            && !self.types_assignable(&param_type, &arg_type)
                                            && !Self::is_open_type(&param_type)
                                            && !Self::is_open_type(&arg_type)
                                            && !array_of_any_ok
                                            && !self.is_valid_cast(&arg_type, &param_type)
                                        {
                                            return Err((
                                                ErrorType::TypeMismatch {
                                                    expected: param_type.to_string(),
                                                    found: arg_type.to_string(),
                                                },
                                                "Mismatched argument type".to_string(),
                                                arg.pos.clone(),
                                            ));
                                        }
                                    }
                                    return Ok(ret);
                                }
                            }
                        }
                    }

                    if let Type::FunctionType {
                        param_types,
                        return_type,
                        type_params,
                        type_param_bounds: _,
                    } = callee_type
                    {
                        let explicit_args: Vec<Type> = type_args
                            .iter()
                            .map(|t| {
                                self.get_type_from_expr_with_params(t, &self.current_type_params)
                            })
                            .collect();
                        let bound_args: Option<Vec<Type>> = if !explicit_args.is_empty() {
                            Some(explicit_args)
                        } else {
                            let hint = self.hint();
                            Self::infer_type_args_from_hint(
                                &FunctionInfo {
                                    param_types: param_types.clone(),
                                    return_type: *return_type.clone(),
                                    type_params: type_params.clone(),
                                    type_param_bounds: Vec::new(),
                                },
                                hint.as_ref(),
                            )
                        };
                        let (param_types, return_type) = if let Some(args) = bound_args {
                            if args.len() != type_params.len() {
                                return Err((
                                    ErrorType::TypeError,
                                    format!(
                                        "Generic function expects {} type argument(s), but got {}",
                                        type_params.len(),
                                        args.len()
                                    ),
                                    callee.pos.clone(),
                                ));
                            }
                            let subst: HashMap<String, Type> =
                                type_params.iter().cloned().zip(args).collect();
                            (
                                param_types
                                    .iter()
                                    .map(|t| t.substitute_type_vars(&subst))
                                    .collect::<Vec<_>>(),
                                Box::new(return_type.substitute_type_vars(&subst)),
                            )
                        } else {
                            (param_types.clone(), return_type.clone())
                        };
                        if arguments.len() != param_types.len() {
                            return Err((
                                ErrorType::TypeError,
                                format!(
                                    "Expected {} arguments, but got {}. {:?}",
                                    param_types.len(),
                                    arguments.len(),
                                    arguments
                                ),
                                expr.pos.clone(),
                            ));
                        }

                        for (arg, param_type) in arguments.iter().zip(param_types.iter()) {
                            let arg_type =
                                self.get_expression_type_with_environment(arg, &self.environment)?;
                            let iface_ok = if let Type::Interface {
                                name: iface_name, ..
                            } = param_type
                            {
                                if let Some(iface_info) =
                                    self.environment.get_interface_type(iface_name)
                                {
                                    self.type_implements_interface(&arg_type, &iface_info)
                                } else {
                                    false
                                }
                            } else {
                                false
                            };
                            let array_of_any_ok = matches!((&arg_type, param_type),
                                (Type::Array { element_type, len: l1 },
                                 Type::Array { element_type: expected_el, len: l2 })
                                    if l1 == l2 && (Self::is_open_type(expected_el) || Self::is_open_type(element_type)));
                            if !iface_ok
                                && !self.types_assignable(param_type, &arg_type)
                                && !Self::is_open_type(param_type)
                                && !array_of_any_ok
                                && !self.is_valid_cast(&arg_type, param_type)
                            {
                                return Err((
                                    ErrorType::TypeMismatch {
                                        expected: param_type.to_string(),
                                        found: arg_type.to_string(),
                                    },
                                    "Mismatched argument type".to_string(),
                                    arg.pos.clone(),
                                ));
                            }
                        }

                        Ok(*return_type)
                    } else {
                        Err((
                            ErrorType::TypeError,
                            "Expression is not a function".to_string(),
                            callee.pos.clone(),
                        ))
                    }
                }
            }
            ExpressionKind::StructLiteral { type_name, fields } => {
                if let Some(struct_info) = self.environment.get_struct_type(type_name) {
                    let declared_fields = &struct_info.fields;
                    let mut all_match = true;
                    let mut subst: HashMap<String, Type> = HashMap::default();
                    for (field_name, field_expr) in fields {
                        if !declared_fields.contains_key(field_name) {
                            all_match = false;
                            break;
                        }
                        let field_type = &declared_fields[field_name];
                        let field_type_owned = field_type.clone();
                        let expr_type = self.expression_type_with_hint(
                            field_expr,
                            &self.environment,
                            Some(field_type_owned),
                        )?;
                        if !Self::unify_param_type(field_type, &expr_type, &mut subst) {
                            return Err((
                                ErrorType::TypeMismatch {
                                    expected: field_type.to_string(),
                                    found: expr_type.to_string(),
                                },
                                format!(
                                    "Conflicting types for generic parameter in field '{field_name}'"),
                                field_expr.pos.clone(),
                            ));
                        }
                        let field_has_vars = Self::type_has_vars(field_type);
                        if &expr_type != field_type
                            && !field_has_vars
                            && !self.types_assignable(field_type, &expr_type)
                            && !Self::is_open_type(field_type)
                            && !Self::is_open_type(&expr_type)
                        {
                            return Err((
                                ErrorType::TypeMismatch {
                                    expected: field_type.to_string(),
                                    found: expr_type.to_string(),
                                },
                                format!(
                                    "Expected type '{field_type}' for field '{field_name}', but got '{expr_type}'"),
                                field_expr.pos.clone(),
                            ));
                        }
                    }
                    if all_match {
                        if !struct_info.type_params.is_empty() {
                            if let Some(Type::Struct {
                                name,
                                type_args: hint_args,
                                ..
                            }) = self.hint()
                            {
                                if name == struct_info.name
                                    && hint_args.len() == struct_info.type_params.len()
                                {
                                    for (p, a) in struct_info.type_params.iter().zip(hint_args) {
                                        subst.entry(p.clone()).or_insert(a);
                                    }
                                }
                            }
                        }
                        let type_args: Vec<Type> = struct_info
                            .type_params
                            .iter()
                            .map(|p| subst.get(p).cloned().unwrap_or(Type::Any))
                            .collect();
                        let applied_fields: HashMap<String, Type> = declared_fields
                            .iter()
                            .map(|(k, v)| (k.clone(), v.substitute_type_vars(&subst)))
                            .collect();
                        return Ok(Type::Struct {
                            name: struct_info.name.clone(),
                            fields: alloc::rc::Rc::new(applied_fields),
                            type_args,
                        });
                    }
                    if fields.len() == declared_fields.len() {
                        let mut provided: Vec<(&String, &Expression)> =
                            fields.iter().map(|(n, e)| (n, e)).collect();
                        provided.sort_by(|a, b| a.0.cmp(b.0));
                        let mut decl_names: Vec<(&String, &Type)> =
                            declared_fields.iter().collect();
                        decl_names.sort_by(|a, b| a.0.cmp(b.0));
                        for ((_, expr), (_, ty)) in provided.iter().zip(decl_names.iter()) {
                            let expr_ty =
                                self.get_expression_type_with_environment(expr, &self.environment)?;
                            if &expr_ty != *ty
                                && !self.types_assignable(ty, &expr_ty)
                                && !Self::is_open_type(ty)
                                && !Self::is_open_type(&expr_ty)
                            {
                                return Err((
                                    ErrorType::TypeMismatch {
                                        expected: ty.to_string(),
                                        found: expr_ty.to_string(),
                                    },
                                    "Mismatched field type in positional struct initialization"
                                        .to_string(),
                                    expr.pos.clone(),
                                ));
                            }
                        }
                        let type_args: Vec<Type> = struct_info
                            .type_params
                            .iter()
                            .map(|p| subst.get(p).cloned().unwrap_or(Type::Any))
                            .collect();
                        let applied_fields: HashMap<String, Type> = declared_fields
                            .iter()
                            .map(|(k, v)| (k.clone(), v.substitute_type_vars(&subst)))
                            .collect();
                        return Ok(Type::Struct {
                            name: struct_info.name.clone(),
                            fields: alloc::rc::Rc::new(applied_fields),
                            type_args,
                        });
                    }
                    Err((
                        ErrorType::TypeError,
                        format!("Struct '{type_name}' fields do not match"),
                        expr.pos.clone(),
                    ))
                } else {
                    Err((
                        ErrorType::TypeError,
                        format!("Unknown struct type '{type_name}'"),
                        expr.pos.clone(),
                    ))
                }
            }
            ExpressionKind::MemberAccess { object, member } => {
                let object_type =
                    self.get_expression_type_with_environment(object, &self.environment)?;

                if object_type == Type::Any {
                    return Ok(Type::Any);
                }

                if let Type::Interface {
                    name: iface_name,
                    methods,
                    ..
                } = &object_type
                {
                    if let Some(m) = methods.get(member) {
                        return Ok(Type::FunctionType {
                            param_types: m.param_types.clone(),
                            return_type: Box::new(m.return_type.clone()),
                            type_params: Vec::new(),
                            type_param_bounds: Vec::new(),
                        });
                    }
                    let _ = iface_name;
                    return Err((
                        ErrorType::TypeError,
                        format!("Interface '{iface_name}' has no method named '{member}'"),
                        expr.pos.clone(),
                    ));
                }

                if let Type::Enum {
                    name,
                    type_args,
                    variants,
                    ..
                } = &object_type
                {
                    if variants.iter().any(|v| &v.name == member) {
                        return Ok(object_type.clone());
                    }
                    if let Some(enum_info) = self.environment.get_enum_type(name) {
                        if let Some(method_info) = enum_info.methods.get(member) {
                            let (param_types, return_type) =
                                Self::method_signature_without_self(method_info, name);
                            let subst = self.bind_method_subst(
                                &enum_info.type_params,
                                type_args,
                                None,
                                &[],
                                &return_type,
                                &[],
                            );
                            let param_types: Vec<Type> = param_types
                                .iter()
                                .map(|p| p.substitute_type_vars(&subst))
                                .collect();
                            let return_type = return_type.substitute_type_vars(&subst);
                            return Ok(Type::FunctionType {
                                param_types,
                                return_type: Box::new(return_type),
                                type_params: Vec::new(),
                                type_param_bounds: Vec::new(),
                            });
                        }
                    }
                }

                if let Type::Struct {
                    name,
                    fields,
                    type_args,
                } = object_type
                {
                    if let Some(field_type) = fields.get(member) {
                        Ok(field_type.clone())
                    } else if let Some(struct_info) = self.environment.get_struct_type(&name) {
                        if struct_info.methods.contains_key(member) {
                            let method_info = &struct_info.methods[member];
                            let (param_types, return_type) =
                                Self::method_signature_without_self(method_info, &name);
                            let subst = self.bind_method_subst(
                                &struct_info.type_params,
                                &type_args,
                                None,
                                &[],
                                &return_type,
                                &[],
                            );
                            let param_types: Vec<Type> = param_types
                                .iter()
                                .map(|p| p.substitute_type_vars(&subst))
                                .collect();
                            let return_type = return_type.substitute_type_vars(&subst);
                            Ok(Type::FunctionType {
                                param_types,
                                return_type: Box::new(return_type),
                                type_params: Vec::new(),
                                type_param_bounds: Vec::new(),
                            })
                        } else {
                            Err((
                                ErrorType::TypeError,
                                format!("Struct '{name}' has no field or method named '{member}'"),
                                expr.pos.clone(),
                            ))
                        }
                    } else {
                        Err((
                            ErrorType::TypeError,
                            format!("Unknown struct type '{name}'"),
                            expr.pos.clone(),
                        ))
                    }
                } else {
                    Err((
                        ErrorType::TypeError,
                        format!(
                            "Cannot access member '{member}' on non-struct type '{object_type}'"
                        ),
                        expr.pos.clone(),
                    ))
                }
            }
            ExpressionKind::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    return Ok(Type::Array {
                        element_type: Box::new(Type::Any),
                        len: 0,
                    });
                }
                let mut types: Vec<Type> = Vec::with_capacity(elements.len());
                for el in elements {
                    let t = self.get_expression_type_with_environment(el, &self.environment)?;
                    types.push(t);
                }
                let first = &types[0];
                if types
                    .iter()
                    .all(|t| t == first || Self::is_open_type(t) || Self::is_open_type(first))
                {
                    let et = types
                        .iter()
                        .find(|t| !Self::is_open_type(t))
                        .cloned()
                        .unwrap_or(Type::Any);
                    return Ok(Type::Array {
                        element_type: Box::new(et),
                        len: types.len(),
                    });
                }
                if let Some(iface_ty) = self.common_interface_of(&types, environment) {
                    return Ok(Type::Array {
                        element_type: Box::new(iface_ty),
                        len: types.len(),
                    });
                }
                Err((
                    ErrorType::TypeMismatch {
                        expected: first.to_string(),
                        found: types[1].to_string(),
                    },
                    "Array elements must all have the same type".to_string(),
                    elements[1].pos.clone(),
                ))
            }
            ExpressionKind::ArrayRepeat { value, len } => {
                let value_type =
                    self.get_expression_type_with_environment(value, &self.environment)?;
                let element_type = match self.hint() {
                    Some(Type::Array { element_type, .. })
                        if Self::is_open_type(&value_type)
                            && !Self::is_open_type(&element_type) =>
                    {
                        *element_type
                    }
                    _ => value_type,
                };
                Ok(Type::Array {
                    element_type: Box::new(element_type),
                    len: *len,
                })
            }
            ExpressionKind::Index { object, index } => {
                let object_type =
                    self.get_expression_type_with_environment(object, &self.environment)?;
                let index_type =
                    self.get_expression_type_with_environment(index, &self.environment)?;
                if index_type != Type::Int {
                    return Err((
                        ErrorType::TypeMismatch {
                            expected: "int".to_string(),
                            found: index_type.to_string(),
                        },
                        "Array index must be of type int".to_string(),
                        index.pos.clone(),
                    ));
                }
                match object_type {
                    Type::Array { element_type, .. } => Ok(*element_type),
                    _ => Err((
                        ErrorType::TypeError,
                        format!("Cannot index value of type '{object_type}'"),
                        expr.pos.clone(),
                    )),
                }
            }
        }
    }
}

/// Extract the best position for an error from a resolved import
fn pos_of_import(import: &ResolvedImport) -> crate::Position {
    match import {
        ResolvedImport::Function { pos, .. } | ResolvedImport::ForeignFunction { pos, .. } => {
            pos.clone()
        }
        _ => crate::Position {
            file: String::new(),
            line: 1,
            column: 1,
            offset: 0,
            length: 0,
        },
    }
}

/// The `Type` of a pattern literal
fn pattern_literal_type(lit: &crate::ast::PatternLiteral) -> Type {
    match lit {
        crate::ast::PatternLiteral::Int(_) => Type::Int,
        crate::ast::PatternLiteral::Double(_) => Type::Double,
        crate::ast::PatternLiteral::Bool(_) => Type::Bool,
        crate::ast::PatternLiteral::String(_) => Type::String,
    }
}

/// Coverage of a pattern: (covered enum variant names, contains a wildcard)
fn pattern_coverage(pattern: &crate::ast::Pattern) -> (Vec<String>, bool) {
    use crate::ast::Pattern as P;
    let mut covered: Vec<String> = Vec::new();
    let mut wildcard = false;
    match pattern {
        P::Wildcard => wildcard = true,
        P::Literal(_) | P::Binding(_) => {}
        P::EnumVariant { variant, .. } => covered.push(variant.clone()),
        P::Or(alts) => {
            for alt in alts {
                let (c, w) = pattern_coverage(alt);
                for v in c {
                    if !covered.contains(&v) {
                        covered.push(v);
                    }
                }
                wildcard = wildcard || w;
            }
        }
    }
    (covered, wildcard)
}

/// Bool-literal coverage of a pattern: (covers `true`, covers `false`)
fn pattern_bool_coverage(pattern: &crate::ast::Pattern) -> (bool, bool) {
    use crate::ast::Pattern as P;
    let mut t = false;
    let mut f = false;
    match pattern {
        P::Literal(crate::ast::PatternLiteral::Bool(true)) => t = true,
        P::Literal(crate::ast::PatternLiteral::Bool(false)) => f = true,
        P::Or(alts) => {
            for alt in alts {
                let (nt, nf) = pattern_bool_coverage(alt);
                t = t || nt;
                f = f || nf;
            }
        }
        _ => {}
    }
    (t, f)
}
