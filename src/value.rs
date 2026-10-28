//! Runtime value system for the Frut
//!
//! Provides runtime values and operations for the interpreter.

use crate::{
    ast::{Parameter, Statement},
    types::Type,
    HashMap,
};
use alloc::{
    boxed::Box,
    fmt,
    string::{String, ToString},
    sync::Arc,
    vec::Vec,
};
use core::fmt::Debug;

/// A wrapper for native functions that can be called from Frut code
#[derive(Clone)]
pub struct NativeFnWrapper(Arc<dyn Fn(Vec<Value>) -> Result<Value, String> + Send + Sync>);

impl NativeFnWrapper {
    /// Create a new native function wrapper
    pub fn new<F>(func: F) -> Self
    where
        F: Fn(Vec<Value>) -> Result<Value, String> + Send + Sync + 'static,
    {
        Self(Arc::new(func))
    }

    /// Call the native function with the given arguments
    pub fn call(&self, args: Vec<Value>) -> Result<Value, String> {
        (self.0)(args)
    }
}

impl Debug for NativeFnWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NativeFn")
    }
}

impl PartialEq for NativeFnWrapper {
    fn eq(&self, other: &Self) -> bool {
        let self_ptr = self.0.as_ref() as *const _ as *const ();
        let other_ptr = other.0.as_ref() as *const _ as *const ();
        self_ptr == other_ptr
    }
}

/// Type alias for native function implementation
pub type NativeFn = NativeFnWrapper;

/// Runtime values for the Frut
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Int(i64),
    Bool(bool),
    Double(f64),
    /// Raw byte sequence (binary data). Created by natives (fs/pty/events);
    /// no source-level literal - use `bytes_from_*` bridges
    Bytes(alloc::vec::Vec<u8>),
    Function {
        name: String,
        /// Module (domain) this function was declared in, e.g. "self", "std", "std.io"
        module: String,
        params: Vec<Parameter>,
        return_type: Type,
        body: Arc<Vec<Statement>>,
        bound_self: Option<Box<Value>>,
    },
    NativeFunction {
        name: String,
        arity: Option<usize>,
        /// Declared signature of the `foreign func`. 
        param_types: Vec<Type>,
        return_type: Type,
        func: NativeFn,
    },
    Struct {
        type_name: String,
        fields: HashMap<String, Value>,
    },
    Enum {
        type_name: String,
        variant: String,
        payload: Vec<Value>,
    },
    Tuple(Vec<Value>),
    Array {
        element_type: Type,
        elements: Vec<Value>,
    },
    Void,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{s}"),
            Value::Int(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Double(n) => write!(f, "{n}"),
            Value::Bytes(b) => {
                write!(f, "bytes[")?;
                for (i, byte) in b.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{byte:02x}")?;
                }
                write!(f, "]")
            }
            Value::Function { name, .. } => write!(f, "<func {name}>"),
            Value::NativeFunction { name, .. } => write!(f, "<native func {name}>"),
            Value::Struct { type_name, fields } => {
                write!(f, "{type_name} {{ ")?;
                for (i, (name, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{name}: {value}")?;
                }
                write!(f, " }}")
            }
            Value::Enum {
                type_name,
                variant,
                payload,
            } => {
                write!(f, "{type_name}.{variant}")?;
                if !payload.is_empty() {
                    write!(f, "(")?;
                    for (i, value) in payload.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{value}")?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Value::Tuple(elements) => {
                write!(f, "(")?;
                for (i, value) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?;
                }
                write!(f, ")")
            }
            Value::Array { elements, .. } => {
                write!(f, "[")?;
                for (i, value) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?;
                }
                write!(f, "]")
            }
            Value::Void => write!(f, "void"),
        }
    }
}

impl Value {
    /// Get the type of this value
    pub fn get_type(&self) -> Type {
        match self {
            Value::String(_) => Type::String,
            Value::Int(_) => Type::Int,
            Value::Bool(_) => Type::Bool,
            Value::Double(_) => Type::Double,
            Value::Bytes(_) => Type::Bytes,
            Value::Function {
                params,
                return_type,
                bound_self,
                ..
            } => {
                let skip = usize::from(bound_self.is_some());
                Type::FunctionType {
                    param_types: params
                        .iter()
                        .skip(skip)
                        .map(|p| crate::types::type_from_expr(&p.param_type))
                        .collect(),
                    return_type: Box::new(return_type.clone()),
                    type_params: Vec::new(),
                    type_param_bounds: Vec::new(),
                }
            }
            Value::NativeFunction {
                param_types,
                return_type,
                ..
            } => Type::FunctionType {
                param_types: param_types.clone(),
                return_type: Box::new(return_type.clone()),
                type_params: Vec::new(),
                type_param_bounds: Vec::new(),
            },
            Value::Struct { type_name, fields } => Type::Struct {
                name: type_name.clone(),
                fields: alloc::rc::Rc::new(
                    fields
                        .iter()
                        .map(|(k, v)| (k.clone(), v.get_type()))
                        .collect(),
                ),
                type_args: Vec::new(),
            },
            Value::Enum {
                type_name,
                variant,
                payload,
            } => Type::Enum {
                name: type_name.clone(),
                variants: vec![crate::types::EnumVariant {
                    name: variant.clone(),
                    payload_types: payload.iter().map(|v| v.get_type()).collect(),
                }],
                type_args: Vec::new(),
            },
            Value::Tuple(elements) => Type::Tuple {
                element_types: elements.iter().map(|v| v.get_type()).collect(),
            },
            Value::Array {
                element_type,
                elements,
            } => Type::Array {
                element_type: Box::new(element_type.clone()),
                len: elements.len(),
            },
            Value::Void => Type::Void,
        }
    }

    /// Create a value from a boolean literal
    pub fn from_bool(b: bool) -> Self {
        Value::Bool(b)
    }

    /// Create a value from an integer literal
    pub fn from_int(n: i64) -> Self {
        Value::Int(n)
    }

    /// Create a value from a double literal
    pub fn from_double(n: f64) -> Self {
        Value::Double(n)
    }

    /// Create a value from a string literal
    pub fn from_string(s: String) -> Self {
        Value::String(s)
    }

    /// Create a native function value.
    pub fn native_function<F>(name: &str, arity: impl Into<Option<usize>>, func: F) -> Self
    where
        F: Fn(Vec<Value>) -> Result<Value, String> + Send + Sync + 'static,
    {
        let arity = arity.into();
        Value::NativeFunction {
            name: name.to_string(),
            param_types: (0..arity.unwrap_or(0)).map(|_| Type::Any).collect(),
            return_type: Type::Any,
            arity,
            func: NativeFnWrapper(Arc::new(func)),
        }
    }

    /// Create a native function value with a known signature, so it can be
    /// stored in a slot annotated as `func(...)`.
    pub fn native_function_typed<F>(
        name: &str,
        param_types: Vec<Type>,
        return_type: Type,
        func: F,
    ) -> Self
    where
        F: Fn(Vec<Value>) -> Result<Value, String> + Send + Sync + 'static,
    {
        Value::NativeFunction {
            name: name.to_string(),
            arity: Some(param_types.len()),
            param_types,
            return_type,
            func: NativeFnWrapper(Arc::new(func)),
        }
    }

    /// Perform unary operations
    pub fn unary_op(&self, op: crate::ast::UnaryOperator) -> Result<Self, String> {
        match op {
            crate::ast::UnaryOperator::Not => match self {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                _ => Err(format!("Cannot apply '!' operator to {}", self.get_type())),
            },
            crate::ast::UnaryOperator::Plus => match self {
                Value::Int(n) => Ok(Value::Int(*n)),
                Value::Double(n) => Ok(Value::Double(*n)),
                _ => Err(format!("Cannot apply unary '+' to {}", self.get_type())),
            },
            crate::ast::UnaryOperator::Minus => match self {
                Value::Int(n) => n
                    .checked_neg()
                    .map(Value::Int)
                    .ok_or_else(|| "Integer overflow in unary '-'".to_string()),
                Value::Double(n) => Ok(Value::Double(-*n)),
                _ => Err(format!("Cannot apply unary '-' to {}", self.get_type())),
            },
            crate::ast::UnaryOperator::BitNot => match self {
                Value::Int(n) => Ok(Value::Int(!n)),
                _ => Err(format!("Cannot apply '~' operator to {}", self.get_type())),
            },
        }
    }

    /// Perform binary operations
    pub fn binary_op(&self, op: crate::ast::BinaryOperator, other: &Self) -> Result<Self, String> {
        match op {
            crate::ast::BinaryOperator::Add => match (self, other) {
                (Value::Int(a), Value::Int(b)) => a
                    .checked_add(*b)
                    .map(Value::Int)
                    .ok_or_else(|| "Integer overflow in addition".to_string()),
                (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a + b)),
                (Value::Bytes(a), Value::Bytes(b)) => {
                    let mut out = a.clone();
                    out.extend_from_slice(b);
                    Ok(Value::Bytes(out))
                }
                (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{a}{b}"))),
                (Value::String(a), Value::Int(b)) => Ok(Value::String(format!("{a}{b}"))),
                (Value::String(a), Value::Double(b)) => Ok(Value::String(format!("{a}{b}"))),
                (Value::String(a), Value::Bool(b)) => Ok(Value::String(format!("{a}{b}"))),
                (Value::String(a), b) => Ok(Value::String(format!("{a}{b}"))),
                (a, Value::String(b)) => Ok(Value::String(format!("{a}{b}"))),
                _ => Err("Type mismatch in addition".to_string()),
            },
            crate::ast::BinaryOperator::Sub => match (self, other) {
                (Value::Int(a), Value::Int(b)) => a
                    .checked_sub(*b)
                    .map(Value::Int)
                    .ok_or_else(|| "Integer overflow in subtraction".to_string()),
                (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a - b)),
                _ => Err("Type mismatch in subtraction".to_string()),
            },
            crate::ast::BinaryOperator::Mul => match (self, other) {
                (Value::Int(a), Value::Int(b)) => a
                    .checked_mul(*b)
                    .map(Value::Int)
                    .ok_or_else(|| "Integer overflow in multiplication".to_string()),
                (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a * b)),
                _ => Err("Type mismatch in multiplication".to_string()),
            },
            crate::ast::BinaryOperator::Div => match (self, other) {
                (Value::Int(a), Value::Int(b)) => {
                    if *b == 0 {
                        Err("Division by zero".to_string())
                    } else {
                        a.checked_div(*b)
                            .map(Value::Int)
                            .ok_or_else(|| "Integer overflow in division".to_string())
                    }
                }
                (Value::Double(a), Value::Double(b)) => {
                    if *b == 0.0 {
                        Err("Division by zero".to_string())
                    } else {
                        Ok(Value::Double(a / b))
                    }
                }
                _ => Err("Type mismatch in division".to_string()),
            },
            crate::ast::BinaryOperator::Mod => match (self, other) {
                (Value::Int(a), Value::Int(b)) => {
                    if *b == 0 {
                        Err("Division by zero".to_string())
                    } else {
                        a.checked_rem(*b)
                            .map(Value::Int)
                            .ok_or_else(|| "Integer overflow in modulo".to_string())
                    }
                }
                _ => Err("Modulo operation only supported for integers".to_string()),
            },
            crate::ast::BinaryOperator::Less => match (self, other) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                (Value::Double(a), Value::Double(b)) => Ok(Value::Bool(a < b)),
                _ => Err("Type mismatch in comparison".to_string()),
            },
            crate::ast::BinaryOperator::LessEqual => match (self, other) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
                (Value::Double(a), Value::Double(b)) => Ok(Value::Bool(a <= b)),
                _ => Err("Type mismatch in comparison".to_string()),
            },
            crate::ast::BinaryOperator::Greater => match (self, other) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                (Value::Double(a), Value::Double(b)) => Ok(Value::Bool(a > b)),
                _ => Err("Type mismatch in comparison".to_string()),
            },
            crate::ast::BinaryOperator::GreaterEqual => match (self, other) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                (Value::Double(a), Value::Double(b)) => Ok(Value::Bool(a >= b)),
                _ => Err("Type mismatch in comparison".to_string()),
            },
            crate::ast::BinaryOperator::Equal => match (self, other) {
                (
                    Value::Enum {
                        type_name: t1,
                        variant: v1,
                        payload: p1,
                    },
                    Value::Enum {
                        type_name: t2,
                        variant: v2,
                        payload: p2,
                    },
                ) => {
                    let variants_equal = t1 == t2 && v1 == v2;
                    if !(variants_equal && p1.len() == p2.len()) {
                        Ok(Value::Bool(false))
                    } else {
                        let mut equal = true;
                        for (a, b) in p1.iter().zip(p2.iter()) {
                            if !values_equal(a, b) {
                                equal = false;
                                break;
                            }
                        }
                        Ok(Value::Bool(equal))
                    }
                }
                _ => Ok(Value::Bool(self == other)),
            },
            crate::ast::BinaryOperator::NotEqual => match (self, other) {
                (Value::Enum { .. }, Value::Enum { .. }) => {
                    let eq = match self.binary_op(crate::ast::BinaryOperator::Equal, other) {
                        Ok(Value::Bool(b)) => b,
                        _ => false,
                    };
                    Ok(Value::Bool(!eq))
                }
                _ => Ok(Value::Bool(self != other)),
            },
            crate::ast::BinaryOperator::LogicalAnd => match (self, other) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(*a && *b)),
                _ => Err("Logical AND requires boolean operands".to_string()),
            },
            crate::ast::BinaryOperator::LogicalOr => match (self, other) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(*a || *b)),
                _ => Err("Logical OR requires boolean operands".to_string()),
            },
            crate::ast::BinaryOperator::BitAnd => match (self, other) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
                _ => Err("Bitwise AND requires integer operands".to_string()),
            },
            crate::ast::BinaryOperator::BitOr => match (self, other) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
                _ => Err("Bitwise OR requires integer operands".to_string()),
            },
            crate::ast::BinaryOperator::BitXor => match (self, other) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
                _ => Err("Bitwise XOR requires integer operands".to_string()),
            },
            crate::ast::BinaryOperator::ShiftLeft => match (self, other) {
                (Value::Int(a), Value::Int(b)) => {
                    if *b < 0 || *b >= 64 {
                        Err("Shift amount out of range".to_string())
                    } else {
                        Ok(Value::Int(a << b))
                    }
                }
                _ => Err("Shift left requires integer operands".to_string()),
            },
            crate::ast::BinaryOperator::ShiftRight => match (self, other) {
                (Value::Int(a), Value::Int(b)) => {
                    if *b < 0 || *b >= 64 {
                        Err("Shift amount out of range".to_string())
                    } else {
                        Ok(Value::Int(a >> b))
                    }
                }
                _ => Err("Shift right requires integer operands".to_string()),
            },
        }
    }
}

/// Compare two values structurally, tolerating `f64` (NaN == NaN) and
/// functions (compared by identity)
fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Double(x), Value::Double(y)) => x.to_bits() == y.to_bits(),
        (Value::Function { .. }, Value::Function { .. }) => a == b,
        (Value::NativeFunction { .. }, Value::NativeFunction { .. }) => a == b,
        (
            Value::Struct {
                type_name: t1,
                fields: f1,
            },
            Value::Struct {
                type_name: t2,
                fields: f2,
            },
        ) => {
            if t1 != t2 || f1.len() != f2.len() {
                return false;
            }
            f1.iter().all(|(k, v)| match f2.get(k) {
                Some(v2) => values_equal(v, v2),
                None => false,
            })
        }
        (
            Value::Enum {
                type_name: t1,
                variant: v1,
                payload: p1,
            },
            Value::Enum {
                type_name: t2,
                variant: v2,
                payload: p2,
            },
        ) => {
            if t1 != t2 || v1 != v2 || p1.len() != p2.len() {
                return false;
            }
            p1.iter().zip(p2.iter()).all(|(x, y)| values_equal(x, y))
        }
        (Value::Tuple(t1), Value::Tuple(t2)) => {
            t1.len() == t2.len() && t1.iter().zip(t2.iter()).all(|(x, y)| values_equal(x, y))
        }
        _ => a == b,
    }
}

/// Runtime schema of an enum: variant name -> payload type names
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantSchema {
    pub name: String,
    pub payload_types: Vec<Type>,
}

/// Environment for storing runtime values with module support
#[derive(Debug, Clone)]
pub struct RuntimeEnvironment {
    scopes: Vec<HashMap<String, Value>>,
    // Replace flat functions with module-based structure
    modules: HashMap<String, ModuleEnvironment>,
    /// Per-module native layers: module domain -> (native name -> value)
    natives: HashMap<String, HashMap<String, Value>>,
    current_module: String,
    struct_schemas: HashMap<String, Vec<String>>,
    enum_schemas: HashMap<String, Vec<EnumVariantSchema>>,
    /// Imported aliases per module: module -> (local name -> canonical name)
    /// E.g. in module "self", "println" -> "std.io__println"
    aliases: HashMap<String, HashMap<String, String>>,
    /// Declared interface names
    interfaces: alloc::collections::BTreeSet<String>,
}

/// A module environment containing symbols for a specific module
#[derive(Debug, Clone)]
pub struct ModuleEnvironment {
    functions: HashMap<String, Value>,
    types: HashMap<String, TypeInfo>,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub name: String,
    pub fields: Vec<String>,
}

impl Default for RuntimeEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

impl RuntimeEnvironment {
    /// Create a new runtime environment
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::default()],
            modules: HashMap::default(),
            natives: HashMap::default(),
            current_module: "self".to_string(), // Default to current project
            struct_schemas: HashMap::default(),
            enum_schemas: HashMap::default(),
            aliases: HashMap::default(),
            interfaces: alloc::collections::BTreeSet::new(),
        }
    }

    /// Create a new runtime environment with a specific current module
    pub fn with_module(module_name: String) -> Self {
        let mut env = Self::new();
        env.current_module = module_name;
        env
    }

    /// Get or create a module environment
    fn get_module(&mut self, module_name: &str) -> &mut ModuleEnvironment {
        if !self.modules.contains_key(module_name) {
            self.modules.insert(
                module_name.to_string(),
                ModuleEnvironment {
                    functions: HashMap::default(),
                    types: HashMap::default(),
                },
            );
        }
        self.modules.get_mut(module_name).unwrap()
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::default());
    }

    /// Exit the current scope
    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Define a variable in the current scope
    pub fn define_variable(&mut self, name: String, value: Value) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.insert(name, value);
    }

    /// Define a function in the current module
    pub fn define_function(&mut self, name: String, value: Value) {
        let current_module = self.current_module.clone();
        let module = self.get_module(&current_module);
        module.functions.insert(name, value);
    }

    /// Define a function in a specific module
    pub fn define_function_in_module(&mut self, module_name: &str, name: String, value: Value) {
        let module = self.get_module(module_name);
        module.functions.insert(name, value);
    }

    /// Define a type in the current module
    pub fn define_type(&mut self, name: String, type_info: TypeInfo) {
        let current_module = self.current_module.clone();
        let module = self.get_module(&current_module);
        module.types.insert(name, type_info);
    }

    /// Get a variable from any scope, then from the current module's functions,
    /// then from the current module's native layer.
    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        // First check variable scopes
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }

        // Then check functions (for backward compatibility)
        self.get_function(name).or_else(|| self.get_native(name))
    }

    /// Get a function from the current module only.
    pub fn get_function(&self, name: &str) -> Option<&Value> {
        self.modules
            .get(&self.current_module)
            .and_then(|module| module.functions.get(name))
    }

    /// Get a function from a specific module.
    pub fn get_function_from_module(&self, module_name: &str, name: &str) -> Option<&Value> {
        if let Some(module) = self.modules.get(module_name) {
            module.functions.get(name)
        } else {
            None
        }
    }

    /// Bind a native function into the module's native layer
    pub fn bind_native(&mut self, module: String, name: String, value: Value) {
        self.natives.entry(module).or_default().insert(name, value);
    }

    /// Get a native function from the current module's native layer
    pub fn get_native(&self, name: &str) -> Option<&Value> {
        if let Some(value) = self.get_native_in(&self.current_module, name) {
            return Some(value);
        }
        for (_, module_natives) in &self.natives {
            if let Some(value) = module_natives.get(name) {
                return Some(value);
            }
        }
        None
    }

    /// Get a native function from an explicit module domain
    pub fn get_native_in(&self, module: &str, name: &str) -> Option<&Value> {
        self.natives.get(module).and_then(|m| m.get(name))
    }

    /// Update a variable in any scope
    pub fn set_variable(&mut self, name: &str, value: Value) -> Result<(), String> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(var_value) = scope.get_mut(name) {
                *var_value = value;
                return Ok(());
            }
        }
        Err(format!("Variable '{name}' not found"))
    }

    /// Get all variables from all scopes as a vector of (name, value) pairs
    pub fn get_all_variables(&self) -> Vec<(String, &Value)> {
        let mut all_vars = Vec::new();
        for scope in &self.scopes {
            for (name, value) in scope {
                all_vars.push((name.clone(), value));
            }
        }
        all_vars
    }

    /// Define an alias for an imported symbol in a module
    pub fn define_alias(&mut self, module: &str, local: String, canonical: String) {
        let entry = self.aliases.entry(module.to_string()).or_default();
        entry.insert(local, canonical);
    }

    /// Look up the canonical name for an imported alias in a module
    pub fn get_alias(&self, module: &str, local: &str) -> Option<&String> {
        self.aliases.get(module).and_then(|m| m.get(local))
    }

    /// Get all aliases defined in a module
    pub fn aliases_in(&self, module: &str) -> Vec<(String, String)> {
        self.aliases
            .get(module)
            .map(|m| m.iter().map(|(k, v)| (k.clone(), v.clone())).collect())
            .unwrap_or_default()
    }

    /// Get all function names defined in a module
    pub fn module_symbols(&self, module_name: &str) -> Vec<(String, Value)> {
        self.modules
            .get(module_name)
            .map(|m| {
                m.functions
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Register a struct schema by name with its field names
    pub fn define_schema(&mut self, name: String, fields: Vec<String>) {
        self.struct_schemas.insert(name, fields);
    }

    /// Get a struct schema field names by name
    pub fn get_schema(&self, name: &str) -> Option<&Vec<String>> {
        self.struct_schemas.get(name)
    }

    /// Register an enum schema by name with its variants
    pub fn define_enum_schema(&mut self, name: String, variants: Vec<EnumVariantSchema>) {
        self.enum_schemas.insert(name, variants);
    }

    /// Get an enum schema by name
    pub fn get_enum_schema(&self, name: &str) -> Option<&Vec<EnumVariantSchema>> {
        self.enum_schemas.get(name)
    }

    /// Register an interface name
    pub fn define_interface(&mut self, name: String) {
        self.interfaces.insert(name);
    }

    /// Whether a name is a declared interface
    pub fn is_interface(&self, name: &str) -> bool {
        self.interfaces.contains(name)
    }

    /// Set current module
    pub fn set_current_module(&mut self, module_name: String) {
        self.current_module = module_name;
    }

    /// Get current module name
    pub fn get_current_module(&self) -> &str {
        &self.current_module
    }

    /// Get all modules
    pub fn get_modules(&self) -> &HashMap<String, ModuleEnvironment> {
        &self.modules
    }
}
