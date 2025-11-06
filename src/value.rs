//! Runtime value system for the Frut
//!
//! Provides runtime values and operations for the interpreter.

use crate::{ast::{Parameter, Statement}, types::Type, HashMap};
use alloc::{fmt, string::{String, ToString}, sync::Arc, vec::Vec};
use core::fmt::Debug;

/// A wrapper for native functions that can be called from Frut code
#[derive(Clone)]
pub struct NativeFnWrapper(Arc<dyn Fn(Vec<Value>) -> Result<Value, String> + Send + Sync>);

impl NativeFnWrapper {
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
    Function {
        name: String,
        params: Vec<Parameter>,
        return_type: Type,
        body: Vec<Statement>,
    },
    NativeFunction {
        name: String,
        arity: Option<usize>,
        func: NativeFn,
    },
    Struct {
        type_name: String,
        fields: HashMap<String, Value>,
    },
    Void,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{}", s),
            Value::Int(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Double(n) => write!(f, "{}", n),
            Value::Function { name, .. } => write!(f, "<func {}>", name),
            Value::NativeFunction { name, .. } => write!(f, "<native func {}>", name),
            Value::Struct { type_name, fields } => {
                write!(f, "{} {{ ", type_name)?;
                for (i, (name, value)) in fields.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}: {}", name, value)?;
                }
                write!(f, " }}")
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
            Value::Function { .. } => Type::Function,
            Value::NativeFunction { .. } => Type::Function,
            Value::Struct { type_name, fields } => Type::Struct { 
                name: type_name.clone(),
                fields: fields.iter().map(|(k, v)| (k.clone(), v.get_type())).collect(),
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
    
    /// Create a native function value
    pub fn native_function<F>(name: &str, arity: impl Into<Option<usize>>, func: F) -> Self
    where
        F: Fn(Vec<Value>) -> Result<Value, String> + Send + Sync + 'static,
    {
        Value::NativeFunction {
            name: name.to_string(),
            arity: arity.into(),
            func: NativeFnWrapper(Arc::new(func)),
        }
    }

    /// Perform unary operations
    pub fn unary_op(&self, op: crate::ast::UnaryOperator) -> Result<Self, String> {
        match op {
            crate::ast::UnaryOperator::Not => {
                match self {
                    Value::Bool(b) => Ok(Value::Bool(!b)),
                    _ => Err(format!("Cannot apply '!' operator to {}", self.get_type())),
                }
            }
            crate::ast::UnaryOperator::Plus => {
                match self {
                    Value::Int(n) => Ok(Value::Int(*n)),
                    Value::Double(n) => Ok(Value::Double(*n)),
                    _ => Err(format!("Cannot apply unary '+' to {}", self.get_type())),
                }
            }
            crate::ast::UnaryOperator::Minus => {
                match self {
                    Value::Int(n) => Ok(Value::Int(-*n)),
                    Value::Double(n) => Ok(Value::Double(-*n)),
                    _ => Err(format!("Cannot apply unary '-' to {}", self.get_type())),
                }
            }
        }
    }

    /// Perform binary operations
    pub fn binary_op(&self, op: crate::ast::BinaryOperator, other: &Self) -> Result<Self, String> {
        match op {
            crate::ast::BinaryOperator::Add => {
                match (self, other) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a + b)),
                    (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{a}{b}"))),
                    (Value::String(a), Value::Int(b)) => Ok(Value::String(format!("{a}{b}"))),
                    (Value::String(a), Value::Double(b)) => Ok(Value::String(format!("{a}{b}"))),
                    (Value::String(a), Value::Bool(b)) => Ok(Value::String(format!("{a}{b}"))),
                    _ => Err("Type mismatch in addition".to_string()),
                }
            }
            crate::ast::BinaryOperator::Sub => {
                match (self, other) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a - b)),
                    _ => Err("Type mismatch in subtraction".to_string()),
                }
            }
            crate::ast::BinaryOperator::Mul => {
                match (self, other) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a * b)),
                    _ => Err("Type mismatch in multiplication".to_string()),
                }
            }
            crate::ast::BinaryOperator::Div => {
                match (self, other) {
                    (Value::Int(a), Value::Int(b)) => {
                        if *b == 0 {
                            Err("Division by zero".to_string())
                        } else {
                            Ok(Value::Int(a / b))
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
                }
            }
            crate::ast::BinaryOperator::Mod => {
                match (self, other) {
                    (Value::Int(a), Value::Int(b)) => {
                        if *b == 0 {
                            Err("Division by zero".to_string())
                        } else {
                            Ok(Value::Int(a % b))
                        }
                    }
                    _ => Err("Modulo operation only supported for integers".to_string()),
                }
            }
            crate::ast::BinaryOperator::Less => {
                match (self, other) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                    (Value::Double(a), Value::Double(b)) => Ok(Value::Bool(a < b)),
                    _ => Err("Type mismatch in comparison".to_string()),
                }
            }
            crate::ast::BinaryOperator::LessEqual => {
                match (self, other) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
                    (Value::Double(a), Value::Double(b)) => Ok(Value::Bool(a <= b)),
                    _ => Err("Type mismatch in comparison".to_string()),
                }
            }
            crate::ast::BinaryOperator::Greater => {
                match (self, other) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                    (Value::Double(a), Value::Double(b)) => Ok(Value::Bool(a > b)),
                    _ => Err("Type mismatch in comparison".to_string()),
                }
            }
            crate::ast::BinaryOperator::GreaterEqual => {
                match (self, other) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                    (Value::Double(a), Value::Double(b)) => Ok(Value::Bool(a >= b)),
                    _ => Err("Type mismatch in comparison".to_string()),
                }
            }
            crate::ast::BinaryOperator::Equal => {
                Ok(Value::Bool(self == other))
            }
            crate::ast::BinaryOperator::NotEqual => {
                Ok(Value::Bool(self != other))
            }
            crate::ast::BinaryOperator::LogicalAnd => {
                match (self, other) {
                    (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(*a && *b)),
                    _ => Err("Logical AND requires boolean operands".to_string()),
                }
            }
            crate::ast::BinaryOperator::LogicalOr => {
                match (self, other) {
                    (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(*a || *b)),
                    _ => Err("Logical OR requires boolean operands".to_string()),
                }
            }
        }
    }
}

/// Environment for storing runtime values
#[derive(Debug, Clone)]
pub struct RuntimeEnvironment {
    scopes: Vec<HashMap<String, Value>>,
    functions: HashMap<String, Value>,
    struct_schemas: HashMap<String, Vec<String>>,
}

impl RuntimeEnvironment {
    /// Create a new runtime environment
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::default()],
            functions: HashMap::default(),
            struct_schemas: HashMap::default(),
        }
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

    /// Define a function in the current scope
    pub fn define_function(&mut self, name: String, value: Value) {
        self.functions.insert(name, value);
    }

    /// Get a variable from any scope
    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        self.functions.get(name)
    }

    /// Update a variable in any scope
    pub fn set_variable(&mut self, name: &str, value: Value) -> Result<(), String> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(var_value) = scope.get_mut(name) {
                *var_value = value;
                return Ok(());
            }
        }
        Err(format!("Variable '{}' not found", name))
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

    /// Register a struct schema by name with its field names
    pub fn define_schema(&mut self, name: String, fields: Vec<String>) {
        self.struct_schemas.insert(name, fields);
    }

    /// Get a struct schema field names by name
    pub fn get_schema(&self, name: &str) -> Option<&Vec<String>> {
        self.struct_schemas.get(name)
    }
}
