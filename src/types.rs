//! Type system for the Frut
//!
//! Provides type definitions and type checking functionality.

use core::fmt;

use alloc::{boxed::Box, string::{String, ToString}, vec::Vec};
use crate::HashMap;

/// Supported data types in the Frut
#[derive(Debug, Clone)]
pub enum Type {
    String,
    Int,
    Bool,
    Double,
    Function,
    FunctionType {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
    Struct {
        name: String,
        fields: HashMap<String, Type>,
    },
    Void,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::String, Type::String) => true,
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Double, Type::Double) => true,
            (Type::Function, Type::Function) => true,
            (Type::FunctionType { param_types: p1, return_type: r1 }, 
             Type::FunctionType { param_types: p2, return_type: r2 }) => {
                p1 == p2 && r1 == r2
            },
            (Type::Struct { name: n1, .. }, Type::Struct { name: n2, .. }) => {
                n1 == n2
            },
            (Type::Void, Type::Void) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::String => write!(f, "string"),
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Double => write!(f, "double"),
            Type::Function => write!(f, "function"),
            Type::FunctionType { param_types, return_type } => {
                let params = param_types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "func({}) -> {}", params, return_type)
            }
            Type::Struct { name, .. } => write!(f, "{}", name),
            Type::Void => write!(f, "void"),
        }
    }
}

impl From<&str> for Type {
    fn from(s: &str) -> Self {
        match s {
            "string" => Type::String,
            "int" => Type::Int,
            "bool" => Type::Bool,
            "double" => Type::Double,
            "function" => Type::Function,
            "void" => Type::Void,
            _ => Type::Struct {
                name: s.to_string(),
                fields: HashMap::default(),
            },
        }
    }
}

/// Type registry for managing available types
#[derive(Debug, Clone)]
pub struct TypeRegistry {
    types: Vec<Type>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
        }
    }

    pub fn register_type(&mut self, type_: Type) {
        self.types.push(type_);
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.iter().find(|t| format!("{}", t) == name)
    }
}

impl Default for TypeRegistry {
    fn default() -> Self {
        Self::new()
    }
}
