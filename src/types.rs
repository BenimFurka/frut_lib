//! Type system for the Frut
//!
//! Provides type definitions and type checking functionality.

use core::fmt;

use crate::HashMap;
use alloc::{
    boxed::Box,
    rc::Rc,
    string::{String, ToString},
    vec::Vec,
};

/// A variant of an enum type with an optional payload
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    /// Payload type names, e.g. `Some(int)` -> `["int"]`. Empty for a unit variant
    pub payload_types: Vec<Type>,
}

/// A method signature of an interface (without `self`)
#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceMethod {
    pub name: String,
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

/// Supported data types in the Frut
#[derive(Debug, Clone)]
pub enum Type {
    String,
    Int,
    Bool,
    Double,
    /// Raw byte sequence (binary data, no UTF-8 semantics)
    Bytes,
    Function,
    FunctionType {
        param_types: Vec<Type>,
        return_type: Box<Type>,
        /// Generic type parameter names of the declaration
        /// (`func f<T>(x: T)` -> `["T"]`)
        type_params: Vec<String>,
        /// Interface bounds per type parameter (`T: Shape` -> `Some`)
        type_param_bounds: Vec<Option<String>>,
    },
    Struct {
        name: String,
        /// Field name -> type
        fields: Rc<HashMap<String, Type>>,
        /// Generic type arguments: `Box<int>` -> `[int]`. Empty for
        /// non-generic types and unapplied generic declarations
        type_args: Vec<Type>,
    },
    Enum {
        name: String,
        variants: Vec<EnumVariant>,
        /// Generic type arguments: `Option<int>` -> `[int]`. Empty for
        /// non-generic types and unapplied generic declarations
        type_args: Vec<Type>,
    },
    Interface {
        name: String,
        /// Method name -> signature (without `self`)
        methods: HashMap<String, InterfaceMethod>,
        /// Generic type parameter names: `interface Container<T>` -> `["T"]`
        type_params: Vec<String>,
    },
    Tuple {
        element_types: Vec<Type>,
    },
    /// Fixed-length array, e.g. `[int; 3]`
    Array {
        element_type: Box<Type>,
        len: usize,
    },
    /// A generic type variable (`T` from `func f<T>(x: T)`)
    TypeVar {
        name: String,
        /// Interface bound: `T: Shape` -> `Some("Shape")`
        bound: Option<String>,
    },
    /// A value of any type (internal fallback; not expressible in source)
    Any,
    Void,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::String, Type::String) => true,
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Double, Type::Double) => true,
            (Type::Bytes, Type::Bytes) => true,
            (Type::Function, Type::Function) => true,
            (
                Type::FunctionType {
                    param_types: p1,
                    return_type: r1,
                    ..
                },
                Type::FunctionType {
                    param_types: p2,
                    return_type: r2,
                    ..
                },
            ) => p1 == p2 && r1 == r2,
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
            ) => n1 == n2 && a1 == a2,
            (
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
            ) => n1 == n2 && a1 == a2,
            (
                Type::Interface {
                    name: n1,
                    type_params: p1,
                    ..
                },
                Type::Interface {
                    name: n2,
                    type_params: p2,
                    ..
                },
            ) => n1 == n2 && p1 == p2,
            (Type::Tuple { element_types: t1 }, Type::Tuple { element_types: t2 }) => t1 == t2,
            (
                Type::Array {
                    element_type: e1,
                    len: l1,
                },
                Type::Array {
                    element_type: e2,
                    len: l2,
                },
            ) => e1 == e2 && l1 == l2,
            (Type::TypeVar { name: n1, .. }, Type::TypeVar { name: n2, .. }) => n1 == n2,
            (Type::Any, Type::Any) => true,
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
            Type::Bytes => write!(f, "bytes"),
            Type::Function => write!(f, "function"),
            Type::FunctionType {
                param_types,
                return_type,
                ..
            } => {
                let params = param_types
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "func({params}): {return_type}")
            }
            Type::Struct {
                name, type_args, ..
            } => {
                write!(f, "{name}")?;
                if !type_args.is_empty() {
                    write!(
                        f,
                        "<{}>",
                        type_args
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                }
                Ok(())
            }
            Type::Enum {
                name, type_args, ..
            } => {
                write!(f, "{name}")?;
                if !type_args.is_empty() {
                    write!(
                        f,
                        "<{}>",
                        type_args
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                }
                Ok(())
            }
            Type::Interface {
                name, type_params, ..
            } => {
                write!(f, "{name}")?;
                if !type_params.is_empty() {
                    write!(f, "<{}>", type_params.join(", "))?;
                }
                Ok(())
            }
            Type::Tuple { element_types } => {
                let elems = element_types
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({elems})")
            }
            Type::Array { element_type, len } => write!(f, "[{element_type}; {len}]"),
            Type::TypeVar { name, .. } => write!(f, "{name}"),
            Type::Any => write!(f, "any"),
            Type::Void => write!(f, "void"),
        }
    }
}

impl Type {
    /// Mangled runtime name used by the monomorphization pass:
    /// `Box<int>` -> `Box$int`, `Option<Box<int>>` -> `Option$Box$int`
    pub fn mangle(&self) -> String {
        match self {
            Type::Struct {
                name, type_args, ..
            }
            | Type::Enum {
                name, type_args, ..
            } => {
                if type_args.is_empty() {
                    name.clone()
                } else {
                    let mut s = name.clone();
                    for arg in type_args {
                        s.push('$');
                        s.push_str(&arg.mangle());
                    }
                    s
                }
            }
            Type::Interface { name, .. } => name.clone(),
            Type::Array { element_type, len } => {
                format!("arr${}${}", element_type.mangle(), len)
            }
            Type::TypeVar { name, .. } => name.clone(),
            other => other.to_string(),
        }
    }

    /// Substitute type variables in this type
    pub fn substitute_type_vars(&self, subst: &HashMap<String, Type>) -> Type {
        match self {
            Type::TypeVar { name, .. } => subst.get(name).cloned().unwrap_or_else(|| self.clone()),
            Type::Struct {
                name,
                fields,
                type_args,
            } => Type::Struct {
                name: name.clone(),
                fields: Rc::new(
                    fields
                        .iter()
                        .map(|(k, v)| (k.clone(), v.substitute_type_vars(subst)))
                        .collect(),
                ),
                type_args: type_args
                    .iter()
                    .map(|a| a.substitute_type_vars(subst))
                    .collect(),
            },
            Type::Enum {
                name,
                variants,
                type_args,
            } => Type::Enum {
                name: name.clone(),
                variants: variants
                    .iter()
                    .map(|v| crate::types::EnumVariant {
                        name: v.name.clone(),
                        payload_types: v
                            .payload_types
                            .iter()
                            .map(|t| t.substitute_type_vars(subst))
                            .collect(),
                    })
                    .collect(),
                type_args: type_args
                    .iter()
                    .map(|a| a.substitute_type_vars(subst))
                    .collect(),
            },
            Type::Interface {
                name,
                methods,
                type_params,
            } => Type::Interface {
                name: name.clone(),
                methods: methods
                    .iter()
                    .map(|(k, m)| {
                        (
                            k.clone(),
                            crate::types::InterfaceMethod {
                                name: m.name.clone(),
                                param_types: m
                                    .param_types
                                    .iter()
                                    .map(|t| t.substitute_type_vars(subst))
                                    .collect(),
                                return_type: m.return_type.substitute_type_vars(subst),
                            },
                        )
                    })
                    .collect(),
                type_params: type_params.clone(),
            },
            Type::Array { element_type, len } => Type::Array {
                element_type: Box::new(element_type.substitute_type_vars(subst)),
                len: *len,
            },
            Type::Tuple { element_types } => Type::Tuple {
                element_types: element_types
                    .iter()
                    .map(|t| t.substitute_type_vars(subst))
                    .collect(),
            },
            Type::FunctionType {
                param_types,
                return_type,
                ..
            } => Type::FunctionType {
                param_types: param_types
                    .iter()
                    .map(|t| t.substitute_type_vars(subst))
                    .collect(),
                return_type: Box::new(return_type.substitute_type_vars(subst)),
                type_params: Vec::new(),
                type_param_bounds: Vec::new(),
            },
            other => other.clone(),
        }
    }
}

/// Convert a `TypeExpr` to a `Type`, resolving primitive names and arrays
pub fn type_from_expr(expr: &crate::ast::TypeExpr) -> Type {
    match expr {
        crate::ast::TypeExpr::Named(name) => Type::from(name.as_str()),
        crate::ast::TypeExpr::Generic { name, args } => {
            let base = Type::from(name.as_str());
            let type_args: Vec<Type> = args.iter().map(type_from_expr).collect();
            match base {
                Type::Struct { name, fields, .. } => Type::Struct {
                    name,
                    fields,
                    type_args,
                },
                Type::Enum { name, variants, .. } => Type::Enum {
                    name,
                    variants,
                    type_args,
                },
                other => other,
            }
        }
        crate::ast::TypeExpr::Array { element_type, len } => Type::Array {
            element_type: Box::new(type_from_expr(element_type)),
            len: *len,
        },
        crate::ast::TypeExpr::Function { params, ret } => Type::FunctionType {
            param_types: params.iter().map(type_from_expr).collect(),
            return_type: Box::new(type_from_expr(ret)),
            type_params: Vec::new(),
            type_param_bounds: Vec::new(),
        },
        crate::ast::TypeExpr::Void => Type::Void,
        crate::ast::TypeExpr::Inferred => Type::Any,
    }
}

impl From<&str> for Type {
    fn from(s: &str) -> Self {
        if s.starts_with('[') && s.ends_with(']') {
            let inner = &s[1..s.len() - 1];
            if let Some(semi) = inner.rfind(';') {
                let elem_str = inner[..semi].trim();
                let len_str = inner[semi + 1..].trim();
                if let Ok(len) = len_str.parse::<usize>() {
                    let elem = Type::from(elem_str);
                    return Type::Array {
                        element_type: alloc::boxed::Box::new(elem),
                        len,
                    };
                }
            }
        }

        match s {
            "string" => Type::String,
            "int" => Type::Int,
            "bool" => Type::Bool,
            "double" => Type::Double,
            "bytes" => Type::Bytes,
            "function" => Type::Function,
            "void" => Type::Void,
            "any" => Type::Any,
            _ => Type::Struct {
                name: s.to_string(),
                fields: Rc::new(HashMap::default()),
                type_args: Vec::new(),
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
        Self { types: Vec::new() }
    }

    pub fn register_type(&mut self, type_: Type) {
        self.types.push(type_);
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.iter().find(|t| format!("{t}") == name)
    }
}

impl Default for TypeRegistry {
    fn default() -> Self {
        Self::new()
    }
}
