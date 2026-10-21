use crate::{Type, Value};
use alloc::boxed::Box;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

#[derive(Clone, Debug)]
pub struct PrimitiveMethodSig {
    pub name: String,
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

fn int_methods() -> Vec<PrimitiveMethodSig> {
    vec![PrimitiveMethodSig {
        name: "abs".to_string(),
        param_types: vec![],
        return_type: Type::Int,
    }]
}

fn double_methods() -> Vec<PrimitiveMethodSig> {
    vec![PrimitiveMethodSig {
        name: "abs".to_string(),
        param_types: vec![],
        return_type: Type::Double,
    }]
}

fn string_methods() -> Vec<PrimitiveMethodSig> {
    vec![
        PrimitiveMethodSig {
            name: "len".to_string(),
            param_types: vec![],
            return_type: Type::Int,
        },
        PrimitiveMethodSig {
            name: "lower".to_string(),
            param_types: vec![],
            return_type: Type::String,
        },
        PrimitiveMethodSig {
            name: "upper".to_string(),
            param_types: vec![],
            return_type: Type::String,
        },
        PrimitiveMethodSig {
            name: "contains".to_string(),
            param_types: vec![Type::String],
            return_type: Type::Bool,
        },
    ]
}

fn bool_methods() -> Vec<PrimitiveMethodSig> {
    vec![PrimitiveMethodSig {
        name: "to_int".to_string(),
        param_types: vec![],
        return_type: Type::Int,
    }]
}

/// Methods for bytes values
fn bytes_methods() -> Vec<PrimitiveMethodSig> {
    vec![
        PrimitiveMethodSig {
            name: "len".to_string(),
            param_types: vec![],
            return_type: Type::Int,
        },
        PrimitiveMethodSig {
            name: "at".to_string(),
            param_types: vec![Type::Int],
            return_type: Type::Int,
        },
        PrimitiveMethodSig {
            name: "slice".to_string(),
            param_types: vec![Type::Int, Type::Int],
            return_type: Type::Bytes,
        },
    ]
}

/// Methods for array values
fn array_methods() -> Vec<PrimitiveMethodSig> {
    vec![PrimitiveMethodSig {
        name: "len".to_string(),
        param_types: vec![],
        return_type: Type::Int,
    }]
}

/// Auto-generated methods for enum values
fn enum_methods(ty: &Type) -> Vec<PrimitiveMethodSig> {
    let mut out = Vec::new();
    if let Type::Enum { variants, .. } = ty {
        // `is_Variant()` for each variant
        for variant in variants {
            out.push(PrimitiveMethodSig {
                name: format!("is_{}", variant.name),
                param_types: vec![],
                return_type: Type::Bool,
            });
        }

        let max_len = variant_payload_types(ty).len();
        let element_type = common_payload_element_type(ty);
        out.push(PrimitiveMethodSig {
            name: "payload".to_string(),
            param_types: vec![],
            return_type: Type::Array {
                element_type: Box::new(element_type),
                len: max_len,
            },
        });
    }
    out
}

fn variant_payload_types(ty: &Type) -> Vec<Type> {
    if let Type::Enum { variants, .. } = ty {
        variants
            .iter()
            .flat_map(|v| v.payload_types.iter().cloned())
            .collect()
    } else {
        Vec::new()
    }
}

/// The statically-known element type of `payload()`
fn common_payload_element_type(ty: &Type) -> Type {
    if let Type::Enum { variants, .. } = ty {
        let mut first: Option<&Type> = None;
        let mut agreed = true;
        let mut any_payload = false;
        for v in variants {
            for t in v.payload_types.iter() {
                any_payload = true;
                match first {
                    None => first = Some(t),
                    Some(prev) => {
                        if prev != t {
                            agreed = false;
                        }
                    }
                }
            }
        }
        if any_payload && agreed {
            if let Some(t) = first {
                return t.clone();
            }
        }
    }
    Type::Any
}

pub fn get_methods_for(ty: &Type) -> Option<Vec<PrimitiveMethodSig>> {
    match ty {
        Type::Int => Some(int_methods()),
        Type::Double => Some(double_methods()),
        Type::String => Some(string_methods()),
        Type::Bool => Some(bool_methods()),
        Type::Bytes => Some(bytes_methods()),
        Type::Enum { .. } => Some(enum_methods(ty)),
        Type::Array { .. } => Some(array_methods()),
        _ => None,
    }
}

pub fn find_method_sig(ty: &Type, name: &str) -> Option<PrimitiveMethodSig> {
    get_methods_for(ty).and_then(|v| v.into_iter().find(|m| m.name == name))
}

pub fn call(receiver: &Value, name: &str, mut args: Vec<Value>) -> Option<Result<Value, String>> {
    match (receiver, name) {
        (Value::Int(i), "abs") => Some(Ok(Value::Int(i.abs()))),
        (Value::Double(d), "abs") => Some(Ok(Value::Double(d.abs()))),
        (Value::String(s), "len") => Some(Ok(Value::Int(s.chars().count() as i64))),
        (Value::String(s), "contains") => {
            if args.len() != 1 {
                return Some(Err("contains expects 1 argument".to_string()));
            }
            match args.remove(0) {
                Value::String(sub) => Some(Ok(Value::Bool(s.contains(&sub)))),
                other => Some(Err(format!(
                    "contains expects string, got {}",
                    other.get_type()
                ))),
            }
        }
        (Value::String(s), "lower") => Some(Ok(Value::String(s.to_lowercase()))),
        (Value::String(s), "upper") => Some(Ok(Value::String(s.to_uppercase()))),
        (Value::Bool(b), "to_int") => Some(Ok(Value::Int(if *b { 1 } else { 0 }))),
        (Value::Bytes(b), "len") => Some(Ok(Value::Int(b.len() as i64))),
        (Value::Bytes(b), "at") => {
            if args.len() != 1 {
                return Some(Err("at expects 1 argument".to_string()));
            }
            match args.remove(0) {
                Value::Int(i) if i >= 0 && (i as usize) < b.len() => {
                    Some(Ok(Value::Int(b[i as usize] as i64)))
                }
                Value::Int(_) => Some(Err("at: index out of range".to_string())),
                other => Some(Err(format!("at expects int, got {}", other.get_type()))),
            }
        }
        (Value::Bytes(b), "slice") => {
            if args.len() != 2 {
                return Some(Err("slice expects 2 arguments".to_string()));
            }
            let a = match args.remove(0) {
                Value::Int(i) => i,
                other => return Some(Err(format!("slice expects int, got {}", other.get_type()))),
            };
            let e = match args.remove(0) {
                Value::Int(i) => i,
                other => return Some(Err(format!("slice expects int, got {}", other.get_type()))),
            };
            let a = (a.max(0) as usize).min(b.len());
            let e = (e.max(0) as usize).min(b.len());
            if a > e {
                return Some(Err("slice: bad range".to_string()));
            }
            Some(Ok(Value::Bytes(b[a..e].to_vec())))
        }
        (Value::Array { elements, .. }, "len") => Some(Ok(Value::Int(elements.len() as i64))),
        (
            Value::Enum {
                variant,
                payload: _,
                ..
            },
            name,
        ) if name.starts_with("is_") => Some(Ok(Value::Bool(format!("is_{variant}") == *name))),
        (Value::Enum { payload, .. }, "payload") => {
            let element_type = payload.first().map(|v| v.get_type()).unwrap_or(Type::Any);
            Some(Ok(Value::Array {
                element_type,
                elements: payload.clone(),
            }))
        }
        _ => None,
    }
}
