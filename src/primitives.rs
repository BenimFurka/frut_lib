use crate::{Type, Value};
use alloc::string::{String, ToString};
use alloc::vec::Vec;

#[derive(Clone, Debug)]
pub struct PrimitiveMethodSig {
    pub name: String,
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

fn int_methods() -> Vec<PrimitiveMethodSig> {
    vec![PrimitiveMethodSig { name: "abs".to_string(), param_types: vec![], return_type: Type::Int }]
}

fn double_methods() -> Vec<PrimitiveMethodSig> {
    vec![PrimitiveMethodSig { name: "abs".to_string(), param_types: vec![], return_type: Type::Double }]
}

fn string_methods() -> Vec<PrimitiveMethodSig> {
    vec![
        PrimitiveMethodSig { name: "len".to_string(), param_types: vec![], return_type: Type::Int },
        PrimitiveMethodSig { name: "lower".to_string(), param_types: vec![], return_type: Type::String },
        PrimitiveMethodSig { name: "upper".to_string(), param_types: vec![], return_type: Type::String },
        PrimitiveMethodSig { name: "contains".to_string(), param_types: vec![Type::String], return_type: Type::Bool },
    ]
}

fn bool_methods() -> Vec<PrimitiveMethodSig> {
    vec![PrimitiveMethodSig { name: "to_int".to_string(), param_types: vec![], return_type: Type::Int }]
}

pub fn get_methods_for(ty: &Type) -> Option<Vec<PrimitiveMethodSig>> {
    match ty {
        Type::Int => Some(int_methods()),
        Type::Double => Some(double_methods()),
        Type::String => Some(string_methods()),
        Type::Bool => Some(bool_methods()),
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
            if args.len() != 1 { return Some(Err("contains expects 1 argument".to_string())); }
            match args.remove(0) {
                Value::String(sub) => Some(Ok(Value::Bool(s.contains(&sub)) )),
                other => Some(Err(format!("contains expects string, got {}", other.get_type()))),
            }
        }
        (Value::String(s), "lower") => Some(Ok(Value::String(s.to_lowercase()))),
        (Value::String(s), "upper") => Some(Ok(Value::String(s.to_uppercase()))),
        (Value::Bool(b), "to_int") => Some(Ok(Value::Int(if *b { 1 } else { 0 }))),
        _ => None,
    }
}
