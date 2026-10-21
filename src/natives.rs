//! Native function registry (interface only, no implementations).
//!
//! `frut_lib` stays `no_std` and does not ship any native implementations.
//! Hosts (e.g. `frut_interp`) register their implementations into the
//! registry.

use crate::types::Type;
use crate::value::{NativeFnWrapper, RuntimeEnvironment, Value};
use crate::HashMap;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

/// Metadata + implementation of a native (foreign) function
#[derive(Clone)]
pub struct NativeFnDef {
    /// Function name as declared by `foreign func <name>`
    pub name: &'static str,
    /// Fully-qualified module domain the native belongs to, e.g. `"std.native.io"`
    ///
    /// A native declared in `std/native/io.ft` is bound into the `std.native.io`
    /// module domain
    pub module: &'static str,
    /// Named parameter types: `(param_name, param_type)`
    pub params: Vec<(String, Type)>,
    /// Return type
    pub return_type: Type,
    /// The actual implementation
    pub func: NativeFnWrapper,
}

impl core::fmt::Debug for NativeFnDef {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("NativeFnDef")
            .field("name", &self.name)
            .field("params", &self.params)
            .field("return_type", &self.return_type)
            .finish_non_exhaustive()
    }
}

/// Registry of native functions. Interface only - implementations are added by the host
#[derive(Debug, Default, Clone)]
pub struct NativeRegistry {
    funcs: HashMap<String, NativeFnDef>,
}

impl NativeRegistry {
    /// Create an empty registry
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a native function. If a function with the same name already exists,
    /// it is replaced
    pub fn register(&mut self, func: NativeFnDef) {
        self.funcs.insert(func.name.to_string(), func);
    }

    /// Look up a native function by name
    pub fn get(&self, name: &str) -> Option<&NativeFnDef> {
        self.funcs.get(name)
    }

    /// Check whether a native function with the given name is registered
    pub fn contains(&self, name: &str) -> bool {
        self.funcs.contains_key(name)
    }

    /// Number of registered natives
    pub fn len(&self) -> usize {
        self.funcs.len()
    }

    /// Whether the registry is empty
    pub fn is_empty(&self) -> bool {
        self.funcs.is_empty()
    }

    /// Bind all registered natives into the runtime environment's per-module
    /// native layers (one `HashMap` per module domain).
    ///
    /// Each native is stored under its short name inside the module domain
    /// declared by [`NativeFnDef::module`], so e.g. a native registered with
    /// `module = "std.native.io"` is reachable via `env.get_native_in("std.native.io", name)`
    pub fn bind_all(&self, env: &mut RuntimeEnvironment) {
        for func in self.funcs.values() {
            let value = Value::NativeFunction {
                name: func.name.to_string(),
                arity: Some(func.params.len()),
                param_types: func.params.iter().map(|(_, ty)| ty.clone()).collect(),
                return_type: func.return_type.clone(),
                func: func.func.clone(),
            };
            env.bind_native(func.module.to_string(), func.name.to_string(), value);
        }
    }
}
