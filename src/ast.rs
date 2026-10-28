//! Abstract Syntax Tree (AST) definitions for the Frut
//!
//! This module defines the token types, AST node structures, and related types
//! used throughout the parsing and semantic analysis phases.

use core::fmt;
use core::hash::BuildHasherDefault;

use alloc::{
    boxed::Box,
    string::{String, ToString},
    vec::Vec,
};

#[allow(deprecated)]
pub type HashMap<K, V> = hashbrown::HashMap<K, V, BuildHasherDefault<core::hash::SipHasher>>;

// Re-export ErrorCollection for use in this module
pub use crate::ErrorCollection;

/// Position information for error reporting
#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    /// File path
    pub file: String,
    /// Line number (1-based)
    pub line: usize,
    /// Column number (1-based)
    pub column: usize,
    /// Absolute offset in the source
    pub offset: usize,
    /// Length of the problematic code segment
    pub length: usize,
}

impl Position {
    /// Create a new position with filename
    pub fn new(file: String, line: usize, column: usize, offset: usize, length: usize) -> Self {
        Self {
            file,
            line,
            column,
            offset,
            length,
        }
    }
}

/// Token types for the Frut lexer
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Var,
    If,
    Elif,
    Else,
    True,
    False,
    Func,
    Return,
    While,
    Break,
    As,
    Type,
    Ext,
    Import,
    Foreign,
    Enum,
    Interface,
    Match,

    // Types
    StringType,
    IntType,
    BoolType,
    DoubleType,
    BytesType,
    VoidType,

    // Identifiers and literals
    Identifier(String),
    StringLiteral(String),
    IntLiteral(i64),
    DoubleLiteral(f64),

    // Operators
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=
    Equal,        // ==
    NotEqual,     // !=
    LogicalAnd,   // &&
    LogicalOr,    // ||
    LogicalNot,   // !
    Assign,       // =
    FatArrow,     // =>
    Pipe,         // | (or-patterns in match)

    // Brackets and separators
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Semicolon, // ;
    LParen,    // (
    RParen,    // )
    Comma,     // ,
    Colon,     // :
    Dot,       // .

    // Comments
    Comment(String),

    /// A numeric literal that could not be represented
    InvalidNumber(String),

    /// A string literal that reaches end-of-file before its closing quote
    UnclosedString,

    // Special
    EOF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Var => write!(f, "var"),
            Token::If => write!(f, "if"),
            Token::Elif => write!(f, "elif"),
            Token::Else => write!(f, "else"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Func => write!(f, "func"),
            Token::Return => write!(f, "return"),
            Token::While => write!(f, "while"),
            Token::Break => write!(f, "break"),
            Token::As => write!(f, "as"),
            Token::Type => write!(f, "type"),
            Token::Ext => write!(f, "ext"),
            Token::Import => write!(f, "import"),
            Token::Foreign => write!(f, "foreign"),
            Token::Enum => write!(f, "enum"),
            Token::Interface => write!(f, "interface"),
            Token::Match => write!(f, "match"),
            Token::StringType => write!(f, "string"),
            Token::IntType => write!(f, "int"),
            Token::BoolType => write!(f, "bool"),
            Token::DoubleType => write!(f, "double"),
            Token::BytesType => write!(f, "bytes"),
            Token::VoidType => write!(f, "void"),
            Token::Identifier(s) => write!(f, "identifier '{s}'"),
            Token::StringLiteral(s) => write!(f, "string literal \"{s}\""),
            Token::IntLiteral(n) => write!(f, "integer literal {n}"),
            Token::DoubleLiteral(n) => write!(f, "double literal {n}"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LogicalAnd => write!(f, "&&"),
            Token::LogicalOr => write!(f, "||"),
            Token::LogicalNot => write!(f, "!"),
            Token::Assign => write!(f, "="),
            Token::FatArrow => write!(f, "=>"),
            Token::Pipe => write!(f, "|"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Semicolon => write!(f, ";"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Dot => write!(f, "."),
            Token::Comment(s) => write!(f, "comment \"{s}\""),
            Token::InvalidNumber(n) => write!(f, "invalid number literal '{n}'"),
            Token::UnclosedString => write!(f, "unclosed string"),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

impl Token {
    /// Check if the token is a literal value
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            Token::StringLiteral(_)
                | Token::IntLiteral(_)
                | Token::DoubleLiteral(_)
                | Token::True
                | Token::False
        )
    }

    /// Check if the token is a binary operator
    pub fn is_binary_operator(&self) -> bool {
        matches!(
            self,
            Token::Plus
                | Token::Minus
                | Token::Star
                | Token::Slash
                | Token::Percent
                | Token::Less
                | Token::LessEqual
                | Token::Greater
                | Token::GreaterEqual
                | Token::Equal
                | Token::NotEqual
                | Token::LogicalAnd
                | Token::LogicalOr
                | Token::As
        )
    }

    /// Check if the token is a unary operator
    pub fn is_unary_operator(&self) -> bool {
        matches!(self, Token::Plus | Token::Minus | Token::LogicalNot)
    }

    /// Check if the token is a type keyword
    pub fn is_type(&self) -> bool {
        matches!(
            self,
            Token::StringType
                | Token::IntType
                | Token::BoolType
                | Token::DoubleType
                | Token::BytesType
                | Token::VoidType
        )
    }

    /// Check if the token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            Token::Var
                | Token::If
                | Token::Elif
                | Token::Else
                | Token::True
                | Token::False
                | Token::Func
                | Token::Return
                | Token::While
                | Token::Import
                | Token::Foreign
                | Token::Enum
                | Token::Interface
                | Token::Match
        )
    }
}

/// Import item specification
#[derive(Debug, Clone, PartialEq)]
pub enum ImportItem {
    /// Specific identifier (function, class, etc.)
    Identifier(String),
    /// Wildcard import all items
    Wildcard,
}

/// Import statement structure
#[derive(Debug, Clone, PartialEq)]
pub struct ImportStatement {
    /// Full library path (e.g., "my_lib.io", "self.utils", "external_lib.math")
    pub library_path: String,
    /// Import item specification
    pub item: ImportItem,
}

/// Unary operators for expressions
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    Plus,  // +
    Minus, // -
    Not,   // !
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Plus => write!(f, "+"),
            UnaryOperator::Minus => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}

/// Binary operators for expressions
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    // Arithmetic
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %

    // Comparison
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=
    Equal,        // ==
    NotEqual,     // !=

    // Logical
    LogicalAnd, // &&
    LogicalOr,  // ||
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::Mod => write!(f, "%"),
            BinaryOperator::Less => write!(f, "<"),
            BinaryOperator::LessEqual => write!(f, "<="),
            BinaryOperator::Greater => write!(f, ">"),
            BinaryOperator::GreaterEqual => write!(f, ">="),
            BinaryOperator::Equal => write!(f, "=="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::LogicalAnd => write!(f, "&&"),
            BinaryOperator::LogicalOr => write!(f, "||"),
        }
    }
}

impl From<Token> for BinaryOperator {
    fn from(token: Token) -> Self {
        match token {
            Token::Plus => BinaryOperator::Add,
            Token::Minus => BinaryOperator::Sub,
            Token::Star => BinaryOperator::Mul,
            Token::Slash => BinaryOperator::Div,
            Token::Percent => BinaryOperator::Mod,
            Token::Less => BinaryOperator::Less,
            Token::LessEqual => BinaryOperator::LessEqual,
            Token::Greater => BinaryOperator::Greater,
            Token::GreaterEqual => BinaryOperator::GreaterEqual,
            Token::Equal => BinaryOperator::Equal,
            Token::NotEqual => BinaryOperator::NotEqual,
            Token::LogicalAnd => BinaryOperator::LogicalAnd,
            Token::LogicalOr => BinaryOperator::LogicalOr,
            _ => panic!("Token {token:?} is not a binary operator"),
        }
    }
}

impl From<Token> for UnaryOperator {
    fn from(token: Token) -> Self {
        match token {
            Token::Plus => UnaryOperator::Plus,
            Token::Minus => UnaryOperator::Minus,
            Token::LogicalNot => UnaryOperator::Not,
            _ => panic!("Token {token:?} is not a unary operator"),
        }
    }
}

/// Statement types in the AST
#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub kind: StatementKind,
    pub pos: crate::Position,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    /// Variable declaration: `var name: type = value`
    VariableDeclaration {
        name: String,
        var_type: TypeExpr,
        initializer: Expression,
    },
    /// Variable assignment: `name = value`
    Assignment {
        target: Expression,
        value: Expression,
    },
    /// If statement: `if condition { ... } [elif ...] [else ...]`
    IfStatement {
        condition: Expression,
        then_branch: Vec<Statement>,
        elif_branches: Vec<(Expression, Vec<Statement>)>,
        else_branch: Option<Vec<Statement>>,
    },
    /// While statement: `while condition { ... }`
    WhileStatement {
        condition: Expression,
        body: Vec<Statement>,
    },
    /// Break statement: `break`
    Break,
    /// Match statement: `match scrutinee { pattern => body, ... }`
    MatchStatement {
        scrutinee: Expression,
        arms: Vec<MatchArm>,
    },
    /// Block of statements: `{ ... }`
    Block(Vec<Statement>),
    /// Function declaration: `func name(params) -> type { ... }`
    FunctionDeclaration {
        name: String,
        /// Generic type parameters: `func identity<T>(...)`
        type_params: Vec<String>,
        /// Interface bounds per type parameter:
        /// `func f<T: Shape, U>` -> `[Some("Shape"), None]`
        type_param_bounds: Vec<Option<String>>,
        params: Vec<Parameter>,
        return_type: TypeExpr,
        body: Vec<Statement>,
    },
    /// Return statement: `return ...`
    Return { value: Option<Expression> },
    /// Expression statement: `expr;`
    ExpressionStatement(Expression),
    /// Type declaration: `type Name { field1: type1; field2: type2; }`
    /// or a generic type: `type Box<T> { value: T; }`
    TypeDeclaration {
        name: String,
        /// Generic type parameters: `type Box<T>` -> `["T"]`
        type_params: Vec<String>,
        /// Interface bounds per type parameter
        type_param_bounds: Vec<Option<String>>,
        fields: Vec<Field>,
    },
    /// Enum declaration: `enum Name { Variant, OtherVariant(int, string), ... }`
    /// or a generic enum: `enum Option<T> { Some(T), None }`
    EnumDeclaration {
        name: String,
        /// Generic type parameters: `enum Option<T>` -> `["T"]`
        type_params: Vec<String>,
        /// Interface bounds per type parameter (parallel to `type_params`)
        type_param_bounds: Vec<Option<String>>,
        variants: Vec<EnumVariantDecl>,
    },
    /// Extension declaration: `ext TypeName { func method(self: TypeName, ...): ReturnType { ... } }`
    /// or an interface implementation: `ext TypeName : I1, I2 { ... }`
    ExtDeclaration {
        type_name: String,
        /// Generic type parameters of the extended type:
        /// `ext MyVec<T> : Container<T>` -> `["T"]`
        type_params: Vec<String>,
        /// Interfaces implemented by this block: `ext Rect : Shape, Sized` -> `["Shape", "Sized"]`
        /// Empty for a plain `ext` block without interface declarations
        interfaces: Vec<String>,
        /// Type arguments per interface:
        /// `ext MyVec<T> : Container<T>` -> `[[Named("T")]]`;
        /// `ext MyVec : Container<int>` -> `[[Named("int")]]`
        interface_args: Vec<Vec<TypeExpr>>,
        methods: Vec<Statement>,
    },
    /// Interface declaration: `interface Name { func method(params): Ret; ... }`
    /// Method signatures only, no bodies
    InterfaceDeclaration {
        name: String,
        /// Generic type parameters: `interface Container<T>` -> `["T"]`
        type_params: Vec<String>,
        /// Interface bounds per type parameter
        type_param_bounds: Vec<Option<String>>,
        /// Method signatures without `self`
        methods: Vec<InterfaceMethodDecl>,
    },
    /// Import statement: `import my_lib.io.println;` or `import external_lib.math.*;`
    ImportStatement(ImportStatement),
    /// Foreign function declaration: `foreign fn print(s: String): Void;`
    ForeignFunctionDeclaration {
        name: String,
        /// Generic type parameters: `foreign fn f<T>(...)`
        type_params: Vec<String>,
        /// Interface bounds per type parameter
        type_param_bounds: Vec<Option<String>>,
        params: Vec<Parameter>,
        return_type: TypeExpr,
    },
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            StatementKind::VariableDeclaration {
                name,
                var_type,
                initializer,
            } => {
                write!(f, "var {name}: {var_type} = {initializer}")
            }
            StatementKind::Assignment { target, value } => {
                write!(f, "{target} = {value}")
            }
            StatementKind::IfStatement {
                condition,
                then_branch: _,
                elif_branches,
                else_branch: _,
            } => {
                write!(f, "if {condition} {{ ... }}")?;
                for (cond, _statements) in elif_branches {
                    write!(f, " elif {cond} {{ ... }}")?;
                }
                Ok(())
            }
            StatementKind::WhileStatement { condition, .. } => {
                write!(f, "while {condition} {{ ... }}")
            }
            StatementKind::Break => write!(f, "break"),
            StatementKind::MatchStatement { scrutinee, arms } => {
                write!(f, "match {} {{ {} arms }}", scrutinee, arms.len())
            }
            StatementKind::Block(statements) => {
                write!(f, "{{ ")?;
                for (i, stmt) in statements.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{stmt}")?;
                }
                write!(f, " }}")
            }
            StatementKind::FunctionDeclaration {
                name,
                params,
                return_type,
                ..
            } => {
                write!(
                    f,
                    "func {}({}) -> {} {{ ... }}",
                    name,
                    params
                        .iter()
                        .map(|p| format!("{}: {}", p.name, p.param_type))
                        .collect::<Vec<_>>()
                        .join(", "),
                    return_type
                )
            }
            StatementKind::Return { value } => {
                if let Some(val) = value {
                    write!(f, "return {val}")
                } else {
                    write!(f, "return")
                }
            }
            StatementKind::ExpressionStatement(expr) => write!(f, "{expr}"),
            StatementKind::TypeDeclaration {
                name,
                type_params,
                type_param_bounds: _,
                fields,
            } => {
                if type_params.is_empty() {
                    write!(f, "type {name} {{ ")?;
                } else {
                    write!(f, "type {}<{}> {{ ", name, type_params.join(", "))?;
                }
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}: {}", field.name, field.field_type)?;
                }
                write!(f, " }}")
            }
            StatementKind::EnumDeclaration {
                name,
                type_params,
                type_param_bounds: _,
                variants,
            } => {
                if type_params.is_empty() {
                    write!(f, "enum {name} {{ ")?;
                } else {
                    write!(f, "enum {}<{}> {{ ", name, type_params.join(", "))?;
                }
                for (i, variant) in variants.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{variant}")?;
                }
                write!(f, " }}")
            }
            StatementKind::ExtDeclaration {
                type_name,
                interfaces,
                methods: _,
                ..
            } => {
                if interfaces.is_empty() {
                    write!(f, "ext {type_name} {{ ... }}")
                } else {
                    write!(f, "ext {} : {} {{ ... }}", type_name, interfaces.join(", "))
                }
            }
            StatementKind::InterfaceDeclaration {
                name,
                type_params: _,
                type_param_bounds: _,
                methods,
            } => {
                write!(f, "interface {name} {{ ")?;
                for (i, m) in methods.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{m}")?;
                }
                write!(f, " }}")
            }
            StatementKind::ImportStatement(import_stmt) => {
                write!(f, "import {}", import_stmt.library_path)?;
                match &import_stmt.item {
                    ImportItem::Identifier(name) => write!(f, ".{name}")?,
                    ImportItem::Wildcard => write!(f, ".*")?,
                }
                Ok(())
            }
            StatementKind::ForeignFunctionDeclaration {
                name,
                type_params: _,
                type_param_bounds: _,
                params,
                return_type,
            } => {
                write!(f, "foreign fn {name}(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", p.name, p.param_type)?;
                }
                write!(f, "): {return_type}")
            }
        }
    }
}

/// Expression types in the AST
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub pos: crate::Position,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    // Literals
    StringLiteral(String),
    IntLiteral(i64),
    DoubleLiteral(f64),
    BoolLiteral(bool),

    // Variables
    Variable(String),

    // Unary operations
    Unary {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },

    // Binary operations
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },

    // Function calls
    FunctionCall {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
        /// Explicit generic type arguments: `vec_new<int>()` -> `[int]`
        /// Empty when the call relies on inference (`vec_new()`) or the
        /// callee is not generic
        type_args: Vec<TypeExpr>,
    },

    // Type casting
    Cast {
        expr: Box<Expression>,
        target_type: TypeExpr,
    },

    // Struct literal
    StructLiteral {
        type_name: String,
        fields: Vec<(String, Expression)>,
    },

    // Member access
    MemberAccess {
        object: Box<Expression>,
        member: String,
    },

    // Array literal: `[1, 2, 3]`
    ArrayLiteral(Vec<Expression>),

    // Array repeat literal: `[0; 4]` -> four zeroes
    ArrayRepeat {
        value: Box<Expression>,
        len: usize,
    },

    // Index access: `arr[i]`
    Index {
        object: Box<Expression>,
        index: Box<Expression>,
    },
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ExpressionKind::StringLiteral(s) => write!(f, "\"{s}\""),
            ExpressionKind::IntLiteral(n) => write!(f, "{n}"),
            ExpressionKind::DoubleLiteral(n) => write!(f, "{n}"),
            ExpressionKind::BoolLiteral(b) => write!(f, "{b}"),
            ExpressionKind::Variable(name) => write!(f, "{name}"),
            ExpressionKind::Unary { operator, operand } => {
                write!(f, "{operator}{operand}")
            }
            ExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                write!(f, "({left} {operator} {right})")
            }
            ExpressionKind::FunctionCall {
                callee,
                arguments,
                type_args,
            } => {
                let generics = if type_args.is_empty() {
                    String::new()
                } else {
                    format!(
                        "<{}>",
                        type_args
                            .iter()
                            .map(|t| format!("{t}"))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                };
                write!(
                    f,
                    "{}{}({})",
                    callee,
                    generics,
                    arguments
                        .iter()
                        .map(|a| format!("{a}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            ExpressionKind::Cast { expr, target_type } => {
                write!(f, "({expr} as {target_type})")
            }
            ExpressionKind::StructLiteral { type_name, fields } => {
                write!(f, "{type_name} {{ ")?;
                for (i, (name, expr)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{name}: {expr}")?;
                }
                write!(f, " }}")
            }
            ExpressionKind::MemberAccess { object, member } => {
                write!(f, "{object}.{member}")
            }
            ExpressionKind::ArrayLiteral(elements) => {
                write!(f, "[")?;
                for (i, expr) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{expr}")?;
                }
                write!(f, "]")
            }
            ExpressionKind::ArrayRepeat { value, len } => {
                write!(f, "[{value}; {len}]")
            }
            ExpressionKind::Index { object, index } => {
                write!(f, "{object}[{index}]")
            }
        }
    }
}

/// A type expression in the AST
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    /// A named type: primitive (`int`, `string`, ...), a user type (`Point`),
    /// or a generic parameter (`T`)
    Named(String),
    /// Generic type application: `Box<int>`, `Result<T, E>`
    Generic { name: String, args: Vec<TypeExpr> },
    /// Fixed-length array: `[T; N]`
    Array {
        element_type: Box<TypeExpr>,
        len: usize,
    },
    /// Function type: `func(int, string): bool`
    Function {
        params: Vec<TypeExpr>,
        ret: Box<TypeExpr>,
    },
    /// No type (`void`)
    Void,
    /// Type to be inferred from the initializer: `var x = 10;`
    Inferred,
}

impl TypeExpr {
    /// The plain name of this type expression
    pub fn base_name(&self) -> &str {
        match self {
            TypeExpr::Named(name) => name,
            TypeExpr::Generic { name, .. } => name,
            TypeExpr::Array { .. } => "",
            TypeExpr::Function { .. } => "",
            TypeExpr::Void => "void",
            TypeExpr::Inferred => "",
        }
    }
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExpr::Named(name) => write!(f, "{name}"),
            TypeExpr::Generic { name, args } => {
                let arg_str = args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{name}<{arg_str}>")
            }
            TypeExpr::Array { element_type, len } => {
                write!(f, "[{element_type}; {len}]")
            }
            TypeExpr::Function { params, ret } => {
                let param_str = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "func({param_str}): {ret}")
            }
            TypeExpr::Void => write!(f, "void"),
            TypeExpr::Inferred => write!(f, "_"),
        }
    }
}

/// Function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub param_type: TypeExpr,
    pub pos: crate::Position,
}

/// Struct field
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub field_type: TypeExpr,
    pub pos: crate::Position,
}

/// An enum variant declaration: `Name` or `Name(payload_types...)`
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantDecl {
    pub name: String,
    pub payload_types: Vec<TypeExpr>,
    pub pos: crate::Position,
}

impl fmt::Display for EnumVariantDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.payload_types.is_empty() {
            write!(f, "{}", self.name)
        } else {
            let types = self
                .payload_types
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            write!(f, "{}({})", self.name, types)
        }
    }
}

/// Literal value inside a match pattern
#[derive(Debug, Clone, PartialEq)]
pub enum PatternLiteral {
    Int(i64),
    Double(f64),
    Bool(bool),
    String(String),
}

impl fmt::Display for PatternLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PatternLiteral::Int(n) => write!(f, "{n}"),
            PatternLiteral::Double(n) => write!(f, "{n}"),
            PatternLiteral::Bool(b) => write!(f, "{b}"),
            PatternLiteral::String(s) => write!(f, "\"{s}\""),
        }
    }
}

/// A match pattern
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// `_`
    Wildcard,
    /// `42`, `3.14`, `"red"`, `true`
    Literal(PatternLiteral),
    /// `v` - binding of a payload element
    Binding(String),
    /// `Some(v)`, `Option.Some(v)`, `None`, `Rect(w, h)`
    /// `enum_name` is `None` for the short form
    EnumVariant {
        enum_name: Option<String>,
        variant: String,
        args: Vec<Pattern>,
    },
    /// `a | b` - allowed at the top level of an arm only
    Or(Vec<Pattern>),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Wildcard => write!(f, "_"),
            Pattern::Literal(lit) => write!(f, "{lit}"),
            Pattern::Binding(name) => write!(f, "{name}"),
            Pattern::EnumVariant {
                enum_name,
                variant,
                args,
            } => {
                if let Some(en) = enum_name {
                    write!(f, "{en}.")?;
                }
                write!(f, "{variant}")?;
                if !args.is_empty() {
                    write!(f, "(")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{arg}")?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Pattern::Or(alts) => {
                for (i, alt) in alts.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{alt}")?;
                }
                Ok(())
            }
        }
    }
}

/// One arm of a match statement: `pattern => body`
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    /// Block statements or a single statement
    pub body: Vec<Statement>,
    pub pos: crate::Position,
}

/// An interface method declaration: `func name(params): Ret;`
#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceMethodDecl {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: TypeExpr,
    pub pos: crate::Position,
}

impl fmt::Display for InterfaceMethodDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name, p.param_type))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "func {}({}): {}", self.name, params, self.return_type)
    }
}

/// A parsed source file
#[derive(Debug, Clone)]
pub struct File {
    pub name: String, // just the filename, e.g., "main.ft"
    pub code: String,
    pub ast: Option<Vec<Statement>>, // NOTE: filled after parsing
}

/// A module containing files and submodules
#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub files: Vec<File>,
    pub submodules: Vec<Module>,
}

impl Module {
    /// Create a new module
    pub fn new(name: String) -> Self {
        Self {
            name,
            files: Vec::new(),
            submodules: Vec::new(),
        }
    }

    /// Add a file to this module
    pub fn add_file(&mut self, file: File) {
        self.files.push(file);
    }

    /// Add a submodule to this module
    pub fn add_submodule(&mut self, submodule: Module) {
        self.submodules.push(submodule);
    }

    /// Find a file by name in this module or its submodules
    pub fn find_file(&self, name: &str) -> Option<&File> {
        // Search in current module
        if let Some(file) = self.files.iter().find(|f| f.name == name) {
            return Some(file);
        }

        // Search in submodules
        for submodule in &self.submodules {
            if let Some(file) = submodule.find_file(name) {
                return Some(file);
            }
        }

        None
    }

    /// Find a submodule by name
    pub fn find_submodule(&self, name: &str) -> Option<&Module> {
        self.submodules.iter().find(|m| m.name == name)
    }

    /// Find a submodule by name (mutable)
    pub fn find_submodule_mut(&mut self, name: &str) -> Option<&mut Module> {
        self.submodules.iter_mut().find(|m| m.name == name)
    }

    /// Get all files in this module and all submodules recursively
    pub fn get_all_files(&self) -> Vec<&File> {
        let mut files = Vec::new();

        // Add files from current module
        files.extend(&self.files);

        // Add files from all submodules
        for submodule in &self.submodules {
            files.extend(submodule.get_all_files());
        }

        files
    }
}

/// Result of parsing multiple files
#[derive(Debug, Clone)]
pub struct ParseProjectResult {
    pub project: Module,
    pub errors: ErrorCollection,
}

/// Represents a source file provided to the core (can be in-memory or from disk)
#[derive(Debug, Clone)]
pub struct SourceFile {
    pub path: String,
    pub code: String,
    pub ast: Option<Vec<Statement>>, // NOTE: filled after parsing
}

impl ParseProjectResult {
    pub fn is_success(&self) -> bool {
        self.errors.is_empty()
    }
}

/// Result of parsing operations
#[derive(Debug, Clone, Default)]
pub struct ParseResult {
    /// Parsed AST statements
    pub statements: Vec<Statement>,
    /// Collection of errors encountered during parsing
    pub errors: crate::ErrorCollection,
    /// Source filename
    pub filename: String,
}

impl ParseResult {
    /// Create a new parse result
    pub fn new(
        statements: Vec<Statement>,
        errors: crate::ErrorCollection,
        filename: String,
    ) -> Self {
        Self {
            statements,
            errors,
            filename,
        }
    }

    /// Check if parsing was successful (no errors)
    pub fn is_success(&self) -> bool {
        self.errors.is_empty()
    }

    /// Get the number of errors
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }
}

/// Token with position information
#[derive(Debug, Clone)]
pub struct TokenWithPosition {
    pub token: Token,
    pub position: crate::Position,
}

impl TokenWithPosition {
    pub fn new(token: Token, position: crate::Position) -> Self {
        Self { token, position }
    }
}
