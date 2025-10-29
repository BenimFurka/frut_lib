//! Abstract Syntax Tree (AST) definitions for the Frut
//!
//! This module defines the token types, AST node structures, and related types
//! used throughout the parsing and semantic analysis phases.

use core::fmt;

use alloc::{boxed::Box, string::String, vec::Vec};

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
    Import,
    As,
    Type,
    Ext,

    // Types
    StringType,
    IntType,
    BoolType,
    DoubleType,
    VoidType,

    // Identifiers and literals
    Identifier(String),
    StringLiteral(String),
    IntLiteral(i64),
    DoubleLiteral(f64),

    // Operators
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Percent,        // %
    Less,           // <
    LessEqual,      // <=
    Greater,        // >
    GreaterEqual,   // >=
    Equal,          // ==
    NotEqual,       // !=
    LogicalAnd,     // &&
    LogicalOr,      // ||
    LogicalNot,     // !
    Assign,         // =

    // Brackets and separators
    LBrace,         // {
    RBrace,         // }
    Semicolon,      // ;
    LParen,         // (
    RParen,         // )
    Comma,          // ,
    Colon,          // :
    Dot,            // .

    // Comments
    Comment(String),

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
            Token::Import => write!(f, "import"),
            Token::As => write!(f, "as"),
            Token::Type => write!(f, "type"),
            Token::Ext => write!(f, "ext"),
            Token::StringType => write!(f, "string"),
            Token::IntType => write!(f, "int"),
            Token::BoolType => write!(f, "bool"),
            Token::DoubleType => write!(f, "double"),
            Token::VoidType => write!(f, "void"),
            Token::Identifier(s) => write!(f, "identifier '{}'", s),
            Token::StringLiteral(s) => write!(f, "string literal \"{}\"", s),
            Token::IntLiteral(n) => write!(f, "integer literal {}", n),
            Token::DoubleLiteral(n) => write!(f, "double literal {}", n),
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
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Semicolon => write!(f, ";"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Dot => write!(f, "."),
            Token::Comment(s) => write!(f, "comment \"{}\"", s),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

impl Token {
    /// Check if the token is a literal value
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            Token::StringLiteral(_) | Token::IntLiteral(_) | Token::DoubleLiteral(_) | Token::True | Token::False
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
            Token::StringType | Token::IntType | Token::BoolType | Token::DoubleType | Token::VoidType
        )
    }

    /// Check if the token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            Token::Var | Token::If | Token::Elif | Token::Else | Token::True | Token::False | Token::Func | Token::Return | Token::While | Token::Import
        )
    }
}

/// Unary operators for expressions
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    Plus,   // +
    Minus,  // -
    Not,    // !
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
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Mod,    // %

    // Comparison
    Less,           // <
    LessEqual,      // <=
    Greater,        // >
    GreaterEqual,   // >=
    Equal,          // ==
    NotEqual,       // !=

    // Logical
    LogicalAnd,     // &&
    LogicalOr,      // ||
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
            _ => panic!("Token {:?} is not a binary operator", token),
        }
    }
}

impl From<Token> for UnaryOperator {
    fn from(token: Token) -> Self {
        match token {
            Token::Plus => UnaryOperator::Plus,
            Token::Minus => UnaryOperator::Minus,
            Token::LogicalNot => UnaryOperator::Not,
            _ => panic!("Token {:?} is not a unary operator", token),
        }
    }
}

/// Statement types in the AST
#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub kind: StatementKind,
    pub pos: crate::Position,
}

/// The kind of import statement
#[derive(Debug, Clone, PartialEq)]
pub enum ImportKind {
    /// Wildcard import: `import path.*`
    Wildcard,
    /// Grouped import: `import path.{item1, item2}`
    Group(Vec<String>),
    /// Single item import: `import path.item`
    Single(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    /// Variable declaration: `var name: type = value`
    VariableDeclaration {
        name: String,
        var_type: String,
        initializer: Expression,
    },
    /// Variable assignment: `name = value`
    Assignment {
        name: String,
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
    /// Block of statements: `{ ... }`
    Block(Vec<Statement>),
    /// Function declaration: `func name(params) -> type { ... }`
    FunctionDeclaration {
        name: String,
        params: Vec<Parameter>,
        return_type: String,
        body: Vec<Statement>,
    },
    /// Return statement: `return ...`
    Return {
        value: Option<Expression>,
    },
    /// Expression statement: `expr;`
    ExpressionStatement(Expression),
    /// Import statement: `import path...`
    Import {
        path: Vec<String>,
        kind: ImportKind,
    },
    /// Type declaration: `type Name { field1: type1; field2: type2; }`
    TypeDeclaration {
        name: String,
        fields: Vec<Field>,
    },
    /// Extension declaration: `ext TypeName { func method(self: TypeName, ...): ReturnType { ... } }`
    ExtDeclaration {
        type_name: String,
        methods: Vec<Statement>,
    },
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            StatementKind::VariableDeclaration { name, var_type, initializer } => {
                write!(f, "var {}: {} = {}", name, var_type, initializer)
            }
            StatementKind::Assignment { name, value } => {
                write!(f, "{} = {}", name, value)
            }
            StatementKind::IfStatement { condition, then_branch: _, elif_branches, else_branch: _ } => {
                write!(f, "if {} {{ ... }}", condition)?;
                for (cond, _statements) in elif_branches {
                    write!(f, " elif {} {{ ... }}", cond)?;
                }
                Ok(())
            }
            StatementKind::WhileStatement { condition, .. } => {
                write!(f, "while {} {{ ... }}", condition)
            }
            StatementKind::Block(statements) => {
                write!(f, "{{ ")?;
                for (i, stmt) in statements.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}", stmt)?;
                }
                write!(f, " }}")
            }
            StatementKind::FunctionDeclaration { name, params, return_type, .. } => {
                write!(f, "func {}({}) -> {} {{ ... }}", name, params.iter().map(|p| format!("{}: {}", p.name, p.param_type)).collect::<Vec<_>>().join(", "), return_type)
            }
            StatementKind::Return { value } => {
                if let Some(val) = value {
                    write!(f, "return {}", val)
                } else {
                    write!(f, "return")
                }
            }
            StatementKind::ExpressionStatement(expr) => write!(f, "{}", expr),
            StatementKind::Import { path, kind } => {
                let path_str = path.join(".");
                match kind {
                    ImportKind::Wildcard => write!(f, "import {}.*", path_str),
                    ImportKind::Single(name) => write!(f, "import {}.{}", path_str, name),
                    ImportKind::Group(names) => write!(f, "import {}.{{{}}}", path_str, names.join(", ")),
                }
            }
            StatementKind::TypeDeclaration { name, fields } => {
                write!(f, "type {} {{ ", name)?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 { write!(f, "; ")?; }
                    write!(f, "{}: {}", field.name, field.field_type)?;
                }
                write!(f, " }}")
            }
            StatementKind::ExtDeclaration { type_name, methods: _ } => {
                write!(f, "ext {} {{ ... }}", type_name)
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
    },
    
    // Type casting
    Cast {
        expr: Box<Expression>,
        target_type: String,
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
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ExpressionKind::StringLiteral(s) => write!(f, "\"{}\"", s),
            ExpressionKind::IntLiteral(n) => write!(f, "{}", n),
            ExpressionKind::DoubleLiteral(n) => write!(f, "{}", n),
            ExpressionKind::BoolLiteral(b) => write!(f, "{}", b),
            ExpressionKind::Variable(name) => write!(f, "{}", name),
            ExpressionKind::Unary { operator, operand } => {
                write!(f, "{}{}", operator, operand)
            }
            ExpressionKind::Binary { left, operator, right } => {
                write!(f, "({} {} {})", left, operator, right)
            }
            ExpressionKind::FunctionCall { callee, arguments } => {
                write!(f, "{}({})", callee, arguments.iter().map(|a| format!("{}", a)).collect::<Vec<_>>().join(", "))
            }
            ExpressionKind::Cast { expr, target_type } => {
                write!(f, "({} as {})", expr, target_type)
            }
            ExpressionKind::StructLiteral { type_name, fields } => {
                write!(f, "{} {{ ", type_name)?;
                for (i, (name, expr)) in fields.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}: {}", name, expr)?;
                }
                write!(f, " }}")
            }
            ExpressionKind::MemberAccess { object, member } => {
                write!(f, "{}.{}", object, member)
            }
        }
    }
}

/// Function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub param_type: String,
    pub pos: crate::Position,
}

/// Struct field
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub field_type: String,
    pub pos: crate::Position,
}

/// Result of parsing operations
#[derive(Debug, Clone)]
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
    pub fn new(statements: Vec<Statement>, errors: crate::ErrorCollection, filename: String) -> Self {
        Self { statements, errors, filename }
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

impl Default for ParseResult {
    fn default() -> Self {
        Self {
            statements: Vec::new(),
            errors: crate::ErrorCollection::new(),
            filename: String::new(),
        }
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
