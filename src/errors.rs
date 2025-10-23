//! Error system for the Frut
//!
//! Provides structured error reporting with position information for IDE integration
//! and debugging purposes.

use core::fmt;

use alloc::{string::String, vec::Vec};

use crate::ast::Position;

/// Error types for the Frut
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorType {
    // Lexical errors
    UnexpectedToken,
    UnclosedString,
    UnclosedComment,
    InvalidNumber,
    InvalidEscapeSequence,

    // Syntax errors
    SyntaxError,
    ExpectedToken(String),
    ExpectedExpression,
    ExpectedStatement,
    ExpectedType,
    ExpectedIdentifier,

    // Type errors
    TypeError,
    TypeMismatch { expected: String, found: String },
    InvalidOperation,

    // Runtime errors
    DivisionByZero,
    VariableError,
    UndefinedVariable(String),
    VariableAlreadyDefined(String),
    InvalidVariableName,

    // System errors
    RuntimeError,
    IndexOutOfBounds,
    InternalError,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorType::UnexpectedToken => write!(f, "Unexpected token"),
            ErrorType::UnclosedString => write!(f, "Unclosed string literal"),
            ErrorType::UnclosedComment => write!(f, "Unclosed comment"),
            ErrorType::InvalidNumber => write!(f, "Invalid number format"),
            ErrorType::InvalidEscapeSequence => write!(f, "Invalid escape sequence"),
            ErrorType::SyntaxError => write!(f, "Syntax error"),
            ErrorType::ExpectedToken(token) => write!(f, "Expected token: {}", token),
            ErrorType::ExpectedExpression => write!(f, "Expected expression"),
            ErrorType::ExpectedStatement => write!(f, "Expected statement"),
            ErrorType::ExpectedType => write!(f, "Expected type"),
            ErrorType::ExpectedIdentifier => write!(f, "Expected identifier"),
            ErrorType::TypeError => write!(f, "Type error"),
            ErrorType::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            ErrorType::InvalidOperation => write!(f, "Invalid operation"),
            ErrorType::DivisionByZero => write!(f, "Division by zero"),
            ErrorType::VariableError => write!(f, "Variable error"),
            ErrorType::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            ErrorType::VariableAlreadyDefined(name) => write!(f, "Variable already defined: {}", name),
            ErrorType::InvalidVariableName => write!(f, "Invalid variable name"),
            ErrorType::RuntimeError => write!(f, "Runtime error"),
            ErrorType::IndexOutOfBounds => write!(f, "Index out of bounds"),
            ErrorType::InternalError => write!(f, "Internal error"),
        }
    }
}

/// Error codes for consistent error identification
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorCode {
    UnexpectedToken = 1,
    UnclosedString = 2,
    UnclosedComment = 3,
    InvalidNumber = 4,
    InvalidEscapeSequence = 5,
    SyntaxError = 6,
    ExpectedToken = 7,
    ExpectedExpression = 8,
    ExpectedStatement = 9,
    ExpectedType = 10,
    ExpectedIdentifier = 11,
    TypeError = 12,
    TypeMismatch = 13,
    InvalidOperation = 14,
    DivisionByZero = 15,
    VariableError = 16,
    UndefinedVariable = 17,
    VariableAlreadyDefined = 18,
    InvalidVariableName = 19,
    RuntimeError = 20,
    IndexOutOfBounds = 21,
    InternalError = 22,
}

impl From<ErrorType> for ErrorCode {
    fn from(error_type: ErrorType) -> Self {
        match error_type {
            ErrorType::UnexpectedToken => ErrorCode::UnexpectedToken,
            ErrorType::UnclosedString => ErrorCode::UnclosedString,
            ErrorType::UnclosedComment => ErrorCode::UnclosedComment,
            ErrorType::InvalidNumber => ErrorCode::InvalidNumber,
            ErrorType::InvalidEscapeSequence => ErrorCode::InvalidEscapeSequence,
            ErrorType::SyntaxError => ErrorCode::SyntaxError,
            ErrorType::ExpectedToken(_) => ErrorCode::ExpectedToken,
            ErrorType::ExpectedExpression => ErrorCode::ExpectedExpression,
            ErrorType::ExpectedStatement => ErrorCode::ExpectedStatement,
            ErrorType::ExpectedType => ErrorCode::ExpectedType,
            ErrorType::ExpectedIdentifier => ErrorCode::ExpectedIdentifier,
            ErrorType::TypeError => ErrorCode::TypeError,
            ErrorType::TypeMismatch { .. } => ErrorCode::TypeMismatch,
            ErrorType::InvalidOperation => ErrorCode::InvalidOperation,
            ErrorType::DivisionByZero => ErrorCode::DivisionByZero,
            ErrorType::VariableError => ErrorCode::VariableError,
            ErrorType::UndefinedVariable(_) => ErrorCode::UndefinedVariable,
            ErrorType::VariableAlreadyDefined(_) => ErrorCode::VariableAlreadyDefined,
            ErrorType::InvalidVariableName => ErrorCode::InvalidVariableName,
            ErrorType::RuntimeError => ErrorCode::RuntimeError,
            ErrorType::IndexOutOfBounds => ErrorCode::IndexOutOfBounds,
            ErrorType::InternalError => ErrorCode::InternalError,
        }
    }
}

/// Structured error report with all necessary information for IDE integration
#[derive(Debug, Clone, PartialEq)]
pub struct ErrorReport {
    /// Error type
    pub error_type: ErrorType,
    /// Human-readable error message
    pub message: String,
    /// Position information
    pub position: Position,
    /// Error code for programmatic handling
    pub code: ErrorCode,
    /// Code snippet for context
    pub code_snippet: String,
}

impl ErrorReport {
    /// Create a new error report
    pub fn new(
        error_type: ErrorType,
        message: String,
        position: Position,
        code_snippet: String,
    ) -> Self {
        let code = ErrorCode::from(error_type.clone());
        Self {
            error_type,
            message,
            position,
            code,
            code_snippet,
        }
    }

    /// Create an error report with a simple message
    pub fn simple(error_type: ErrorType, message: String, file: String, line: usize, column: usize, code_snippet: String) -> Self {
        let position = Position::new(file, line, column, 0, 0);
        Self::new(error_type, message, position, code_snippet)
    }

    /// Create an error report with file information
    pub fn with_file(
        error_type: ErrorType,
        message: String,
        file: String,
        line: usize,
        column: usize,
        offset: usize,
        length: usize,
        code_snippet: String,
    ) -> Self {
        let position = Position::new(file, line, column, offset, length);
        Self::new(error_type, message, position, code_snippet)
    }
}

/// Result type for parsing operations
pub type Result<T> = core::result::Result<T, ErrorReport>;

/// Collection of errors for batch processing
#[derive(Debug, Clone)]
pub struct ErrorCollection {
    pub errors: Vec<ErrorReport>,
}

impl ErrorCollection {
    /// Create a new empty error collection
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Add an error to the collection
    pub fn add_error(&mut self, error: ErrorReport) {
        self.errors.push(error);
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get the number of errors
    pub fn len(&self) -> usize {
        self.errors.len()
    }

    /// Check if the collection is empty
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    /// Clear all errors
    pub fn clear(&mut self) {
        self.errors.clear();
    }
}

impl Default for ErrorCollection {
    fn default() -> Self {
        Self::new()
    }
}
