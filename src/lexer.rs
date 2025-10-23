//! Lexical analyzer for the Frut
//!
//! Tokenizes input source code into a stream of tokens for parsing.

use alloc::string::String;
use alloc::vec::Vec;

use crate::ast::{Token, TokenWithPosition, Position};

/// Lexer for tokenizing Frut source code
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
    filename: String,
}

impl Lexer {
    /// Create a new lexer from input string
    pub fn new(input: &str, filename: String) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
            filename,
        }
    }

    /// Tokenize the entire input
    pub fn tokenize(&mut self) -> Vec<TokenWithPosition> {
        let mut tokens = Vec::new();
        while self.position < self.input.len() {
            let token = self.next_token();
            if let Some(token) = token {
                tokens.push(token);
            }
        }
        tokens
    }

    /// Get the next token from the input
    fn next_token(&mut self) -> Option<TokenWithPosition> {
        self.skip_whitespace();

        if self.position >= self.input.len() {
            return None;
        }

        let current_pos = self.position;
        let current_line = self.line;
        let current_column = self.column;

        let token = match self.input[self.position] {
            '+' => {
                self.advance();
                Token::Plus
            }
            '-' => {
                self.advance();
                Token::Minus
            }
            '*' => {
                self.advance();
                Token::Star
            }
            '/' => {
                if self.position + 1 < self.input.len() {
                    match self.input[self.position + 1] {
                        '/' => {
                            self.read_single_line_comment();
                            return None;
                        }
                        '*' => {
                            self.read_multi_line_comment();
                            return None;
                        }
                        _ => {
                            self.advance();
                            Token::Slash
                        }
                    }
                } else {
                    self.advance();
                    Token::Slash
                }
            }
            '%' => {
                self.advance();
                Token::Percent
            }
            '<' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Token::LessEqual
                } else {
                    Token::Less
                }
            }
            '>' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Token::GreaterEqual
                } else {
                    Token::Greater
                }
            }
            '=' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            '!' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Token::NotEqual
                } else {
                    Token::LogicalNot
                }
            }
            '&' => {
                self.advance();
                if self.current_char() == Some('&') {
                    self.advance();
                    Token::LogicalAnd
                } else {
                    // Invalid token
                    self.advance();
                    return None;
                }
            }
            '|' => {
                self.advance();
                if self.current_char() == Some('|') {
                    self.advance();
                    Token::LogicalOr
                } else {
                    // Invalid token
                    self.advance();
                    return None;
                }
            }
            '{' => {
                self.advance();
                Token::LBrace
            }
            '}' => {
                self.advance();
                Token::RBrace
            }
            ';' => {
                self.advance();
                Token::Semicolon
            }
            '(' => {
                self.advance();
                Token::LParen
            }
            ')' => {
                self.advance();
                Token::RParen
            }
            ',' => {
                self.advance();
                Token::Comma
            }
            ':' => {
                self.advance();
                Token::Colon
            }
            '.' => {
                self.advance();
                Token::Dot
            }
            '"' => self.read_string_literal(),
            c if c.is_ascii_digit() => self.read_number_literal(),
            c if c.is_ascii_alphabetic() || c == '_' => self.read_identifier_or_keyword(),
            _ => {
                self.advance();
                return None;
            }
        };

        let length = self.position - current_pos;
        let position = Position::new(self.filename.clone(), current_line, current_column, current_pos, length);

        Some(TokenWithPosition::new(token, position))
    }

    /// Read a string literal
    fn read_string_literal(&mut self) -> Token {
        self.advance();

        let start = self.position;
        while self.position < self.input.len() && self.input[self.position] != '"' {
            if self.input[self.position] == '\\' {
                self.advance();
            }
            if self.position < self.input.len() {
                self.advance();
            }
        }

        if self.position >= self.input.len() {
            return Token::EOF;
        }

        let end = self.position;
        self.advance();

        let literal: String = self.input[start..end].iter().collect();
        Token::StringLiteral(literal)
    }

    /// Read a number literal
    fn read_number_literal(&mut self) -> Token {
        let start = self.position;
        let mut is_float = false;

        while self.position < self.input.len() &&
              (self.input[self.position].is_ascii_digit() || self.input[self.position] == '.') {
            if self.input[self.position] == '.' {
                if is_float {
                    break;
                }
                is_float = true;
            }
            self.advance();
        }

        let end = self.position;
        let literal: String = self.input[start..end].iter().collect();

        if is_float {
            match literal.parse::<f64>() {
                Ok(n) => Token::DoubleLiteral(n),
                Err(_) => Token::EOF, // Invalid number
            }
        } else {
            match literal.parse::<i64>() {
                Ok(n) => Token::IntLiteral(n),
                Err(_) => Token::EOF, // Invalid number
            }
        }
    }

    /// Read an identifier or keyword
    fn read_identifier_or_keyword(&mut self) -> Token {
        let start = self.position;

        while self.position < self.input.len() &&
              (self.input[self.position].is_ascii_alphanumeric() || self.input[self.position] == '_') {
            self.advance();
        }

        let end = self.position;
        let text: String = self.input[start..end].iter().collect();

        match text.as_str() {
            "var" => Token::Var,
            "if" => Token::If,
            "elif" => Token::Elif,
            "else" => Token::Else,
            "true" => Token::True,
            "false" => Token::False,
            "func" => Token::Func,
            "return" => Token::Return,
            "while" => Token::While,
            "import" => Token::Import,
            "string" => Token::StringType,
            "int" => Token::IntType,
            "bool" => Token::BoolType,
            "double" => Token::DoubleType,
            "void" => Token::VoidType,
            "as" => Token::As,
            _ => Token::Identifier(text),
        }
    }

    /// Read a single-line comment (// ... end of line) and consume it
    fn read_single_line_comment(&mut self) {
        self.advance();
        self.advance();

        while self.position < self.input.len() && self.input[self.position] != '\n' {
            self.advance();
        }

        if self.position < self.input.len() && self.input[self.position] == '\n' {
            self.advance();
        }
    }

    /// Read a multi-line comment (/* ... */) and consume it
    fn read_multi_line_comment(&mut self) {
        self.advance();
        self.advance();

        while self.position + 1 < self.input.len() {
            if self.input[self.position] == '*' && self.input[self.position + 1] == '/' {
                self.advance();
                self.advance(); 
                return;
            }
            self.advance();
        }
    }

    /// Skip whitespace characters
    fn skip_whitespace(&mut self) {
        while self.position < self.input.len() && self.input[self.position].is_whitespace() {
            if self.input[self.position] == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.position += 1;
        }
    }

    /// Get the current character
    fn current_char(&self) -> Option<char> {
        if self.position < self.input.len() {
            Some(self.input[self.position])
        } else {
            None
        }
    }

    /// Advance to the next character, updating line/column correctly
    fn advance(&mut self) {
        if self.position < self.input.len() {
            if self.input[self.position] == '\n' {
                self.position += 1;
                self.line += 1;
                self.column = 1;
            } else {
                self.position += 1;
                self.column += 1;
            }
        }
    }
}
