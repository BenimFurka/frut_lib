//! Lexical analyzer for the Frut
//!
//! Tokenizes input source code into a stream of tokens for parsing.

use alloc::string::String;
use alloc::vec::Vec;

use crate::ast::{Position, Token, TokenWithPosition};

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
                if self.current_char() == Some('=') {
                    self.advance();
                    Token::PlusAssign
                } else {
                    Token::Plus
                }
            }
            '-' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Token::MinusAssign
                } else {
                    Token::Minus
                }
            }
            '*' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Token::StarAssign
                } else {
                    Token::Star
                }
            }
            '/' => {
                if self.position + 1 < self.input.len() {
                    match self.input[self.position + 1] {
                        '=' => {
                            self.advance();
                            self.advance();
                            Token::SlashAssign
                        }
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
                if self.current_char() == Some('=') {
                    self.advance();
                    Token::PercentAssign
                } else {
                    Token::Percent
                }
            }
            '<' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Token::LessEqual
                } else if self.current_char() == Some('<') {
                    self.advance();
                    if self.current_char() == Some('=') {
                        self.advance();
                        Token::ShiftLeftAssign
                    } else {
                        Token::ShiftLeft
                    }
                } else {
                    Token::Less
                }
            }
            '>' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Token::GreaterEqual
                } else if self.current_char() == Some('>') {
                    self.advance();
                    if self.current_char() == Some('=') {
                        self.advance();
                        Token::ShiftRightAssign
                    } else {
                        Token::ShiftRight
                    }
                } else {
                    Token::Greater
                }
            }
            '=' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Token::Equal
                } else if self.current_char() == Some('>') {
                    self.advance();
                    Token::FatArrow
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
                } else if self.current_char() == Some('=') {
                    self.advance();
                    Token::BitAndAssign
                } else {
                    Token::BitAnd
                }
            }
            '^' => {
                self.advance();
                if self.current_char() == Some('=') {
                    self.advance();
                    Token::BitXorAssign
                } else {
                    Token::BitXor
                }
            }
            '~' => {
                self.advance();
                Token::BitNot
            }
            '|' => {
                self.advance();
                if self.current_char() == Some('|') {
                    self.advance();
                    Token::LogicalOr
                } else if self.current_char() == Some('=') {
                    self.advance();
                    Token::BitOrAssign
                } else {
                    // Single `|`: bitwise OR, or an or-pattern separator in
                    // match arms
                    Token::Pipe
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
            '[' => {
                self.advance();
                Token::LBracket
            }
            ']' => {
                self.advance();
                Token::RBracket
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
        let position = Position::new(
            self.filename.clone(),
            current_line,
            current_column,
            current_pos,
            length,
        );

        Some(TokenWithPosition::new(token, position))
    }

    /// Read a string literal, decoding escape sequences (`\\`, `\"`, `\n`,
    /// `\t`, `\r`) into their actual characters
    fn read_string_literal(&mut self) -> Token {
        self.advance();

        let mut literal = String::new();
        while self.position < self.input.len() && self.input[self.position] != '"' {
            let c = self.input[self.position];

            if c == '$'
                && self.position + 1 < self.input.len()
                && self.input[self.position + 1] == '{'
            {
                literal.push('$');
                literal.push('{');
                self.advance();
                self.advance();

                let mut depth: i32 = 0;
                let mut in_string = false;
                while self.position < self.input.len() {
                    let cc = self.input[self.position];
                    if in_string {
                        literal.push(cc);
                        self.advance();
                        if cc == '\\' && self.position < self.input.len() {
                            literal.push(self.input[self.position]);
                            self.advance();
                        } else if cc == '"' {
                            in_string = false;
                        }
                        continue;
                    }
                    match cc {
                        '"' => {
                            in_string = true;
                            literal.push(cc);
                            self.advance();
                        }
                        '(' | '[' => {
                            depth += 1;
                            literal.push(cc);
                            self.advance();
                        }
                        ')' | ']' => {
                            depth -= 1;
                            literal.push(cc);
                            self.advance();
                        }
                        '}' if depth == 0 => {
                            literal.push(cc);
                            self.advance();
                            break;
                        }
                        '\\' if self.position + 1 < self.input.len()
                            && self.input[self.position + 1] == '$' =>
                        {
                            literal.push(cc);
                            literal.push(self.input[self.position + 1]);
                            self.advance();
                            self.advance();
                        }
                        _ => {
                            literal.push(cc);
                            self.advance();
                        }
                    }
                }
                continue;
            }

            if c == '\\' {
                self.advance();
                if self.position >= self.input.len() {
                    return Token::UnclosedString;
                }
                let escaped = self.input[self.position];
                self.advance();
                match escaped {
                    'n' => literal.push('\n'),
                    't' => literal.push('\t'),
                    'r' => literal.push('\r'),
                    '\\' => literal.push('\\'),
                    '"' => literal.push('"'),
                    other => {
                        literal.push('\\');
                        literal.push(other);
                    }
                }
            } else {
                literal.push(c);
                self.advance();
            }
        }

        if self.position >= self.input.len() {
            return Token::UnclosedString;
        }

        self.advance();

        Token::StringLiteral(literal)
    }

    /// Read a number literal (decimal or hex `0x...`)
    fn read_number_literal(&mut self) -> Token {
        let start = self.position;

        // Hex literal: 0x / 0X + hex digits → IntLiteral
        if self.input[self.position] == '0'
            && self.position + 1 < self.input.len()
            && (self.input[self.position + 1] == 'x' || self.input[self.position + 1] == 'X')
        {
            self.advance(); // '0'
            self.advance(); // 'x'
            let digits_start = self.position;
            while self.position < self.input.len() && self.input[self.position].is_ascii_hexdigit()
            {
                self.advance();
            }
            let digits: String = self.input[digits_start..self.position].iter().collect();
            return match i64::from_str_radix(&digits, 16) {
                Ok(n) => Token::IntLiteral(n),
                Err(_) => Token::InvalidNumber(format!("0x{digits}")),
            };
        }

        let mut is_float = false;

        while self.position < self.input.len()
            && (self.input[self.position].is_ascii_digit() || self.input[self.position] == '.')
        {
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
                Ok(n) if n.is_finite() => Token::DoubleLiteral(n),
                _ => Token::InvalidNumber(literal),
            }
        } else {
            match literal.parse::<i64>() {
                Ok(n) => Token::IntLiteral(n),
                Err(_) => Token::InvalidNumber(literal),
            }
        }
    }

    /// Read an identifier or keyword
    fn read_identifier_or_keyword(&mut self) -> Token {
        let start = self.position;

        while self.position < self.input.len()
            && (self.input[self.position].is_ascii_alphanumeric()
                || self.input[self.position] == '_')
        {
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
            "break" => Token::Break,
            "as" => Token::As,
            "type" => Token::Type,
            "ext" => Token::Ext,
            "import" => Token::Import,
            "foreign" => Token::Foreign,
            "enum" => Token::Enum,
            "interface" => Token::Interface,
            "match" => Token::Match,
            "string" => Token::StringType,
            "int" => Token::IntType,
            "bool" => Token::BoolType,
            "double" => Token::DoubleType,
            "void" => Token::VoidType,
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
