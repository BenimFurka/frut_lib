//! Parser for the Frut
//!
//! Converts a stream of tokens into an Abstract Syntax Tree (AST).

use alloc::boxed::Box;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use crate::ast::{Parameter, Token, Statement, Expression, BinaryOperator, UnaryOperator, TokenWithPosition, StatementKind, ExpressionKind};
use crate::ErrorCollection;

/// Parser for converting tokens to AST
pub struct Parser {
    tokens: Vec<TokenWithPosition>,
    position: usize,
    errors: ErrorCollection,
    source: String,
    filename: String,
}

impl Parser {
    /// Create a new parser from tokens
    pub fn new(tokens: Vec<TokenWithPosition>, source: String, filename: String) -> Self {
        Self {
            tokens,
            position: 0,
            errors: ErrorCollection::new(),
            source,
            filename,
        }
    }

    /// Parse all statements
    pub fn parse(&mut self) -> crate::ParseResult {
        let mut statements = Vec::new();

        while let Some(token) = self.current_token() {
            if token == &Token::EOF {
                break;
            }

            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(_) => {
                    self.synchronize();
                    if self.current_token().map_or(true, |t| t == &Token::EOF) {
                        break;
                    }
                }
            }
        }

        crate::ParseResult::new(statements, self.errors.clone(), self.filename.clone())
    }
    
    /// Parse statement
    fn parse_statement(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().map(|twp| twp.position.clone());

        let result = match self.current_token() {
            Some(Token::Var) => {
                let result = self.parse_variable_declaration();
                result
            },
            Some(Token::If) => {
                self.parse_if_statement()
            },
            Some(Token::While) => self.parse_while_statement(),
            Some(Token::Func) => self.parse_function_declaration(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(Token::Import) => self.parse_import_statement(),
            Some(Token::Type) => self.parse_type_declaration(),
            Some(Token::Ext) => self.parse_ext_declaration(),
            Some(Token::LBrace) => {
                self.parse_block()
            },
            Some(Token::Identifier(_)) => {
                let start_pos = self.current_token_with_position().unwrap().position.clone();
                let expr = self.parse_expression()?;
                if self.current_token() == Some(&Token::Assign) {
                    match &expr.kind {
                        ExpressionKind::Variable(_) | ExpressionKind::MemberAccess { .. } => {
                            self.advance();
                            let value = self.parse_expression()?;
                            Ok(Statement { kind: StatementKind::Assignment { target: expr, value }, pos: start_pos })
                        }
                        _ => {
                            let pos = self.current_token_with_position().map(|twp| twp.position.clone()).unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                            self.errors.add_error(crate::ErrorReport::with_file(
                                crate::ErrorType::SyntaxError,
                                "Invalid assignment target".to_string(),
                                self.filename.clone(),
                                pos.line,
                                pos.column,
                                pos.offset,
                                pos.length,
                                self.extract_code_snippet(pos.line, pos.column),
                            ));
                            return Err(());
                        }
                    }
                } else {
                    Ok(Statement { kind: StatementKind::ExpressionStatement(expr), pos: start_pos })
                }
            },
            _ => {
                    let pos = self.current_token_with_position().map(|twp| twp.position.clone()).unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedStatement,
                        "Expected statement".to_string(),
                        self.filename.clone(),
                        pos.line,
                        pos.column,
                        pos.offset,
                        pos.length,
                        self.extract_code_snippet(pos.line, pos.column),
                    ));
                return Err(());
            }
        };

        
        let result = result.map(|mut stmt| {
            if let Some(start_pos) = start_pos {
                stmt.pos = start_pos;
            }
            stmt
        });

        if let Ok(ref stmt) = result {
            if matches!(&stmt.kind, StatementKind::VariableDeclaration { .. } | StatementKind::Assignment { .. } | StatementKind::Return { .. } | StatementKind::ExpressionStatement { .. }) {
                if let Some(token) = self.current_token() {
                    if token == &Token::Semicolon {
                        self.advance();
                    }
                }
            }
        }
        result
    }

    /// Parse import statement
    fn parse_import_statement(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::Import)?;

        let mut segments: Vec<String> = Vec::new();

        let first = match self.current_token_with_position() {
            Some(twp) => match &twp.token {
                Token::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    name
                }
                _ => {
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedIdentifier,
                        "Expected identifier after 'import'".to_string(),
                        self.filename.clone(),
                        twp.position.line,
                        twp.position.column,
                        twp.position.offset,
                        twp.position.length,
                        self.extract_code_snippet(twp.position.line, twp.position.column),
                    ));
                    return Err(());
                }
            },
            None => {
                let pos = self.tokens.last().unwrap().position.clone();
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedIdentifier,
                    "Expected identifier after 'import'".to_string(),
                    self.filename.clone(),
                    pos.line,
                    pos.column,
                    pos.offset,
                    pos.length,
                    self.extract_code_snippet(pos.line, pos.column),
                ));
                return Err(());
            }
        };
        segments.push(first);

        use crate::ast::StatementKind;
        use crate::ast::ImportKind;
        let mut import_kind: Option<ImportKind> = None;

        loop {
            if self.current_token() != Some(&Token::Dot) {
                break;
            }
            self.advance();

            match self.current_token_with_position() {
                Some(twp) => match &twp.token {
                    Token::Identifier(name) => {
                        let name = name.clone();
                        self.advance();
                        segments.push(name);
                        continue;
                    }
                    Token::Star => {
                        self.advance();
                        import_kind = Some(ImportKind::Wildcard);
                        break;
                    }
                    Token::LBrace => {
                        self.advance();
                        let mut names: Vec<String> = Vec::new();
                        loop {
                            match self.current_token_with_position() {
                                Some(twp2) => match &twp2.token {
                                    Token::Identifier(n) => {
                                        names.push(n.clone());
                                        self.advance();
                                        if self.current_token() == Some(&Token::Comma) {
                                            self.advance();
                                            continue;
                                        }
                                    }
                                    _ => {
                                        self.errors.add_error(crate::ErrorReport::with_file(
                                            crate::ErrorType::ExpectedIdentifier,
                                            "Expected identifier in import group".to_string(),
                                            self.filename.clone(),
                                            twp2.position.line,
                                            twp2.position.column,
                                            twp2.position.offset,
                                            twp2.position.length,
                                            self.extract_code_snippet(twp2.position.line, twp2.position.column),
                                        ));
                                        return Err(());
                                    }
                                },
                                None => {
                                    let pos = self.tokens.last().unwrap().position.clone();
                                    self.errors.add_error(crate::ErrorReport::with_file(
                                        crate::ErrorType::ExpectedIdentifier,
                                        "Expected identifier in import group".to_string(),
                                        self.filename.clone(),
                                        pos.line,
                                        pos.column,
                                        pos.offset,
                                        pos.length,
                                        self.extract_code_snippet(pos.line, pos.column),
                                    ));
                                    return Err(());
                                }
                            }
                            break;
                        }
                        self.expect_token(Token::RBrace)?;
                        import_kind = Some(ImportKind::Group(names));
                        break;
                    }
                    _ => {
                        self.errors.add_error(crate::ErrorReport::with_file(
                            crate::ErrorType::ExpectedToken("identifier, '*' or '{'".to_string()),
                            "Invalid import syntax".to_string(),
                            self.filename.clone(),
                            twp.position.line,
                            twp.position.column,
                            twp.position.offset,
                            twp.position.length,
                            self.extract_code_snippet(twp.position.line, twp.position.column),
                        ));
                        return Err(());
                    }
                },
                None => break,
            }
        }

        let (path, kind) = if let Some(k) = import_kind {
            (segments, k)
        } else {
            if segments.len() < 2 {
                let pos = self.tokens[self.position.saturating_sub(1)].position.clone();
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::SyntaxError,
                    "Invalid import: expected 'module.item'".to_string(),
                    self.filename.clone(),
                    pos.line,
                    pos.column,
                    pos.offset,
                    pos.length,
                    self.extract_code_snippet(pos.line, pos.column),
                ));
                return Err(());
            }
            let item = segments.pop().unwrap();
            (segments, ImportKind::Single(item))
        };

        self.expect_token(Token::Semicolon)?;

        Ok(Statement {
            kind: StatementKind::Import { path, kind },
            pos: start_pos,
        })
    }

    /// Parse type declaration
    fn parse_type_declaration(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::Type)?;
        
        let name = match self.current_token_with_position() {
            Some(twp) => match &twp.token {
                Token::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    name
                }
                _ => {
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedIdentifier,
                        "Expected identifier after 'type'".to_string(),
                        self.filename.clone(),
                        twp.position.line,
                        twp.position.column,
                        twp.position.offset,
                        twp.position.length,
                        self.extract_code_snippet(twp.position.line, twp.position.column),
                    ));
                    return Err(());
                }
            },
            None => {
                let pos = self.tokens.last().unwrap().position.clone();
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedIdentifier,
                    "Expected identifier after 'type'".to_string(),
                    self.filename.clone(),
                    pos.line,
                    pos.column,
                    pos.offset,
                    pos.length,
                    self.extract_code_snippet(pos.line, pos.column),
                ));
                return Err(());
            }
        };

        self.expect_token(Token::LBrace)?;

        let mut fields = Vec::new();
        while let Some(token) = self.current_token() {
            if token == &Token::RBrace {
                break;
            }
            
            let field_pos = self.current_token_with_position().unwrap().position.clone();
            let field_name = match self.current_token_with_position() {
                Some(twp) => match &twp.token {
                    Token::Identifier(name) => {
                        let name = name.clone();
                        self.advance();
                        name
                    }
                    _ => {
                        self.errors.add_error(crate::ErrorReport::with_file(
                            crate::ErrorType::ExpectedIdentifier,
                            "Expected field name".to_string(),
                            self.filename.clone(),
                            twp.position.line,
                            twp.position.column,
                            twp.position.offset,
                            twp.position.length,
                            self.extract_code_snippet(twp.position.line, twp.position.column),
                        ));
                        self.synchronize();
                        continue;
                    }
                },
                None => {
                    let pos = self.tokens.last().unwrap().position.clone();
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedIdentifier,
                        "Expected field name".to_string(),
                        self.filename.clone(),
                        pos.line,
                        pos.column,
                        pos.offset,
                        pos.length,
                        self.extract_code_snippet(pos.line, pos.column),
                    ));
                    self.synchronize();
                    continue;
                }
            };

            self.expect_token(Token::Colon)?;

            let field_type = match self.current_token_with_position() {
                Some(twp) => match &twp.token {
                    Token::Identifier(type_name) => {
                        let type_name = type_name.clone();
                        self.advance();
                        type_name
                    }
                    Token::StringType => {
                        self.advance();
                        "string".to_string()
                    }
                    Token::IntType => {
                        self.advance();
                        "int".to_string()
                    }
                    Token::BoolType => {
                        self.advance();
                        "bool".to_string()
                    }
                    Token::DoubleType => {
                        self.advance();
                        "double".to_string()
                    }
                    Token::VoidType => {
                        self.advance();
                        "void".to_string()
                    }
                    _ => {
                        self.errors.add_error(crate::ErrorReport::with_file(
                            crate::ErrorType::ExpectedType,
                            "Expected type name".to_string(),
                            self.filename.clone(),
                            twp.position.line,
                            twp.position.column,
                            twp.position.offset,
                            twp.position.length,
                            self.extract_code_snippet(twp.position.line, twp.position.column),
                        ));
                        self.synchronize();
                        continue;
                    }
                },
                None => {
                    let pos = self.tokens.last().unwrap().position.clone();
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedType,
                        "Expected type name".to_string(),
                        self.filename.clone(),
                        pos.line,
                        pos.column,
                        pos.offset,
                        pos.length,
                        self.extract_code_snippet(pos.line, pos.column),
                    ));
                    self.synchronize();
                    continue;
                }
            };

            let field = crate::ast::Field {
                name: field_name,
                field_type,
                pos: field_pos,
            };
            fields.push(field);

            match self.current_token() {
                Some(Token::Semicolon) => {
                    self.advance();
                }
                Some(Token::RBrace) => {
                    break;
                }
                _ => {
                    let pos = self.current_token_with_position().map(|twp| twp.position.clone()).unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedToken("; or }".to_string()),
                        "Expected ';' or '}'".to_string(),
                        self.filename.clone(),
                        pos.line,
                        pos.column,
                        pos.offset,
                        pos.length,
                        self.extract_code_snippet(pos.line, pos.column),
                    ));
                    self.synchronize();
                    continue;
                }
            }
        }

        self.expect_token(Token::RBrace)?;

        Ok(Statement {
            kind: StatementKind::TypeDeclaration { name, fields },
            pos: start_pos,
        })
    }

    /// Parse extension declaration
    fn parse_ext_declaration(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::Ext)?;
        
        let type_name = match self.current_token_with_position() {
            Some(twp) => match &twp.token {
                Token::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    name
                }
                _ => {
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedIdentifier,
                        "Expected identifier after 'ext'".to_string(),
                        self.filename.clone(),
                        twp.position.line,
                        twp.position.column,
                        twp.position.offset,
                        twp.position.length,
                        self.extract_code_snippet(twp.position.line, twp.position.column),
                    ));
                    return Err(());
                }
            },
            None => {
                let pos = self.tokens.last().unwrap().position.clone();
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedIdentifier,
                    "Expected identifier after 'ext'".to_string(),
                    self.filename.clone(),
                    pos.line,
                    pos.column,
                    pos.offset,
                    pos.length,
                    self.extract_code_snippet(pos.line, pos.column),
                ));
                return Err(());
            }
        };

        self.expect_token(Token::LBrace)?;

        let mut methods = Vec::new();
        while let Some(token) = self.current_token() {
            if token == &Token::RBrace {
                break;
            }
            
            methods.push(self.parse_statement()?);
        }

        self.expect_token(Token::RBrace)?;

        Ok(Statement {
            kind: StatementKind::ExtDeclaration { type_name, methods },
            pos: start_pos,
        })
    }

    /// Parse variable delcaration
    fn parse_variable_declaration(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::Var)?;
        let name = match self.current_token_with_position() {
            Some(twp) => match &twp.token {
                Token::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    name
                }
                _ => {
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedIdentifier,
                        "Expected identifier after 'var'".to_string(),
                        self.filename.clone(),
                        twp.position.line,
                        twp.position.column,
                        twp.position.offset,
                        twp.position.length,
                        self.extract_code_snippet(twp.position.line, twp.position.column),
                    ));
                    return Err(());
                }
            },
            None => {
                let pos = self.current_token_with_position().map(|twp| twp.position.clone()).unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedIdentifier,
                    "Expected identifier after 'var'".to_string(),
                    self.filename.clone(),
                    pos.line,
                    pos.column,
                    pos.offset,
                    pos.length,
                    self.extract_code_snippet(pos.line, pos.column),
                ));
                return Err(());
            }
        };

        self.expect_token(Token::Colon)?;

        let var_type = match self.current_token_with_position() {
            Some(twp) => match &twp.token {
                Token::StringType => { self.advance(); "string".to_string() },
                Token::IntType => { self.advance(); "int".to_string() },
                Token::BoolType => { self.advance(); "bool".to_string() },
                Token::DoubleType => { self.advance(); "double".to_string() },
                Token::VoidType => { self.advance(); "void".to_string() },
                Token::Identifier(type_name) => { 
                    let type_name = type_name.clone();
                    self.advance(); 
                    type_name 
                },
                _ => {
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedType,
                        "Expected type after ':' (or implicitly after identifier)".to_string(),
                        self.filename.clone(),
                        twp.position.line,
                        twp.position.column,
                        twp.position.offset,
                        twp.position.length,
                        self.extract_code_snippet(twp.position.line, twp.position.column),
                    ));
                    return Err(());
                }
            },
            None => {
                 let pos = self.current_token_with_position().map(|twp| twp.position.clone()).unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                 self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedType,
                    "Expected type after ':' (or implicitly after identifier)".to_string(),
                    self.filename.clone(),
                    pos.line,
                    pos.column,
                    pos.offset,
                    pos.length,
                    self.extract_code_snippet(pos.line, pos.column),
                ));
                return Err(());
            }
        };

        self.expect_token(Token::Assign)?;
        let initializer = self.parse_expression()?;
        Ok(Statement {
            kind: StatementKind::VariableDeclaration {
                name,
                var_type,
                initializer,
            },
            pos: start_pos,
        })
    }

    /// Parse return statement
    fn parse_return_statement(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::Return)?;

        let value = if self.current_token() != Some(&Token::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        Ok(Statement {
            kind: StatementKind::Return { value },
            pos: start_pos,
        })
    }

    fn parse_function_declaration(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::Func)?;

        let name = match self.current_token_with_position() {
            Some(twp) if matches!(twp.token, Token::Identifier(_)) => {
                if let Token::Identifier(name) = self.current_token().unwrap().clone() {
                    self.advance();
                    name
                } else {
                    unreachable!()
                }
            }
            _ => {
                let pos = self.current_token_with_position().map(|twp| twp.position.clone()).unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedIdentifier,
                    "Expected function name".to_string(),
                    self.filename.clone(),
                    pos.line,
                    pos.column,
                    pos.offset,
                    pos.length,
                    self.extract_code_snippet(pos.line, pos.column),
                ));
                return Err(());
            }
        };

        self.expect_token(Token::LParen)?;
        let params = self.parse_parameters()?;
        self.expect_token(Token::RParen)?;

        let return_type = if self.current_token() == Some(&Token::Colon) {
            self.advance();
            match self.current_token() {
                Some(Token::IntType) => {
                    self.advance();
                    "int".to_string()
                }
                Some(Token::StringType) => {
                    self.advance();
                    "string".to_string()
                }
                Some(Token::BoolType) => {
                    self.advance();
                    "bool".to_string()
                }
                Some(Token::DoubleType) => {
                    self.advance();
                    "double".to_string()
                }
                Some(Token::VoidType) => {
                    self.advance();
                    "void".to_string()
                }
                Some(Token::Identifier(type_name)) => {
                    let type_name = type_name.clone();
                    self.advance();
                    type_name
                }
                _ => {
                    let pos = self.current_token_with_position().map(|twp| twp.position.clone()).unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedType,
                        "Expected return type".to_string(),
                        self.filename.clone(),
                        pos.line,
                        pos.column,
                        pos.offset,
                        pos.length,
                        self.extract_code_snippet(pos.line, pos.column),
                    ));
                    return Err(());
                }
            }
        } else {
            "void".to_string()
        };

        self.expect_token(Token::LBrace)?;
        let body = self.parse_block_statements()?;
        self.expect_token(Token::RBrace)?;

        Ok(Statement {
            kind: StatementKind::FunctionDeclaration {
                name,
                params,
                return_type,
                body,
            },
            pos: start_pos,
        })
    }

    /// Parse parameters
    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, ()> {
        let mut params = Vec::new();

        if self.current_token() == Some(&Token::RParen) {
            return Ok(params);
        }

        loop {
            let pos = self.current_token_with_position().unwrap().position.clone();
            let name = match self.current_token_with_position() {
                Some(twp) if matches!(twp.token, Token::Identifier(_)) => {
                    if let Token::Identifier(name) = self.current_token().unwrap().clone() {
                        self.advance();
                        name
                    } else {
                        unreachable!()
                    }
                }
                _ => {
                    let pos = self.current_token_with_position().map(|twp| twp.position.clone()).unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedIdentifier,
                        "Expected parameter name".to_string(),
                        self.filename.clone(),
                        pos.line,
                        pos.column,
                        pos.offset,
                        pos.length,
                        self.extract_code_snippet(pos.line, pos.column),
                    ));
                    return Err(());
                }
            };

            self.expect_token(Token::Colon)?;

            let param_type = match self.current_token() {
                Some(Token::IntType) => {
                    self.advance();
                    "int".to_string()
                }
                Some(Token::StringType) => {
                    self.advance();
                    "string".to_string()
                }
                Some(Token::BoolType) => {
                    self.advance();
                    "bool".to_string()
                }
                Some(Token::DoubleType) => {
                    self.advance();
                    "double".to_string()
                }
                Some(Token::Identifier(type_name)) => {
                    let type_name = type_name.clone();
                    self.advance();
                    type_name
                }
                _ => {
                    let pos = self.current_token_with_position().map(|twp| twp.position.clone()).unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedType,
                        "Expected parameter type".to_string(),
                        self.filename.clone(),
                        pos.line,
                        pos.column,
                        pos.offset,
                        pos.length,
                        self.extract_code_snippet(pos.line, pos.column),
                    ));
                    return Err(());
                }
            };

            params.push(Parameter { name, param_type, pos });

            if self.current_token() == Some(&Token::RParen) {
                break;
            }

            self.expect_token(Token::Comma)?;
        }

        Ok(params)
    }

    /// Parse if statement
    fn parse_if_statement(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::If)?;

        let condition = self.parse_expression()?;

        self.expect_token(Token::LBrace)?;

        let then_branch = self.parse_block_statements()?;

        self.expect_token(Token::RBrace)?;

        let mut elif_branches = Vec::new();
        let mut else_branch = None;

        // Parse elif branches
        while let Some(token) = self.current_token() {
            if matches!(*token, Token::Elif) {
                self.advance();
                let elif_condition = self.parse_expression()?;
                self.expect_token(Token::LBrace)?;
                let elif_statements = self.parse_block_statements()?;
                self.expect_token(Token::RBrace)?;
                elif_branches.push((elif_condition, elif_statements));
            } else {
                break;
            }
        }

        // Parse else branch
        if let Some(token) = self.current_token() {
            if matches!(*token, Token::Else) {
                self.advance();
                self.expect_token(Token::LBrace)?;
                else_branch = Some(self.parse_block_statements()?);
                self.expect_token(Token::RBrace)?;
            }
        }

        Ok(Statement {
            kind: StatementKind::IfStatement {
                condition,
                then_branch,
                elif_branches,
                else_branch,
            },
            pos: start_pos,
        })
    }

    /// Parse whilete statement
    fn parse_while_statement(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::While)?;

        let condition = self.parse_expression()?;

        self.expect_token(Token::LBrace)?;
        let body = self.parse_block_statements()?;
        self.expect_token(Token::RBrace)?;

        Ok(Statement {
            kind: StatementKind::WhileStatement {
                condition,
                body,
            },
            pos: start_pos,
        })
    }

    /// Parse a block of statements
    fn parse_block(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::LBrace)?;
        let statements = self.parse_block_statements()?;
        self.expect_token(Token::RBrace)?;
        Ok(Statement {
            kind: StatementKind::Block(statements),
            pos: start_pos,
        })
    }

    /// Parse statements inside a block
    fn parse_block_statements(&mut self) -> Result<Vec<Statement>, ()> {
        let mut statements = Vec::new();

        while let Some(token) = self.current_token() {
            if matches!(*token, Token::RBrace | Token::EOF) {
                break;
            }
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    /// Parse an expression
    fn parse_expression(&mut self) -> Result<Expression, ()> {
        self.parse_cast_expression()
    }
    
    /// Parse type casting expression
    fn parse_cast_expression(&mut self) -> Result<Expression, ()> {
        let mut expr = self.parse_logical_or()?;
        
        while let Some(token) = self.current_token() {
            if *token != Token::As {
                break;
            }
            
            self.advance();
            
            let target_type = match self.current_token() {
                Some(Token::IntType) => "int".to_string(),
                Some(Token::DoubleType) => "double".to_string(),
                Some(Token::StringType) => "string".to_string(),
                Some(Token::BoolType) => "bool".to_string(),
                Some(Token::Identifier(type_name)) => type_name.clone(),
                _ => {
                    let pos = self.current_token_with_position().unwrap().position.clone();
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::TypeError,
                        "Expected type after 'as' operator".to_string(),
                        self.filename.clone(),
                        pos.line,
                        pos.column,
                        pos.offset,
                        pos.length,
                        self.extract_code_snippet(pos.line, pos.column),
                    ));
                    return Err(());
                }
            };
            self.advance();
            
            let pos = expr.pos.clone();
            expr = Expression {
                kind: ExpressionKind::Cast {
                    expr: Box::new(expr),
                    target_type,
                },
                pos,
            };
        }
        
        Ok(expr)
    }

    /// Parse logical OR expression
    fn parse_logical_or(&mut self) -> Result<Expression, ()> {
        let mut left = self.parse_logical_and()?;

        while let Some(token) = self.current_token() {
            if *token != Token::LogicalOr {
                break;
            }
            let operator = BinaryOperator::from(token.clone());
            self.advance();
            let right = self.parse_logical_and()?;
            let pos = left.pos.clone(); 
            left = Expression {
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                pos,
            };
        }

        Ok(left)
    }

    /// Parse logical AND expression
    fn parse_logical_and(&mut self) -> Result<Expression, ()> {
        let mut left = self.parse_equality()?;

        while let Some(token) = self.current_token() {
            if *token != Token::LogicalAnd {
                break;
            }
            let operator = BinaryOperator::from(token.clone());
            self.advance();
            let right = self.parse_equality()?;
            let pos = left.pos.clone();
            left = Expression {
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                pos,
            };
        }

        Ok(left)
    }

    /// Parse equality expressions
    fn parse_equality(&mut self) -> Result<Expression, ()> {
        let mut left = self.parse_comparison()?;

        while let Some(token) = self.current_token() {
            if !matches!(*token, Token::Equal | Token::NotEqual) {
                break;
            }
            let operator = BinaryOperator::from(token.clone());
            self.advance();
            let right = self.parse_comparison()?;
            let pos = left.pos.clone();
            left = Expression {
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                pos,
            };
        }

        Ok(left)
    }

    /// Parse comparison expressions
    fn parse_comparison(&mut self) -> Result<Expression, ()> {
        let mut left = self.parse_term()?;

        while let Some(token) = self.current_token() {
            if !matches!(
                *token,
                Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual
            ) {
                break;
            }
            let operator = BinaryOperator::from(token.clone());
            self.advance();
            let right = self.parse_term()?;
            let pos = left.pos.clone();
            left = Expression {
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                pos,
            };
        }

        Ok(left)
    }

    /// Parse arithmetic terms
    fn parse_term(&mut self) -> Result<Expression, ()> {
        let mut left = self.parse_factor()?;

        while let Some(token) = self.current_token() {
            if !matches!(*token, Token::Plus | Token::Minus) {
                break;
            }
            let operator = BinaryOperator::from(token.clone());
            self.advance();
            let right = self.parse_factor()?;
            let pos = left.pos.clone();
            left = Expression {
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                pos,
            };
        }

        Ok(left)
    }

    /// Parse factors
    fn parse_factor(&mut self) -> Result<Expression, ()> {
        let mut left = self.parse_unary()?;

        while let Some(token) = self.current_token() {
            if !matches!(*token, Token::Star | Token::Slash | Token::Percent) {
                break;
            }
            let operator = BinaryOperator::from(token.clone());
            self.advance();
            let right = self.parse_unary()?;
            let pos = left.pos.clone();
            left = Expression {
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                pos,
            };
        }

        Ok(left)
    }

    /// Parse unary expressions
    fn parse_unary(&mut self) -> Result<Expression, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        if let Some(token) = self.current_token() {
            if matches!(*token, Token::Plus | Token::Minus | Token::LogicalNot) {
                let operator = UnaryOperator::from(token.clone());
                self.advance();
                let operand = self.parse_unary()?;
                return Ok(Expression {
                    kind: ExpressionKind::Unary {
                        operator,
                        operand: Box::new(operand),
                    },
                    pos: start_pos,
                });
            }
        }
        self.parse_primary()
    }

    /// Parse primary
    fn parse_primary(&mut self) -> Result<Expression, ()> {
        let mut expr = self.parse_literal_or_variable()?;

        loop {
            match self.current_token() {
                Some(Token::LParen) => {
                    self.advance(); 
                    let args = self.parse_arguments()?;
                    self.expect_token(Token::RParen)?;
                    let pos = expr.pos.clone();
                    expr = Expression {
                        kind: ExpressionKind::FunctionCall {
                            callee: Box::new(expr),
                            arguments: args,
                        },
                        pos,
                    };
                }
                Some(Token::As) => {
                    let pos = self.current_token_with_position().unwrap().position.clone();
                    self.advance();
                    
                    let target_type = match self.current_token() {
                        Some(Token::IntType) => "int".to_string(),
                        Some(Token::DoubleType) => "double".to_string(),
                        Some(Token::StringType) => "string".to_string(),
                        Some(Token::BoolType) => "bool".to_string(),
                        Some(Token::Identifier(type_name)) => type_name.clone(),
                        _ => {
                            self.errors.add_error(crate::ErrorReport::with_file(
                                crate::ErrorType::TypeError,
                                "Expected type after 'as' operator".to_string(),
                                self.filename.clone(),
                                pos.line,
                                pos.column,
                                pos.offset,
                                pos.length,
                                self.extract_code_snippet(pos.line, pos.column),
                            ));
                            return Err(());
                        }
                    };
                    self.advance();
                    
                    expr = Expression {
                        kind: ExpressionKind::Cast {
                            expr: Box::new(expr),
                            target_type,
                        },
                        pos,
                    };
                }
                Some(Token::Dot) => {
                    self.advance();
                    let member = match self.current_token_with_position() {
                        Some(twp) => match &twp.token {
                            Token::Identifier(name) => {
                                let name = name.clone();
                                self.advance();
                                name
                            }
                            _ => {
                                self.errors.add_error(crate::ErrorReport::with_file(
                                    crate::ErrorType::ExpectedIdentifier,
                                    "Expected identifier after '.'".to_string(),
                                    self.filename.clone(),
                                    twp.position.line,
                                    twp.position.column,
                                    twp.position.offset,
                                    twp.position.length,
                                    self.extract_code_snippet(twp.position.line, twp.position.column),
                                ));
                                return Err(());
                            }
                        },
                        None => {
                            let pos = self.tokens.last().unwrap().position.clone();
                            self.errors.add_error(crate::ErrorReport::with_file(
                                crate::ErrorType::ExpectedIdentifier,
                                "Expected identifier after '.'".to_string(),
                                self.filename.clone(),
                                pos.line,
                                pos.column,
                                pos.offset,
                                pos.length,
                                self.extract_code_snippet(pos.line, pos.column),
                            ));
                            return Err(());
                        }
                    };
                    let pos = expr.pos.clone();
                    expr = Expression {
                        kind: ExpressionKind::MemberAccess {
                            object: Box::new(expr),
                            member,
                        },
                        pos,
                    };
                }
                Some(Token::LBrace) => {
                    if let ExpressionKind::Variable(type_name) = &expr.kind {
                        let looks_like_type = type_name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
                        if !looks_like_type {
                            break;
                        }
                        self.advance();
                        let mut fields = Vec::new();
                        
                        while let Some(token) = self.current_token() {
                            if token == &Token::RBrace {
                                break;
                            }
                            
                            let field_name = match self.current_token_with_position() {
                                Some(twp) => match &twp.token {
                                    Token::Identifier(name) => {
                                        let name = name.clone();
                                        self.advance();
                                        name
                                    }
                                    _ => {
                                        self.errors.add_error(crate::ErrorReport::with_file(
                                            crate::ErrorType::ExpectedIdentifier,
                                            "Expected field name in struct literal".to_string(),
                                            self.filename.clone(),
                                            twp.position.line,
                                            twp.position.column,
                                            twp.position.offset,
                                            twp.position.length,
                                            self.extract_code_snippet(twp.position.line, twp.position.column),
                                        ));
                                        self.synchronize();
                                        continue;
                                    }
                                },
                                None => {
                                    let pos = self.tokens.last().unwrap().position.clone();
                                    self.errors.add_error(crate::ErrorReport::with_file(
                                        crate::ErrorType::ExpectedIdentifier,
                                        "Expected field name in struct literal".to_string(),
                                        self.filename.clone(),
                                        pos.line,
                                        pos.column,
                                        pos.offset,
                                        pos.length,
                                        self.extract_code_snippet(pos.line, pos.column),
                                    ));
                                    self.synchronize();
                                    continue;
                                }
                            };
                            
                            let field_value = if self.current_token() == Some(&Token::Colon) {
                                self.advance();
                                self.parse_expression()?
                            } else {
                                let pos = expr.pos.clone();
                                Expression {
                                    kind: ExpressionKind::Variable(field_name.clone()),
                                    pos,
                                }
                            };
                            
                            fields.push((field_name, field_value));
                            
                            match self.current_token() {
                                Some(Token::Comma) => {
                                    self.advance();
                                }
                                Some(Token::RBrace) => {
                                    break;
                                }
                                _ => {
                                    let pos = self.current_token_with_position().map(|twp| twp.position.clone()).unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                                    self.errors.add_error(crate::ErrorReport::with_file(
                                        crate::ErrorType::ExpectedToken(", or }".to_string()),
                                        "Expected ',' or '}' in struct literal".to_string(),
                                        self.filename.clone(),
                                        pos.line,
                                        pos.column,
                                        pos.offset,
                                        pos.length,
                                        self.extract_code_snippet(pos.line, pos.column),
                                    ));
                                    self.synchronize();
                                    continue;
                                }
                            }
                        }
                        
                        self.expect_token(Token::RBrace)?;
                        
                        let pos = expr.pos.clone();
                        expr = Expression {
                            kind: ExpressionKind::StructLiteral {
                                type_name: type_name.clone(),
                                fields,
                            },
                            pos,
                        };
                    } else {
                        break;
                    }
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    /// Parse arguments
    fn parse_arguments(&mut self) -> Result<Vec<Expression>, ()> {
        let mut args = Vec::new();
        if self.current_token() == Some(&Token::RParen) {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expression()?);
            if self.current_token() != Some(&Token::Comma) {
                break;
            }
            self.advance();
        }

        Ok(args)
    }

    /// Parse literal or variable
    fn parse_literal_or_variable(&mut self) -> Result<Expression, ()> {
        let twp = self.current_token_with_position().cloned();
        match twp {
            Some(twp) => match &twp.token {
                Token::StringLiteral(s) => {
                    let original = s.clone();
                    self.advance();

                    if !original.contains("${") {
                        return Ok(Expression { kind: ExpressionKind::StringLiteral(original), pos: twp.position });
                    }

                    let mut parts: Vec<Result<Expression, ()>> = Vec::new();
                    let mut buf = String::new();
                    let chars: Vec<char> = original.chars().collect();
                    let mut i = 0;
                    while i < chars.len() {
                        if chars[i] == '\\' {
                            if i + 1 < chars.len() {
                                if chars[i + 1] == '$' {
                                    buf.push('$');
                                    i += 2;
                                    continue;
                                } else {
                                    buf.push(chars[i + 1]);
                                    i += 2;
                                    continue;
                                }
                            } else {
                                buf.push('\\');
                                i += 1;
                                continue;
                            }
                        }
                        if chars[i] == '$' && i + 1 < chars.len() && chars[i + 1] == '{' {
                            if !buf.is_empty() {
                                let text_expr = Expression {
                                    kind: ExpressionKind::StringLiteral(buf.clone()),
                                    pos: twp.position.clone(),
                                };
                                parts.push(Ok(text_expr));
                                buf.clear();
                            }
                            i += 2;
                            let start_expr = i;
                            let mut found = false;
                            while i < chars.len() {
                                if chars[i] == '}' {
                                    found = true;
                                    break;
                                }
                                i += 1;
                            }
                            if !found {
                                self.errors.add_error(crate::ErrorReport::with_file(
                                    crate::ErrorType::SyntaxError,
                                    "Unterminated interpolation: expected '}'".to_string(),
                                    self.filename.clone(),
                                    twp.position.line,
                                    twp.position.column,
                                    twp.position.offset,
                                    twp.position.length,
                                    self.extract_code_snippet(twp.position.line, twp.position.column),
                                ));
                                return Err(());
                            }
                            let inner: String = chars[start_expr..i].iter().collect();
                            i += 1;

                            let mut lexer = crate::lexer::Lexer::new(&inner, format!("{}<interp>", self.filename));
                            let tokens = lexer.tokenize();
                            let mut p = Parser::new(tokens, inner.clone(), format!("{}<interp>", self.filename));
                            match p.parse_expression() {
                                Ok(expr) => parts.push(Ok(expr)),
                                Err(_) => {
                                    self.errors.add_error(crate::ErrorReport::with_file(
                                        crate::ErrorType::SyntaxError,
                                        "Invalid expression inside interpolation".to_string(),
                                        self.filename.clone(),
                                        twp.position.line,
                                        twp.position.column,
                                        twp.position.offset,
                                        twp.position.length,
                                        self.extract_code_snippet(twp.position.line, twp.position.column),
                                    ));
                                    return Err(());
                                }
                            }
                            continue;
                        }
                        buf.push(chars[i]);
                        i += 1;
                    }

                    if !buf.is_empty() {
                        let text_expr = Expression { kind: ExpressionKind::StringLiteral(buf.clone()), pos: twp.position.clone() };
                        parts.push(Ok(text_expr));
                    }

                    let mut iter = parts.into_iter();
                    let first = match iter.next() { Some(Ok(e)) => e, _ => {
                        return Ok(Expression { kind: ExpressionKind::StringLiteral(String::new()), pos: twp.position });
                    } };
                    let mut acc = first;
                    for part in iter {
                        let rhs = part?;
                        acc = Expression {
                            kind: ExpressionKind::Binary { left: Box::new(acc), operator: BinaryOperator::Add, right: Box::new(rhs) },
                            pos: twp.position.clone(),
                        };
                    }
                    Ok(acc)
                }
                Token::IntLiteral(n) => {
                    let value = *n;
                    self.advance();
                    Ok(Expression {
                        kind: ExpressionKind::IntLiteral(value),
                        pos: twp.position,
                    })
                }
                Token::DoubleLiteral(n) => {
                    let value = *n;
                    self.advance();
                    Ok(Expression {
                        kind: ExpressionKind::DoubleLiteral(value),
                        pos: twp.position,
                    })
                }
                Token::True => {
                    self.advance();
                    Ok(Expression {
                        kind: ExpressionKind::BoolLiteral(true),
                        pos: twp.position,
                    })
                }
                Token::False => {
                    self.advance();
                    Ok(Expression {
                        kind: ExpressionKind::BoolLiteral(false),
                        pos: twp.position,
                    })
                }
                Token::Identifier(name) => {
                    let name = name.clone();
                    self.advance();
                    Ok(Expression {
                        kind: ExpressionKind::Variable(name),
                        pos: twp.position,
                    })
                }
                Token::LParen => {
                    self.advance();
                    let expr = self.parse_expression()?;
                    self.expect_token(Token::RParen)?;
                    Ok(expr)
                }
                Token::LBrace => {
                    self.advance();
                    let expr = self.parse_expression()?;
                    self.expect_token(Token::RBrace)?;
                    Ok(expr)
                }
                _ => {
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedExpression,
                        "Expected expression".to_string(),
                        self.filename.clone(),
                        twp.position.line,
                        twp.position.column,
                        twp.position.offset,
                        twp.position.length,
                        self.extract_code_snippet(twp.position.line, twp.position.column),
                    ));
                    Err(())
                }
            },
            None => {
                 let pos = self.current_token_with_position().map(|twp| twp.position.clone()).unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                 self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedExpression,
                    "Expected expression".to_string(),
                    self.filename.clone(),
                    pos.line,
                    pos.column,
                    pos.offset,
                    pos.length,
                    self.extract_code_snippet(pos.line, pos.column),
                ));
                Err(())
            }
        }
    }

    /// Except token
    fn expect_token(&mut self, expected: Token) -> Result<(), ()> {
        if let Some(twp) = self.current_token_with_position() {
            if twp.token == expected {
                self.advance();
                Ok(())
            } else {
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedToken(format!("{}", expected)),
                    format!("Expected '{}', found '{}'", expected, twp.token),
                    self.filename.clone(),
                    twp.position.line,
                    twp.position.column,
                    twp.position.offset,
                    twp.position.length,
                    self.extract_code_snippet(twp.position.line, twp.position.column),
                ));
                Err(())
            }
        } else {
            let last_token = self.tokens.last().unwrap();
            self.errors.add_error(crate::ErrorReport::with_file(
                crate::ErrorType::ExpectedToken(format!("{}", expected)),
                format!("Expected '{}', found EOF", expected),
                self.filename.clone(),
                last_token.position.line,
                last_token.position.column,
                last_token.position.offset,
                last_token.position.length,
                self.extract_code_snippet(last_token.position.line, last_token.position.column),
            ));
            Err(())
        }
    }

    /// Synchronize parser after an error
    fn synchronize(&mut self) {
        while let Some(token) = self.current_token() {
            if matches!(*token, Token::Semicolon | Token::RBrace) {
                self.advance();
                break;
            }
            self.advance();
        }
    }

    /// Get current token
    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.position).map(|t| &t.token)
    }

    /// Get current token with position
    fn current_token_with_position(&self) -> Option<&TokenWithPosition> {
        self.tokens.get(self.position)
    }

    /// Advance to next token
    fn advance(&mut self) {
        if self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    /// Extract full line from source based on line number
    fn extract_code_snippet(&self, line: usize, _column: usize) -> String {
        let lines: Vec<&str> = self.source.lines().collect();
        if line > 0 && line <= lines.len() {
            lines[line - 1].to_string()
        } else {
            "".to_string()
        }
    }
}
