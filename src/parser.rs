//! Parser for the Frut
//!
//! Converts a stream of tokens into an Abstract Syntax Tree (AST).

use alloc::boxed::Box;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use crate::ast::{
    BinaryOperator, Expression, ExpressionKind, ImportItem, ImportStatement, Parameter, Statement,
    StatementKind, Token, TokenWithPosition, UnaryOperator,
};
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
                    if self.current_token().is_none_or(|t| t == &Token::EOF) {
                        break;
                    }
                }
            }
        }

        crate::ParseResult::new(statements, self.errors.clone(), self.filename.clone())
    }

    /// Parse statement
    fn parse_statement(&mut self) -> Result<Statement, ()> {
        let start_pos = self
            .current_token_with_position()
            .map(|twp| twp.position.clone());

        let result = match self.current_token() {
            Some(Token::Var) => self.parse_variable_declaration(),
            Some(Token::If) => self.parse_if_statement(),
            Some(Token::Match) => self.parse_match_statement(),
            Some(Token::While) => self.parse_while_statement(),
            Some(Token::Break) => {
                let pos = self.current_token_with_position().unwrap().position.clone();
                self.advance();
                Ok(Statement {
                    kind: StatementKind::Break,
                    pos,
                })
            }
            Some(Token::Func) => self.parse_function_declaration(),
            Some(Token::Foreign) => self.parse_foreign_function_declaration(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(Token::Type) => self.parse_type_declaration(),
            Some(Token::Enum) => self.parse_enum_declaration(),
            Some(Token::Ext) => self.parse_ext_declaration(),
            Some(Token::Interface) => self.parse_interface_declaration(),
            Some(Token::Import) => self.parse_import_statement(),
            Some(Token::LBrace) => self.parse_block(),
            Some(Token::Identifier(_)) => {
                let start_pos = self.current_token_with_position().unwrap().position.clone();
                let expr = self.parse_expression()?;
                if self.current_token() == Some(&Token::Assign) {
                    match &expr.kind {
                        ExpressionKind::Variable(_)
                        | ExpressionKind::MemberAccess { .. }
                        | ExpressionKind::Index { .. } => {
                            self.advance();
                            let value = self.parse_expression()?;
                            Ok(Statement {
                                kind: StatementKind::Assignment {
                                    target: expr,
                                    value,
                                },
                                pos: start_pos,
                            })
                        }
                        _ => {
                            let pos = self
                                .current_token_with_position()
                                .map(|twp| twp.position.clone())
                                .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
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
                    Ok(Statement {
                        kind: StatementKind::ExpressionStatement(expr),
                        pos: start_pos,
                    })
                }
            }
            _ => {
                let pos = self
                    .current_token_with_position()
                    .map(|twp| twp.position.clone())
                    .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
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
            if matches!(
                &stmt.kind,
                StatementKind::VariableDeclaration { .. }
                    | StatementKind::Assignment { .. }
                    | StatementKind::Return { .. }
                    | StatementKind::ExpressionStatement { .. }
                    | StatementKind::ImportStatement { .. }
                    | StatementKind::ForeignFunctionDeclaration { .. }
                    | StatementKind::Break
            ) {
                if let Some(token) = self.current_token() {
                    if token == &Token::Semicolon {
                        self.advance();
                    }
                }
            }
        }
        result
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

        let (type_params, type_param_bounds) = if self.current_token() == Some(&Token::Less) {
            self.parse_type_params()?
        } else {
            (Vec::new(), Vec::new())
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
                Some(_) => self.parse_type_name()?,
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
                    let pos = self
                        .current_token_with_position()
                        .map(|twp| twp.position.clone())
                        .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
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
            kind: StatementKind::TypeDeclaration {
                name,
                type_params,
                type_param_bounds,
                fields,
            },
            pos: start_pos,
        })
    }

    /// Parse enum declaration: `enum Name { Variant, Other(int, string), ... }`
    fn parse_enum_declaration(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::Enum)?;

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
                        "Expected identifier after 'enum'".to_string(),
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
                    "Expected identifier after 'enum'".to_string(),
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

        let (type_params, type_param_bounds) = if self.current_token() == Some(&Token::Less) {
            self.parse_type_params()?
        } else {
            (Vec::new(), Vec::new())
        };

        self.expect_token(Token::LBrace)?;

        let mut variants = Vec::new();
        while let Some(token) = self.current_token() {
            if token == &Token::RBrace {
                break;
            }

            let variant_pos = self.current_token_with_position().unwrap().position.clone();
            let variant_name = match self.current_token_with_position() {
                Some(twp) => match &twp.token {
                    Token::Identifier(name) => {
                        let name = name.clone();
                        self.advance();
                        name
                    }
                    _ => {
                        self.errors.add_error(crate::ErrorReport::with_file(
                            crate::ErrorType::ExpectedIdentifier,
                            "Expected variant name".to_string(),
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
                        "Expected variant name".to_string(),
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

            let payload_types = if self.current_token() == Some(&Token::LParen) {
                self.advance();
                let types = self.parse_enum_variant_payload()?;
                self.expect_token(Token::RParen)?;
                types
            } else {
                Vec::new()
            };

            variants.push(crate::ast::EnumVariantDecl {
                name: variant_name,
                payload_types,
                pos: variant_pos,
            });

            match self.current_token() {
                Some(Token::Comma) => {
                    self.advance();
                }
                Some(Token::RBrace) => {
                    break;
                }
                _ => {
                    let pos = self
                        .current_token_with_position()
                        .map(|twp| twp.position.clone())
                        .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedToken(", or }".to_string()),
                        "Expected ',' or '}' in enum declaration".to_string(),
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
            kind: StatementKind::EnumDeclaration {
                name,
                type_params,
                type_param_bounds,
                variants,
            },
            pos: start_pos,
        })
    }

    /// Parse the payload types of an enum variant: `int, string`
    fn parse_enum_variant_payload(&mut self) -> Result<Vec<crate::ast::TypeExpr>, ()> {
        let mut types = Vec::new();

        if self.current_token() == Some(&Token::RParen) {
            return Ok(types);
        }

        loop {
            let type_name = match self.current_token() {
                Some(_) => self.parse_type_name()?,
                None => {
                    let pos = self.tokens.last().unwrap().position.clone();
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedType,
                        "Expected type name in enum variant payload".to_string(),
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
            types.push(type_name);

            if self.current_token() == Some(&Token::RParen) {
                break;
            }
            self.expect_token(Token::Comma)?;
        }

        Ok(types)
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
                Token::BytesType => {
                    self.advance();
                    "bytes".to_string()
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

        let (ext_type_params, _ext_type_param_bounds) =
            if self.current_token() == Some(&Token::Less) {
                self.parse_type_params()?
            } else {
                (Vec::new(), Vec::new())
            };

        let mut interfaces = Vec::new();
        let mut interface_args = Vec::new();
        if self.current_token() == Some(&Token::Colon) {
            self.advance();
            loop {
                let iface_name = match self.current_token() {
                    Some(Token::Identifier(name)) => {
                        let name = name.clone();
                        self.advance();
                        name
                    }
                    _ => {
                        let pos = self
                            .current_token_with_position()
                            .map(|twp| twp.position.clone())
                            .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                        self.errors.add_error(crate::ErrorReport::with_file(
                            crate::ErrorType::ExpectedIdentifier,
                            "Expected interface name after ':' in ext declaration".to_string(),
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
                let args = if self.current_token() == Some(&Token::Less) {
                    self.parse_type_args()?
                } else {
                    Vec::new()
                };
                interfaces.push(iface_name);
                interface_args.push(args);
                if self.current_token() == Some(&Token::Comma) {
                    self.advance();
                    continue;
                }
                break;
            }
        }

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
            kind: StatementKind::ExtDeclaration {
                type_name,
                type_params: ext_type_params,
                interfaces,
                interface_args,
                methods,
            },
            pos: start_pos,
        })
    }

    /// Parse an interface declaration: `interface Name { func name(params): Ret; ... }`
    /// Method signatures only (no bodies, no `self`)
    fn parse_interface_declaration(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::Interface)?;

        let name = match self.current_token() {
            Some(Token::Identifier(name)) => {
                let name = name.clone();
                self.advance();
                name
            }
            _ => {
                let pos = self
                    .current_token_with_position()
                    .map(|twp| twp.position.clone())
                    .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedIdentifier,
                    "Expected interface name".to_string(),
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

        let (type_params, type_param_bounds) = if self.current_token() == Some(&Token::Less) {
            self.parse_type_params()?
        } else {
            (Vec::new(), Vec::new())
        };

        self.expect_token(Token::LBrace)?;

        let mut methods = Vec::new();
        while let Some(token) = self.current_token() {
            if token == &Token::RBrace {
                break;
            }
            let method_pos = self
                .current_token_with_position()
                .map(|twp| twp.position.clone())
                .unwrap_or_else(|| start_pos.clone());
            if self.current_token() != Some(&Token::Func) {
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedStatement,
                    "Expected 'func' method signature in interface".to_string(),
                    self.filename.clone(),
                    method_pos.line,
                    method_pos.column,
                    method_pos.offset,
                    method_pos.length,
                    self.extract_code_snippet(method_pos.line, method_pos.column),
                ));
                return Err(());
            }
            self.advance(); // consume `func`

            let mname = match self.current_token() {
                Some(Token::Identifier(mname)) => {
                    let mname = mname.clone();
                    self.advance();
                    mname
                }
                _ => {
                    let pos = self
                        .current_token_with_position()
                        .map(|twp| twp.position.clone())
                        .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedIdentifier,
                        "Expected interface method name".to_string(),
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

            let params = if let Some(Token::Identifier(n)) = self.current_token() {
                if n == "self"
                    && matches!(
                        self.peek_token(1),
                        Some(&Token::RParen) | Some(&Token::Comma)
                    )
                {
                    self.advance();
                    if self.current_token() == Some(&Token::Comma) {
                        self.advance();
                        self.parse_parameters()?
                    } else {
                        Vec::new()
                    }
                } else {
                    self.parse_parameters()?
                }
            } else {
                self.parse_parameters()?
            };
            self.expect_token(Token::RParen)?;

            let return_type = if self.current_token() == Some(&Token::Colon) {
                self.advance();
                match self.current_token() {
                    Some(_) => self.parse_type_name()?,
                    None => {
                        let pos = self.tokens.last().unwrap().position.clone();
                        self.errors.add_error(crate::ErrorReport::with_file(
                            crate::ErrorType::ExpectedType,
                            "Expected return type in interface method".to_string(),
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
                crate::ast::TypeExpr::Void
            };

            if self.current_token() == Some(&Token::Semicolon) {
                self.advance();
            }

            methods.push(crate::ast::InterfaceMethodDecl {
                name: mname,
                params,
                return_type,
                pos: method_pos,
            });
        }

        self.expect_token(Token::RBrace)?;

        Ok(Statement {
            kind: StatementKind::InterfaceDeclaration {
                name,
                type_params,
                type_param_bounds,
                methods,
            },
            pos: start_pos,
        })
    }

    /// Parse import statement
    fn parse_import_statement(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::Import)?;

        let mut library_path = String::new();

        match self.current_token_with_position() {
            Some(twp) => match &twp.token {
                Token::Identifier(name) => {
                    library_path.push_str(name);
                    self.advance();
                }
                _ => {
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedIdentifier,
                        "Expected library name".to_string(),
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
                    "Expected library name".to_string(),
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

        while self.current_token() == Some(&Token::Dot) {
            self.advance();
            match self.current_token_with_position() {
                Some(twp) => match &twp.token {
                    Token::Identifier(module_name) => {
                        library_path.push('.');
                        library_path.push_str(module_name);
                        self.advance();
                    }
                    Token::Star => {
                        // Wildcard import
                        self.advance();
                        let import_stmt = ImportStatement {
                            library_path,
                            item: ImportItem::Wildcard,
                        };
                        return Ok(Statement {
                            kind: StatementKind::ImportStatement(import_stmt),
                            pos: start_pos,
                        });
                    }
                    _ => {
                        self.errors.add_error(crate::ErrorReport::with_file(
                            crate::ErrorType::ExpectedIdentifier,
                            "Expected module name or * after dot".to_string(),
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
                        "Expected module name or * after dot".to_string(),
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
        }

        // Parse the final item (specific identifier)
        let item = if self.current_token() == Some(&Token::Dot) {
            self.advance();
            match self.current_token_with_position() {
                Some(twp) => match &twp.token {
                    Token::Identifier(item_name) => {
                        let item_name = item_name.clone();
                        self.advance();
                        ImportItem::Identifier(item_name)
                    }
                    Token::Star => {
                        self.advance();
                        ImportItem::Wildcard
                    }
                    _ => {
                        self.errors.add_error(crate::ErrorReport::with_file(
                            crate::ErrorType::ExpectedIdentifier,
                            "Expected identifier or * after dot".to_string(),
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
                        "Expected identifier or * after dot".to_string(),
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
            let parts: Vec<&str> = library_path.rsplitn(2, '.').collect();
            if parts.len() == 2 {
                let item_name = parts[0].to_string();
                library_path = parts[1].to_string();
                ImportItem::Identifier(item_name)
            } else {
                let pos = self
                    .current_token_with_position()
                    .map(|twp| twp.position.clone())
                    .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedIdentifier,
                    "Expected import item".to_string(),
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

        let import_stmt = ImportStatement { library_path, item };

        Ok(Statement {
            kind: StatementKind::ImportStatement(import_stmt),
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
                let pos = self
                    .current_token_with_position()
                    .map(|twp| twp.position.clone())
                    .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
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

        let var_type = if self.current_token() == Some(&Token::Colon) {
            self.advance();
            match self.current_token_with_position() {
                Some(_) => self.parse_type_name()?,
                None => {
                    let pos = self
                        .current_token_with_position()
                        .map(|twp| twp.position.clone())
                        .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedType,
                        "Expected type after ':'".to_string(),
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
            crate::ast::TypeExpr::Inferred
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
                let pos = self
                    .current_token_with_position()
                    .map(|twp| twp.position.clone())
                    .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
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

        let (type_params, type_param_bounds) = if self.current_token() == Some(&Token::Less) {
            self.parse_type_params()?
        } else {
            (Vec::new(), Vec::new())
        };

        self.expect_token(Token::LParen)?;
        let params = self.parse_parameters()?;
        self.expect_token(Token::RParen)?;

        let return_type = if self.current_token() == Some(&Token::Colon) {
            self.advance();
            match self.current_token() {
                Some(_) => self.parse_type_name()?,
                None => {
                    let pos = self.tokens.last().unwrap().position.clone();
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
            crate::ast::TypeExpr::Void
        };

        self.expect_token(Token::LBrace)?;
        let body = self.parse_block_statements()?;
        self.expect_token(Token::RBrace)?;

        Ok(Statement {
            kind: StatementKind::FunctionDeclaration {
                name,
                type_params,
                type_param_bounds,
                params,
                return_type,
                body,
            },
            pos: start_pos,
        })
    }

    /// Parse generic type parameters: `<T, U>` with optional interface
    /// bounds: `<T: Shape, U>`.
    fn parse_type_params(&mut self) -> Result<(Vec<String>, Vec<Option<String>>), ()> {
        self.expect_token(Token::Less)?;
        let mut params = Vec::new();
        let mut bounds = Vec::new();
        if self.current_token() == Some(&Token::Greater) {
            self.advance();
            return Ok((params, bounds));
        }
        loop {
            let name = match self.current_token() {
                Some(Token::Identifier(name)) => {
                    let name = name.clone();
                    self.advance();
                    name
                }
                _ => {
                    let pos = self
                        .current_token_with_position()
                        .map(|twp| twp.position.clone())
                        .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedIdentifier,
                        "Expected type parameter name".to_string(),
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
            let bound = if self.current_token() == Some(&Token::Colon) {
                self.advance();
                match self.current_token() {
                    Some(Token::Identifier(iface)) => {
                        let iface = iface.clone();
                        self.advance();
                        Some(iface)
                    }
                    _ => {
                        let pos = self
                            .current_token_with_position()
                            .map(|twp| twp.position.clone())
                            .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                        self.errors.add_error(crate::ErrorReport::with_file(
                            crate::ErrorType::ExpectedIdentifier,
                            "Expected interface name after ':' in type parameter bound".to_string(),
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
                None
            };
            params.push(name);
            bounds.push(bound);
            if self.current_token() == Some(&Token::Comma) {
                self.advance();
                continue;
            }
            break;
        }
        self.expect_token(Token::Greater)?;
        Ok((params, bounds))
    }

    /// Parse generic type arguments: `<int, T>` (as type expressions)
    fn parse_type_args(&mut self) -> Result<Vec<crate::ast::TypeExpr>, ()> {
        self.expect_token(Token::Less)?;
        let mut args = Vec::new();
        if self.current_token() == Some(&Token::Greater) {
            self.advance();
            return Ok(args);
        }
        loop {
            args.push(self.parse_type_name()?);
            if self.current_token() == Some(&Token::Comma) {
                self.advance();
                continue;
            }
            break;
        }
        self.expect_token(Token::Greater)?;
        Ok(args)
    }

    /// Try resolve the call of explicitly typed arguments: `vec_new<int>()`
    fn try_parse_call_type_args(&mut self) -> Option<Vec<crate::ast::TypeExpr>> {
        let saved_pos = self.position;
        let saved_errors = self.errors.errors.len();
        let attempt = (|| {
            self.advance();
            let mut args = Vec::new();
            if self.current_token() == Some(&Token::Greater) {
                self.advance();
                return Some(args);
            }
            loop {
                args.push(self.parse_type_name().ok()?);
                if self.current_token() == Some(&Token::Comma) {
                    self.advance();
                    continue;
                }
                break;
            }
            if self.current_token() != Some(&Token::Greater) {
                return None;
            }
            self.advance();
            if self.current_token() != Some(&Token::LParen) {
                return None;
            }
            Some(args)
        })();
        if attempt.is_none() {
            self.position = saved_pos;
            self.errors.errors.truncate(saved_errors);
        }
        attempt
    }

    /// Can this expression be a call calee for type args
    fn is_named_callee(expr: &Expression) -> bool {
        matches!(
            &expr.kind,
            ExpressionKind::Variable(_) | ExpressionKind::MemberAccess { .. }
        )
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
                    let pos = self
                        .current_token_with_position()
                        .map(|twp| twp.position.clone())
                        .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
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
                Some(_) => self.parse_type_name()?,
                None => {
                    let pos = self.tokens.last().unwrap().position.clone();
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

            params.push(Parameter {
                name,
                param_type,
                pos,
            });

            if self.current_token() == Some(&Token::RParen) {
                break;
            }

            self.expect_token(Token::Comma)?;
        }

        Ok(params)
    }

    fn parse_foreign_function_declaration(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::Foreign)?;
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
                let pos = self
                    .current_token_with_position()
                    .map(|twp| twp.position.clone())
                    .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
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

        let (type_params, type_param_bounds) = if self.current_token() == Some(&Token::Less) {
            self.parse_type_params()?
        } else {
            (Vec::new(), Vec::new())
        };

        self.expect_token(Token::LParen)?;
        let params = self.parse_parameters()?;
        self.expect_token(Token::RParen)?;

        let return_type = if self.current_token() == Some(&Token::Colon) {
            self.advance();
            match self.current_token() {
                Some(_) => self.parse_type_name()?,
                None => {
                    let pos = self.tokens.last().unwrap().position.clone();
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
            crate::ast::TypeExpr::Void
        };

        self.expect_token(Token::Semicolon)?;

        Ok(Statement {
            kind: StatementKind::ForeignFunctionDeclaration {
                name,
                type_param_bounds,
                type_params,
                params,
                return_type,
            },
            pos: start_pos,
        })
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
            kind: StatementKind::WhileStatement { condition, body },
            pos: start_pos,
        })
    }

    /// Parse match statement:
    /// `match expr { pattern => body, ... }`
    fn parse_match_statement(&mut self) -> Result<Statement, ()> {
        let start_pos = self.current_token_with_position().unwrap().position.clone();
        self.expect_token(Token::Match)?;

        let scrutinee = self.parse_expression()?;

        self.expect_token(Token::LBrace)?;

        let mut arms: Vec<crate::ast::MatchArm> = Vec::new();
        loop {
            match self.current_token() {
                Some(Token::RBrace) => {
                    self.advance();
                    break;
                }
                Some(Token::EOF) => {
                    let pos = self
                        .current_token_with_position()
                        .map(|twp| twp.position.clone())
                        .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::UnexpectedToken,
                        "Expected '}' to close match statement".to_string(),
                        self.filename.clone(),
                        pos.line,
                        pos.column,
                        pos.offset,
                        pos.length,
                        self.extract_code_snippet(pos.line, pos.column),
                    ));
                    return Err(());
                }
                _ => {
                    let arm = self.parse_match_arm()?;
                    arms.push(arm);
                }
            }
        }

        Ok(Statement {
            kind: StatementKind::MatchStatement { scrutinee, arms },
            pos: start_pos,
        })
    }

    /// Parse one match arm: `pattern => body [,]`
    fn parse_match_arm(&mut self) -> Result<crate::ast::MatchArm, ()> {
        let start_pos = self
            .current_token_with_position()
            .map(|twp| twp.position.clone())
            .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());

        let pattern = self.parse_or_pattern()?;

        self.expect_token(Token::FatArrow)?;

        let body = match self.current_token() {
            Some(Token::LBrace) => {
                self.advance();
                let stmts = self.parse_block_statements()?;
                self.expect_token(Token::RBrace)?;
                stmts
            }
            _ => vec![self.parse_statement()?],
        };

        if self.current_token() == Some(&Token::Comma) {
            self.advance();
        }

        Ok(crate::ast::MatchArm {
            pattern,
            body,
            pos: start_pos,
        })
    }

    /// Parse an or-pattern: `atomic | atomic | ...` (top level of an arm)
    fn parse_or_pattern(&mut self) -> Result<crate::ast::Pattern, ()> {
        let mut alts = vec![self.parse_atomic_pattern()?];
        while self.current_token() == Some(&Token::Pipe) {
            self.advance();
            alts.push(self.parse_atomic_pattern()?);
        }
        if alts.len() == 1 {
            Ok(alts.pop().unwrap())
        } else {
            Ok(crate::ast::Pattern::Or(alts))
        }
    }

    /// Parse an atomic pattern: `_`, a literal, `Variant(args)`,
    /// `Enum.Variant(args)`, or a bare identifier (binding inside args)
    fn parse_atomic_pattern(&mut self) -> Result<crate::ast::Pattern, ()> {
        let twp = match self.current_token_with_position() {
            Some(twp) => twp,
            None => {
                let pos = self.tokens.last().unwrap().position.clone();
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedExpression,
                    "Expected pattern".to_string(),
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
        let pos = twp.position.clone();
        match &twp.token {
            Token::Identifier(name) if name == "_" => {
                self.advance();
                Ok(crate::ast::Pattern::Wildcard)
            }
            Token::IntLiteral(n) => {
                let n = *n;
                self.advance();
                Ok(crate::ast::Pattern::Literal(
                    crate::ast::PatternLiteral::Int(n),
                ))
            }
            Token::DoubleLiteral(n) => {
                let n = *n;
                self.advance();
                Ok(crate::ast::Pattern::Literal(
                    crate::ast::PatternLiteral::Double(n),
                ))
            }
            Token::StringLiteral(s) => {
                let s = s.clone();
                self.advance();
                Ok(crate::ast::Pattern::Literal(
                    crate::ast::PatternLiteral::String(s),
                ))
            }
            Token::True => {
                self.advance();
                Ok(crate::ast::Pattern::Literal(
                    crate::ast::PatternLiteral::Bool(true),
                ))
            }
            Token::False => {
                self.advance();
                Ok(crate::ast::Pattern::Literal(
                    crate::ast::PatternLiteral::Bool(false),
                ))
            }
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                if self.current_token() == Some(&Token::Dot) {
                    self.advance();
                    let variant = match self.current_token_with_position() {
                        Some(twp) => match &twp.token {
                            Token::Identifier(v) => v.clone(),
                            _ => {
                                self.errors.add_error(crate::ErrorReport::with_file(
                                    crate::ErrorType::ExpectedIdentifier,
                                    "Expected variant name after '.' in pattern".to_string(),
                                    self.filename.clone(),
                                    twp.position.line,
                                    twp.position.column,
                                    twp.position.offset,
                                    twp.position.length,
                                    self.extract_code_snippet(
                                        twp.position.line,
                                        twp.position.column,
                                    ),
                                ));
                                return Err(());
                            }
                        },
                        None => {
                            let pos = self.tokens.last().unwrap().position.clone();
                            self.errors.add_error(crate::ErrorReport::with_file(
                                crate::ErrorType::ExpectedIdentifier,
                                "Expected variant name after '.' in pattern".to_string(),
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
                    let args = if self.current_token() == Some(&Token::LParen) {
                        self.advance();
                        let args = self.parse_pattern_args()?;
                        self.expect_token(Token::RParen)?;
                        args
                    } else {
                        Vec::new()
                    };
                    Ok(crate::ast::Pattern::EnumVariant {
                        enum_name: Some(name),
                        variant,
                        args,
                    })
                } else if self.current_token() == Some(&Token::LParen) {
                    self.advance();
                    let args = self.parse_pattern_args()?;
                    self.expect_token(Token::RParen)?;
                    Ok(crate::ast::Pattern::EnumVariant {
                        enum_name: None,
                        variant: name,
                        args,
                    })
                } else {
                    Ok(crate::ast::Pattern::EnumVariant {
                        enum_name: None,
                        variant: name,
                        args: Vec::new(),
                    })
                }
            }
            _ => {
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedExpression,
                    format!("Expected pattern, found {}", twp.token),
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

    /// Parse the arguments of a variant pattern: `v, _, 42` (no `|` inside)
    fn parse_pattern_args(&mut self) -> Result<Vec<crate::ast::Pattern>, ()> {
        let mut args = Vec::new();
        if self.current_token() == Some(&Token::RParen) {
            return Ok(args);
        }
        loop {
            let arg = self.parse_pattern_arg()?;
            args.push(arg);
            match self.current_token() {
                Some(Token::Comma) => {
                    self.advance();
                }
                _ => break,
            }
        }
        Ok(args)
    }

    /// Parse one pattern argument: `_`, a literal, or a binding identifier
    fn parse_pattern_arg(&mut self) -> Result<crate::ast::Pattern, ()> {
        let twp = match self.current_token_with_position() {
            Some(twp) => twp,
            None => {
                let pos = self.tokens.last().unwrap().position.clone();
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedExpression,
                    "Expected pattern argument".to_string(),
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
        let pos = twp.position.clone();
        match &twp.token {
            Token::Identifier(name) if name == "_" => {
                self.advance();
                Ok(crate::ast::Pattern::Wildcard)
            }
            Token::IntLiteral(n) => {
                let n = *n;
                self.advance();
                Ok(crate::ast::Pattern::Literal(
                    crate::ast::PatternLiteral::Int(n),
                ))
            }
            Token::DoubleLiteral(n) => {
                let n = *n;
                self.advance();
                Ok(crate::ast::Pattern::Literal(
                    crate::ast::PatternLiteral::Double(n),
                ))
            }
            Token::StringLiteral(s) => {
                let s = s.clone();
                self.advance();
                Ok(crate::ast::Pattern::Literal(
                    crate::ast::PatternLiteral::String(s),
                ))
            }
            Token::True => {
                self.advance();
                Ok(crate::ast::Pattern::Literal(
                    crate::ast::PatternLiteral::Bool(true),
                ))
            }
            Token::False => {
                self.advance();
                Ok(crate::ast::Pattern::Literal(
                    crate::ast::PatternLiteral::Bool(false),
                ))
            }
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                Ok(crate::ast::Pattern::Binding(name))
            }
            _ => {
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedExpression,
                    format!("Expected pattern argument, found {}", twp.token),
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
                Some(_) => self.parse_type_name()?,
                None => {
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
                if operator == UnaryOperator::Minus {
                    if let Some(Token::InvalidNumber(text)) =
                        self.tokens.get(self.position + 1).map(|t| &t.token)
                    {
                        if matches!(text.as_str(), "9223372036854775808" | "0x8000000000000000") {
                            let pos = self.current_token_with_position().unwrap().position.clone();
                            self.advance();
                            let lit_pos =
                                self.current_token_with_position().unwrap().position.clone();
                            self.advance();
                            let _ = pos;
                            return Ok(Expression {
                                kind: ExpressionKind::IntLiteral(i64::MIN),
                                pos: lit_pos,
                            });
                        }
                    }
                }
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
        let mut call_type_args: Vec<crate::ast::TypeExpr> = Vec::new();

        loop {
            match self.current_token() {
                Some(Token::Less) if Self::is_named_callee(&expr) => {
                    match self.try_parse_call_type_args() {
                        Some(args) => call_type_args = args,
                        None => break,
                    }
                }
                Some(Token::LParen) => {
                    self.advance();
                    let args = self.parse_arguments()?;
                    self.expect_token(Token::RParen)?;
                    let pos = expr.pos.clone();
                    expr = Expression {
                        kind: ExpressionKind::FunctionCall {
                            callee: Box::new(expr),
                            arguments: args,
                            type_args: core::mem::take(&mut call_type_args),
                        },
                        pos,
                    };
                }
                Some(Token::LBracket) => {
                    let pos = expr.pos.clone();
                    self.advance();
                    let index = self.parse_expression()?;
                    self.expect_token(Token::RBracket)?;
                    expr = Expression {
                        kind: ExpressionKind::Index {
                            object: Box::new(expr),
                            index: Box::new(index),
                        },
                        pos,
                    };
                }
                Some(Token::As) => {
                    let pos = self.current_token_with_position().unwrap().position.clone();
                    self.advance();

                    let target_type = match self.current_token() {
                        Some(_) => self.parse_type_name()?,
                        None => {
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
                                    self.extract_code_snippet(
                                        twp.position.line,
                                        twp.position.column,
                                    ),
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
                        let looks_like_type = type_name
                            .chars()
                            .next()
                            .map(|c| c.is_uppercase())
                            .unwrap_or(false);
                        if !looks_like_type {
                            break;
                        }

                        if !self.looks_like_struct_literal() {
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
                                            self.extract_code_snippet(
                                                twp.position.line,
                                                twp.position.column,
                                            ),
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
                                    let pos = self
                                        .current_token_with_position()
                                        .map(|twp| twp.position.clone())
                                        .unwrap_or_else(|| {
                                            self.tokens.last().unwrap().position.clone()
                                        });
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

    /// Parse array literal elements: `[1, 2, 3]` (between the brackets)
    fn parse_array_elements(&mut self) -> Result<Vec<Expression>, ()> {
        let mut elements = Vec::new();
        if self.current_token() == Some(&Token::RBracket) {
            self.advance();
            return Ok(elements);
        }

        loop {
            elements.push(self.parse_expression()?);
            if self.current_token() != Some(&Token::Comma) {
                break;
            }
            self.advance();
        }

        self.expect_token(Token::RBracket)?;
        Ok(elements)
    }

    /// Parse a type expression: primitive types, identifiers (user types,
    /// enums, generic parameters), fixed-length arrays `[T; N]`, and generic
    /// applications like `Box<int>`
    fn parse_type_name(&mut self) -> Result<crate::ast::TypeExpr, ()> {
        if self.current_token() == Some(&Token::LBracket) {
            self.advance();
            let element_type = self.parse_type_name()?;
            if self.current_token() != Some(&Token::Semicolon) {
                self.expect_token(Token::RBracket)?;
                return Ok(crate::ast::TypeExpr::Array {
                    element_type: Box::new(element_type),
                    len: 0,
                });
            }
            self.advance(); // Semicolon
            let len = match self.current_token() {
                Some(Token::IntLiteral(n)) => {
                    let n = *n;
                    self.advance();
                    n
                }
                _ => {
                    let pos = self
                        .current_token_with_position()
                        .map(|twp| twp.position.clone())
                        .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::ExpectedType,
                        "Expected array length".to_string(),
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
            self.expect_token(Token::RBracket)?;
            if len < 0 {
                let pos = self
                    .current_token_with_position()
                    .map(|twp| twp.position.clone())
                    .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
                self.errors.add_error(crate::ErrorReport::with_file(
                    crate::ErrorType::ExpectedType,
                    "Array length must be non-negative".to_string(),
                    self.filename.clone(),
                    pos.line,
                    pos.column,
                    pos.offset,
                    pos.length,
                    self.extract_code_snippet(pos.line, pos.column),
                ));
                return Err(());
            }
            return Ok(crate::ast::TypeExpr::Array {
                element_type: Box::new(element_type),
                len: len as usize,
            });
        }

        // Function type
        if self.current_token() == Some(&Token::Func) {
            self.advance();
            self.expect_token(Token::LParen)?;
            let mut params = Vec::new();
            if self.current_token() != Some(&Token::RParen) {
                loop {
                    params.push(self.parse_type_name()?);
                    if self.current_token() == Some(&Token::Comma) {
                        self.advance();
                        continue;
                    }
                    break;
                }
            }
            self.expect_token(Token::RParen)?;
            self.expect_token(Token::Colon)?;
            let ret = self.parse_type_name()?;
            return Ok(crate::ast::TypeExpr::Function {
                params,
                ret: Box::new(ret),
            });
        }

        // Simple type
        match self.current_token_with_position() {
            Some(twp) => match &twp.token {
                Token::StringType => {
                    self.advance();
                    Ok(crate::ast::TypeExpr::Named("string".to_string()))
                }
                Token::IntType => {
                    self.advance();
                    Ok(crate::ast::TypeExpr::Named("int".to_string()))
                }
                Token::BoolType => {
                    self.advance();
                    Ok(crate::ast::TypeExpr::Named("bool".to_string()))
                }
                Token::DoubleType => {
                    self.advance();
                    Ok(crate::ast::TypeExpr::Named("double".to_string()))
                }
                Token::VoidType => {
                    self.advance();
                    Ok(crate::ast::TypeExpr::Void)
                }
                Token::Identifier(type_name) => {
                    let type_name = type_name.clone();
                    let pos = twp.position.clone();
                    self.advance();
                    // TODO: fkasjf;askjl why
                    if type_name == "bytes" {
                        return Ok(crate::ast::TypeExpr::Named("bytes".to_string()));
                    }
                    // lol
                    if type_name == "any" {
                        self.errors.add_error(crate::ErrorReport::with_file(
                            crate::ErrorType::ExpectedType,
                            "type 'any' is not allowed; use a generic function or an interface instead".to_string(),
                            self.filename.clone(),
                            pos.line,
                            pos.column,
                            pos.offset,
                            pos.length,
                            self.extract_code_snippet(pos.line, pos.column),
                        ));
                        return Err(());
                    }
                    // Generic application
                    if self.current_token() == Some(&Token::Less) {
                        self.advance();
                        let mut args = Vec::new();
                        loop {
                            args.push(self.parse_type_name()?);
                            if self.current_token() == Some(&Token::Comma) {
                                self.advance();
                                continue;
                            }
                            break;
                        }
                        self.expect_token(Token::Greater)?;
                        Ok(crate::ast::TypeExpr::Generic {
                            name: type_name,
                            args,
                        })
                    } else {
                        Ok(crate::ast::TypeExpr::Named(type_name))
                    }
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
                    Err(())
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
                Err(())
            }
        }
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
                        return Ok(Expression {
                            kind: ExpressionKind::StringLiteral(original),
                            pos: twp.position,
                        });
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
                            let mut depth: i32 = 0;
                            let mut in_string = false;
                            while i < chars.len() {
                                let c = chars[i];
                                if in_string {
                                    if c == '\\' {
                                        i += 1; // skip escaped character
                                    } else if c == '"' {
                                        in_string = false;
                                    }
                                    i += 1;
                                    continue;
                                }
                                match c {
                                    '"' => {
                                        in_string = true;
                                        i += 1;
                                    }
                                    '(' | '[' => {
                                        depth += 1;
                                        i += 1;
                                    }
                                    ')' | ']' => {
                                        depth -= 1;
                                        i += 1;
                                    }
                                    '}' if depth == 0 => {
                                        found = true;
                                        break;
                                    }
                                    _ => {
                                        i += 1;
                                    }
                                }
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
                                    self.extract_code_snippet(
                                        twp.position.line,
                                        twp.position.column,
                                    ),
                                ));
                                return Err(());
                            }
                            let inner: String = chars[start_expr..i].iter().collect();
                            i += 1;

                            let mut lexer = crate::lexer::Lexer::new(
                                &inner,
                                format!("{}<interp>", self.filename),
                            );
                            let tokens = lexer.tokenize();
                            let mut p = Parser::new(
                                tokens,
                                inner.clone(),
                                format!("{}<interp>", self.filename),
                            );
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
                                        self.extract_code_snippet(
                                            twp.position.line,
                                            twp.position.column,
                                        ),
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
                        let text_expr = Expression {
                            kind: ExpressionKind::StringLiteral(buf.clone()),
                            pos: twp.position.clone(),
                        };
                        parts.push(Ok(text_expr));
                    }

                    let mut iter = parts.into_iter();
                    let first = match iter.next() {
                        Some(Ok(e)) => e,
                        _ => {
                            return Ok(Expression {
                                kind: ExpressionKind::StringLiteral(String::new()),
                                pos: twp.position,
                            });
                        }
                    };
                    let mut acc = first;
                    for part in iter {
                        let rhs = part?;
                        acc = Expression {
                            kind: ExpressionKind::Binary {
                                left: Box::new(acc),
                                operator: BinaryOperator::Add,
                                right: Box::new(rhs),
                            },
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
                Token::LBracket => {
                    let pos = twp.position.clone();
                    self.advance();
                    let elements = self.parse_array_elements()?;
                    Ok(Expression {
                        kind: ExpressionKind::ArrayLiteral(elements),
                        pos,
                    })
                }
                Token::InvalidNumber(text) => {
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::InvalidNumber,
                        format!("Invalid number literal '{text}' (out of range or malformed)"),
                        self.filename.clone(),
                        twp.position.line,
                        twp.position.column,
                        twp.position.offset,
                        twp.position.length,
                        self.extract_code_snippet(twp.position.line, twp.position.column),
                    ));
                    self.advance();
                    Err(())
                }
                Token::UnclosedString => {
                    self.errors.add_error(crate::ErrorReport::with_file(
                        crate::ErrorType::UnclosedString,
                        "Unclosed string literal".to_string(),
                        self.filename.clone(),
                        twp.position.line,
                        twp.position.column,
                        twp.position.offset,
                        twp.position.length,
                        self.extract_code_snippet(twp.position.line, twp.position.column),
                    ));
                    self.advance();
                    Err(())
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
                let pos = self
                    .current_token_with_position()
                    .map(|twp| twp.position.clone())
                    .unwrap_or_else(|| self.tokens.last().unwrap().position.clone());
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
                    crate::ErrorType::ExpectedToken(format!("{expected}")),
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
                crate::ErrorType::ExpectedToken(format!("{expected}")),
                format!("Expected '{expected}', found EOF"),
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

    /// Peek a token `offset` positions ahead without advancing
    fn peek_token(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.position + offset).map(|t| &t.token)
    }

    /// Check whether the upcoming `{` starts a struct literal
    /// (`Type { field: value }` or `Type { field }`) rather than a block
    fn looks_like_struct_literal(&self) -> bool {
        // Token after `{` must be an identifier (a field name).
        if !matches!(self.peek_token(1), Some(Token::Identifier(_))) {
            return false;
        }
        // Field list forms: `field: value`, `field,`, `field }`
        matches!(
            self.peek_token(2),
            Some(Token::Colon) | Some(Token::Comma) | Some(Token::RBrace)
        )
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
