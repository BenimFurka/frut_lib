//! Semantic analyzer for the Frut
//!
//! Performs semantic analysis including type checking, name resolution,
//! and control flow analysis.

use alloc::boxed::Box;
use alloc::string::{String, ToString};
use alloc::vec::Vec;

use crate::ast::{Expression, ExpressionKind, Statement, StatementKind, UnaryOperator, BinaryOperator};
use crate::errors::{ErrorCollection, ErrorReport, ErrorType};
use crate::types::Type;
use crate::HashMap;

/// Represents a symbol in the environment (either a variable or a function)
#[derive(Debug, Clone)]
enum Symbol {
    Variable(VariableInfo),
    Function(FunctionInfo),
}

/// Represents a variable in the environment
#[derive(Debug, Clone)]
struct VariableInfo {
    var_type: Type,
    is_initialized: bool,
}

#[derive(Debug, Clone)]
struct FunctionInfo {
    param_types: Vec<Type>,
    return_type: Type,
}

/// Environment for tracking variables and their scopes
#[derive(Debug, Clone)]
struct Environment {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl Environment {
    /// Create a new environment
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::default()],
        }
    }
    /// Enter a new scope
    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::default());
    }

    /// Exit the current scope
    fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Define a variable in the current scope
    fn define_variable(&mut self, name: String, var_type: Type) -> Result<(), (ErrorType, String)> {
        let current_scope = self.scopes.last_mut().unwrap();
        if current_scope.contains_key(&name) {
            return Err((
                ErrorType::VariableAlreadyDefined(name.clone()),
                format!("Variable '{}' is already defined in this scope", name),
            ));
        }
        current_scope.insert(
            name,
            Symbol::Variable(VariableInfo {
                var_type,
                is_initialized: false,
            })
        );
        Ok(())
    }

    /// Define a function in the current scope
    fn define_function(&mut self, name: String, info: FunctionInfo) -> Result<(), (ErrorType, String)> {
        let current_scope = self.scopes.last_mut().unwrap();
        if current_scope.contains_key(&name) {
            return Err((
                ErrorType::VariableAlreadyDefined(name.clone()),
                format!("Function '{}' is already defined in this scope", name),
            ));
        }
        current_scope.insert(name, Symbol::Function(info));
        Ok(())
    }

    /// Get a function from any scope
    fn get_function(&self, name: &str) -> Option<&FunctionInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(Symbol::Function(func_info)) = scope.get(name) {
                return Some(func_info);
            }
        }
        None
    }

    /// Get a variable from any scope
    fn get_variable(&self, name: &str) -> Option<&VariableInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(Symbol::Variable(var_info)) = scope.get(name) {
                return Some(var_info);
            }
        }
        None
    }

    /// Update a variable's initialization status
    fn set_variable_initialized(&mut self, name: &str) -> Result<(), (ErrorType, String)> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(Symbol::Variable(var_info)) = scope.get_mut(name) {
                var_info.is_initialized = true;
                return Ok(());
            }
        }
        Err((
            ErrorType::UndefinedVariable(name.to_string()),
            format!("Variable '{}' is not defined", name),
        ))
    }
}

/// Type checker for expressions
struct TypeChecker<'a> {
    environment: &'a Environment,
}

impl<'a> TypeChecker<'a> {
    fn new(environment: &'a Environment) -> Self {
        Self { environment }
    }
    
    /// Check if a cast from source_type to target_type is valid
    fn is_valid_cast(&self, source_type: &Type, target_type: &Type) -> bool {
        match (source_type, target_type) {
            // Int to Double is always valid
            (Type::Int, Type::Double) => true,
            // Double to Int is valid (truncates decimal part)
            (Type::Double, Type::Int) => true,
            // String to Int/Double if the string is a valid number
            (Type::String, Type::Int) | (Type::String, Type::Double) => true,
            // Any type to String is valid
            (_, Type::String) => true,
            // Bool to Int (true=1, false=0)
            (Type::Bool, Type::Int) => true,
            // Int to Bool (0=false, non-zero=true)
            (Type::Int, Type::Bool) => true,
            // Same type is always valid
            (t1, t2) if t1 == t2 => true,
            // Invalid cast
            _ => false,
        }
    }

    /// Get the type of an expression
    fn get_expression_type(&self, expr: &Expression) -> Result<Type, (ErrorType, String, crate::Position)> {
        match &expr.kind {
            ExpressionKind::StringLiteral(_) => Ok(Type::String),
            ExpressionKind::IntLiteral(_) => Ok(Type::Int),
            ExpressionKind::DoubleLiteral(_) => Ok(Type::Double),
            ExpressionKind::BoolLiteral(_) => Ok(Type::Bool),
            ExpressionKind::Variable(name) => {
                for scope in self.environment.scopes.iter().rev() {
                    if let Some(symbol) = scope.get(name) {
                        match symbol {
                            Symbol::Variable(var_info) => {
                                return Ok(var_info.var_type.clone());
                            }
                            Symbol::Function(func_info) => {
                                return Ok(Type::FunctionType {
                                    param_types: func_info.param_types.clone(),
                                    return_type: Box::new(func_info.return_type.clone()),
                                });
                            }
                        }
                    }
                }
                
                if self.environment.get_function(name).is_some() {
                    let func_info = self.environment.get_function(name).unwrap();
                    Ok(Type::FunctionType {
                        param_types: func_info.param_types.clone(),
                        return_type: Box::new(func_info.return_type.clone()),
                    })
                } else {
                    Err((
                        ErrorType::UndefinedVariable(name.to_string()),
                        format!("Variable or function '{}' is not defined", name),
                        expr.pos.clone(),
                    ))
                }
            }
            ExpressionKind::Unary { operator, operand } => {
                let operand_type = self.get_expression_type(operand)?;

                match operator {
                    UnaryOperator::Not => {
                        if operand_type == Type::Bool {
                            Ok(Type::Bool)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: "bool".to_string(),
                                    found: format!("{}", operand_type),
                                },
                                "Cannot apply '!' operator to non-boolean type".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                    UnaryOperator::Plus | UnaryOperator::Minus => {
                        if operand_type == Type::Int || operand_type == Type::Double {
                            Ok(operand_type)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: "int or double".to_string(),
                                    found: format!("{}", operand_type),
                                },
                                "Cannot apply unary +/- to non-numeric type".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                }
            }
            ExpressionKind::Binary { left, operator, right } => {
                let left_type = self.get_expression_type(left)?;
                let right_type = self.get_expression_type(right)?;

                match operator {
                    BinaryOperator::Add => {
                        if left_type == right_type {
                            Ok(left_type)
                        } else if left_type == Type::String && right_type == Type::String {
                            Ok(Type::String)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: "matching types".to_string(),
                                    found: format!("{} and {}", left_type, right_type),
                                },
                                "Cannot perform addition operation on non-matching or mismatched types".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                    BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => {
                        if left_type == right_type && (left_type == Type::Int || left_type == Type::Double) {
                            Ok(left_type)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: "matching numeric types".to_string(),
                                    found: format!("{} and {}", left_type, right_type),
                                },
                                "Cannot perform arithmetic operation on non-numeric or mismatched types".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                    BinaryOperator::Less | BinaryOperator::LessEqual | BinaryOperator::Greater | BinaryOperator::GreaterEqual => {
                        if left_type == right_type && (left_type == Type::Int || left_type == Type::Double) {
                            Ok(Type::Bool)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: "matching numeric types".to_string(),
                                    found: format!("{} and {}", left_type, right_type),
                                },
                                "Cannot compare non-numeric or mismatched types".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                    BinaryOperator::Equal | BinaryOperator::NotEqual => {
                        if left_type == right_type {
                            Ok(Type::Bool)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: format!("{}", left_type),
                                    found: format!("{}", right_type),
                                },
                                "Cannot compare different types".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                    BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
                        if left_type == Type::Bool && right_type == Type::Bool {
                            Ok(Type::Bool)
                        } else {
                            Err((
                                ErrorType::TypeMismatch {
                                    expected: "bool and bool".to_string(),
                                    found: format!("{} and {}", left_type, right_type),
                                },
                                "Cannot perform logical operation on non-boolean types".to_string(),
                                expr.pos.clone(),
                            ))
                        }
                    }
                }
            }
            ExpressionKind::Cast { expr, target_type } => {
                let expr_type = self.get_expression_type(expr)?;
                let target_type = Type::from(target_type.as_str());
                
                if !self.is_valid_cast(&expr_type, &target_type) {
                    return Err((
                        ErrorType::TypeError,
                        format!("Cannot cast from {} to {}", expr_type, target_type),
                        expr.pos.clone(),
                    ));
                }
                
                Ok(target_type)
            }
            ExpressionKind::FunctionCall { callee, arguments } => {
                let callee_type = self.get_expression_type(callee)?;

                if let ExpressionKind::Variable(name) = &callee.kind {
                    if let Some(func_info) = self.environment.get_function(name) {
                        if func_info.param_types.len() != arguments.len() {
                            return Err((
                                ErrorType::TypeError,
                                format!(
                                    "Expected {} arguments, but got {}",
                                    func_info.param_types.len(),
                                    arguments.len()
                                ),
                                expr.pos.clone(),
                            ));
                        }
                    }
                }

                if let Type::FunctionType { param_types, return_type } = callee_type {
                    if arguments.len() != param_types.len() {
                        return Err((
                            ErrorType::TypeError,
                            format!("Expected {} arguments, but got {}", param_types.len(), arguments.len()),
                            expr.pos.clone(),
                        ));
                    }

                    for (arg, param_type) in arguments.iter().zip(param_types.iter()) {
                        let arg_type = self.get_expression_type(arg)?;
                        if arg_type != *param_type {
                            return Err((
                                ErrorType::TypeMismatch {
                                    expected: param_type.to_string(),
                                    found: arg_type.to_string(),
                                },
                                "Mismatched argument type".to_string(),
                                arg.pos.clone(),
                            ));
                        }
                    }

                    Ok(*return_type)
                } else {
                    Err((
                        ErrorType::TypeError,
                        "Expression is not a function".to_string(),
                        callee.pos.clone(),
                    ))
                }
            }
        }
    }
}

/// Semantic analyzer
pub struct SemanticAnalyzer {
    errors: ErrorCollection,
    environment: Environment,
    filename: String,
    source: String,
    current_return_type: Option<Type>,
}

impl SemanticAnalyzer {
    /// Create a new semantic analyzer
    pub fn new(filename: String, source: String) -> Self {
        let environment = Environment::new();

        Self {
            errors: ErrorCollection::new(),
            environment,
            filename,
            source,
            current_return_type: None,
        }
    }
    
    /// Predeclare a function into the current environment (for multi-file import resolution)
    pub fn predeclare_function(
        &mut self,
        name: String,
        param_types: Vec<Type>,
        return_type: Type,
    ) -> Result<(), ErrorReport> {
        let func_info = FunctionInfo {
            param_types,
            return_type,
        };
        
        if let Err((err, msg)) = self.environment.define_function(name, func_info) {
            return Err(self.create_error(err, msg, &crate::Position::new(self.filename.clone(), 1, 1, 0, 1)));
        }
        Ok(())
    }
    
    /// Predeclare a variable and mark it as initialized (for imported variables)
    pub fn predeclare_initialized_variable(&mut self, name: String, var_type: Type) -> Result<(), ErrorReport> {
        match self.environment.define_variable(name.clone(), var_type) {
            Ok(_) => {
                if let Err((err, msg)) = self.environment.set_variable_initialized(&name) {
                    return Err(self.create_error(err, msg, &crate::Position::new(self.filename.clone(), 1, 1, 0, 1)));
                }
                Ok(())
            }
            Err((err, msg)) => Err(self.create_error(err, msg, &crate::Position::new(self.filename.clone(), 1, 1, 0, 1))),
        }
    }

    /// Extract a code snippet from the source at the given line
    fn extract_code_snippet(&self, line: usize) -> String {
        if line == 0 {
            return "".to_string();
        }
        
        let lines: Vec<&str> = self.source.lines().collect();
        if line > 0 && line <= lines.len() {
            lines[line - 1].to_string()
        } else {
            "".to_string()
        }
    }

    /// Create an error report with proper position information
    fn create_error(&self, error_type: ErrorType, message: String, pos: &crate::Position) -> ErrorReport {
        ErrorReport::with_file(
            error_type,
            message,
            self.filename.clone(),
            pos.line,
            pos.column,
            pos.offset,
            pos.length,
            self.extract_code_snippet(pos.line),
        )
    }

    /// Analyze a list of statements
    pub fn analyze(&mut self, statements: &[Statement]) -> Result<(), ErrorCollection> {
        for statement in statements {
            if let Err(error) = self.analyze_statement(statement) {
                self.errors.add_error(error);
            }
        }

        if self.errors.has_errors() {
            Err(self.errors.clone())
        } else {
            Ok(())
        }
    }

    /// Analyze a single statement
    fn analyze_statement(&mut self, statement: &Statement) -> Result<(), ErrorReport> {
        match &statement.kind {
            StatementKind::VariableDeclaration { name, var_type, initializer } => {
                let parsed_type = Type::from(var_type.as_str());

                if parsed_type == Type::Void {
                    return Err(self.create_error(
                        ErrorType::SyntaxError,
                        "Variable of type 'void' is not allowed".to_string(),
                        &statement.pos
                    ));
                }

                match self.environment.define_variable(name.clone(), parsed_type.clone()) {
                    Ok(_) => {},
                    Err((error_type, message)) => {
                        return Err(self.create_error(error_type, message, &statement.pos));
                    }
                }

                let type_checker = TypeChecker::new(&self.environment);
                let initializer_type = match type_checker.get_expression_type(initializer) {
                    Ok(t) => t,
                    Err((error_type, message, pos)) => {
                        return Err(self.create_error(error_type, message, &pos));
                    }
                };

                if initializer_type != parsed_type {
                    return Err(self.create_error(
                        ErrorType::TypeMismatch {
                            expected: format!("{}", parsed_type),
                            found: format!("{}", initializer_type),
                        },
                        format!("Type mismatch in variable declaration for '{}'", name),
                        &initializer.pos
                    ));
                }

                match self.environment.set_variable_initialized(name) {
                    Ok(_) => {},
                    Err((error_type, message)) => {
                        return Err(self.create_error(error_type, message, &statement.pos));
                    }
                }
                Ok(())
            }
            StatementKind::Assignment { name, value } => {
                if self.environment.get_variable(name).is_none() {
                    return Err(self.create_error(
                        ErrorType::UndefinedVariable(name.clone()),
                        format!("Cannot assign to undefined variable '{}'", name),
                        &statement.pos
                    ));
                }

                let type_checker = TypeChecker::new(&self.environment);
                let value_type = match type_checker.get_expression_type(value) {
                    Ok(t) => t,
                    Err((error_type, message, pos)) => {
                        return Err(self.create_error(error_type, message, &pos));
                    }
                };
                let var_info = self.environment.get_variable(name).unwrap();

                if value_type != var_info.var_type {
                    return Err(self.create_error(
                        ErrorType::TypeMismatch {
                            expected: format!("{}", var_info.var_type),
                            found: format!("{}", value_type),
                        },
                        format!("Type mismatch in assignment to variable '{}'", name),
                        &value.pos
                    ));
                }
                Ok(())
            }
            StatementKind::IfStatement { condition, then_branch, elif_branches, else_branch } => {
                let type_checker = TypeChecker::new(&self.environment);
                let condition_type = match type_checker.get_expression_type(condition) {
                    Ok(t) => t,
                    Err((error_type, message, pos)) => {
                        return Err(self.create_error(error_type, message, &pos));
                    }
                };

                if condition_type != Type::Bool {
                    return Err(self.create_error(
                        ErrorType::TypeMismatch {
                            expected: "bool".to_string(),
                            found: format!("{}", condition_type),
                        },
                        "If condition must be of type bool".to_string(),
                        &condition.pos
                    ));
                }

                self.environment.enter_scope();
                for stmt in then_branch {
                    match self.analyze_statement(stmt) {
                        Ok(_) => {},
                        Err(error) => {
                            self.errors.add_error(error);
                        }
                    }
                }
                self.environment.exit_scope();

                for (elif_condition, elif_branch) in elif_branches {
                    let elif_condition_type = {
                        let type_checker = TypeChecker::new(&self.environment);
                        match type_checker.get_expression_type(elif_condition) {
                            Ok(t) => t,
                            Err((error_type, message, pos)) => {
                                return Err(self.create_error(error_type, message, &pos));
                            }
                        }
                    };
                    if elif_condition_type != Type::Bool {
                        return Err(self.create_error(
                            ErrorType::TypeMismatch {
                                expected: "bool".to_string(),
                                found: format!("{}", elif_condition_type),
                            },
                            "Elif condition must be of type bool".to_string(),
                            &elif_condition.pos
                        ));
                    }

                    self.environment.enter_scope();
                    for stmt in elif_branch {
                        match self.analyze_statement(stmt) {
                            Ok(_) => {},
                            Err(error) => {
                                self.errors.add_error(error);
                            }
                        }
                    }
                    self.environment.exit_scope();
                }

                if let Some(else_branch) = else_branch {
                    self.environment.enter_scope();
                    for stmt in else_branch {
                        match self.analyze_statement(stmt) {
                            Ok(_) => {},
                            Err(error) => {
                                self.errors.add_error(error);
                            }
                        }
                    }
                    self.environment.exit_scope();
                }

                Ok(())
            }
            StatementKind::WhileStatement { condition, body } => {
                let type_checker = TypeChecker::new(&self.environment);
                let condition_type = match type_checker.get_expression_type(condition) {
                    Ok(t) => t,
                    Err((error_type, message, pos)) => {
                        return Err(self.create_error(error_type, message, &pos));
                    }
                };

                if condition_type != Type::Bool {
                    return Err(self.create_error(
                        ErrorType::TypeMismatch {
                            expected: "bool".to_string(),
                            found: format!("{}", condition_type),
                        },
                        "While loop condition must be of type bool".to_string(),
                        &condition.pos
                    ));
                }

                self.environment.enter_scope();
                for stmt in body {
                    if let Err(error) = self.analyze_statement(stmt) {
                        self.errors.add_error(error);
                    }
                }
                self.environment.exit_scope();

                Ok(())
            }
            StatementKind::Block(statements) => {
                self.environment.enter_scope();
                for stmt in statements {
                    match self.analyze_statement(stmt) {
                        Ok(_) => {},
                        Err(error) => {
                            self.errors.add_error(error);
                        }
                    }
                }
                self.environment.exit_scope();
                Ok(())
            }
            StatementKind::FunctionDeclaration {
                name,
                params,
                return_type,
                body,
            } => {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| Type::from(p.param_type.as_str()))
                    .collect();
                let return_type_ty = Type::from(return_type.as_str());
                let func_info = FunctionInfo {
                    param_types: param_types.clone(),
                    return_type: return_type_ty.clone(),
                };
                
                if let Err((error_type, message)) = self
                    .environment
                    .define_function(name.clone(), func_info)
                {
                    return Err(self.create_error(error_type, message, &statement.pos));
                }

                self.environment.enter_scope();
                
                for param in params {
                    if let Err((err, msg)) = self.environment.define_variable(
                        param.name.clone(),
                        Type::from(param.param_type.as_str())
                    ) {
                        return Err(self.create_error(err, msg, &param.pos));
                    }
                    if let Err((err, msg)) = self.environment.set_variable_initialized(&param.name) {
                        return Err(self.create_error(err, msg, &param.pos));
                    }
                }

                let prev_return_type = self.current_return_type.take();
                self.current_return_type = Some(return_type_ty);

                for stmt in body {
                    if let Err(e) = self.analyze_statement(stmt) {
                        self.errors.add_error(e);
                    }
                }

                self.environment.exit_scope();
                self.current_return_type = prev_return_type;

                Ok(())
            }
            StatementKind::Return { value } => {
                let ret_type = match value {
                    Some(expr) => {
                        let type_checker = TypeChecker::new(&self.environment);
                        match type_checker.get_expression_type(expr) {
                            Ok(t) => t,
                            Err((err, msg, pos)) => return Err(self.create_error(err, msg, &pos)),
                        }
                    }
                    None => Type::Void,
                };

                match &self.current_return_type {
                    Some(expected_type) => {
                        if &ret_type != expected_type {
                            return Err(self.create_error(
                                ErrorType::TypeMismatch {
                                    expected: expected_type.to_string(),
                                    found: ret_type.to_string(),
                                },
                                "Mismatched return type".to_string(),
                                &statement.pos,
                            ));
                        }
                    }
                    None => {
                        return Err(self.create_error(
                            ErrorType::SyntaxError,
                            "Return statement outside of function".to_string(),
                            &statement.pos,
                        ));
                    }
                }
                Ok(())
            }
            StatementKind::ExpressionStatement(expr) => {
                let type_checker = TypeChecker::new(&self.environment);
                if let Err((err, msg, pos)) = type_checker.get_expression_type(expr) {
                    return Err(self.create_error(err, msg, &pos));
                }
                Ok(())
            }
            StatementKind::Import { .. } => {
                // TODO: ummmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
                Ok(())
            }
        }
    }
}