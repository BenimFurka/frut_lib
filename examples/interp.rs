use frut_lib::ast::{Expression, ExpressionKind, Statement, StatementKind};
use frut_lib::value::{RuntimeEnvironment, Value};
use frut_lib::{parse_files, analyze_project, File as FrutFile};

// Result
type InterpretResult = Result<Value, String>;

// Simple Interpreter for demonstration
struct SimpleInterpreter {
    env: RuntimeEnvironment,
}

impl SimpleInterpreter {
    fn new() -> Self {
        let mut env = RuntimeEnvironment::new();

        // Add a simple built-in println function
        env.define_function(
            "print".to_string(),
            Value::native_function("print", None::<usize>, |args| {
                let output = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join("");
                println!("{}", output);
                Ok(Value::Void)
            }),
        );

        SimpleInterpreter { env }
    }

    fn interpret(&mut self, statements: &[Statement]) -> Result<(), String> {
        for statement in statements {
            if let InterpretResult::Err(e) = self.interpret_statement(statement) {
                return Err(e);
            }
        }
        Ok(())
    }

    fn interpret_statement(&mut self, stmt: &Statement) -> InterpretResult {
        match &stmt.kind {
            StatementKind::ExpressionStatement(expr) => {
                self.interpret_expression(expr)?;
                Ok(Value::Void)
            }
            StatementKind::VariableDeclaration { name, initializer, .. } => {
                let value = self.interpret_expression(initializer)?;
                self.env.define_variable(name.clone(), value);
                Ok(Value::Void)
            }
            StatementKind::Assignment { name, value } => {
                let new_value = self.interpret_expression(value)?;
                if self.env.get_variable(name).is_some() {
                    self.env.set_variable(name, new_value).unwrap(); // set_variable returns Result<(), String>
                    Ok(Value::Void)
                } else {
                    Err(format!("Cannot assign to undefined variable '{}'", name))
                }
            }
            StatementKind::Block(statements) => {
                self.env.enter_scope();
                let result = (|| {
                    for stmt in statements {
                        let res = self.interpret_statement(stmt)?;
                        // Simplified example
                        if let Value::Void = res {} else {}
                    }
                    Ok(Value::Void)
                })();
                self.env.exit_scope();
                result
            }
            // Other StatementKinds (If, While, Function, Return, etc.) can be added as needed
            _ => Err("Statement type not implemented in example".to_string()),
        }
    }

    fn interpret_expression(&mut self, expr: &Expression) -> InterpretResult {
        match &expr.kind {
            ExpressionKind::IntLiteral(n) => Ok(Value::Int(*n)),
            ExpressionKind::StringLiteral(s) => Ok(Value::String(s.clone())),
            ExpressionKind::BoolLiteral(b) => Ok(Value::Bool(*b)),
            ExpressionKind::DoubleLiteral(n) => Ok(Value::Double(*n)),
            ExpressionKind::Variable(name) => {
                self.env
                    .get_variable(name)
                    .cloned()
                    .ok_or_else(|| format!("Undefined variable: {}", name))
            }
            ExpressionKind::Binary { left, operator, right } => {
                let l_val = self.interpret_expression(left)?;
                let r_val = self.interpret_expression(right)?;

                match l_val.binary_op(*operator, &r_val) {
                    Ok(v) => Ok(v),
                    Err(e) => Err(e),
                }
            }
            ExpressionKind::Unary { operator, operand } => {
                let op_val = self.interpret_expression(operand)?;
                match op_val.unary_op(*operator) {
                    Ok(v) => Ok(v),
                    Err(e) => Err(e),
                }
            }
            ExpressionKind::FunctionCall { callee, arguments } => {
                if let ExpressionKind::Variable(func_name) = &callee.kind {
                    let func_val = self
                        .env
                        .get_variable(func_name)
                        .ok_or_else(|| format!("Function '{}' is not defined", func_name))?
                        .clone();

                    match func_val {
                        Value::NativeFunction { func, arity, .. } => {
                            if let Some(expected_arity) = arity {
                                if arguments.len() != expected_arity {
                                    return Err(format!(
                                        "Function '{}' expected {} arguments, got {}",
                                        func_name,
                                        expected_arity,
                                        arguments.len()
                                    ));
                                }
                            }
                            let mut args = Vec::new();
                            for arg_expr in arguments {
                                args.push(self.interpret_expression(arg_expr)?);
                            }
                            func.call(args)
                        }
                        Value::Function { .. } => {
                            // Simplified example
                            Err("Calling user-defined functions is not implemented in this example".to_string())
                        }
                        _ => Err(format!("'{}' is not a function", func_name)),
                    }
                } else {
                    Err("Callee must be a variable name".to_string())
                }
            }

            ExpressionKind::Cast { expr, target_type } => {
                let val = self.interpret_expression(expr)?;
                // Type cast
                match target_type.as_str() {
                    "int" => match val {
                        Value::Int(n) => Ok(Value::Int(n)),
                        Value::Double(n) => Ok(Value::Int(n as i64)),
                        Value::Bool(b) => Ok(Value::Int(if b { 1 } else { 0 })),
                        Value::String(s) => s.parse::<i64>()
                            .map(Value::Int)
                            .map_err(|_| format!("Cannot convert string '{}' to int", s)),
                        _ => unreachable!("Invalid cast to int: {:?}", val),
                    },
                    "double" => match val {
                        Value::Int(n) => Ok(Value::Double(n as f64)),
                        Value::Double(n) => Ok(Value::Double(n)),
                        Value::String(s) => s.parse::<f64>()
                            .map(Value::Double)
                            .map_err(|_| format!("Cannot convert string '{}' to double", s)),
                        _ => unreachable!("Invalid cast to double: {:?}", val),
                    },
                    "string" => match val {
                        Value::Void => Ok(Value::String("void".to_string())),
                        Value::Int(n) => Ok(Value::String(n.to_string())),
                        Value::Double(n) => Ok(Value::String(n.to_string())),
                        Value::Bool(b) => Ok(Value::String(b.to_string())),
                        Value::String(s) => Ok(Value::String(s)),
                        Value::Function { .. } => Ok(Value::String("<function>".to_string())),
                        Value::NativeFunction { .. } => Ok(Value::String("<native function>".to_string())),
                    },
                    "bool" => match val {
                        Value::Void => Ok(Value::Bool(false)),
                        Value::Int(n) => Ok(Value::Bool(n != 0)),
                        Value::Double(n) => Ok(Value::Bool(n != 0.0)),
                        Value::Bool(b) => Ok(Value::Bool(b)),
                        Value::String(s) => Ok(Value::Bool(!s.is_empty())),
                        Value::Function { .. } | Value::NativeFunction { .. } => Ok(Value::Bool(true)),
                    },
                    _ => unreachable!("Unknown target type '{}' for cast", target_type),
                }
            }
        }
    }
}

fn main() {
    let code = r#"
        var greeting: string = "Hello, ";
        var name: string = "Frut!";
        print(greeting, name);
        var x: int = 10;
        var y: int = 20;
        var sum: int = x + y;
        print("Sum is: ", sum);
    "#;

    let files = vec![FrutFile {
        path: "example.ft".to_string(),
        code: code.to_string(),
        ast: None,
    }];

    let mut project = parse_files(files).project;

    // Analyze
    if let Err(errors) = analyze_project(&mut project) {
        println!("Found {} errors during analysis:", errors.len());
        for error in errors.errors {
            println!("  - {}", error.message);
        }
        return;
    }

    println!("Analysis successful, starting interpretation...");

    // Intepretation
    if let Some(first_file) = project.files.first() {
        if let Some(ref ast) = first_file.ast {
            let mut interpreter = SimpleInterpreter::new();
            match interpreter.interpret(&ast) {
                Ok(()) => println!("Interpretation finished successfully."),
                Err(e) => println!("Runtime error: {}", e),
            }
        }
    }
}