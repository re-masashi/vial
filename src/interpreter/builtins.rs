use std::io::{self};

use super::error::InterpreterError;
use super::value::Value;

/// Executes a runtime builtin by name using the given arguments.
///
/// This dispatches the builtin identified by `name` and returns the result of that operation
/// or an `InterpreterError` describing why the builtin could not be executed.
///
/// # Returns
/// `Ok(Value)` containing the builtin's result (commonly `Value::Null` for side-effectful builtins),
/// or `Err(InterpreterError)` if the builtin name is unknown, argument counts/types are invalid,
/// input/output fails, or a conversion cannot be performed.
///
/// # Examples
///
/// ```
/// # use crate::interpreter::{builtins::execute_builtin, value::Value, error::InterpreterError};
/// // Convert integer to string
/// let res = execute_builtin("int_to_string", vec![Value::Int(42)]).unwrap();
/// assert_eq!(res, Value::String("42".to_string()));
/// ```
pub fn execute_builtin(name: &str, args: Vec<Value>) -> Result<Value, InterpreterError> {
    match name {
        "print" => {
            for arg in args {
                match arg {
                    Value::Int(i) => print!("{}", i),
                    Value::Float(f) => print!("{}", f),
                    Value::Bool(b) => print!("{}", b),
                    Value::String(s) => print!("{}", s),
                    Value::Ptr(_) => print!("<ptr>"),
                    Value::Null => print!("null"),
                }
            }
            Ok(Value::Null)
        }

        "println" => {
            for arg in args {
                match arg {
                    Value::Int(i) => print!("{}", i),
                    Value::Float(f) => print!("{}", f),
                    Value::Bool(b) => print!("{}", b),
                    Value::String(s) => print!("{}", s),
                    Value::Ptr(_) => print!("<ptr>"),
                    Value::Null => print!("null"),
                }
            }
            println!();
            Ok(Value::Null)
        }

        "input" => {
            let mut buffer = String::new();
            io::stdin()
                .read_line(&mut buffer)
                .map_err(InterpreterError::IOError)?;
            Ok(Value::String(buffer.trim().to_string()))
        }

        "int_to_string" => {
            if args.len() != 1 {
                return Err(InterpreterError::TypeError {
                    expected: "one argument",
                    got: format!("{} arguments", args.len()),
                });
            }
            let i = args[0].as_int()?;
            Ok(Value::String(i.to_string()))
        }

        "float_to_string" => {
            if args.len() != 1 {
                return Err(InterpreterError::TypeError {
                    expected: "one argument",
                    got: format!("{} arguments", args.len()),
                });
            }
            let f = args[0].as_float()?;
            Ok(Value::String(f.to_string()))
        }

        "string_to_int" => {
            if args.len() != 1 {
                return Err(InterpreterError::TypeError {
                    expected: "one argument",
                    got: format!("{} arguments", args.len()),
                });
            }
            match &args[0] {
                Value::String(s) => match s.parse::<i64>() {
                    Ok(i) => Ok(Value::Int(i)),
                    Err(_) => Err(InterpreterError::TypeError {
                        expected: "string that can be parsed as integer",
                        got: format!("string: {}", s),
                    }),
                },
                _ => Err(InterpreterError::TypeError {
                    expected: "string",
                    got: args[0].type_name().to_string(),
                }),
            }
        }

        _ => Err(InterpreterError::UnknownBuiltin(name.to_string())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print_builtin() {
        let args = vec![Value::Int(42), Value::String("hello".to_string())];
        let result = execute_builtin("print", args).unwrap();
        assert_eq!(result, Value::Null);
    }

    #[test]
    fn test_println_builtin() {
        let args = vec![Value::Float(3.14)];
        let result = execute_builtin("println", args).unwrap();
        assert_eq!(result, Value::Null);
    }

    #[test]
    fn test_int_to_string_builtin() {
        let args = vec![Value::Int(123)];
        let result = execute_builtin("int_to_string", args).unwrap();
        assert_eq!(result, Value::String("123".to_string()));

        // Test with wrong number of arguments
        let args = vec![Value::Int(1), Value::Int(2)];
        let result = execute_builtin("int_to_string", args);
        assert!(result.is_err());
    }

    #[test]
    fn test_string_to_int_builtin() {
        let args = vec![Value::String("456".to_string())];
        let result = execute_builtin("string_to_int", args).unwrap();
        assert_eq!(result, Value::Int(456));

        // Test with invalid string
        let args = vec![Value::String("not_a_number".to_string())];
        let result = execute_builtin("string_to_int", args);
        assert!(result.is_err());

        // Test with non-string value
        let args = vec![Value::Int(123)];
        let result = execute_builtin("string_to_int", args);
        assert!(result.is_err());
    }

    #[test]
    fn test_float_to_string_builtin() {
        let args = vec![Value::Float(3.14)];
        let result = execute_builtin("float_to_string", args).unwrap();
        assert_eq!(result, Value::String("3.14".to_string()));
    }

    #[test]
    fn test_unknown_builtin() {
        let args = vec![Value::Int(1)];
        let result = execute_builtin("unknown_builtin", args);
        match result {
            Err(InterpreterError::UnknownBuiltin(name)) => {
                assert_eq!(name, "unknown_builtin");
            }
            _ => panic!("Expected UnknownBuiltin error"),
        }
    }

    #[test]
    fn test_builtin_error_handling() {
        // Test int_to_string with wrong argument count
        let args = vec![Value::Int(1), Value::Int(2)];
        let result = execute_builtin("int_to_string", args);
        match result {
            Err(InterpreterError::TypeError { expected, got }) => {
                assert_eq!(expected, "one argument");
                assert!(got.contains("arguments"));
            }
            _ => panic!("Expected TypeError"),
        }
    }
}