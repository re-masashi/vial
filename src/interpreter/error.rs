use crate::ir::{FunctionId, ValueId};

#[derive(Debug)]
pub enum InterpreterError {
    TypeError { expected: &'static str, got: String },
    UndefinedValue(ValueId),
    UndefinedFunction(FunctionId),
    UnknownBuiltin(String),
    DivisionByZero,
    InvalidHeapAccess,
    StackOverflow,
    NoMatchingCase,
    IOError(std::io::Error),
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io;

    #[test]
    fn test_error_creation() {
        let error = InterpreterError::DivisionByZero;
        match error {
            InterpreterError::DivisionByZero => (), // Expected
            _ => panic!("Expected DivisionByZero error"),
        }

        let error = InterpreterError::TypeError {
            expected: "int",
            got: "float".to_string(),
        };
        match error {
            InterpreterError::TypeError { expected, got } => {
                assert_eq!(expected, "int");
                assert_eq!(got, "float");
            }
            _ => panic!("Expected TypeError"),
        }

        let error = InterpreterError::UndefinedValue(ValueId(42));
        match error {
            InterpreterError::UndefinedValue(id) => assert_eq!(id, ValueId(42)),
            _ => panic!("Expected UndefinedValue error"),
        }

        let error = InterpreterError::UndefinedFunction(FunctionId(1));
        match error {
            InterpreterError::UndefinedFunction(id) => assert_eq!(id, FunctionId(1)),
            _ => panic!("Expected UndefinedFunction error"),
        }

        let io_error = io::Error::new(io::ErrorKind::Other, "test");
        let error = InterpreterError::IOError(io_error);
        match error {
            InterpreterError::IOError(_) => (), // Expected
            _ => panic!("Expected IOError"),
        }
    }

    /// Verifies that the `Debug` representation of select `InterpreterError` variants contains their
    /// variant names and relevant field values.
    ///
    /// # Examples
    ///
    /// ```
    /// let error = InterpreterError::DivisionByZero;
    /// let debug_str = format!("{:?}", error);
    /// assert!(debug_str.contains("DivisionByZero"));
    ///
    /// let error = InterpreterError::TypeError {
    ///     expected: "int",
    ///     got: "float".to_string(),
    /// };
    /// let debug_str = format!("{:?}", error);
    /// assert!(debug_str.contains("TypeError"));
    /// assert!(debug_str.contains("int"));
    /// assert!(debug_str.contains("float"));
    /// ```
    #[test]
    fn test_error_display() {
        let error = InterpreterError::DivisionByZero;
        let debug_str = format!("{:?}", error);
        assert!(debug_str.contains("DivisionByZero"));

        let error = InterpreterError::TypeError {
            expected: "int",
            got: "float".to_string(),
        };
        let debug_str = format!("{:?}", error);
        assert!(debug_str.contains("TypeError"));
        assert!(debug_str.contains("int"));
        assert!(debug_str.contains("float"));
    }

    /// Verifies that every `InterpreterError` variant can be constructed.
    ///
    /// Constructs one instance of each `InterpreterError` variant and asserts that
    /// the collection contains nine entries, covering all defined variants.
    ///
    /// # Examples
    ///
    /// ```
    /// let errors = vec![
    ///     InterpreterError::DivisionByZero,
    ///     InterpreterError::InvalidHeapAccess,
    ///     InterpreterError::StackOverflow,
    ///     InterpreterError::NoMatchingCase,
    ///     InterpreterError::UnknownBuiltin("test".to_string()),
    ///     InterpreterError::TypeError { expected: "test", got: "test".to_string() },
    ///     InterpreterError::UndefinedValue(ValueId(1)),
    ///     InterpreterError::UndefinedFunction(FunctionId(1)),
    ///     InterpreterError::IOError(std::io::Error::new(std::io::ErrorKind::Other, "test")),
    /// ];
    /// assert_eq!(errors.len(), 9);
    /// ```
    #[test]
    fn test_error_variants_coverage() {
        let errors = vec![
            InterpreterError::DivisionByZero,
            InterpreterError::InvalidHeapAccess,
            InterpreterError::StackOverflow,
            InterpreterError::NoMatchingCase,
            InterpreterError::UnknownBuiltin("test".to_string()),
            InterpreterError::TypeError {
                expected: "test",
                got: "test".to_string(),
            },
            InterpreterError::UndefinedValue(ValueId(1)),
            InterpreterError::UndefinedFunction(FunctionId(1)),
            InterpreterError::IOError(io::Error::new(io::ErrorKind::Other, "test")),
        ];

        assert_eq!(errors.len(), 9); // All variants should be covered
    }
}
