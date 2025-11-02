#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Ptr(HeapPtr), // Pointer to heap object
    Null,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub struct HeapPtr {
    pub id: usize, // Index into heap
}

impl Value {
    pub fn as_int(&self) -> Result<i64, super::error::InterpreterError> {
        match self {
            Value::Int(i) => Ok(*i),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "int",
                got: self.type_name().to_string(),
            }),
        }
    }

    pub fn as_float(&self) -> Result<f64, super::error::InterpreterError> {
        match self {
            Value::Float(f) => Ok(*f),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "float",
                got: self.type_name().to_string(),
            }),
        }
    }

    pub fn as_bool(&self) -> Result<bool, super::error::InterpreterError> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "bool",
                got: self.type_name().to_string(),
            }),
        }
    }

    pub fn as_ptr(&self) -> Result<HeapPtr, super::error::InterpreterError> {
        match self {
            Value::Ptr(ptr) => Ok(*ptr),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "ptr",
                got: self.type_name().to_string(),
            }),
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Bool(_) => "bool",
            Value::String(_) => "string",
            Value::Ptr(_) => "ptr",
            Value::Null => "null",
        }
    }

    // Arithmetic operations
    pub fn add(&self, other: &Value) -> Result<Value, super::error::InterpreterError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + *b as f64)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
            (Value::String(a), Value::Int(b)) => Ok(Value::String(format!("{}{}", a, b))),
            (Value::String(a), Value::Float(b)) => Ok(Value::String(format!("{}{}", a, b))),
            (Value::String(a), Value::Bool(b)) => Ok(Value::String(format!("{}{}", a, b))),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "compatible types for addition",
                got: format!("{} and {}", self.type_name(), other.type_name()),
            }),
        }
    }

    pub fn sub(&self, other: &Value) -> Result<Value, super::error::InterpreterError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - *b as f64)),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "numeric types for subtraction",
                got: format!("{} and {}", self.type_name(), other.type_name()),
            }),
        }
    }

    pub fn mul(&self, other: &Value) -> Result<Value, super::error::InterpreterError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * *b as f64)),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "numeric types for multiplication",
                got: format!("{} and {}", self.type_name(), other.type_name()),
            }),
        }
    }

    pub fn div(&self, other: &Value) -> Result<Value, super::error::InterpreterError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(super::error::InterpreterError::DivisionByZero)
                } else {
                    Ok(Value::Int(a / b))
                }
            }
            (Value::Float(a), Value::Float(b)) => {
                if *b == 0.0 {
                    Err(super::error::InterpreterError::DivisionByZero)
                } else {
                    Ok(Value::Float(a / b))
                }
            }
            (Value::Int(a), Value::Float(b)) => {
                if *b == 0.0 {
                    Err(super::error::InterpreterError::DivisionByZero)
                } else {
                    Ok(Value::Float(*a as f64 / b))
                }
            }
            (Value::Float(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(super::error::InterpreterError::DivisionByZero)
                } else {
                    Ok(Value::Float(a / *b as f64))
                }
            }
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "numeric types for division",
                got: format!("{} and {}", self.type_name(), other.type_name()),
            }),
        }
    }

    // Comparison operations
    pub fn eq(&self, other: &Value) -> Result<Value, super::error::InterpreterError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a == b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool((a - b).abs() < f64::EPSILON)),
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a == b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a == b)),
            (Value::Null, Value::Null) => Ok(Value::Bool(true)),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "comparable types",
                got: format!("{} and {}", self.type_name(), other.type_name()),
            }),
        }
    }

    pub fn lt(&self, other: &Value) -> Result<Value, super::error::InterpreterError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a < b)),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "ordered types",
                got: format!("{} and {}", self.type_name(), other.type_name()),
            }),
        }
    }

    pub fn gt(&self, other: &Value) -> Result<Value, super::error::InterpreterError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a > b)),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "ordered types",
                got: format!("{} and {}", self.type_name(), other.type_name()),
            }),
        }
    }

    pub fn le(&self, other: &Value) -> Result<Value, super::error::InterpreterError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a <= b)),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "ordered types",
                got: format!("{} and {}", self.type_name(), other.type_name()),
            }),
        }
    }

    pub fn ge(&self, other: &Value) -> Result<Value, super::error::InterpreterError> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a >= b)),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "ordered types",
                got: format!("{} and {}", self.type_name(), other.type_name()),
            }),
        }
    }

    pub fn ne(&self, other: &Value) -> Result<Value, super::error::InterpreterError> {
        match self.eq(other) {
            Ok(Value::Bool(b)) => Ok(Value::Bool(!b)),
            Err(e) => Err(e),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "boolean for comparison",
                got: "non-boolean".to_string(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_creation() {
        let int_val = Value::Int(42);
        let float_val = Value::Float(3.14);
        let bool_val = Value::Bool(true);
        let string_val = Value::String("hello".to_string());
        let null_val = Value::Null;

        assert_eq!(int_val.as_int().unwrap(), 42);
        assert_eq!(float_val.as_float().unwrap(), 3.14);
        assert_eq!(bool_val.as_bool().unwrap(), true);
        assert_eq!(string_val, Value::String("hello".to_string()));
        match null_val {
            Value::Null => (),
            _ => panic!("Expected null value"),
        }
    }

    #[test]
    fn test_type_conversions() {
        let int_val = Value::Int(42);
        assert_eq!(int_val.as_int().unwrap(), 42);

        let float_val = Value::Float(3.14);
        assert_eq!(float_val.as_float().unwrap(), 3.14);

        let bool_val = Value::Bool(true);
        assert_eq!(bool_val.as_bool().unwrap(), true);

        // Test type errors
        let result = int_val.as_float();
        assert!(result.is_err());
    }

    #[test]
    fn test_arithmetic_operations() {
        let val1 = Value::Int(10);
        let val2 = Value::Int(5);

        assert_eq!(val1.add(&val2).unwrap(), Value::Int(15));
        assert_eq!(val1.sub(&val2).unwrap(), Value::Int(5));
        assert_eq!(val1.mul(&val2).unwrap(), Value::Int(50));
        assert_eq!(val1.div(&val2).unwrap(), Value::Int(2));

        let val3 = Value::Float(10.0);
        let val4 = Value::Float(2.0);

        assert_eq!(val3.add(&val4).unwrap(), Value::Float(12.0));
        assert_eq!(val3.div(&val4).unwrap(), Value::Float(5.0));
    }

    #[test]
    fn test_comparison_operations() {
        let val1 = Value::Int(10);
        let val2 = Value::Int(5);

        assert_eq!(val1.eq(&val2).unwrap(), Value::Bool(false));
        assert_eq!(val1.gt(&val2).unwrap(), Value::Bool(true));
        assert_eq!(val1.lt(&val2).unwrap(), Value::Bool(false));

        let val3 = Value::Int(10);
        assert_eq!(val1.eq(&val3).unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_string_operations() {
        let str1 = Value::String("Hello".to_string());
        let str2 = Value::String(" World".to_string());

        assert_eq!(
            str1.add(&str2).unwrap(),
            Value::String("Hello World".to_string())
        );
    }

    #[test]
    fn test_heap_ptr() {
        let ptr1 = HeapPtr { id: 42 };
        let ptr2 = HeapPtr { id: 42 };

        assert_eq!(ptr1.id, 42);
        assert_eq!(ptr1, ptr2);
        assert_eq!(format!("{:?}", ptr1), "HeapPtr { id: 42 }");
    }
}
