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
    /// Extracts the inner `i64` from an `Int` value.
    ///
    /// # Returns
    ///
    /// `Ok(i64)` containing the integer when the value is `Value::Int`, `Err(InterpreterError::TypeError)` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    /// let v = Value::Int(42);
    /// assert_eq!(v.as_int().unwrap(), 42);
    /// ```
    pub fn as_int(&self) -> Result<i64, super::error::InterpreterError> {
        match self {
            Value::Int(i) => Ok(*i),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "int",
                got: self.type_name().to_string(),
            }),
        }
    }

    /// Extracts the inner `f64` from a `Value::Float`.
    ///
    /// # Returns
    ///
    /// `Ok(f)` containing the inner `f64` if the value is `Value::Float`, `Err(InterpreterError::TypeError)` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    /// let v = Value::Float(1.23);
    /// assert_eq!(v.as_float().unwrap(), 1.23);
    /// ```
    pub fn as_float(&self) -> Result<f64, super::error::InterpreterError> {
        match self {
            Value::Float(f) => Ok(*f),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "float",
                got: self.type_name().to_string(),
            }),
        }
    }

    /// Extracts the contained boolean when this `Value` is a `Bool`.
    ///
    /// # Returns
    ///
    /// `Ok(true)` or `Ok(false)` when the `Value` is `Bool` with that value; returns
    /// `Err(InterpreterError::TypeError)` when the `Value` is not a `Bool`.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    /// let v = Value::Bool(true);
    /// assert_eq!(v.as_bool().unwrap(), true);
    /// ```
    pub fn as_bool(&self) -> Result<bool, super::error::InterpreterError> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "bool",
                got: self.type_name().to_string(),
            }),
        }
    }

    /// Extracts the inner heap pointer from the value.
    ///
    /// Returns the contained `HeapPtr` when the value is a `Ptr`; otherwise returns a
    /// `TypeError` indicating that a `ptr` was expected.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::{Value, HeapPtr};
    ///
    /// let v = Value::Ptr(HeapPtr { id: 3 });
    /// let p = v.as_ptr().unwrap();
    /// assert_eq!(p.id, 3);
    /// ```
    pub fn as_ptr(&self) -> Result<HeapPtr, super::error::InterpreterError> {
        match self {
            Value::Ptr(ptr) => Ok(*ptr),
            _ => Err(super::error::InterpreterError::TypeError {
                expected: "ptr",
                got: self.type_name().to_string(),
            }),
        }
    }

    /// Get the runtime type name of the value.
    ///
    /// # Returns
    ///
    /// A `&'static str` with one of: `"int"`, `"float"`, `"bool"`, `"string"`, `"ptr"`, or `"null"`.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    ///
    /// let v = Value::Int(42);
    /// assert_eq!(v.type_name(), "int");
    /// let s = Value::String(String::from("hi"));
    /// assert_eq!(s.type_name(), "string");
    /// ```
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
    /// Adds two runtime Values using type-appropriate semantics.
    ///
    /// On numeric operands (int and/or float) performs numeric addition, promoting to float when mixed.
    /// On string operands, concatenates strings; strings are also concatenated with int, float, or bool by
    /// converting the non-string operand to its textual representation.
    ///
    /// # Returns
    ///
    /// `Ok(Value)` containing the sum (an `Int`, `Float`, or `String`) on success, or `Err(InterpreterError::TypeError)`
    /// when the operand types are not compatible for addition.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    ///
    /// // integer addition
    /// let r = Value::Int(2).add(&Value::Int(3)).unwrap();
    /// assert_eq!(r, Value::Int(5));
    ///
    /// // mixed int and float -> float
    /// let r = Value::Int(2).add(&Value::Float(0.5)).unwrap();
    /// assert_eq!(r, Value::Float(2.5));
    ///
    /// // string concatenation
    /// let r = Value::String("hello ".into()).add(&Value::String("world".into())).unwrap();
    /// assert_eq!(r, Value::String("hello world".into()));
    /// ```
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

    /// Subtracts another `Value` from this `Value`.
    ///
    /// Supports integer and floating-point operands; mixed int/float produces a `Float`.
    ///
    /// # Returns
    /// `Value::Int` when both operands are integers; `Value::Float` when either operand is a float.
    /// Returns a `TypeError` if operands are not numeric types.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    ///
    /// let a = Value::Int(10);
    /// let b = Value::Float(2.5);
    /// assert_eq!(a.sub(&b).unwrap(), Value::Float(7.5));
    ///
    /// let c = Value::Int(3);
    /// assert_eq!(c.sub(&Value::Int(1)).unwrap(), Value::Int(2));
    /// ```
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

    /// Compute the product of two runtime values.
    ///
    /// # Returns
    ///
    /// `Value::Int` when both operands are integers, `Value::Float` when either operand is a float, or an `InterpreterError::TypeError` describing incompatible operand types.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    ///
    /// let a = Value::Int(6);
    /// let b = Value::Int(7);
    /// assert_eq!(a.mul(&b).unwrap(), Value::Int(42));
    ///
    /// let x = Value::Int(2);
    /// let y = Value::Float(2.5);
    /// assert_eq!(x.mul(&y).unwrap(), Value::Float(5.0));
    /// ```
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

    /// Compute the quotient of two numeric `Value`s.
    ///
    /// Performs integer division when both operands are `Value::Int`, and floating-point
    /// division when either operand is a `Value::Float`. Returns an `InterpreterError::DivisionByZero`
    /// if the right-hand operand is zero, or `InterpreterError::TypeError` if either operand is not numeric.
    ///
    /// # Returns
    ///
    /// `Value::Int` with the integer quotient for two ints, or `Value::Float` with the floating-point
    /// quotient for any combination involving a float.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    ///
    /// let a = Value::Int(6);
    /// let b = Value::Int(2);
    /// assert_eq!(a.div(&b).unwrap(), Value::Int(3));
    ///
    /// let x = Value::Float(7.5);
    /// let y = Value::Int(2);
    /// assert_eq!(x.div(&y).unwrap(), Value::Float(3.75));
    /// ```
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
    /// Compares two runtime `Value`s for equality and yields a boolean `Value`.
    ///
    /// Performs type-specific equality:
    /// - Integer vs integer: exact equality.
    /// - Float vs float: approximate equality using `f64::EPSILON`.
    /// - Bool vs bool and String vs string: exact equality.
    /// - Null vs null: equal.
    ///
    /// # Returns
    ///
    /// `Ok(Value::Bool(true))` if the values are considered equal, `Ok(Value::Bool(false))` if not; returns `Err(InterpreterError::TypeError{..})` when the operands are not comparable.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    ///
    /// let a = Value::Int(42);
    /// let b = Value::Int(42);
    /// assert_eq!(a.eq(&b).unwrap(), Value::Bool(true));
    ///
    /// let x = Value::Float(0.1 + 0.2);
    /// let y = Value::Float(0.3);
    /// // approximate equality for floats
    /// assert!(matches!(x.eq(&y).unwrap(), Value::Bool(_)));
    /// ```
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

    /// Determines whether `self` is less than `other`.
    ///
    /// Compares two values when both are the same ordered type (int, float, or string)
    /// and returns a `Value::Bool` reflecting the comparison. If the operands are not
    /// both ordered types, returns a `TypeError` with `expected: "ordered types"`.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    ///
    /// assert_eq!(Value::Int(1).lt(&Value::Int(2)).unwrap(), Value::Bool(true));
    /// assert_eq!(Value::Float(1.5).lt(&Value::Float(1.5)).unwrap(), Value::Bool(false));
    /// assert_eq!(Value::String("a".into()).lt(&Value::String("b".into())).unwrap(), Value::Bool(true));
    /// ```
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

    /// Compare two runtime values for "greater than".
    ///
    /// Returns `Value::Bool(true)` if the left operand is greater than the right operand,
    /// `Value::Bool(false)` if it is not. Operands must be both integers, both floats,
    /// or both strings; other type combinations produce an error.
    ///
    /// # Errors
    ///
    /// Returns `Err(InterpreterError::TypeError)` when operands are not both ordered types
    /// (both `int`, both `float`, or both `string`).
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    ///
    /// let a = Value::Int(3);
    /// let b = Value::Int(2);
    /// assert_eq!(a.gt(&b).unwrap(), Value::Bool(true));
    ///
    /// let x = Value::Float(1.5);
    /// let y = Value::Float(2.0);
    /// assert_eq!(x.gt(&y).unwrap(), Value::Bool(false));
    ///
    /// let s1 = Value::String("b".into());
    /// let s2 = Value::String("a".into());
    /// assert_eq!(s1.gt(&s2).unwrap(), Value::Bool(true));
    /// ```
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

    /// Compares two values and returns a `Value::Bool` indicating whether the left value is less than or equal to the right.
    ///
    /// Returns `Value::Bool(true)` if `self` is less than or equal to `other` for supported ordered types, `Value::Bool(false)` otherwise.
    /// Supported ordered comparisons are:
    /// - `Int` vs `Int`
    /// - `Float` vs `Float`
    /// - `String` vs `String`
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    ///
    /// let a = Value::Int(1);
    /// let b = Value::Int(2);
    /// assert_eq!(a.le(&b).unwrap(), Value::Bool(true));
    ///
    /// let s1 = Value::String("a".to_string());
    /// let s2 = Value::String("b".to_string());
    /// assert_eq!(s1.le(&s2).unwrap(), Value::Bool(true));
    /// ```
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

    /// Compares two values and yields whether the left operand is greater than or equal to the right.
    ///
    /// # Returns
    ///
    /// `Value::Bool(true)` if the left value is greater than or equal to the right for supported types, `Value::Bool(false)` otherwise.
    ///
    /// # Errors
    ///
    /// Returns `InterpreterError::TypeError` when the operands are not both integers, both floats, or both strings; the error's `expected` is `"ordered types"` and `got` describes the operand types.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    ///
    /// let a = Value::Int(3);
    /// let b = Value::Int(2);
    /// assert_eq!(a.ge(&b).unwrap(), Value::Bool(true));
    ///
    /// let s = Value::String("a".into());
    /// let n = Value::Int(1);
    /// assert!(s.ge(&n).is_err());
    /// ```
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

    /// Computes whether two `Value`s are not equal.
    ///
    /// This returns a `Value::Bool` with `true` when the operands are not equal and `false` when they are equal.
    /// If equality comparison fails, the underlying error is returned. If equality yields a non-boolean result,
    /// a `TypeError` with `expected: "boolean for comparison"` and `got: "non-boolean"` is produced.
    ///
    /// # Examples
    ///
    /// ```
    /// use vial::interpreter::value::Value;
    ///
    /// let a = Value::Int(1);
    /// let b = Value::Int(2);
    /// assert_eq!(a.ne(&b).unwrap(), Value::Bool(true));
    ///
    /// let x = Value::String("hi".to_string());
    /// let y = Value::String("hi".to_string());
    /// assert_eq!(x.ne(&y).unwrap(), Value::Bool(false));
    /// ```
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
    use crate::interpreter::value::{HeapPtr, Value};

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
