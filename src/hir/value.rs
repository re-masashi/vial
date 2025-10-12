//! Value representations in the HIR

use super::{AllocationPreference, TypeId};

/// A value in the HIR
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    /// A constant value
    Constant(Constant),
    /// A parameter to a function
    Parameter { index: usize, ty: TypeId },
    /// A local variable in a function
    Local {
        ty: TypeId,
        allocation: AllocationPreference,
    },
    /// The result of an instruction
    InstructionResult {
        ty: TypeId,
        allocation: AllocationPreference,
    },
}

use std::hash::{Hash, Hasher};

/// A constant value
#[derive(Debug, Clone)]
pub enum Constant {
    /// Integer constant
    Int(i128),
    /// Float constant
    Float(f64),
    /// Boolean constant
    Bool(bool),
    /// Character constant
    Char(char),
    /// String constant
    String(String),
    /// Null pointer
    Null(TypeId), // Include the type for null pointers
    /// Unit value (empty tuple)
    Unit,
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Constant::Int(a), Constant::Int(b)) => a == b,
            (Constant::Float(a), Constant::Float(b)) => a.to_bits() == b.to_bits(),
            (Constant::Bool(a), Constant::Bool(b)) => a == b,
            (Constant::Char(a), Constant::Char(b)) => a == b,
            (Constant::String(a), Constant::String(b)) => a == b,
            (Constant::Null(a), Constant::Null(b)) => a == b,
            (Constant::Unit, Constant::Unit) => true,
            _ => false,
        }
    }
}

impl Eq for Constant {}

impl Hash for Constant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Constant::Int(i) => {
                state.write_u8(0); // tag
                i.hash(state);
            }
            Constant::Float(f) => {
                state.write_u8(1); // tag
                f.to_bits().hash(state);
            }
            Constant::Bool(b) => {
                state.write_u8(2); // tag
                b.hash(state);
            }
            Constant::Char(c) => {
                state.write_u8(3); // tag
                c.hash(state);
            }
            Constant::String(s) => {
                state.write_u8(4); // tag
                s.hash(state);
            }
            Constant::Null(ty) => {
                state.write_u8(5); // tag
                ty.hash(state);
            }
            Constant::Unit => {
                state.write_u8(6); // tag
            }
        }
    }
}

/// A unique identifier for a value in the HIR
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(pub usize);
