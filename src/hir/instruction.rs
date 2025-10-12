//! Instructions in the HIR

use super::{AllocationInfo, BlockId, TypeId, ValueId};

/// An instruction in the HIR
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Instruction {
    pub opcode: Opcode,
    pub args: Vec<ValueId>,
    pub ty: TypeId,
    pub is_pure: bool, // Whether the instruction has side effects
    pub allocation_info: Option<AllocationInfo>, // Allocation info for memory operations
}

/// The operation code for an instruction
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Opcode {
    // Binary operations
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,

    // Unary operations
    Neg,
    Not,
    BitNot,

    // Memory operations
    Load,
    Store,
    Alloca,
    AllocaHeap,
    GetElementPtr,
    Cast,

    // Control flow
    Jump,
    Branch, // Conditional branch
    Switch, // Multi-way branch
    Phi,    // Phi node for SSA

    // Function calls
    Call,
    Return,
    CallIndirect,

    // Aggregate operations
    ExtractValue, // Extract from struct/tuple
    InsertValue,  // Insert into struct/tuple

    // Other operations
    Nop,
    Unreachable,
}

/// A phi instruction specifically for handling control flow merges
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhiInstruction {
    pub ty: TypeId,
    pub incoming_values: Vec<(ValueId, BlockId)>,
}

/// A phi node that can be part of a basic block's parameters
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PhiNode {
    pub result: ValueId,
    pub ty: TypeId,
    pub incomings: Vec<(ValueId, BlockId)>,
}

impl Instruction {
    /// Check if an instruction is a terminator instruction
    pub fn is_terminator(&self) -> bool {
        matches!(
            self.opcode,
            Opcode::Jump | Opcode::Branch | Opcode::Switch | Opcode::Return | Opcode::Unreachable
        )
    }

    /// Check if an instruction has no side effects
    pub fn is_pure(&self) -> bool {
        self.is_pure
    }
}
