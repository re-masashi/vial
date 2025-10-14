//! High-level Intermediate Representation (HIR)
//!
//! This module implements a typed, allocation-aware HIR designed for
//! optimization passes like escape analysis, scalar replacement, and CTFE.

pub mod builder;
pub mod cfg;
pub mod display;
pub mod from_ast;
pub mod function;
pub mod instruction;
pub mod types;
pub mod utils;
pub mod value;

#[cfg(test)]
pub mod tests;

pub use builder::Builder;
pub use cfg::ControlFlowGraph;
pub use display::HirDisplay;
pub use function::{BasicBlock, Function, FunctionId, FunctionSignature, Param, Terminator};
pub use instruction::{Instruction, Opcode, PhiNode};
pub use types::{FloatType, IntType, Type, TypeContext, TypeId};
pub use utils::{ConstantFolder, DominatorTree, Inliner};
pub use value::{Constant, Value, ValueId};

/// A unique identifier for a basic block within a function
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

/// Represents the location of an instruction within a function
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstLocation {
    pub block: BlockId,
    pub index: usize,
}

/// Memory allocation preference for values
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum AllocationPreference {
    /// Prefer stack allocation
    Stack,
    /// Prefer heap allocation
    Heap,
    /// No specific preference, let the allocator decide
    #[default]
    Default,
}

/// Information about memory allocation for a type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AllocationInfo {
    pub preference: AllocationPreference,
    pub alignment: Option<usize>, // None means use default alignment for the type
    pub is_pinned: bool,          // Whether the memory location should not move
}

impl Default for AllocationInfo {
    fn default() -> Self {
        AllocationInfo {
            preference: AllocationPreference::Default,
            alignment: None,
            is_pinned: false,
        }
    }
}
