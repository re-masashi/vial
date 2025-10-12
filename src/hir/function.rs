//! Function representation in the HIR

use super::{AllocationPreference, BlockId, Instruction, PhiNode, TypeId, ValueId};
use std::collections::HashMap;

/// A function in the HIR
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub signature: FunctionSignature,
    pub basic_blocks: Vec<BasicBlock>,
    pub instructions: Vec<Instruction>,
    pub block_params: HashMap<BlockId, Vec<ValueId>>,
}

/// Function signature
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub params: Vec<Param>,
    pub return_type: TypeId,
    pub is_variadic: bool,
}

/// Function parameter
#[derive(Debug, Clone)]
pub struct Param {
    pub name: Option<String>,
    pub ty: TypeId,
    pub allocation: AllocationPreference,
}

/// A basic block in the HIR
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub params: Vec<ValueId>, // Parameters passed to this block (for phi nodes)
    pub instructions: Vec<usize>, // Indices into the function's instruction list
    pub phi_nodes: Vec<PhiNode>, // Phi nodes at the beginning of the block
    pub terminator: Terminator,
}

/// The terminator instruction for a basic block
#[derive(Debug, Clone)]
pub enum Terminator {
    /// Unconditional jump to another block
    Jump { target: BlockId, args: Vec<ValueId> },
    /// Conditional branch
    Branch {
        condition: ValueId,
        then_block: BlockId,
        else_block: BlockId,
        then_args: Vec<ValueId>,
        else_args: Vec<ValueId>,
    },
    /// Switch statement
    Switch {
        value: ValueId,
        targets: Vec<(i128, BlockId)>, // (value, block) pairs
        default: BlockId,
    },
    /// Return from function
    Return { value: Option<ValueId> },
    /// Unreachable code
    Unreachable,
}

impl Function {
    /// Get a basic block by its ID
    pub fn get_block(&self, block_id: BlockId) -> Option<&BasicBlock> {
        self.basic_blocks.iter().find(|block| block.id == block_id)
    }

    /// Get a mutable reference to a basic block by its ID
    pub fn get_block_mut(&mut self, block_id: BlockId) -> Option<&mut BasicBlock> {
        self.basic_blocks
            .iter_mut()
            .find(|block| block.id == block_id)
    }

    /// Get an instruction by its index
    pub fn get_instruction(&self, idx: usize) -> Option<&Instruction> {
        self.instructions.get(idx)
    }

    /// Get a mutable reference to an instruction by its index
    pub fn get_instruction_mut(&mut self, idx: usize) -> Option<&mut Instruction> {
        self.instructions.get_mut(idx)
    }
}

/// A unique identifier for a function within the module
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub usize);
