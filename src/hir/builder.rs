//! Builder for constructing HIR

use super::{
    AllocationInfo, AllocationPreference, BasicBlock, BlockId, Function, FunctionSignature,
    Instruction, Opcode, PhiNode, Terminator, TypeId, ValueId,
};
use std::collections::HashMap;

/// Builder for constructing HIR functions
pub struct Builder {
    current_function: Option<Function>,
    current_block: Option<BlockId>,
    next_value_id: usize,
    next_block_id: usize,
    instructions: Vec<Instruction>,
    block_params: HashMap<BlockId, Vec<ValueId>>,
}

impl Builder {
    pub fn new() -> Self {
        Builder {
            current_function: None,
            current_block: None,
            next_value_id: 0,
            next_block_id: 0,
            instructions: Vec::new(),
            block_params: HashMap::new(),
        }
    }

    /// Start building a new function
    pub fn start_function(&mut self, name: String, signature: FunctionSignature) -> Function {
        let function = Function {
            name,
            signature,
            basic_blocks: Vec::new(),
            instructions: Vec::new(),
            block_params: HashMap::new(),
        };

        self.current_function = Some(function.clone());
        self.current_block = None;
        self.next_value_id = 0;
        self.next_block_id = 0;
        self.instructions.clear();
        self.block_params.clear();

        function
    }

    /// Create a new basic block
    pub fn create_block(&mut self) -> BlockId {
        let block_id = BlockId(self.next_block_id);
        self.next_block_id += 1;

        // Add the block to the current function
        if let Some(ref mut func) = self.current_function {
            func.basic_blocks.push(BasicBlock {
                id: block_id,
                params: Vec::new(),
                instructions: Vec::new(),
                phi_nodes: Vec::new(),
                terminator: Terminator::Unreachable,
            });
        }

        block_id
    }

    /// Add parameters to a block (for phi nodes)
    pub fn add_block_params(&mut self, block: BlockId, params: Vec<ValueId>) {
        self.block_params.insert(block, params);
    }

    /// Set the current block for inserting instructions
    pub fn switch_to_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    /// Create a constant value
    pub fn const_int(&mut self, _value: i128, _ty: TypeId) -> ValueId {
        let value_id = ValueId(self.next_value_id);
        self.next_value_id += 1;

        // In a real implementation, we'd store the constant value in the function
        // For now, we'll just return a value ID
        value_id
    }

    /// Create a constant boolean value
    pub fn const_bool(&mut self, _value: bool) -> ValueId {
        let value_id = ValueId(self.next_value_id);
        self.next_value_id += 1;

        value_id
    }

    /// Create a constant float value
    pub fn const_float(&mut self, _value: f64, _ty: TypeId) -> ValueId {
        let value_id = ValueId(self.next_value_id);
        self.next_value_id += 1;

        // In a real implementation, we'd store the float value in the function
        // For now, we'll just return a value ID
        value_id
    }

    /// Create a constant string value
    pub fn const_string(&mut self, _value: &str, _ty: TypeId) -> ValueId {
        let value_id = ValueId(self.next_value_id);
        self.next_value_id += 1;

        // In a real implementation, we'd store the string value in the function
        // For now, we'll just return a value ID
        value_id
    }

    /// Create a binary operation instruction
    pub fn binary_op(
        &mut self,
        opcode: Opcode,
        left: ValueId,
        right: ValueId,
        ty: TypeId,
    ) -> ValueId {
        let inst = Instruction {
            opcode,
            args: vec![left, right],
            ty,
            is_pure: true,         // Most binary ops are pure
            allocation_info: None, // No allocation info for binary ops
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        // Return the value ID that represents the result of this instruction
        ValueId(inst_idx)
    }

    /// Create an allocation instruction (stack)
    pub fn alloca(&mut self, ty: TypeId, alignment: Option<usize>) -> ValueId {
        let inst = Instruction {
            opcode: Opcode::Alloca,
            args: vec![],
            ty,
            is_pure: false, // Allocations have side effects
            allocation_info: Some(AllocationInfo {
                preference: AllocationPreference::Stack,
                alignment,
                is_pinned: false,
            }),
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        ValueId(inst_idx)
    }

    /// Create an allocation instruction (heap)
    pub fn alloca_heap(&mut self, ty: TypeId) -> ValueId {
        let inst = Instruction {
            opcode: Opcode::AllocaHeap,
            args: vec![],
            ty,
            is_pure: false, // Heap allocations have side effects
            allocation_info: Some(AllocationInfo {
                preference: AllocationPreference::Heap,
                alignment: None,
                is_pinned: false,
            }),
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        ValueId(inst_idx)
    }

    /// Create a load instruction
    pub fn load(&mut self, ptr: ValueId, ty: TypeId) -> ValueId {
        let inst = Instruction {
            opcode: Opcode::Load,
            args: vec![ptr],
            ty,
            is_pure: false, // Loads can observe side effects
            allocation_info: None,
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        ValueId(inst_idx)
    }

    /// Create a store instruction
    pub fn store(&mut self, ptr: ValueId, value: ValueId) -> ValueId {
        let inst = Instruction {
            opcode: Opcode::Store,
            args: vec![ptr, value],
            ty: TypeId(0), // Store instructions don't return a meaningful value, but we need a placeholder
            is_pure: false, // Stores have side effects
            allocation_info: None,
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        ValueId(inst_idx)
    }

    /// Create a function call instruction
    pub fn call(&mut self, _func: ValueId, args: Vec<ValueId>, return_ty: TypeId) -> ValueId {
        let inst = Instruction {
            opcode: Opcode::Call,
            args,
            ty: return_ty,
            is_pure: false, // Function calls can have side effects
            allocation_info: None,
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        ValueId(inst_idx)
    }

    /// Create an effect invocation instruction
    pub fn invoke_effect(
        &mut self,
        effect_id: ValueId,
        args: Vec<ValueId>,
        return_ty: TypeId,
    ) -> ValueId {
        let inst = Instruction {
            opcode: Opcode::PerformEffect,
            args: std::iter::once(effect_id).chain(args).collect(), // Effect ID first, then arguments
            ty: return_ty,
            is_pure: false, // Effects have side effects
            allocation_info: None,
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        ValueId(inst_idx)
    }

    /// Create a get element pointer instruction
    pub fn get_element_ptr(
        &mut self,
        ptr: ValueId,
        indices: Vec<ValueId>,
        result_ty: TypeId,
    ) -> ValueId {
        let inst = Instruction {
            opcode: Opcode::GetElementPtr,
            args: std::iter::once(ptr).chain(indices).collect(),
            ty: result_ty,
            is_pure: true, // GEP is side-effect free
            allocation_info: None,
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        ValueId(inst_idx)
    }

    /// Create an array allocation instruction
    pub fn alloc_array(&mut self, element_type: TypeId, size: ValueId, is_heap: bool) -> ValueId {
        let opcode = if is_heap {
            Opcode::AllocaHeap
        } else {
            Opcode::Alloca
        };
        let inst = Instruction {
            opcode,
            args: vec![size], // Size is passed as an argument
            ty: element_type, // Use the provided element_type for the instruction type
            is_pure: false,   // Allocations have side effects
            allocation_info: Some(AllocationInfo {
                preference: if is_heap {
                    AllocationPreference::Heap
                } else {
                    AllocationPreference::Stack
                },
                alignment: None,
                is_pinned: false,
            }),
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        ValueId(inst_idx)
    }

    /// Create a tuple allocation instruction
    pub fn alloc_tuple(&mut self, _element_types: &[TypeId], is_heap: bool) -> ValueId {
        let opcode = if is_heap {
            Opcode::AllocaHeap
        } else {
            Opcode::Alloca
        };
        let inst = Instruction {
            opcode,
            args: vec![],   // Tuples don't need size args like arrays
            ty: TypeId(0), // Should be tuple type in real implementation - need to create tuple type from element_types
            is_pure: false, // Allocations have side effects
            allocation_info: Some(AllocationInfo {
                preference: if is_heap {
                    AllocationPreference::Heap
                } else {
                    AllocationPreference::Stack
                },
                alignment: None,
                is_pinned: false,
            }),
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        ValueId(inst_idx)
    }

    /// Create a map allocation instruction
    pub fn alloc_map(&mut self, _key_type: TypeId, _value_type: TypeId, is_heap: bool) -> ValueId {
        let opcode = if is_heap {
            Opcode::AllocaHeap
        } else {
            Opcode::Alloca
        };
        let inst = Instruction {
            opcode,
            args: vec![],   // Maps don't need initial size in allocation
            ty: TypeId(0),  // Should be map type in real implementation
            is_pure: false, // Allocations have side effects
            allocation_info: Some(AllocationInfo {
                preference: if is_heap {
                    AllocationPreference::Heap
                } else {
                    AllocationPreference::Stack
                },
                alignment: None,
                is_pinned: false,
            }),
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        ValueId(inst_idx)
    }

    /// Create an enum allocation instruction
    pub fn alloc_enum(&mut self, _tag_type: TypeId, _data_type: TypeId, is_heap: bool) -> ValueId {
        let opcode = if is_heap {
            Opcode::AllocaHeap
        } else {
            Opcode::Alloca
        };
        let inst = Instruction {
            opcode,
            args: vec![],   // Enums don't need initial args
            ty: TypeId(0),  // Should be enum type in real implementation
            is_pure: false, // Allocations have side effects
            allocation_info: Some(AllocationInfo {
                preference: if is_heap {
                    AllocationPreference::Heap
                } else {
                    AllocationPreference::Stack
                },
                alignment: None,
                is_pinned: false,
            }),
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        ValueId(inst_idx)
    }

    /// Create a struct allocation instruction
    pub fn alloc_struct(&mut self, _field_types: &[TypeId], is_heap: bool) -> ValueId {
        let opcode = if is_heap {
            Opcode::AllocaHeap
        } else {
            Opcode::Alloca
        };
        let inst = Instruction {
            opcode,
            args: vec![],   // Structs don't need initial args
            ty: TypeId(0),  // Should be struct type in real implementation
            is_pure: false, // Allocations have side effects
            allocation_info: Some(AllocationInfo {
                preference: if is_heap {
                    AllocationPreference::Heap
                } else {
                    AllocationPreference::Stack
                },
                alignment: None,
                is_pinned: false,
            }),
        };

        let inst_idx = self.instructions.len();
        self.instructions.push(inst);

        // Add the instruction to the current block
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.instructions.push(inst_idx);
        }

        ValueId(inst_idx)
    }

    /// Add a phi node to the current block
    pub fn add_phi_node(
        &mut self,
        result: ValueId,
        ty: TypeId,
        incomings: Vec<(ValueId, BlockId)>,
    ) {
        if let Some(block_id) = self.current_block
            && let Some(ref mut func) = self.current_function
            && let Some(block) = func.basic_blocks.iter_mut().find(|b| b.id == block_id)
        {
            block.phi_nodes.push(PhiNode {
                result,
                ty,
                incomings,
            });
        }
    }

    /// Get the next value ID without incrementing it
    pub fn get_next_value_id(&self) -> usize {
        self.next_value_id
    }

    /// Create a return instruction
    pub fn ret(&mut self, value: Option<ValueId>) -> Terminator {
        Terminator::Return { value }
    }

    /// Create a jump instruction
    pub fn jump(&mut self, target: BlockId, args: Vec<ValueId>) -> Terminator {
        Terminator::Jump { target, args }
    }

    /// Create a conditional branch
    pub fn branch(
        &mut self,
        condition: ValueId,
        then_block: BlockId,
        else_block: BlockId,
        then_args: Vec<ValueId>,
        else_args: Vec<ValueId>,
    ) -> Terminator {
        Terminator::Branch {
            condition,
            then_block,
            else_block,
            then_args,
            else_args,
        }
    }

    /// Build the current function and return it
    pub fn build(self) -> Option<Function> {
        if let Some(mut func) = self.current_function {
            func.instructions = self.instructions;
            func.block_params = self.block_params;
            Some(func)
        } else {
            None
        }
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}
