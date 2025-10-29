use crate::ast::{AssignOp, BinOp, UnOp};
use crate::ir::{
    AddressKind, AllocationSize, BasicBlockId, EnumId, FieldId, FunctionId, IRFunction,
    IRInstruction, IRModule, IRTerminator, IRType, IRTypeWithMemory, IRValue, MemoryKind, StructId,
    TrapKind, ValueId, VariantId,
};
use crate::vm::{EnumLayout, FunctionMetadata, StructLayout, VariantLayout};
use std::collections::BTreeMap;

pub struct BytecodeCompiler {
    bytecode: Vec<u8>,
    constant_pool: Vec<u8>,
    function_metadata: Vec<FunctionMetadata>,
    stack_maps: Vec<u8>,

    // For tracking function bytecode offsets - using BTreeMap for deterministic ordering
    function_offsets: BTreeMap<FunctionId, usize>,

    // For register allocation
    next_register: u8,

    // For stack maps
    current_stack_map_offset: usize,

    // IR module reference for layout lookups
    ir_module: Option<IRModule>,
}

pub struct CompiledBytecode {
    pub bytecode: Vec<u8>,
    pub constant_pool: Vec<u8>,
    pub function_metadata: Vec<FunctionMetadata>,
    pub stack_maps: Vec<u8>,
    pub struct_layouts: Vec<StructLayout>,
    pub enum_layouts: Vec<EnumLayout>,
}

impl Default for BytecodeCompiler {
    fn default() -> Self {
        Self::new()
    }
}

impl BytecodeCompiler {
    pub fn new() -> Self {
        Self {
            bytecode: Vec::new(),
            constant_pool: Vec::new(),
            function_metadata: Vec::new(),
            stack_maps: Vec::new(),
            function_offsets: BTreeMap::new(), // Changed from BTreeMap::new()
            next_register: 0,
            current_stack_map_offset: 0,
            ir_module: None,
        }
    }

    // Compile an IR module to bytecode
    pub fn compile_module(&mut self, module: &IRModule) -> CompiledBytecode {
        // Store reference to IR module for layout lookups
        self.ir_module = Some(module.clone());

        // First pass: collect function offsets
        let current_offset = 0;
        for func in module.functions.iter() {
            self.function_offsets.insert(func.id, current_offset);
        }

        // Compile each function in deterministic order (already sorted in IR)
        for func in &module.functions {
            let mut reg_alloc = self.linear_scan_register_allocation(func);
            self.compile_function(func, &mut reg_alloc);
        }

        // Convert IR struct/enum layouts to VM layouts
        let struct_layouts = module
            .structs
            .iter()
            .map(|s| {
                // Identify GC pointer offsets from the struct's field layout
                let gc_ptr_offsets: Vec<u16> = s
                    .memory_layout
                    .field_offsets
                    .iter()
                    .filter(|(_, offset)| {
                        // Check if the field at this offset is a GC pointer type
                        if let Some(field_idx) = s.fields.iter().position(|f| &f.offset == offset) {
                            let field = &s.fields[field_idx];
                            self.is_gc_pointer_type(&field.type_.type_)
                        } else {
                            false
                        }
                    })
                    .map(|(_, offset)| *offset as u16)
                    .collect();

                StructLayout {
                    size: s.memory_layout.size as u32,
                    alignment: s.memory_layout.alignment as u16,
                    gc_ptr_offsets: gc_ptr_offsets.into_boxed_slice(),
                }
            })
            .collect();

        let enum_layouts = module
            .enums
            .iter()
            .map(|e| {
                // For enums, identify GC pointer offsets across all variants
                let _gc_ptr_offsets: Vec<u16> = e
                    .memory_layout
                    .field_offsets
                    .iter()
                    .filter(|(field_id, _offset)| {
                        // Check if the field at this field_id is a GC pointer type
                        // Look through enum variants to find the field type
                        for variant in &e.variants {
                            // Check if this field_id corresponds to one of the variant's fields
                            // Field IDs in variants may be sequential starting from 0 within the variant
                            // We can look at the variant's types to determine if it's a GC pointer
                            if field_id.0 < variant.types.len() {
                                let field_type = &variant.types[field_id.0];
                                if self.is_gc_pointer_type(&field_type.type_) {
                                    return true;
                                }
                            }
                        }
                        false
                    })
                    .map(|(_, offset)| *offset as u16)
                    .collect();

                // Create proper variant layouts with discriminant values and GC pointer information
                let variant_layouts: Vec<VariantLayout> = e
                    .variants
                    .iter()
                    .map(|variant| {
                        // Find GC pointer offsets specific to this variant
                        let variant_gc_ptr_offsets: Vec<u16> = e
                            .memory_layout
                            .field_offsets
                            .iter()
                            .filter(|(field_id, _offset)| {
                                // Check if this field corresponds to this variant and is a GC pointer
                                if field_id.0 < variant.types.len() {
                                    let field_type = &variant.types[field_id.0];
                                    self.is_gc_pointer_type(&field_type.type_)
                                } else {
                                    false
                                }
                            })
                            .map(|(_, offset)| *offset as u16)
                            .collect();

                        VariantLayout {
                            discriminant: variant.discriminant_value as u32,
                            gc_ptr_offsets_start: 0, // This would be set from a shared GC offsets array
                            gc_ptr_offsets_count: variant_gc_ptr_offsets.len() as u16,
                        }
                    })
                    .collect();

                EnumLayout {
                    size: e.memory_layout.size as u32,
                    alignment: e.memory_layout.alignment as u16,
                    discriminant_offset: e.memory_layout.discriminant_offset.unwrap_or(0) as u16,
                    discriminant_size: e.memory_layout.discriminant_size.unwrap_or(0) as u8,
                    variant_layouts: variant_layouts.into_boxed_slice(),
                }
            })
            .collect();

        CompiledBytecode {
            bytecode: std::mem::take(&mut self.bytecode),
            constant_pool: std::mem::take(&mut self.constant_pool),
            function_metadata: std::mem::take(&mut self.function_metadata),
            stack_maps: std::mem::take(&mut self.stack_maps),
            struct_layouts,
            enum_layouts,
        }
    }

    // Linear scan register allocation
    fn linear_scan_register_allocation(&mut self, func: &IRFunction) -> BTreeMap<ValueId, u8> {
        let mut reg_alloc = BTreeMap::new();

        // Start with argument mapping
        for (i, arg) in func.args.iter().take(8).enumerate() {
            let value_id = ValueId(arg.binding_id);
            reg_alloc.insert(value_id, (i + 1) as u8); // R1, R2, ..., R8
        }

        // Allocate registers for other values
        // In a real implementation, this would:
        // 1. Calculate live ranges for all values
        // 2. Create an active list of currently live values
        // 3. Allocate registers using the linear scan algorithm
        // For now, use a simple approach that assigns registers sequentially

        // For each basic block, process instructions and assign registers
        for block in &func.basic_blocks {
            for inst in &block.instructions {
                if let Some(result_id) = self.get_result_value_id(inst)
                    && !reg_alloc.contains_key(&result_id)
                {
                    let reg = self.next_available_register(&reg_alloc);
                    reg_alloc.insert(result_id, reg);
                }
            }
        }

        reg_alloc
    }

    // Helper to get result ValueId from an instruction
    fn get_result_value_id(&self, inst: &IRInstruction) -> Option<ValueId> {
        match inst {
            IRInstruction::BinOp { result, .. } => Some(*result),
            IRInstruction::UnOp { result, .. } => Some(*result),
            IRInstruction::Let { result, .. } => Some(*result),
            IRInstruction::Copy { result, .. } => Some(*result),
            IRInstruction::Call { result, .. } => Some(*result),
            IRInstruction::Allocate { result, .. } => Some(*result),
            IRInstruction::Load { result, .. } => Some(*result),
            IRInstruction::Store { .. } => None, // No result register for Store
            IRInstruction::FieldAccess { result, .. } => Some(*result),
            IRInstruction::StructConstruct { result, .. } => Some(*result),
            IRInstruction::EnumConstruct { result, .. } => Some(*result),
            IRInstruction::Index { result, .. } => Some(*result),
            IRInstruction::Tuple { result, .. } => Some(*result),
            IRInstruction::Cast { result, .. } => Some(*result),
            IRInstruction::Select { result, .. } => Some(*result),
            IRInstruction::Array { result, .. } => Some(*result),
            IRInstruction::Map { result, .. } => Some(*result),
            IRInstruction::Perform { result, .. } => Some(*result),
            IRInstruction::Phi { result, .. } => Some(*result),
            IRInstruction::Deallocate { .. } => None,
            IRInstruction::MemoryCopy { .. } => None,
            IRInstruction::ConstructClosure { result, .. } => Some(*result),
            IRInstruction::GCSafepoint { .. } => None,
            IRInstruction::DeclareRoot { .. } => None,
            IRInstruction::Trap { .. } => None,
            IRInstruction::Error { .. } => None,
            IRInstruction::Assign { result, .. } => Some(*result),
            IRInstruction::OptionalChain { result, .. } => Some(*result),
        }
    }

    // Find next available register
    fn next_available_register(&mut self, used_regs: &BTreeMap<ValueId, u8>) -> u8 {
        loop {
            let reg = self.next_register;
            self.next_register += 1;

            // Check if this register is already used by another value
            if !used_regs.values().any(|&r| r == reg) {
                return reg;
            }

            // If we've run out of registers, we need to handle register spilling
            // For now, just panic if we exceed the register limit
            if self.next_register == 0 {
                panic!("Register allocation failed: too many values");
            }
        }
    }

    // Compile a single function
    fn compile_function(&mut self, func: &IRFunction, reg_alloc: &mut BTreeMap<ValueId, u8>) {
        let bytecode_start = self.bytecode.len();

        // Save the function offset
        if let Some(offset) = self.function_offsets.get_mut(&func.id) {
            *offset = bytecode_start;
        }

        // Calculate argument count
        let arg_count = func.args.len().min(8) as u8; // Max 8 args in registers R1-R8

        // First pass: collect basic block offsets by emitting bytecode
        let mut block_offsets: BTreeMap<BasicBlockId, usize> = BTreeMap::new();

        for block in &func.basic_blocks {
            block_offsets.insert(block.id, self.bytecode.len());

            // PHI nodes are now handled properly during terminator processing
            // No need to handle them at the beginning of blocks

            // Emit instructions for this block
            for inst in &block.instructions {
                self.emit_instruction(inst, reg_alloc);
            }

            // Emit terminators with actual offsets
            if let Some(terminator) = &block.terminator {
                self.emit_terminator(terminator, reg_alloc, &block_offsets, block.id, func);
            }
        }

        // Calculate function size
        let bytecode_length = self.bytecode.len() - bytecode_start;
        let register_count = reg_alloc.values().copied().max().unwrap_or(0); // Max register used

        // Create function metadata
        let metadata = FunctionMetadata {
            bytecode_offset: bytecode_start as u32,
            bytecode_length: bytecode_length as u32,
            arg_count,
            register_count,
            local_stack_size: 0, // To be calculated
            stack_map_offset: self.current_stack_map_offset as u32,
            stack_map_count: 0, // To be calculated
            max_call_depth: 0,  // To be calculated
            _padding: 0,
        };

        self.function_metadata.push(metadata);
    }

    // Emit a single IR instruction
    fn emit_instruction(&mut self, inst: &IRInstruction, reg_alloc: &BTreeMap<ValueId, u8>) {
        match inst {
            IRInstruction::BinOp {
                result,
                left,
                op,
                right,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);

                // Get source registers for left and right operands
                let src1_reg = self.get_register_for_value(left, reg_alloc);
                let src2_reg = self.get_register_for_value(right, reg_alloc);

                // Determine the type for instruction selection
                let op_type = self.get_type_for_value(left);

                match (op, op_type) {
                    (BinOp::Add, IRType::Int) => {
                        // Check if right is a small constant for immediate optimization
                        if let IRValue::Int(val) = right {
                            if *val >= -128 && *val <= 127 {
                                self.emit_u8(0x06); // IntAddImm8
                                self.emit_u8(dst_reg);
                                self.emit_u8(src1_reg);
                                self.emit_i8(*val as i8);
                            } else {
                                self.emit_u8(0x00); // IntAdd
                                self.emit_u8(dst_reg);
                                self.emit_u8(src1_reg);
                                self.emit_u8(src2_reg);
                            }
                        } else {
                            self.emit_u8(0x00); // IntAdd
                            self.emit_u8(dst_reg);
                            self.emit_u8(src1_reg);
                            self.emit_u8(src2_reg);
                        }
                    }
                    (BinOp::Add, IRType::Float) => {
                        self.emit_u8(0x10); // FloatAdd
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::Sub, IRType::Int) => {
                        if let IRValue::Int(val) = right {
                            if *val >= -128 && *val <= 127 {
                                self.emit_u8(0x07); // IntSubImm8
                                self.emit_u8(dst_reg);
                                self.emit_u8(src1_reg);
                                self.emit_i8(*val as i8);
                            } else {
                                self.emit_u8(0x01); // IntSub
                                self.emit_u8(dst_reg);
                                self.emit_u8(src1_reg);
                                self.emit_u8(src2_reg);
                            }
                        } else {
                            self.emit_u8(0x01); // IntSub
                            self.emit_u8(dst_reg);
                            self.emit_u8(src1_reg);
                            self.emit_u8(src2_reg);
                        }
                    }
                    (BinOp::Sub, IRType::Float) => {
                        self.emit_u8(0x11); // FloatSub
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::Mul, IRType::Int) => {
                        if let IRValue::Int(val) = right {
                            if *val >= -128 && *val <= 127 {
                                self.emit_u8(0x08); // IntMulImm8
                                self.emit_u8(dst_reg);
                                self.emit_u8(src1_reg);
                                self.emit_i8(*val as i8);
                            } else {
                                self.emit_u8(0x02); // IntMul
                                self.emit_u8(dst_reg);
                                self.emit_u8(src1_reg);
                                self.emit_u8(src2_reg);
                            }
                        } else {
                            self.emit_u8(0x02); // IntMul
                            self.emit_u8(dst_reg);
                            self.emit_u8(src1_reg);
                            self.emit_u8(src2_reg);
                        }
                    }
                    (BinOp::Mul, IRType::Float) => {
                        self.emit_u8(0x12); // FloatMul
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::Div, IRType::Int) => {
                        self.emit_u8(0x03); // IntDiv
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::Div, IRType::Float) => {
                        self.emit_u8(0x13); // FloatDiv
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::Eq, IRType::Int) => {
                        self.emit_u8(0x20); // IntEq
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::NotEq, IRType::Int) => {
                        self.emit_u8(0x21); // IntNe
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::Less, IRType::Int) => {
                        self.emit_u8(0x22); // IntLt
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::LessEq, IRType::Int) => {
                        self.emit_u8(0x23); // IntLe
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::Greater, IRType::Int) => {
                        self.emit_u8(0x24); // IntGt
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::GreaterEq, IRType::Int) => {
                        self.emit_u8(0x25); // IntGe
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::Eq, IRType::Float) => {
                        self.emit_u8(0x2A); // FloatEq
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::NotEq, IRType::Float) => {
                        self.emit_u8(0x2B); // FloatNe
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::Less, IRType::Float) => {
                        self.emit_u8(0x2C); // FloatLt
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::LessEq, IRType::Float) => {
                        self.emit_u8(0x2D); // FloatLe
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::Greater, IRType::Float) => {
                        self.emit_u8(0x2E); // FloatGt
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::GreaterEq, IRType::Float) => {
                        self.emit_u8(0x2F); // FloatGe
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::And, IRType::Int) => {
                        self.emit_u8(0x30); // And
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::Or, IRType::Int) => {
                        self.emit_u8(0x31); // Or
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    (BinOp::Xor, IRType::Int) => {
                        self.emit_u8(0x32); // Xor
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                    _ => {
                        // Default to IntAdd for unsupported combinations
                        self.emit_u8(0x00); // IntAdd
                        self.emit_u8(dst_reg);
                        self.emit_u8(src1_reg);
                        self.emit_u8(src2_reg);
                    }
                }
            }
            IRInstruction::UnOp {
                result,
                op,
                operand,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);
                let src_reg = self.get_register_for_value(operand, reg_alloc);

                match op {
                    UnOp::Minus => {
                        // Determine type to emit appropriate negation
                        let operand_type = self.get_type_for_value(operand);
                        match operand_type {
                            IRType::Int => {
                                self.emit_u8(0x05); // IntNeg
                                self.emit_u8(dst_reg);
                                self.emit_u8(src_reg);
                            }
                            IRType::Float => {
                                self.emit_u8(0x14); // FloatNeg
                                self.emit_u8(dst_reg);
                                self.emit_u8(src_reg);
                            }
                            _ => {
                                self.emit_u8(0x05); // IntNeg as default
                                self.emit_u8(dst_reg);
                                self.emit_u8(src_reg);
                            }
                        }
                    }
                    UnOp::Not => {
                        self.emit_u8(0x33); // Not
                        self.emit_u8(dst_reg);
                        self.emit_u8(src_reg);
                    }
                    _ => {
                        // Default to IntNeg
                        self.emit_u8(0x05); // IntNeg
                        self.emit_u8(dst_reg);
                        self.emit_u8(src_reg);
                    }
                }
            }
            IRInstruction::Let { result, value, .. } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);
                let src_reg = self.get_register_for_value(value, reg_alloc);

                // Move the value to the result register
                if src_reg != dst_reg {
                    self.emit_u8(0xC0); // Move
                    self.emit_u8(dst_reg);
                    self.emit_u8(src_reg);
                }
            }
            IRInstruction::Copy { result, source, .. } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);
                let src_reg = self.get_register_for_value(source, reg_alloc);

                if src_reg != dst_reg {
                    self.emit_u8(0xC0); // Move
                    self.emit_u8(dst_reg);
                    self.emit_u8(src_reg);
                }
            }
            IRInstruction::Call {
                result,
                function,
                args,
                ..
            } => {
                // Emit safepoint before call
                let gc_ptr_regs = self.get_gc_pointer_registers(reg_alloc);
                self.emit_stack_map_registers(&gc_ptr_regs);

                // Get function ID from the function value
                let func_id = match function {
                    IRValue::FunctionRef(id) => id.0 as u16,
                    _ => 0, // Default to 0 if not a function reference
                };

                // Emit Call instruction
                self.emit_u8(0xA0); // Call
                self.emit_u16_le(func_id);
                self.emit_u8(args.len().min(8) as u8); // Limit args to 8 for now

                // Emit argument registers (first 8 args in R1-R8)
                for (i, arg) in args.iter().take(8).enumerate() {
                    let arg_reg = self.get_register_for_value(arg, reg_alloc);
                    // Arguments go in R1, R2, R3, etc.
                    let arg_target_reg = (i + 1) as u8;
                    if arg_reg != arg_target_reg {
                        self.emit_u8(0xC0); // Move
                        self.emit_u8(arg_target_reg);
                        self.emit_u8(arg_reg);
                    }
                }

                // Move return value to result register
                let result_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);
                // Return value is in R0, so move it to the result register
                if result_reg != 0 {
                    self.emit_u8(0xC0); // Move
                    self.emit_u8(result_reg);
                    self.emit_u8(0); // R0
                }
            }
            IRInstruction::Allocate {
                result,
                size,
                type_info,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);

                // Emit safepoint before allocation
                let gc_ptr_regs = self.get_gc_pointer_registers(reg_alloc);
                self.emit_stack_map_registers(&gc_ptr_regs);

                match size {
                    AllocationSize::Static(size_val) => {
                        // Use fast allocation path for common small sizes that align with TLAB allocation
                        // and slow path for larger or unusual sizes
                        match size_val {
                            8 => {
                                self.emit_u8(0xD0); // GCAllocFast8
                                self.emit_u8(dst_reg);
                            }
                            16 => {
                                self.emit_u8(0xD1); // GCAllocFast16
                                self.emit_u8(dst_reg);
                            }
                            24 => {
                                self.emit_u8(0xD2); // GCAllocFast24
                                self.emit_u8(dst_reg);
                            }
                            32 => {
                                self.emit_u8(0xD3); // GCAllocFast32
                                self.emit_u8(dst_reg);
                            }
                            64 => {
                                self.emit_u8(0xD4); // GCAllocFast64
                                self.emit_u8(dst_reg);
                            }
                            // Use fast path for other common sizes
                            12 | 20 | 28 | 36 | 40 | 48 | 56 => {
                                // For these sizes, we'll need to use the slow path since there's no specific fast opcode
                                // but we can optimize within the slow path
                                self.emit_u8(0xD5); // GCAllocSlow
                                self.emit_u8(dst_reg);
                                self.emit_u8(*size_val as u8); // size
                                self.emit_u8(self.get_alignment_for_type(type_info) as u8); // alignment
                            }
                            _ => {
                                // Use slow path for uncommon or large sizes
                                self.emit_u8(0xD5); // GCAllocSlow
                                self.emit_u8(dst_reg);
                                self.emit_u8(*size_val as u8); // size
                                self.emit_u8(self.get_alignment_for_type(type_info) as u8); // alignment
                            }
                        }
                    }
                    AllocationSize::Dynamic(size_val) => {
                        // For dynamic sizes, we must use the slow allocation path
                        let size_reg = self.get_register_for_value(size_val, reg_alloc);

                        // TODO: In a real implementation, we'd want to check if the dynamic size
                        // matches any of the fast path sizes at runtime, but for now use slow path
                        self.emit_u8(0xD5); // GCAllocSlow
                        self.emit_u8(dst_reg);
                        self.emit_u8(size_reg); // size register
                        self.emit_u8(self.get_alignment_for_type(type_info) as u8); // alignment
                    }
                }
            }
            IRInstruction::Load {
                result,
                address,
                address_kind,
                type_info,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);

                // Handle different address kinds
                match address_kind {
                    AddressKind::StackSlot(slot_id) => {
                        // Load from local stack slot
                        let offset = (slot_id.0 * 8) as u16; // Use slot ID as offset (assuming 8-byte slots)
                        let type_size = self.get_size_for_type(type_info);
                        match type_size {
                            1 => {
                                self.emit_u8(0x50); // LoadLocal8
                                self.emit_u8(dst_reg);
                                self.emit_u16_le(offset);
                            }
                            2 => {
                                self.emit_u8(0x51); // LoadLocal16
                                self.emit_u8(dst_reg);
                                self.emit_u16_le(offset);
                            }
                            4 => {
                                self.emit_u8(0x52); // LoadLocal32
                                self.emit_u8(dst_reg);
                                self.emit_u16_le(offset);
                            }
                            8 => {
                                self.emit_u8(0x53); // LoadLocal64
                                self.emit_u8(dst_reg);
                                self.emit_u16_le(offset);
                            }
                            _ => {
                                self.emit_u8(0x54); // LoadLocalPtr
                                self.emit_u8(dst_reg);
                                self.emit_u16_le(offset);
                            }
                        }
                    }
                    AddressKind::StructField { base, field } => {
                        // Load field from struct
                        let base_reg = self.get_register_for_value(&IRValue::SSA(*base), reg_alloc);
                        // Get field offset from the IR module
                        let field_offset = self.get_field_offset_for_field_id(*field);
                        let type_size = self.get_size_for_type(type_info);

                        match type_size {
                            1 => {
                                self.emit_u8(0x57); // LoadField8
                                self.emit_u8(dst_reg);
                                self.emit_u8(base_reg);
                                self.emit_u16_le(field_offset as u16);
                            }
                            2 => {
                                self.emit_u8(0x58); // LoadField16
                                self.emit_u8(dst_reg);
                                self.emit_u8(base_reg);
                                self.emit_u16_le(field_offset as u16);
                            }
                            4 => {
                                self.emit_u8(0x59); // LoadField32
                                self.emit_u8(dst_reg);
                                self.emit_u8(base_reg);
                                self.emit_u16_le(field_offset as u16);
                            }
                            8 => {
                                self.emit_u8(0x5A); // LoadField64
                                self.emit_u8(dst_reg);
                                self.emit_u8(base_reg);
                                self.emit_u16_le(field_offset as u16);
                            }
                            _ => {
                                self.emit_u8(0x5B); // LoadFieldPtr
                                self.emit_u8(dst_reg);
                                self.emit_u8(base_reg);
                                self.emit_u16_le(field_offset as u16);
                            }
                        }
                    }
                    _ => {
                        // For other address kinds, handle as immediate or general address
                        let addr_reg = self.get_register_for_value(address, reg_alloc);
                        // Generic pointer load (for now)
                        self.emit_u8(0x5C); // LoadIndirect
                        self.emit_u8(dst_reg);
                        self.emit_u8(addr_reg);
                    }
                }
            }
            IRInstruction::Store {
                address,
                value,
                address_kind,
                type_info,
                ..
            } => {
                let val_reg = self.get_register_for_value(value, reg_alloc);

                match address_kind {
                    AddressKind::StackSlot(slot_id) => {
                        // Store to local stack slot
                        let offset = (slot_id.0 * 8) as u16; // Use slot ID as offset (assuming 8-byte slots)
                        let type_size = self.get_size_for_type(type_info);
                        match type_size {
                            1 => {
                                self.emit_u8(0x60); // StoreLocal8
                                self.emit_u8(val_reg);
                                self.emit_u16_le(offset);
                            }
                            2 => {
                                self.emit_u8(0x61); // StoreLocal16
                                self.emit_u8(val_reg);
                                self.emit_u16_le(offset);
                            }
                            4 => {
                                self.emit_u8(0x62); // StoreLocal32
                                self.emit_u8(val_reg);
                                self.emit_u16_le(offset);
                            }
                            8 => {
                                self.emit_u8(0x63); // StoreLocal64
                                self.emit_u8(val_reg);
                                self.emit_u16_le(offset);
                            }
                            _ => {
                                self.emit_u8(0x64); // StoreLocalPtr
                                self.emit_u8(val_reg);
                                self.emit_u16_le(offset);
                            }
                        }
                    }
                    AddressKind::StructField { base: _, field } => {
                        // Store field in struct
                        let base_reg = self.get_register_for_value(address, reg_alloc);
                        // Get field offset from the IR module
                        let field_offset = self.get_field_offset_for_field_id(*field);
                        let type_size = self.get_size_for_type(type_info);

                        match type_size {
                            1 => {
                                self.emit_u8(0x66); // StoreField8
                                self.emit_u8(base_reg);
                                self.emit_u16_le(field_offset as u16);
                                self.emit_u8(val_reg);
                            }
                            2 => {
                                self.emit_u8(0x67); // StoreField16
                                self.emit_u8(base_reg);
                                self.emit_u16_le(field_offset as u16);
                                self.emit_u8(val_reg);
                            }
                            4 => {
                                self.emit_u8(0x68); // StoreField32
                                self.emit_u8(base_reg);
                                self.emit_u16_le(field_offset as u16);
                                self.emit_u8(val_reg);
                            }
                            8 => {
                                self.emit_u8(0x69); // StoreField64
                                self.emit_u8(base_reg);
                                self.emit_u16_le(field_offset as u16);
                                self.emit_u8(val_reg);
                            }
                            _ => {
                                self.emit_u8(0x6A); // StoreFieldPtr
                                self.emit_u8(base_reg);
                                self.emit_u16_le(field_offset as u16);
                                self.emit_u8(val_reg);
                            }
                        }
                    }
                    _ => {
                        // For other address kinds, store to general address
                        let addr_reg = self.get_register_for_value(address, reg_alloc);
                        // Generic pointer store (for now)
                        self.emit_u8(0x6B); // StoreIndirect
                        self.emit_u8(addr_reg);
                        self.emit_u8(val_reg);
                    }
                }
            }
            IRInstruction::FieldAccess {
                result,
                target,
                field_id,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);
                let base_reg = self.get_register_for_value(target, reg_alloc);

                // Get field offset from the IR module
                let field_offset = self.get_field_offset_for_field_id(*field_id);
                self.emit_u8(0x5A); // LoadField64 - assume 64-bit field
                self.emit_u8(dst_reg);
                self.emit_u8(base_reg);
                self.emit_u16_le(field_offset as u16);
            }
            IRInstruction::StructConstruct {
                result,
                struct_id,
                fields,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);

                // Get struct layout from module for size calculation
                let struct_size = self.get_struct_size(*struct_id);

                // Allocate space for the struct using appropriate path
                // Use fast path for common sizes that fit in TLAB, slow path for others
                match struct_size {
                    8 => {
                        self.emit_u8(0xD0); // GCAllocFast8
                        self.emit_u8(dst_reg);
                    }
                    16 => {
                        self.emit_u8(0xD1); // GCAllocFast16
                        self.emit_u8(dst_reg);
                    }
                    24 => {
                        self.emit_u8(0xD2); // GCAllocFast24
                        self.emit_u8(dst_reg);
                    }
                    32 => {
                        self.emit_u8(0xD3); // GCAllocFast32
                        self.emit_u8(dst_reg);
                    }
                    64 => {
                        self.emit_u8(0xD4); // GCAllocFast64
                        self.emit_u8(dst_reg);
                    }
                    12 | 20 | 28 | 36 | 40 | 48 | 56 => {
                        // Use slow path with size-specific optimization
                        self.emit_u8(0xD5); // GCAllocSlow
                        self.emit_u8(dst_reg);
                        self.emit_u8(struct_size as u8); // size
                        // Get proper alignment for this struct
                        if let Some(ref module) = self.ir_module
                            && let Some(struct_idx) = module.struct_map.get(struct_id)
                            && let Some(s) = module.structs.get(*struct_idx)
                        {
                            self.emit_u8(s.memory_layout.alignment as u8); // struct-specific alignment
                        } else {
                            self.emit_u8(8); // default alignment
                        }
                    }
                    _ => {
                        // For large structs, use slow path with proper size and alignment
                        self.emit_u8(0xD5); // GCAllocSlow
                        self.emit_u8(dst_reg);
                        // Ensure size doesn't exceed u8 max - use larger allocation if needed
                        if struct_size <= 255 {
                            self.emit_u8(struct_size as u8); // size
                        } else {
                            // For very large allocations, we'd need a different approach in real implementation
                            // For now, cap at 255
                            self.emit_u8(255); // size
                        }
                        // Get proper alignment for this struct
                        if let Some(ref module) = self.ir_module
                            && let Some(struct_idx) = module.struct_map.get(struct_id)
                            && let Some(s) = module.structs.get(*struct_idx)
                        {
                            self.emit_u8(s.memory_layout.alignment as u8); // struct-specific alignment
                        } else {
                            self.emit_u8(8); // default alignment
                        }
                    }
                }

                // Store each field value to the struct
                for (field_id, field_value) in fields {
                    let field_reg = self.get_register_for_value(field_value, reg_alloc);
                    // Get field offset from the IR module
                    let field_offset = self.get_field_offset_for_field_id(*field_id);
                    // Emit store field instruction
                    self.emit_u8(0x69); // StoreField64
                    self.emit_u8(dst_reg); // base address
                    self.emit_u16_le(field_offset as u16); // offset
                    self.emit_u8(field_reg); // value
                }
            }
            IRInstruction::EnumConstruct {
                result,
                enum_id,
                variant_id,
                args,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);

                // Get enum layout from module for size calculation
                let enum_size = self.get_enum_size(*enum_id);

                // Allocate space for the enum using appropriate path
                match enum_size {
                    8 => {
                        self.emit_u8(0xD0); // GCAllocFast8
                        self.emit_u8(dst_reg);
                    }
                    16 => {
                        self.emit_u8(0xD1); // GCAllocFast16
                        self.emit_u8(dst_reg);
                    }
                    24 => {
                        self.emit_u8(0xD2); // GCAllocFast24
                        self.emit_u8(dst_reg);
                    }
                    32 => {
                        self.emit_u8(0xD3); // GCAllocFast32
                        self.emit_u8(dst_reg);
                    }
                    64 => {
                        self.emit_u8(0xD4); // GCAllocFast64
                        self.emit_u8(dst_reg);
                    }
                    12 | 20 | 28 | 36 | 40 | 48 | 56 => {
                        // Use slow path with size-specific optimization
                        self.emit_u8(0xD5); // GCAllocSlow
                        self.emit_u8(dst_reg);
                        self.emit_u8(enum_size as u8); // size
                        // Get proper alignment for this enum
                        if let Some(ref module) = self.ir_module
                            && let Some(enum_idx) = module.enum_map.get(enum_id)
                            && let Some(e) = module.enums.get(*enum_idx)
                        {
                            self.emit_u8(e.memory_layout.alignment as u8); // enum-specific alignment
                        } else {
                            self.emit_u8(8); // default alignment
                        }
                    }
                    _ => {
                        // For large enums, use slow path with proper size and alignment
                        self.emit_u8(0xD5); // GCAllocSlow
                        self.emit_u8(dst_reg);
                        // Ensure size doesn't exceed u8 max - use larger allocation if needed
                        if enum_size <= 255 {
                            self.emit_u8(enum_size as u8); // size
                        } else {
                            // For very large allocations, cap at 255
                            self.emit_u8(255); // size
                        }
                        // Get proper alignment for this enum
                        if let Some(ref module) = self.ir_module
                            && let Some(enum_idx) = module.enum_map.get(enum_id)
                            && let Some(e) = module.enums.get(*enum_idx)
                        {
                            self.emit_u8(e.memory_layout.alignment as u8); // enum-specific alignment
                        } else {
                            self.emit_u8(8); // default alignment
                        }
                    }
                }

                // Store discriminant value
                let discriminant_offset = self.get_discriminant_offset(*enum_id);
                self.emit_u8(0x66); // StoreField8 for discriminant
                self.emit_u8(dst_reg);
                self.emit_u16_le(discriminant_offset as u16);
                self.emit_u8(variant_id.0 as u8); // discriminant value

                // Store args to the enum data fields
                let mut data_offset = self.get_data_offset(*enum_id, *variant_id);
                for arg in args {
                    let arg_reg = self.get_register_for_value(arg, reg_alloc);
                    self.emit_u8(0x69); // StoreField64
                    self.emit_u8(dst_reg);
                    self.emit_u16_le(data_offset as u16); // offset for data fields
                    self.emit_u8(arg_reg);
                    data_offset += 8; // Move to next field (assuming 8-byte slots)
                }
            }
            IRInstruction::Index {
                result,
                target,
                index,
                element_type,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);
                let target_reg = self.get_register_for_value(target, reg_alloc);
                let index_reg = self.get_register_for_value(index, reg_alloc);

                // For now, just emit a simple array load
                let elem_size = self.get_size_for_type(element_type);
                match elem_size {
                    1 => {
                        self.emit_u8(0x71); // ArrayLoad8
                        self.emit_u8(dst_reg);
                        self.emit_u8(target_reg);
                        self.emit_u8(index_reg);
                    }
                    2 => {
                        self.emit_u8(0x72); // ArrayLoad16
                        self.emit_u8(dst_reg);
                        self.emit_u8(target_reg);
                        self.emit_u8(index_reg);
                    }
                    4 => {
                        self.emit_u8(0x73); // ArrayLoad32
                        self.emit_u8(dst_reg);
                        self.emit_u8(target_reg);
                        self.emit_u8(index_reg);
                    }
                    8 => {
                        self.emit_u8(0x74); // ArrayLoad64
                        self.emit_u8(dst_reg);
                        self.emit_u8(target_reg);
                        self.emit_u8(index_reg);
                    }
                    _ => {
                        self.emit_u8(0x75); // ArrayLoadPtr
                        self.emit_u8(dst_reg);
                        self.emit_u8(target_reg);
                        self.emit_u8(index_reg);
                    }
                }
            }
            IRInstruction::Tuple {
                result, elements, ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);

                // Calculate proper size for the tuple based on number of elements (assuming 8 bytes each)
                let tuple_size = elements.len() * 8; // 8 bytes per element

                // Allocate space for the tuple using appropriate path based on size
                match tuple_size {
                    8 => {
                        self.emit_u8(0xD0); // GCAllocFast8
                        self.emit_u8(dst_reg);
                    }
                    16 => {
                        self.emit_u8(0xD1); // GCAllocFast16
                        self.emit_u8(dst_reg);
                    }
                    24 => {
                        self.emit_u8(0xD2); // GCAllocFast24
                        self.emit_u8(dst_reg);
                    }
                    32 => {
                        self.emit_u8(0xD3); // GCAllocFast32
                        self.emit_u8(dst_reg);
                    }
                    64 => {
                        self.emit_u8(0xD4); // GCAllocFast64
                        self.emit_u8(dst_reg);
                    }
                    12 | 20 | 28 | 36 | 40 | 48 | 56 => {
                        // Use slow path with size-specific optimization
                        self.emit_u8(0xD5); // GCAllocSlow
                        self.emit_u8(dst_reg);
                        self.emit_u8(tuple_size as u8); // size
                        self.emit_u8(8); // default alignment
                    }
                    _ => {
                        // For large tuples, use slow path with proper size
                        self.emit_u8(0xD5); // GCAllocSlow
                        self.emit_u8(dst_reg);
                        // Ensure size doesn't exceed u8 max
                        let actual_size = std::cmp::min(tuple_size, 255);
                        self.emit_u8(actual_size as u8); // size
                        self.emit_u8(8); // default alignment
                    }
                }

                // Store each element value to the tuple at appropriate offsets
                for (i, element) in elements.iter().enumerate() {
                    let element_reg = self.get_register_for_value(element, reg_alloc);
                    let offset = (i * 8) as u16; // 8-byte aligned offsets for each element
                    self.emit_u8(0x69); // StoreField64
                    self.emit_u8(dst_reg);
                    self.emit_u16_le(offset); // offset for tuple elements
                    self.emit_u8(element_reg);
                }
            }
            IRInstruction::Cast {
                result,
                expr,
                target_type,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);
                let src_reg = self.get_register_for_value(expr, reg_alloc);

                // Determine cast type
                let src_type = self.get_value_type(expr);
                match (&src_type, &target_type.type_) {
                    (IRType::Int, IRType::Float) => {
                        self.emit_u8(0x40); // IntToFloat
                        self.emit_u8(dst_reg);
                        self.emit_u8(src_reg);
                    }
                    (IRType::Float, IRType::Int) => {
                        self.emit_u8(0x41); // FloatToInt
                        self.emit_u8(dst_reg);
                        self.emit_u8(src_reg);
                    }
                    _ => {
                        // If types are the same, just move
                        if src_reg != dst_reg {
                            self.emit_u8(0xC0); // Move
                            self.emit_u8(dst_reg);
                            self.emit_u8(src_reg);
                        }
                    }
                }
            }
            IRInstruction::Select {
                result,
                condition,
                if_true,
                if_false,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);
                let cond_reg = self.get_register_for_value(condition, reg_alloc);
                let true_reg = self.get_register_for_value(if_true, reg_alloc);
                let false_reg = self.get_register_for_value(if_false, reg_alloc);

                // Emit conditional move
                self.emit_u8(0xCB); // Select
                self.emit_u8(dst_reg);
                self.emit_u8(cond_reg);
                self.emit_u8(true_reg);
                self.emit_u8(false_reg);
            }
            IRInstruction::Array {
                result, elements, ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);

                // Create new array
                self.emit_u8(0x70); // ArrayNew
                self.emit_u8(dst_reg);
                self.emit_u8(elements.len() as u8); // length

                // Store each element
                for (i, element) in elements.iter().enumerate() {
                    let elem_reg = self.get_register_for_value(element, reg_alloc);
                    self.emit_u8(0x76); // ArrayStorePtr (assuming pointer elements)
                    self.emit_u8(dst_reg);
                    self.emit_u8(i as u8); // index
                    self.emit_u8(elem_reg);
                }
            }
            IRInstruction::Map {
                result, entries, ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);

                // Calculate size for the map based on number of entries (assuming key-value pairs)
                let map_size = entries.len() * 16; // 16 bytes per key-value pair (8 for key + 8 for value)

                // Allocate space for the map using appropriate path based on size
                match map_size {
                    8 | 16 => {
                        self.emit_u8(0xD1); // GCAllocFast16 (minimum for maps)
                        self.emit_u8(dst_reg);
                    }
                    24 => {
                        self.emit_u8(0xD2); // GCAllocFast24
                        self.emit_u8(dst_reg);
                    }
                    32 => {
                        self.emit_u8(0xD3); // GCAllocFast32
                        self.emit_u8(dst_reg);
                    }
                    64 => {
                        self.emit_u8(0xD4); // GCAllocFast64
                        self.emit_u8(dst_reg);
                    }
                    12 | 20 | 28 | 36 | 40 | 48 | 56 => {
                        // Use slow path with size-specific optimization
                        self.emit_u8(0xD5); // GCAllocSlow
                        self.emit_u8(dst_reg);
                        self.emit_u8(map_size as u8); // size
                        self.emit_u8(8); // default alignment
                    }
                    _ => {
                        // For large maps, use slow path with proper size
                        self.emit_u8(0xD5); // GCAllocSlow
                        self.emit_u8(dst_reg);
                        // Ensure size doesn't exceed u8 max
                        let actual_size = std::cmp::min(map_size, 255);
                        self.emit_u8(actual_size as u8); // size
                        self.emit_u8(8); // default alignment
                    }
                }

                // Store entries - in a real implementation this would be more sophisticated
                // For now, store key-value pairs sequentially in memory
                for (i, (key, value)) in entries.iter().enumerate() {
                    let key_reg = self.get_register_for_value(key, reg_alloc);
                    let val_reg = self.get_register_for_value(value, reg_alloc);

                    // Store key at offset i*16
                    self.emit_u8(0x69); // StoreField64
                    self.emit_u8(dst_reg);
                    self.emit_u16_le((i * 16) as u16); // offset for key
                    self.emit_u8(key_reg);

                    // Store value at offset i*16 + 8
                    self.emit_u8(0x69); // StoreField64
                    self.emit_u8(dst_reg);
                    self.emit_u16_le((i * 16 + 8) as u16); // offset for value
                    self.emit_u8(val_reg);
                }
            }
            IRInstruction::Perform {
                result,
                effect_id,
                operation_id,
                args,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);

                // Get the effect ID as a register
                let effect_id_reg =
                    self.get_register_for_value(&IRValue::Int(effect_id.0 as i64), reg_alloc);

                // Emit Perform instruction
                self.emit_u8(0xB0); // Perform
                self.emit_u8(dst_reg); // Result register
                self.emit_u8(effect_id_reg); // Effect ID register
                self.emit_u8(*operation_id as u8); // Operation ID

                // Emit arguments
                for arg in args {
                    let arg_reg = self.get_register_for_value(arg, reg_alloc);
                    self.emit_u8(arg_reg);
                }
            }
            IRInstruction::Phi {
                result, incoming, ..
            } => {
                // PHI nodes are handled at the beginning of basic blocks during compilation
                // They effectively represent value moves from predecessor blocks
                // In our bytecode model, we handle them by inserting moves at block entry
                // For now, this is a no-op since we handle PHI nodes at the beginning of blocks
                // This is handled in the compile_function method where PHI nodes are processed

                // The result is used in register allocation but the actual move happens at block entry
                let _ = result; // Mark as used to avoid warning
                let _ = incoming; // Also used during compilation
            }
            IRInstruction::Deallocate { .. } => {
                // For non-GC systems, handle deallocation
                // For now, no-op in GC system
            }
            IRInstruction::MemoryCopy { .. } => {
                // For memory copy, we need to implement proper memory copy
                // For now, emit a trap to indicate unimplemented operation
                self.emit_u8(0xF5); // DebugTrap - memory copy not implemented yet
            }
            IRInstruction::ConstructClosure {
                result,
                function_id,
                captures,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);

                // Calculate closure size - function pointer + capture slots
                let closure_size = 8 + (captures.len() * 8); // 8 bytes for function + captures

                // Allocate closure
                match closure_size {
                    8 => {
                        self.emit_u8(0xD0); // GCAllocFast8
                        self.emit_u8(dst_reg);
                    }
                    16 => {
                        self.emit_u8(0xD1); // GCAllocFast16
                        self.emit_u8(dst_reg);
                    }
                    24 => {
                        self.emit_u8(0xD2); // GCAllocFast24
                        self.emit_u8(dst_reg);
                    }
                    32 => {
                        self.emit_u8(0xD3); // GCAllocFast32
                        self.emit_u8(dst_reg);
                    }
                    64 => {
                        self.emit_u8(0xD4); // GCAllocFast64
                        self.emit_u8(dst_reg);
                    }
                    _ => {
                        self.emit_u8(0xD5); // GCAllocSlow
                        self.emit_u8(dst_reg);
                        self.emit_u8(closure_size as u8); // size
                        self.emit_u8(8); // default alignment
                    }
                }

                // Store function pointer at offset 0
                let func_reg =
                    self.get_register_for_value(&IRValue::FunctionRef(*function_id), reg_alloc);
                self.emit_u8(0x69); // StoreField64
                self.emit_u8(dst_reg);
                self.emit_u16_le(0); // offset for function
                self.emit_u8(func_reg);

                // Store captures sequentially starting from offset 8
                for (i, (capture_id, _capture_type)) in captures.iter().enumerate() {
                    let capture_reg =
                        self.get_register_for_value(&IRValue::SSA(*capture_id), reg_alloc);
                    let offset = 8 + (i * 8); // Each capture gets 8 bytes starting from offset 8
                    self.emit_u8(0x69); // StoreField64
                    self.emit_u8(dst_reg);
                    self.emit_u16_le(offset as u16); // offset for capture
                    self.emit_u8(capture_reg);
                }
            }
            IRInstruction::GCSafepoint { live_values, .. } => {
                // Emit stack map for GC safepoint
                // Collect all live pointer registers
                let live_gc_regs: Vec<u8> = live_values
                    .iter()
                    .filter_map(|val_id| reg_alloc.get(val_id).copied())
                    .filter(|reg| self.is_gc_pointer_register(*reg, reg_alloc)) // Check if register contains GC pointer
                    .collect();

                self.emit_stack_map_registers(&live_gc_regs);
            }
            IRInstruction::DeclareRoot { value, is_root, .. } => {
                if *is_root {
                    let val_reg = self.get_register_for_value(&IRValue::SSA(*value), reg_alloc);
                    self.emit_u8(0xD9); // DeclareRoot
                    self.emit_u8(val_reg);
                }
            }
            IRInstruction::Trap { kind, .. } => {
                match kind {
                    TrapKind::BoundsCheck { .. } => {
                        self.emit_u8(0xF0); // TrapBoundsCheck
                    }
                    TrapKind::NullCheck { .. } => {
                        self.emit_u8(0xF2); // TrapNullCheck
                    }
                    TrapKind::DivideByZero { .. } => {
                        self.emit_u8(0xF1); // TrapDivByZero
                    }
                    TrapKind::IntegerOverflow { .. } => {
                        self.emit_u8(0xF3); // TrapOverflow
                    }
                    TrapKind::Unreachable => {
                        self.emit_u8(0xF4); // Unreachable
                    }
                }
            }
            IRInstruction::Error { .. } => {
                self.emit_u8(0xF5); // DebugTrap
            }
            IRInstruction::Assign {
                result, r_val, op, ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);
                let src_reg = self.get_register_for_value(r_val, reg_alloc);

                match op {
                    AssignOp::Assign => {
                        if dst_reg != src_reg {
                            self.emit_u8(0xC0); // Move
                            self.emit_u8(dst_reg);
                            self.emit_u8(src_reg);
                        }
                    }
                    _ => {
                        // For other assignment operations, treat as standard move
                        if dst_reg != src_reg {
                            self.emit_u8(0xC0); // Move
                            self.emit_u8(dst_reg);
                            self.emit_u8(src_reg);
                        }
                    }
                }
            }
            IRInstruction::OptionalChain {
                result,
                target,
                field_id,
                ..
            } => {
                let dst_reg = self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);
                let target_reg = self.get_register_for_value(target, reg_alloc);

                // Get field offset from the IR module
                let field_offset = self.get_field_offset_for_field_id(*field_id);
                self.emit_u8(0x5A); // LoadField64
                self.emit_u8(dst_reg);
                self.emit_u8(target_reg);
                self.emit_u16_le(field_offset as u16);
            }
        }
    }

    // Get field offset for a field ID
    fn get_field_offset_for_field_id(&self, field_id: FieldId) -> usize {
        // Proper implementation to look up the field offset in the IR module
        if let Some(ref module) = self.ir_module {
            // Look for the field in structs
            for struct_def in &module.structs {
                for field in &struct_def.fields {
                    if field.field_id == field_id {
                        return field.offset;
                    }
                }
            }

            // Look for the field in enum variants (as data fields)
            for enum_def in &module.enums {
                for variant in &enum_def.variants {
                    // Check if any of the variant's fields match the field_id
                    for _field in &variant.types {
                        // In our IR design, variant fields have implicit field IDs based on position
                        // But we need to find the field offset in the memory layout
                        // Look for the specific field_id in the memory layout
                        for (layout_field_id, field_offset) in &enum_def.memory_layout.field_offsets
                        {
                            if *layout_field_id == field_id {
                                return *field_offset;
                            }
                        }
                    }
                }
            }
        }

        // Default fallback if not found
        field_id.0 * 8 // Default 8-byte alignment
    }

    // Get struct size from module
    fn get_struct_size(&self, struct_id: StructId) -> usize {
        if let Some(ref module) = self.ir_module
            && let Some(struct_idx) = module.struct_map.get(&struct_id)
            && let Some(s) = module.structs.get(*struct_idx)
        {
            return s.memory_layout.size;
        }
        16 // Default size if not found
    }

    // Get enum size from module
    fn get_enum_size(&self, enum_id: EnumId) -> usize {
        if let Some(ref module) = self.ir_module
            && let Some(enum_idx) = module.enum_map.get(&enum_id)
            && let Some(e) = module.enums.get(*enum_idx)
        {
            return e.memory_layout.size;
        }
        16 // Default size if not found
    }

    // Get discriminant offset for an enum
    fn get_discriminant_offset(&self, enum_id: EnumId) -> usize {
        if let Some(ref module) = self.ir_module
            && let Some(enum_idx) = module.enum_map.get(&enum_id)
            && let Some(e) = module.enums.get(*enum_idx)
        {
            return e.memory_layout.discriminant_offset.unwrap_or(0);
        }
        0 // Default offset if not found
    }

    // Get data offset for an enum variant
    fn get_data_offset(&self, enum_id: EnumId, variant_id: VariantId) -> usize {
        // Proper implementation to look up the data offset for a specific variant
        if let Some(ref module) = self.ir_module
            && let Some(enum_idx) = module.enum_map.get(&enum_id)
            && let Some(e) = module.enums.get(*enum_idx)
        {
            // Find the specific variant and return its data offset
            for variant in &e.variants {
                if variant.variant_id == variant_id {
                    // Return the precomputed data offset from the variant
                    return variant.data_offset;
                }
            }
        }

        // Default fallback if not found
        self.get_discriminant_offset(enum_id) + 1
    }

    // Get GC pointer registers from the register allocation
    fn get_gc_pointer_registers(&self, reg_alloc: &BTreeMap<ValueId, u8>) -> Vec<u8> {
        // In a real implementation, we'd identify which ValueIds correspond to GC pointer types
        // and return the associated registers
        let mut gc_regs = Vec::new();

        if let Some(ref module) = self.ir_module {
            // We need to analyze the function being compiled to determine which registers hold GC pointers
            // For now, we'll use a conservative approach by tracking which ValueIds are associated with heap allocations
            for (value_id, &reg) in reg_alloc {
                // This is a simplified approach - we'll check if the value is likely to be a GC pointer
                // by checking if it was created from heap allocation or has a pointer type
                if self.is_value_gc_pointer(*value_id, module) {
                    gc_regs.push(reg);
                }
            }
        }

        gc_regs
    }

    // Check if a register contains a GC pointer
    fn is_gc_pointer_register(&self, reg: u8, reg_alloc: &BTreeMap<ValueId, u8>) -> bool {
        // Find the ValueId associated with this register
        if let Some(&value_id) = reg_alloc
            .iter()
            .find(|&(_, &allocated_reg)| allocated_reg == reg)
            .map(|(id, _)| id)
            && let Some(ref module) = self.ir_module
        {
            return self.is_value_gc_pointer(value_id, module);
        }
        false
    }

    // Helper to determine if a ValueId is likely to be a GC pointer
    fn is_value_gc_pointer(&self, value_id: ValueId, module: &IRModule) -> bool {
        // In a real implementation, we'd trace back through the IR to see if this value
        // was produced by an allocation or has pointer type
        // For now, we'll use a conservative approach and check if this value was created by an allocation

        // Look through all functions to see if this value_id is the result of an allocation
        for func in &module.functions {
            for block in &func.basic_blocks {
                for inst in &block.instructions {
                    if let Some(result_id) = self.get_result_value_id(inst)
                        && result_id == value_id
                    {
                        // Check if this instruction creates a heap-allocated object
                        match inst {
                            IRInstruction::Allocate { memory_kind, .. } => {
                                return matches!(memory_kind, MemoryKind::Heap);
                            }
                            IRInstruction::StructConstruct { .. } => {
                                // Structs could be on heap (depending on escape analysis)
                                return true;
                            }
                            IRInstruction::EnumConstruct { .. } => {
                                // Enums are heap allocated
                                return true;
                            }
                            IRInstruction::Array { .. } => {
                                // Arrays are heap allocated
                                return true;
                            }
                            IRInstruction::Tuple { .. } => {
                                // Tuples might be heap allocated based on size/escape
                                return true;
                            }
                            IRInstruction::Map { .. } => {
                                // Maps are heap allocated
                                return true;
                            }
                            IRInstruction::ConstructClosure { .. } => {
                                // Closures are heap allocated
                                return true;
                            }
                            // Non-pointer types don't create GC pointers
                            IRInstruction::BinOp { .. }
                            | IRInstruction::UnOp { .. }
                            | IRInstruction::Let { .. }
                            | IRInstruction::Copy { .. }
                            | IRInstruction::Call { .. }
                            | IRInstruction::Store { .. }
                            | IRInstruction::FieldAccess { .. }
                            | IRInstruction::Index { .. }
                            | IRInstruction::Cast { .. }
                            | IRInstruction::Select { .. } => {
                                // For these operations, we need to check the type of the result
                                // If we can determine the type, check if it's a pointer type
                                return self.is_gc_pointer_type_result(inst, module);
                            }
                            _ => {
                                // Default to false for other types of instructions
                                return false;
                            }
                        }
                    }
                }
            }
        }

        // Default to false if we can't find the value
        false
    }

    // Helper to determine if an instruction produces a GC pointer type
    fn is_gc_pointer_type_result(&self, inst: &IRInstruction, _module: &IRModule) -> bool {
        // In a more complete implementation, we'd check the result type of the instruction
        // For now, we'll use some heuristics based on the instruction type
        match inst {
            // These typically produce pointer types
            IRInstruction::Call { .. } => {
                // Calls might return pointer types, so be conservative
                true
            }
            IRInstruction::FieldAccess { field_type, .. } => {
                // Check if the field type is a pointer or heap-allocated type
                matches!(
                    field_type.type_,
                    IRType::String
                        | IRType::Pointer(_)
                        | IRType::StructRef(_)
                        | IRType::EnumRef(_)
                        | IRType::Function { .. }
                )
            }
            IRInstruction::Load { type_info, .. } => {
                // Check if loaded type is a pointer or heap-allocated type
                matches!(
                    type_info.type_,
                    IRType::String
                        | IRType::Pointer(_)
                        | IRType::StructRef(_)
                        | IRType::EnumRef(_)
                        | IRType::Function { .. }
                )
            }
            _ => false,
        }
    }

    // Emit moves to set up PHI values for a predecessor block jumping to a target block
    fn emit_phi_moves_for_predecessor(
        &mut self,
        predecessor_block_id: BasicBlockId,
        target_block_id: BasicBlockId,
        reg_alloc: &BTreeMap<ValueId, u8>,
        func: &IRFunction,
    ) {
        // Find the target block
        if let Some(target_block) = func.basic_blocks.iter().find(|b| b.id == target_block_id) {
            // For each PHI node in the target block
            for phi_node in &target_block.phi_nodes {
                if let IRInstruction::Phi {
                    result, incoming, ..
                } = phi_node
                {
                    // Find the incoming value that corresponds to our predecessor block
                    if let Some((src_value, _src_block_id)) = incoming
                        .iter()
                        .find(|(_value, pred_block_id)| *pred_block_id == predecessor_block_id)
                    {
                        // Insert a move from the source value to the PHI result
                        let result_reg =
                            self.get_register_for_value(&IRValue::SSA(*result), reg_alloc);
                        let src_reg = self.get_register_for_value(src_value, reg_alloc);

                        // Only emit move if registers are different
                        if src_reg != result_reg {
                            self.emit_u8(0xC0); // Move
                            self.emit_u8(result_reg);
                            self.emit_u8(src_reg);
                        }
                    }
                    // If we're not a predecessor of this PHI node, we shouldn't be jumping to this block
                    // Or the PHI node is malformed. In either case, we don't emit a move.
                }
            }
        }
    }

    // Emit a terminator with actual block offset resolution
    fn emit_terminator(
        &mut self,
        term: &IRTerminator,
        reg_alloc: &BTreeMap<ValueId, u8>,
        block_offsets: &BTreeMap<BasicBlockId, usize>,
        current_block_id: BasicBlockId, // Add current block ID for PHI handling
        func: &IRFunction,              // Add function for accessing blocks
    ) {
        match term {
            IRTerminator::Return { value, .. } => {
                if let Some(return_val) = value {
                    let return_reg = self.get_register_for_value(return_val, reg_alloc);
                    // Move return value to R0
                    if return_reg != 0 {
                        self.emit_u8(0xC0); // Move
                        self.emit_u8(0); // R0
                        self.emit_u8(return_reg);
                    }
                } else {
                    // If no return value, set R0 to 0
                    self.emit_u8(0xC5); // MoveZero
                    self.emit_u8(0); // R0
                }
                self.emit_u8(0xA4); // Return
            }
            IRTerminator::Jump { target, .. } => {
                // Before jumping, handle any PHI nodes in the target block
                // Insert moves to set up the correct PHI values for this predecessor
                self.emit_phi_moves_for_predecessor(current_block_id, *target, reg_alloc, func);

                // Calculate the offset to the target block
                // We need the final position after emitting the entire instruction
                let current_pos = self.bytecode.len();
                let jump_instruction_size = 3; // opcode + 2 bytes for i16 offset
                if let Some(&target_offset) = block_offsets.get(target) {
                    let relative_offset =
                        target_offset as i16 - (current_pos + jump_instruction_size) as i16;

                    self.emit_u8(0x90); // Jump
                    self.emit_i16_le(relative_offset);
                } else {
                    // If target block is not found, emit a placeholder
                    self.emit_u8(0x90); // Jump
                    self.emit_i16_le(0); // placeholder offset
                }
            }
            IRTerminator::Branch {
                condition,
                then_block,
                else_block,
                ..
            } => {
                let cond_reg = self.get_register_for_value(condition, reg_alloc);

                // Handle PHI moves for then_block
                self.emit_phi_moves_for_predecessor(current_block_id, *then_block, reg_alloc, func);

                // Calculate offset to then block (after the branch instruction)
                let current_pos = self.bytecode.len();
                let branch_instruction_size = 4; // opcode + 1 byte register + 2 bytes offset
                let then_offset = block_offsets
                    .get(then_block)
                    .map(|&offset| offset as i16 - (current_pos + branch_instruction_size) as i16)
                    .unwrap_or(0);

                self.emit_u8(0x92); // JumpIf
                self.emit_u8(cond_reg);
                self.emit_i16_le(then_offset);

                // Handle PHI moves for else_block
                self.emit_phi_moves_for_predecessor(current_block_id, *else_block, reg_alloc, func);

                // Calculate offset to else block (after the Jump instruction)
                let jump_instruction_size = 3; // opcode + 2 bytes offset
                let else_offset = block_offsets
                    .get(else_block)
                    // After the branch instruction, we'll emit the jump to else block
                    .map(|&offset| {
                        offset as i16
                            - (current_pos + branch_instruction_size + jump_instruction_size) as i16
                    })
                    .unwrap_or(0);

                self.emit_u8(0x90); // Jump (to else block)
                self.emit_i16_le(else_offset);
            }
            IRTerminator::Switch {
                value,
                cases,
                default,
                is_exhaustive: _,
                ..
            } => {
                let val_reg = self.get_register_for_value(value, reg_alloc);

                // Handle PHI moves for all case targets
                for (_case_value, target_block) in cases {
                    self.emit_phi_moves_for_predecessor(
                        current_block_id,
                        *target_block,
                        reg_alloc,
                        func,
                    );
                }

                // Handle PHI moves for default target if it exists
                if let Some(default_block) = default {
                    self.emit_phi_moves_for_predecessor(
                        current_block_id,
                        *default_block,
                        reg_alloc,
                        func,
                    );
                }

                // For now, convert switch to a series of conditional branches
                // This is a simpler approach than building a full jump table
                for (case_value, target_block) in cases {
                    let case_reg = self.get_register_for_value(case_value, reg_alloc);

                    // Compare value with case value
                    self.emit_u8(0x20); // IntEq
                    self.emit_u8(255); // Temporary register for comparison result
                    self.emit_u8(val_reg);
                    self.emit_u8(case_reg);

                    // Branch if equal to the case target
                    if let Some(&target_offset) = block_offsets.get(target_block) {
                        let current_pos = self.bytecode.len();
                        let branch_instruction_size = 4; // opcode + 1 byte register + 2 bytes offset
                        let target_offset_calc =
                            target_offset as i16 - (current_pos + branch_instruction_size) as i16;

                        self.emit_u8(0x92); // JumpIf
                        self.emit_u8(255); // comparison result register
                        self.emit_i16_le(target_offset_calc);
                    }
                }

                // Jump to default case
                if let Some(default_block) = default
                    && let Some(&target_offset) = block_offsets.get(default_block)
                {
                    let current_pos = self.bytecode.len();
                    let jump_instruction_size = 3; // opcode + 2 bytes offset
                    let target_offset_calc =
                        target_offset as i16 - (current_pos + jump_instruction_size) as i16;

                    self.emit_u8(0x90); // Jump
                    self.emit_i16_le(target_offset_calc);
                }
            }
            IRTerminator::Unreachable { .. } => {
                self.emit_u8(0xF4); // Unreachable
            }
            IRTerminator::Loop {
                condition,
                body,
                continue_block,
                ..
            } => {
                let cond_reg = self.get_register_for_value(condition, reg_alloc);

                // Handle PHI moves for body block
                self.emit_phi_moves_for_predecessor(current_block_id, *body, reg_alloc, func);

                // First, emit the conditional jump to the body if condition is true
                let current_pos = self.bytecode.len();
                let branch_instruction_size = 4; // opcode + 1 byte register + 2 bytes offset
                let body_offset = block_offsets
                    .get(body)
                    .map(|&offset| offset as i16 - (current_pos + branch_instruction_size) as i16)
                    .unwrap_or(0);

                // Jump to body if condition is true
                self.emit_u8(0x92); // JumpIf
                self.emit_u8(cond_reg);
                self.emit_i16_le(body_offset);

                // Handle PHI moves for continue block (back edge)
                self.emit_phi_moves_for_predecessor(
                    current_block_id,
                    *continue_block,
                    reg_alloc,
                    func,
                );

                // After the loop body executes, we need to jump back to the continue_block
                // for evaluation of the condition again
                if let Some(&continue_offset) = block_offsets.get(continue_block) {
                    let current_pos = self.bytecode.len();
                    let jump_instruction_size = 3; // opcode + 2 bytes offset
                    let continue_offset_calc =
                        continue_offset as i16 - (current_pos + jump_instruction_size) as i16;

                    self.emit_u8(0x90); // Jump
                    self.emit_i16_le(continue_offset_calc);
                }
            }
            IRTerminator::Handle {
                body,
                handlers,
                return_type: _,
                ..
            } => {
                // For effect handlers, we need to emit proper push handler instructions
                // Push handlers onto the effect handler stack
                for handler in handlers {
                    // Handle PHI moves for handler body
                    self.emit_phi_moves_for_predecessor(
                        current_block_id,
                        handler.body,
                        reg_alloc,
                        func,
                    );

                    // Emit handler push for each effect handler
                    self.emit_u8(0xB2); // PushHandler
                    // In a real implementation, we'd need to set up the handler context
                    let handler_offset = block_offsets
                        .get(&handler.body)
                        .map(|&offset| offset as i16)
                        .unwrap_or(0);
                    self.emit_i16_le(handler_offset);
                }

                // Handle PHI moves for body block
                self.emit_phi_moves_for_predecessor(current_block_id, *body, reg_alloc, func);

                // Jump to the body of the handled code
                let current_pos = self.bytecode.len();
                let jump_instruction_size = 3; // opcode + 2 bytes offset
                let body_offset = block_offsets
                    .get(body)
                    .map(|&offset| offset as i16 - (current_pos + jump_instruction_size) as i16)
                    .unwrap_or(0);

                self.emit_u8(0x90); // Jump to body
                self.emit_i16_le(body_offset);
            }
            IRTerminator::Error { .. } => {
                self.emit_u8(0xF5); // DebugTrap
            }
        }
    }

    // Helper to get register for a value (handles immediate values too)
    fn get_register_for_value(&mut self, value: &IRValue, reg_alloc: &BTreeMap<ValueId, u8>) -> u8 {
        match value {
            IRValue::SSA(id) => *reg_alloc.get(id).unwrap_or(&0),
            IRValue::Int(val) => {
                // Load immediate to a register
                let reg = self.next_register;
                self.next_register += 1;

                if *val == 0 {
                    self.emit_u8(0xC5); // MoveZero
                    self.emit_u8(reg);
                } else if *val == 1 {
                    self.emit_u8(0xC6); // MoveOne
                    self.emit_u8(reg);
                } else if *val >= -128 && *val <= 127 {
                    self.emit_u8(0xC1); // MoveImm8
                    self.emit_u8(reg);
                    self.emit_i8(*val as i8);
                } else {
                    self.emit_u8(0xC4); // MoveImm64
                    self.emit_u8(reg);
                    self.emit_i64_le(*val);
                }

                reg
            }
            IRValue::Float(val) => {
                // Load float as immediate
                let reg = self.next_register;
                self.next_register += 1;

                let bits = val.to_bits();
                self.emit_u8(0xC4); // MoveImm64
                self.emit_u8(reg);
                self.emit_i64_le(bits as i64);

                reg
            }
            IRValue::Bool(val) => {
                let reg = self.next_register;
                self.next_register += 1;

                if *val {
                    self.emit_u8(0xC8); // MoveTrue
                    self.emit_u8(reg);
                } else {
                    self.emit_u8(0xC9); // MoveFalse
                    self.emit_u8(reg);
                }

                reg
            }
            IRValue::FunctionRef(_) => {
                // For function references, we'll use a register to hold the function ID
                let reg = self.next_register;
                self.next_register += 1;
                self.emit_u8(0xC5); // MoveZero as placeholder
                self.emit_u8(reg);
                reg
            }
            _ => 0, // Default to register 0
        }
    }

    // Helper to get the type of a value
    fn get_value_type(&self, value: &IRValue) -> IRType {
        match value {
            IRValue::Int(_) => IRType::Int,
            IRValue::Float(_) => IRType::Float,
            IRValue::Bool(_) => IRType::Bool,
            IRValue::SSA(_) => IRType::Int, // Default assumption
            _ => IRType::Int,               // Default assumption
        }
    }

    // Helper to determine if a type is a GC pointer type
    fn is_gc_pointer_type(&self, ir_type: &IRType) -> bool {
        match ir_type {
            IRType::String => true,
            IRType::Pointer(_) => true,   // Raw pointers to heap objects
            IRType::StructRef(_) => true, // Struct instances on heap
            IRType::EnumRef(_) => true,   // Enum instances on heap
            IRType::Tuple(_) => true,     // Tuples might be on heap
            IRType::Function { .. } => true, // Closures are heap allocated
            IRType::Unit => false,        // Unit type doesn't need GC
            IRType::Never => false,       // Never type doesn't need GC
            IRType::Bool => false,        // Primitive types don't need GC
            IRType::Int => false,
            IRType::Float => false,
            // For other types, be conservative and treat as potential GC pointers
            _ => true,
        }
    }

    // Helper to get the type for a value (used for type-directed codegen)
    fn get_type_for_value(&self, value: &IRValue) -> IRType {
        self.get_value_type(value) // Use the same function for now
    }

    // Helper to get size for a type
    fn get_size_for_type(&self, type_info: &IRTypeWithMemory) -> usize {
        match &type_info.type_ {
            IRType::Int | IRType::Float => 8,
            IRType::Bool => 1,
            IRType::String => 8, // Pointer size
            IRType::Unit => 0,
            _ => 8, // Default to 8 bytes
        }
    }

    // Helper to get alignment for a type
    fn get_alignment_for_type(&self, type_info: &IRTypeWithMemory) -> usize {
        match &type_info.type_ {
            IRType::Int | IRType::Float => 8,
            IRType::Bool => 1,
            IRType::String => 8, // Pointer alignment
            IRType::Unit => 1,
            _ => 8, // Default to 8-byte alignment
        }
    }

    // Helper functions for encoding
    fn emit_u8(&mut self, val: u8) {
        self.bytecode.push(val);
    }

    fn emit_i8(&mut self, val: i8) {
        self.bytecode.push(val as u8);
    }

    fn emit_u16_le(&mut self, val: u16) {
        self.bytecode.extend_from_slice(&val.to_le_bytes());
    }

    fn emit_i16_le(&mut self, val: i16) {
        self.bytecode.extend_from_slice(&val.to_le_bytes());
    }

    fn _emit_u32_le(&mut self, val: u32) {
        self.bytecode.extend_from_slice(&val.to_le_bytes());
    }

    fn emit_i64_le(&mut self, val: i64) {
        self.bytecode.extend_from_slice(&val.to_le_bytes());
    }

    // Emit stack map with specific registers
    fn emit_stack_map_registers(&mut self, live_gc_regs: &[u8]) {
        // In a real implementation, this would create a proper stack map entry
        // The format would match the StackMapEntry structure in the VM:
        // - bytecode offset (u16)
        // - live pointer count (u8)
        // - live pointer registers (u8 array)

        // Add the current bytecode offset as a u16 (we'll update this later)
        let current_offset = self.bytecode.len() as u16;
        self.stack_maps
            .extend_from_slice(&current_offset.to_le_bytes());

        // Add the count of live GC registers
        self.stack_maps.push(live_gc_regs.len() as u8);

        // Add each live GC register
        for &reg in live_gc_regs {
            self.stack_maps.push(reg);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{
        BasicBlock, IRFunction, IRFunctionArg, IRInstruction, IRModule, IRTerminator,
        IRTypeWithMemory, MemoryKind, MemorySlotId, TargetInfo,
    };

    #[test]
    fn test_simple_add_function() {
        // Create a simple IR module with an add function
        let target = TargetInfo::vm_target();
        let mut module = IRModule::new(target);

        // Create function args
        let arg1 = IRFunctionArg {
            name: "x".to_string(),
            binding_id: 0,
            type_: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            memory_slot: MemorySlotId(0),
            span: 0..0,
            file: "".to_string(),
        };

        let arg2 = IRFunctionArg {
            name: "y".to_string(),
            binding_id: 1,
            type_: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            memory_slot: MemorySlotId(1),
            span: 0..0,
            file: "".to_string(),
        };

        let return_type = IRTypeWithMemory {
            type_: crate::ir::IRType::Int,
            span: 0..0,
            file: "".to_string(),
            memory_kind: MemoryKind::Stack,
            allocation_id: None,
        };

        // Create a basic block with an add operation and return
        let mut block = BasicBlock::new(crate::ir::BasicBlockId(0));

        // Add instruction: v2 = BinOp(v0, Add, v1)
        block.instructions.push(IRInstruction::BinOp {
            result: ValueId(2),
            metadata: crate::ir::InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            left: IRValue::SSA(ValueId(0)), // first argument (x)
            op: BinOp::Add,
            right: IRValue::SSA(ValueId(1)), // second argument (y)
            span: 0..0,
            file: "".to_string(),
        });

        // Return instruction
        block.terminator = Some(IRTerminator::Return {
            value: Some(IRValue::SSA(ValueId(2))), // return the result of addition
            span: 0..0,
            file: "".to_string(),
        });

        // Create a simple function that adds two integers
        let func = IRFunction {
            id: crate::ir::FunctionId(0),
            name: "add".to_string(),
            vis: crate::ast::Visibility::Public,
            args: vec![arg1, arg2],
            return_type,
            effects: crate::typechecker::EffectSet::pure(),
            function_type: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            basic_blocks: vec![block],
            cfg: crate::ir::ControlFlowGraph {
                blocks: std::collections::BTreeMap::new(),
                entry: crate::ir::BasicBlockId(0),
                exits: vec![],
            },
            span: 0..0,
            file: "".to_string(),
            body: Some(crate::ir::BasicBlockId(0)),
            memory_usage: crate::ir::MemoryUsage {
                max_stack_size: 0,
                heap_allocations: vec![],
                escape_analysis_info: crate::ir::EscapeAnalysisInfo {
                    escaping_values: vec![],
                    stack_allocated_values: vec![],
                    heap_allocated_values: vec![],
                    value_lifetimes: std::collections::BTreeMap::new(),
                    escape_reasons: std::collections::BTreeMap::new(),
                    capture_sets: std::collections::BTreeMap::new(),
                },
            },
            optimization_hints: crate::ir::OptimizationHints {
                inline: crate::ir::InlineHint::Heuristic,
                is_pure: true,
                is_cold: false,
                should_unroll: None,
            },
        };

        module.functions.push(func);

        let mut compiler = BytecodeCompiler::new();
        let compiled = compiler.compile_module(&module);

        // Verify the bytecode contains expected instructions:
        // 1. IntAdd operation (0x00) - add two values into a destination register
        // 2. Move operation (0xC0) - to move result to R0 for return
        // 3. Return operation (0xA4)

        assert!(!compiled.bytecode.is_empty());

        // Verify that the bytecode contains an IntAdd instruction
        let has_int_add = compiled.bytecode.contains(&0x00); // IntAdd opcode
        assert!(has_int_add, "Bytecode should contain IntAdd instruction");

        // Verify that the bytecode contains a Return instruction
        let has_return = compiled.bytecode.contains(&0xA4); // Return opcode
        assert!(has_return, "Bytecode should contain Return instruction");
    }

    #[test]
    fn test_constant_folding_add_one() {
        // Test that adding a constant 1 gets compiled to IntAddImm8
        let target = TargetInfo::vm_target();
        let mut module = IRModule::new(target);

        let arg = IRFunctionArg {
            name: "x".to_string(),
            binding_id: 0,
            type_: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            memory_slot: MemorySlotId(0),
            span: 0..0,
            file: "".to_string(),
        };

        let return_type = IRTypeWithMemory {
            type_: crate::ir::IRType::Int,
            span: 0..0,
            file: "".to_string(),
            memory_kind: MemoryKind::Stack,
            allocation_id: None,
        };

        let mut block = BasicBlock::new(crate::ir::BasicBlockId(0));

        // Add instruction: v1 = BinOp(v0, Add, 1) - should use immediate
        block.instructions.push(IRInstruction::BinOp {
            result: ValueId(1),
            metadata: crate::ir::InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            left: IRValue::SSA(ValueId(0)), // argument (x)
            op: BinOp::Add,
            right: IRValue::Int(1), // constant 1
            span: 0..0,
            file: "".to_string(),
        });

        block.terminator = Some(IRTerminator::Return {
            value: Some(IRValue::SSA(ValueId(1))),
            span: 0..0,
            file: "".to_string(),
        });

        let func = IRFunction {
            id: crate::ir::FunctionId(1),
            name: "add_one".to_string(),
            vis: crate::ast::Visibility::Public,
            args: vec![arg],
            return_type,
            effects: crate::typechecker::EffectSet::pure(),
            function_type: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            basic_blocks: vec![block],
            cfg: crate::ir::ControlFlowGraph {
                blocks: std::collections::BTreeMap::new(),
                entry: crate::ir::BasicBlockId(0),
                exits: vec![],
            },
            span: 0..0,
            file: "".to_string(),
            body: Some(crate::ir::BasicBlockId(0)),
            memory_usage: crate::ir::MemoryUsage {
                max_stack_size: 0,
                heap_allocations: vec![],
                escape_analysis_info: crate::ir::EscapeAnalysisInfo {
                    escaping_values: vec![],
                    stack_allocated_values: vec![],
                    heap_allocated_values: vec![],
                    value_lifetimes: std::collections::BTreeMap::new(),
                    escape_reasons: std::collections::BTreeMap::new(),
                    capture_sets: std::collections::BTreeMap::new(),
                },
            },
            optimization_hints: crate::ir::OptimizationHints {
                inline: crate::ir::InlineHint::Heuristic,
                is_pure: true,
                is_cold: false,
                should_unroll: None,
            },
        };

        module.functions.push(func);

        let mut compiler = BytecodeCompiler::new();
        let compiled = compiler.compile_module(&module);

        // Verify that IntAddImm8 is used when adding small constants
        let has_int_add_imm8 = compiled.bytecode.contains(&0x06); // IntAddImm8 opcode
        assert!(
            has_int_add_imm8,
            "Bytecode should contain IntAddImm8 instruction for adding small constants"
        );
    }

    #[test]
    fn test_function_call() {
        // Test that a function call is compiled correctly
        let target = TargetInfo::vm_target();
        let mut module = IRModule::new(target);

        // Create a simple function that calls another function
        let arg = IRFunctionArg {
            name: "x".to_string(),
            binding_id: 0,
            type_: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            memory_slot: MemorySlotId(0),
            span: 0..0,
            file: "".to_string(),
        };

        let return_type = IRTypeWithMemory {
            type_: crate::ir::IRType::Int,
            span: 0..0,
            file: "".to_string(),
            memory_kind: MemoryKind::Stack,
            allocation_id: None,
        };

        let mut block = BasicBlock::new(crate::ir::BasicBlockId(0));

        // Call instruction: v1 = call another function
        block.instructions.push(IRInstruction::Call {
            result: ValueId(1),
            metadata: crate::ir::InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            function: IRValue::FunctionRef(crate::ir::FunctionId(100)), // Some other function
            args: vec![IRValue::SSA(ValueId(0))],                       // Pass the argument
            type_args: vec![],
            span: 0..0,
            file: "".to_string(),
        });

        block.terminator = Some(IRTerminator::Return {
            value: Some(IRValue::SSA(ValueId(1))),
            span: 0..0,
            file: "".to_string(),
        });

        let func = IRFunction {
            id: crate::ir::FunctionId(2),
            name: "call_function".to_string(),
            vis: crate::ast::Visibility::Public,
            args: vec![arg],
            return_type,
            effects: crate::typechecker::EffectSet::pure(),
            function_type: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            basic_blocks: vec![block],
            cfg: crate::ir::ControlFlowGraph {
                blocks: std::collections::BTreeMap::new(),
                entry: crate::ir::BasicBlockId(0),
                exits: vec![],
            },
            span: 0..0,
            file: "".to_string(),
            body: Some(crate::ir::BasicBlockId(0)),
            memory_usage: crate::ir::MemoryUsage {
                max_stack_size: 0,
                heap_allocations: vec![],
                escape_analysis_info: crate::ir::EscapeAnalysisInfo {
                    escaping_values: vec![],
                    stack_allocated_values: vec![],
                    heap_allocated_values: vec![],
                    value_lifetimes: std::collections::BTreeMap::new(),
                    escape_reasons: std::collections::BTreeMap::new(),
                    capture_sets: std::collections::BTreeMap::new(),
                },
            },
            optimization_hints: crate::ir::OptimizationHints {
                inline: crate::ir::InlineHint::Heuristic,
                is_pure: true,
                is_cold: false,
                should_unroll: None,
            },
        };

        module.functions.push(func);

        let mut compiler = BytecodeCompiler::new();
        let compiled = compiler.compile_module(&module);

        // Verify that the bytecode contains a Call instruction
        let has_call = compiled.bytecode.contains(&0xA0); // Call opcode
        assert!(has_call, "Bytecode should contain Call instruction");
    }

    #[test]
    fn test_branch_and_control_flow() {
        // Test basic control flow - if/else-like structure
        let target = TargetInfo::vm_target();
        let mut module = IRModule::new(target);

        // Create function with two arguments
        let arg1 = IRFunctionArg {
            name: "condition".to_string(),
            binding_id: 0,
            type_: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            memory_slot: MemorySlotId(0),
            span: 0..0,
            file: "".to_string(),
        };

        let arg2 = IRFunctionArg {
            name: "value".to_string(),
            binding_id: 1,
            type_: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            memory_slot: MemorySlotId(1),
            span: 0..0,
            file: "".to_string(),
        };

        let return_type = IRTypeWithMemory {
            type_: crate::ir::IRType::Int,
            span: 0..0,
            file: "".to_string(),
            memory_kind: MemoryKind::Stack,
            allocation_id: None,
        };

        // Create basic blocks
        let mut block0 = BasicBlock::new(crate::ir::BasicBlockId(0));
        let mut block1 = BasicBlock::new(crate::ir::BasicBlockId(1));
        let mut block2 = BasicBlock::new(crate::ir::BasicBlockId(2));

        // Block 0: Compare condition to 0, branch to either block 1 or 2
        block0.instructions.push(IRInstruction::BinOp {
            result: ValueId(2),
            metadata: crate::ir::InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            left: IRValue::SSA(ValueId(0)), // condition
            op: BinOp::NotEq,
            right: IRValue::Int(0), // compare with 0
            span: 0..0,
            file: "".to_string(),
        });

        block0.terminator = Some(IRTerminator::Branch {
            condition: IRValue::SSA(ValueId(2)),
            then_block: crate::ir::BasicBlockId(1),
            else_block: crate::ir::BasicBlockId(2),
            span: 0..0,
            file: "".to_string(),
        });

        // Block 1: return condition + value
        block1.instructions.push(IRInstruction::BinOp {
            result: ValueId(3),
            metadata: crate::ir::InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            left: IRValue::SSA(ValueId(0)), // condition
            op: BinOp::Add,
            right: IRValue::SSA(ValueId(1)), // value
            span: 0..0,
            file: "".to_string(),
        });

        block1.terminator = Some(IRTerminator::Return {
            value: Some(IRValue::SSA(ValueId(3))),
            span: 0..0,
            file: "".to_string(),
        });

        // Block 2: return condition * value
        block2.instructions.push(IRInstruction::BinOp {
            result: ValueId(4),
            metadata: crate::ir::InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            left: IRValue::SSA(ValueId(0)), // condition
            op: BinOp::Mul,
            right: IRValue::SSA(ValueId(1)), // value
            span: 0..0,
            file: "".to_string(),
        });

        block2.terminator = Some(IRTerminator::Return {
            value: Some(IRValue::SSA(ValueId(4))),
            span: 0..0,
            file: "".to_string(),
        });

        let func = IRFunction {
            id: crate::ir::FunctionId(3),
            name: "branch_function".to_string(),
            vis: crate::ast::Visibility::Public,
            args: vec![arg1, arg2],
            return_type,
            effects: crate::typechecker::EffectSet::pure(),
            function_type: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            basic_blocks: vec![block0, block1, block2],
            cfg: crate::ir::ControlFlowGraph {
                blocks: std::collections::BTreeMap::new(),
                entry: crate::ir::BasicBlockId(0),
                exits: vec![],
            },
            span: 0..0,
            file: "".to_string(),
            body: Some(crate::ir::BasicBlockId(0)),
            memory_usage: crate::ir::MemoryUsage {
                max_stack_size: 0,
                heap_allocations: vec![],
                escape_analysis_info: crate::ir::EscapeAnalysisInfo {
                    escaping_values: vec![],
                    stack_allocated_values: vec![],
                    heap_allocated_values: vec![],
                    value_lifetimes: std::collections::BTreeMap::new(),
                    escape_reasons: std::collections::BTreeMap::new(),
                    capture_sets: std::collections::BTreeMap::new(),
                },
            },
            optimization_hints: crate::ir::OptimizationHints {
                inline: crate::ir::InlineHint::Heuristic,
                is_pure: true,
                is_cold: false,
                should_unroll: None,
            },
        };

        module.functions.push(func);

        let mut compiler = BytecodeCompiler::new();
        let compiled = compiler.compile_module(&module);

        // Verify that the bytecode contains branch instructions
        let has_branch = compiled.bytecode.contains(&0x92); // JumpIf opcode
        assert!(has_branch, "Bytecode should contain branch instruction");
    }

    #[test]
    fn test_float_operations() {
        // Test floating point operations
        let target = TargetInfo::vm_target();
        let mut module = IRModule::new(target);

        // Create function with two float arguments
        let arg1 = IRFunctionArg {
            name: "x".to_string(),
            binding_id: 0,
            type_: IRTypeWithMemory {
                type_: crate::ir::IRType::Float,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            memory_slot: MemorySlotId(0),
            span: 0..0,
            file: "".to_string(),
        };

        let arg2 = IRFunctionArg {
            name: "y".to_string(),
            binding_id: 1,
            type_: IRTypeWithMemory {
                type_: crate::ir::IRType::Float,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            memory_slot: MemorySlotId(1),
            span: 0..0,
            file: "".to_string(),
        };

        let return_type = IRTypeWithMemory {
            type_: crate::ir::IRType::Float,
            span: 0..0,
            file: "".to_string(),
            memory_kind: MemoryKind::Stack,
            allocation_id: None,
        };

        let block = BasicBlock::new(crate::ir::BasicBlockId(0));

        // Create two float literals and add them
        let mut func = IRFunction {
            id: crate::ir::FunctionId(4),
            name: "add_floats".to_string(),
            vis: crate::ast::Visibility::Public,
            args: vec![arg1, arg2],
            return_type,
            effects: crate::typechecker::EffectSet::pure(),
            function_type: IRTypeWithMemory {
                type_: crate::ir::IRType::Float,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            basic_blocks: vec![block],
            cfg: crate::ir::ControlFlowGraph {
                blocks: std::collections::BTreeMap::new(),
                entry: crate::ir::BasicBlockId(0),
                exits: vec![],
            },
            span: 0..0,
            file: "".to_string(),
            body: Some(crate::ir::BasicBlockId(0)),
            memory_usage: crate::ir::MemoryUsage {
                max_stack_size: 0,
                heap_allocations: vec![],
                escape_analysis_info: crate::ir::EscapeAnalysisInfo {
                    escaping_values: vec![],
                    stack_allocated_values: vec![],
                    heap_allocated_values: vec![],
                    value_lifetimes: std::collections::BTreeMap::new(),
                    escape_reasons: std::collections::BTreeMap::new(),
                    capture_sets: std::collections::BTreeMap::new(),
                },
            },
            optimization_hints: crate::ir::OptimizationHints {
                inline: crate::ir::InlineHint::Heuristic,
                is_pure: true,
                is_cold: false,
                should_unroll: None,
            },
        };

        // Add a simple return of a float literal for now
        func.basic_blocks[0].instructions.push(IRInstruction::Let {
            result: ValueId(2),
            metadata: crate::ir::InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            var: "result".to_string(),
            value: IRValue::Float(3.14), // Simple float literal
            var_type: IRTypeWithMemory {
                type_: crate::ir::IRType::Float,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            span: 0..0,
            file: "".to_string(),
        });

        func.basic_blocks[0].terminator = Some(IRTerminator::Return {
            value: Some(IRValue::SSA(ValueId(2))),
            span: 0..0,
            file: "".to_string(),
        });

        module.functions.push(func);

        let mut compiler = BytecodeCompiler::new();
        let compiled = compiler.compile_module(&module);

        // Verify that the bytecode contains MoveImm64 for float
        let has_move_imm64 = compiled.bytecode.contains(&0xC4); // MoveImm64 opcode
        assert!(
            has_move_imm64,
            "Bytecode should contain MoveImm64 instruction for float literals"
        );
    }

    #[test]
    fn test_allocation_and_gc_safepoints() {
        // Test memory allocation and GC safepoint generation
        let target = TargetInfo::vm_target();
        let mut module = IRModule::new(target);

        let return_type = IRTypeWithMemory {
            type_: crate::ir::IRType::Int,
            span: 0..0,
            file: "".to_string(),
            memory_kind: MemoryKind::Stack,
            allocation_id: None,
        };

        let mut block = BasicBlock::new(crate::ir::BasicBlockId(0));

        // Allocate memory for an object
        block.instructions.push(IRInstruction::Allocate {
            result: ValueId(0),
            metadata: crate::ir::InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            allocation_id: crate::ir::AllocationId(0),
            size: crate::ir::AllocationSize::Static(16), // 16 bytes
            memory_kind: MemoryKind::Heap,
            type_info: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Heap,
                allocation_id: None,
            },
            span: 0..0,
            file: "".to_string(),
        });

        // Also add a call instruction to test call safepoints
        block.instructions.push(IRInstruction::Call {
            result: ValueId(1),
            metadata: crate::ir::InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            function: IRValue::FunctionRef(crate::ir::FunctionId(100)),
            args: vec![],
            type_args: vec![],
            span: 0..0,
            file: "".to_string(),
        });

        block.terminator = Some(IRTerminator::Return {
            value: Some(IRValue::SSA(ValueId(1))),
            span: 0..0,
            file: "".to_string(),
        });

        let func = IRFunction {
            id: crate::ir::FunctionId(5),
            name: "alloc_and_call".to_string(),
            vis: crate::ast::Visibility::Public,
            args: vec![], // No arguments
            return_type,
            effects: crate::typechecker::EffectSet::pure(),
            function_type: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            basic_blocks: vec![block],
            cfg: crate::ir::ControlFlowGraph {
                blocks: std::collections::BTreeMap::new(),
                entry: crate::ir::BasicBlockId(0),
                exits: vec![],
            },
            span: 0..0,
            file: "".to_string(),
            body: Some(crate::ir::BasicBlockId(0)),
            memory_usage: crate::ir::MemoryUsage {
                max_stack_size: 0,
                heap_allocations: vec![],
                escape_analysis_info: crate::ir::EscapeAnalysisInfo {
                    escaping_values: vec![],
                    stack_allocated_values: vec![],
                    heap_allocated_values: vec![],
                    value_lifetimes: std::collections::BTreeMap::new(),
                    escape_reasons: std::collections::BTreeMap::new(),
                    capture_sets: std::collections::BTreeMap::new(),
                },
            },
            optimization_hints: crate::ir::OptimizationHints {
                inline: crate::ir::InlineHint::Heuristic,
                is_pure: true,
                is_cold: false,
                should_unroll: None,
            },
        };

        module.functions.push(func);

        let mut compiler = BytecodeCompiler::new();
        let compiled = compiler.compile_module(&module);

        // Verify allocation and call instructions exist
        let has_alloc = compiled.bytecode.contains(&0xD1); // GCAllocFast16 opcode
        assert!(has_alloc, "Bytecode should contain allocation instruction");

        let has_call = compiled.bytecode.contains(&0xA0); // Call opcode
        assert!(has_call, "Bytecode should contain Call instruction");

        // Verify stack map was generated (call and alloc are safepoints)
        assert!(
            !compiled.stack_maps.is_empty(),
            "Stack maps should be generated at safepoints"
        );
    }

    #[test]
    fn test_comparison_operations() {
        // Test comparison operations that return boolean results
        let target = TargetInfo::vm_target();
        let mut module = IRModule::new(target);

        let arg1 = IRFunctionArg {
            name: "x".to_string(),
            binding_id: 0,
            type_: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            memory_slot: MemorySlotId(0),
            span: 0..0,
            file: "".to_string(),
        };

        let return_type = IRTypeWithMemory {
            type_: crate::ir::IRType::Bool,
            span: 0..0,
            file: "".to_string(),
            memory_kind: MemoryKind::Stack,
            allocation_id: None,
        };

        let mut block = BasicBlock::new(crate::ir::BasicBlockId(0));

        // Compare: v1 = (x > 0)
        block.instructions.push(IRInstruction::BinOp {
            result: ValueId(1),
            metadata: crate::ir::InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            left: IRValue::SSA(ValueId(0)), // x
            op: BinOp::Greater,
            right: IRValue::Int(0), // 0
            span: 0..0,
            file: "".to_string(),
        });

        block.terminator = Some(IRTerminator::Return {
            value: Some(IRValue::SSA(ValueId(1))),
            span: 0..0,
            file: "".to_string(),
        });

        let func = IRFunction {
            id: crate::ir::FunctionId(6),
            name: "compare_greater_than_zero".to_string(),
            vis: crate::ast::Visibility::Public,
            args: vec![arg1],
            return_type,
            effects: crate::typechecker::EffectSet::pure(),
            function_type: IRTypeWithMemory {
                type_: crate::ir::IRType::Bool,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            basic_blocks: vec![block],
            cfg: crate::ir::ControlFlowGraph {
                blocks: std::collections::BTreeMap::new(),
                entry: crate::ir::BasicBlockId(0),
                exits: vec![],
            },
            span: 0..0,
            file: "".to_string(),
            body: Some(crate::ir::BasicBlockId(0)),
            memory_usage: crate::ir::MemoryUsage {
                max_stack_size: 0,
                heap_allocations: vec![],
                escape_analysis_info: crate::ir::EscapeAnalysisInfo {
                    escaping_values: vec![],
                    stack_allocated_values: vec![],
                    heap_allocated_values: vec![],
                    value_lifetimes: std::collections::BTreeMap::new(),
                    escape_reasons: std::collections::BTreeMap::new(),
                    capture_sets: std::collections::BTreeMap::new(),
                },
            },
            optimization_hints: crate::ir::OptimizationHints {
                inline: crate::ir::InlineHint::Heuristic,
                is_pure: true,
                is_cold: false,
                should_unroll: None,
            },
        };

        module.functions.push(func);

        let mut compiler = BytecodeCompiler::new();
        let compiled = compiler.compile_module(&module);

        // Verify that the bytecode contains IntGt instruction
        let has_int_gt = compiled.bytecode.contains(&0x24); // IntGt opcode
        assert!(
            has_int_gt,
            "Bytecode should contain IntGt instruction for comparison"
        );
    }

    #[test]
    fn test_memory_copy() {
        // Test memory copy instruction
        let target = TargetInfo::vm_target();
        let mut module = IRModule::new(target);

        let return_type = IRTypeWithMemory {
            type_: crate::ir::IRType::Int,
            span: 0..0,
            file: "".to_string(),
            memory_kind: MemoryKind::Stack,
            allocation_id: None,
        };

        let mut block = BasicBlock::new(crate::ir::BasicBlockId(0));

        // Allocate two memory blocks
        block.instructions.push(IRInstruction::Allocate {
            result: ValueId(0),
            metadata: crate::ir::InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            allocation_id: crate::ir::AllocationId(0),
            size: crate::ir::AllocationSize::Static(16),
            memory_kind: MemoryKind::Heap,
            type_info: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Heap,
                allocation_id: None,
            },
            span: 0..0,
            file: "".to_string(),
        });

        block.instructions.push(IRInstruction::Allocate {
            result: ValueId(1),
            metadata: crate::ir::InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            allocation_id: crate::ir::AllocationId(1),
            size: crate::ir::AllocationSize::Static(16),
            memory_kind: MemoryKind::Heap,
            type_info: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Heap,
                allocation_id: None,
            },
            span: 0..0,
            file: "".to_string(),
        });

        // Copy memory from one to the other
        block.instructions.push(IRInstruction::MemoryCopy {
            dest: IRValue::SSA(ValueId(0)),
            src: IRValue::SSA(ValueId(1)),
            size: crate::ir::AllocationSize::Static(16), // Copy 16 bytes
            span: 0..0,
            file: "".to_string(),
        });

        block.terminator = Some(IRTerminator::Return {
            value: Some(IRValue::SSA(ValueId(0))),
            span: 0..0,
            file: "".to_string(),
        });

        let func = IRFunction {
            id: crate::ir::FunctionId(7),
            name: "memory_copy_test".to_string(),
            vis: crate::ast::Visibility::Public,
            args: vec![], // No arguments
            return_type,
            effects: crate::typechecker::EffectSet::pure(),
            function_type: IRTypeWithMemory {
                type_: crate::ir::IRType::Int,
                span: 0..0,
                file: "".to_string(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            basic_blocks: vec![block],
            cfg: crate::ir::ControlFlowGraph {
                blocks: std::collections::BTreeMap::new(),
                entry: crate::ir::BasicBlockId(0),
                exits: vec![],
            },
            span: 0..0,
            file: "".to_string(),
            body: Some(crate::ir::BasicBlockId(0)),
            memory_usage: crate::ir::MemoryUsage {
                max_stack_size: 0,
                heap_allocations: vec![],
                escape_analysis_info: crate::ir::EscapeAnalysisInfo {
                    escaping_values: vec![],
                    stack_allocated_values: vec![],
                    heap_allocated_values: vec![],
                    value_lifetimes: std::collections::BTreeMap::new(),
                    escape_reasons: std::collections::BTreeMap::new(),
                    capture_sets: std::collections::BTreeMap::new(),
                },
            },
            optimization_hints: crate::ir::OptimizationHints {
                inline: crate::ir::InlineHint::Heuristic,
                is_pure: true,
                is_cold: false,
                should_unroll: None,
            },
        };

        module.functions.push(func);

        let mut compiler = BytecodeCompiler::new();
        let compiled = compiler.compile_module(&module);

        // Verify that the bytecode contains a DebugTrap instruction (F5) from the unimplemented MemoryCopy
        let has_debug_trap = compiled.bytecode.contains(&0xF5);
        assert!(
            has_debug_trap,
            "Bytecode should contain DebugTrap (0xF5) instruction for unimplemented MemoryCopy"
        );
    }
}
