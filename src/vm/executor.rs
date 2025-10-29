use super::{DecodedInstruction, EnumLayout, FunctionMetadata, Opcode, Operands, StructLayout};
// Import the decoder function properly
use crate::vm::disassembler::decode_instruction_at;

/// Simple VM implementation to execute Vial bytecode
pub struct VM {
    /// The bytecode to execute
    pub bytecode: Vec<u8>,

    /// Function metadata for the bytecode
    pub function_metadata: Vec<FunctionMetadata>,

    /// Constant pool (if any)
    pub constant_pool: Vec<u8>,

    /// Struct layouts (if any)
    pub struct_layouts: Vec<StructLayout>,

    /// Enum layouts (if any)
    pub enum_layouts: Vec<EnumLayout>,

    /// VM registers (256 registers as per specification)
    pub registers: [i64; 256],

    /// Program counter
    pub pc: usize,

    /// Stack for function calls and local variables
    pub stack: Vec<i64>,

    /// Stack pointer
    pub sp: usize,

    /// Frame pointer for current function
    pub fp: usize,

    /// Whether the VM is running
    pub running: bool,

    /// Entry point function ID (usually 0)
    pub entry_point: usize,
}

impl VM {
    /// Create a new VM instance
    pub fn new(
        bytecode: Vec<u8>,
        function_metadata: Vec<FunctionMetadata>,
        constant_pool: Vec<u8>,
        struct_layouts: Vec<StructLayout>,
        enum_layouts: Vec<EnumLayout>,
    ) -> Self {
        Self {
            bytecode,
            function_metadata,
            constant_pool,
            struct_layouts,
            enum_layouts,
            registers: [0; 256],
            pc: 0,
            stack: Vec::new(),
            sp: 0,
            fp: 0,
            running: false,
            entry_point: 0,
        }
    }

    /// Convenience constructor from slices
    pub fn from_slices(
        bytecode: &[u8],
        function_metadata: &[FunctionMetadata],
        constant_pool: Option<&[u8]>,
        struct_layouts: Option<&[StructLayout]>,
        enum_layouts: Option<&[EnumLayout]>,
    ) -> Self {
        Self::new(
            bytecode.to_vec(),
            function_metadata.to_vec(),
            constant_pool.unwrap_or(&[]).to_vec(),
            struct_layouts.map(|s| s.to_vec()).unwrap_or_default(),
            enum_layouts.map(|e| e.to_vec()).unwrap_or_default(),
        )
    }

    /// Run the VM
    pub fn run(&mut self) -> Result<i64, String> {
        // Set up initial stack
        self.stack.resize(1024, 0); // TODO: Make configurable

        // Set up entry point
        if self.function_metadata.is_empty() {
            return Err("No functions in bytecode".to_string());
        }

        let entry_func = &self.function_metadata[self.entry_point];
        self.pc = entry_func.bytecode_offset as usize;

        // Initialize registers to zero
        self.registers = [0; 256];

        // Set running flag
        self.running = true;

        // Main execution loop
        while self.running && self.pc < self.bytecode.len() {
            let instruction = self.decode_instruction()?;
            self.execute_instruction(&instruction)?;
        }

        // Return value should be in R0 (register 0)
        Ok(self.registers[0])
    }

    /// Decode the instruction at the current PC
    fn decode_instruction(&self) -> Result<DecodedInstruction, String> {
        decode_instruction_at(&self.bytecode, self.pc)
    }

    /// Execute a single instruction
    fn execute_instruction(&mut self, instruction: &DecodedInstruction) -> Result<(), String> {
        match instruction.opcode {
            Opcode::IntAdd => {
                if let Operands::RRR { dst, src1, src2 } = &instruction.operands {
                    let val1 = self.registers[src1.0 as usize];
                    let val2 = self.registers[src2.0 as usize];
                    self.registers[dst.0 as usize] = val1 + val2;
                }
            }

            Opcode::IntSub => {
                if let Operands::RRR { dst, src1, src2 } = &instruction.operands {
                    let val1 = self.registers[src1.0 as usize];
                    let val2 = self.registers[src2.0 as usize];
                    self.registers[dst.0 as usize] = val1 - val2;
                }
            }

            Opcode::IntMul => {
                if let Operands::RRR { dst, src1, src2 } = &instruction.operands {
                    let val1 = self.registers[src1.0 as usize];
                    let val2 = self.registers[src2.0 as usize];
                    self.registers[dst.0 as usize] = val1 * val2;
                }
            }

            Opcode::IntDiv => {
                if let Operands::RRR { dst, src1, src2 } = &instruction.operands {
                    let val1 = self.registers[src1.0 as usize];
                    let val2 = self.registers[src2.0 as usize];
                    if val2 == 0 {
                        return Err("Division by zero".to_string());
                    }
                    self.registers[dst.0 as usize] = val1 / val2;
                }
            }

            Opcode::IntMod => {
                if let Operands::RRR { dst, src1, src2 } = &instruction.operands {
                    let val1 = self.registers[src1.0 as usize];
                    let val2 = self.registers[src2.0 as usize];
                    if val2 == 0 {
                        return Err("Division by zero in modulo".to_string());
                    }
                    self.registers[dst.0 as usize] = val1 % val2;
                }
            }

            Opcode::IntNeg => {
                if let Operands::RR { dst, src } = &instruction.operands {
                    let val = self.registers[src.0 as usize];
                    self.registers[dst.0 as usize] = -val;
                }
            }

            Opcode::IntAddImm8 => {
                if let Operands::RI8 { dst, src, imm } = &instruction.operands {
                    let val = self.registers[src.0 as usize];
                    self.registers[dst.0 as usize] = val + *imm as i64;
                }
            }

            Opcode::IntSubImm8 => {
                if let Operands::RI8 { dst, src, imm } = &instruction.operands {
                    let val = self.registers[src.0 as usize];
                    self.registers[dst.0 as usize] = val - *imm as i64;
                }
            }

            Opcode::IntMulImm8 => {
                if let Operands::RI8 { dst, src, imm } = &instruction.operands {
                    let val = self.registers[src.0 as usize];
                    self.registers[dst.0 as usize] = val * *imm as i64;
                }
            }

            // COMPARISON OPERATIONS
            Opcode::IntEq => {
                if let Operands::RRR { dst, src1, src2 } = &instruction.operands {
                    let val1 = self.registers[src1.0 as usize];
                    let val2 = self.registers[src2.0 as usize];
                    self.registers[dst.0 as usize] = if val1 == val2 { 1 } else { 0 };
                }
            }

            Opcode::IntNe => {
                if let Operands::RRR { dst, src1, src2 } = &instruction.operands {
                    let val1 = self.registers[src1.0 as usize];
                    let val2 = self.registers[src2.0 as usize];
                    self.registers[dst.0 as usize] = if val1 != val2 { 1 } else { 0 };
                }
            }

            Opcode::IntLt => {
                if let Operands::RRR { dst, src1, src2 } = &instruction.operands {
                    let val1 = self.registers[src1.0 as usize];
                    let val2 = self.registers[src2.0 as usize];
                    self.registers[dst.0 as usize] = if val1 < val2 { 1 } else { 0 };
                }
            }

            Opcode::IntLe => {
                if let Operands::RRR { dst, src1, src2 } = &instruction.operands {
                    let val1 = self.registers[src1.0 as usize];
                    let val2 = self.registers[src2.0 as usize];
                    self.registers[dst.0 as usize] = if val1 <= val2 { 1 } else { 0 };
                }
            }

            Opcode::IntGt => {
                if let Operands::RRR { dst, src1, src2 } = &instruction.operands {
                    let val1 = self.registers[src1.0 as usize];
                    let val2 = self.registers[src2.0 as usize];
                    self.registers[dst.0 as usize] = if val1 > val2 { 1 } else { 0 };
                }
            }

            Opcode::IntGe => {
                if let Operands::RRR { dst, src1, src2 } = &instruction.operands {
                    let val1 = self.registers[src1.0 as usize];
                    let val2 = self.registers[src2.0 as usize];
                    self.registers[dst.0 as usize] = if val1 >= val2 { 1 } else { 0 };
                }
            }

            // BITWISE OPERATIONS
            Opcode::And => {
                if let Operands::RR { dst, src } = &instruction.operands {
                    let dst_val = self.registers[dst.0 as usize];
                    let src_val = self.registers[src.0 as usize];
                    self.registers[dst.0 as usize] = dst_val & src_val;
                }
            }

            Opcode::Or => {
                if let Operands::RR { dst, src } = &instruction.operands {
                    let dst_val = self.registers[dst.0 as usize];
                    let src_val = self.registers[src.0 as usize];
                    self.registers[dst.0 as usize] = dst_val | src_val;
                }
            }

            Opcode::Xor => {
                if let Operands::RR { dst, src } = &instruction.operands {
                    let dst_val = self.registers[dst.0 as usize];
                    let src_val = self.registers[src.0 as usize];
                    self.registers[dst.0 as usize] = dst_val ^ src_val;
                }
            }

            Opcode::Not => {
                if let Operands::RR { dst, src } = &instruction.operands {
                    let src_val = self.registers[src.0 as usize];
                    self.registers[dst.0 as usize] = !src_val;
                }
            }

            Opcode::Shl => {
                if let Operands::RR { dst, src } = &instruction.operands {
                    let dst_val = self.registers[dst.0 as usize];
                    let src_val = self.registers[src.0 as usize];
                    self.registers[dst.0 as usize] = dst_val << src_val;
                }
            }

            Opcode::Shr => {
                if let Operands::RR { dst, src } = &instruction.operands {
                    let dst_val = self.registers[dst.0 as usize];
                    let src_val = self.registers[src.0 as usize];
                    self.registers[dst.0 as usize] = dst_val >> src_val;
                }
            }

            // MOVE OPERATIONS
            Opcode::Move => {
                if let Operands::RR { dst, src } = &instruction.operands {
                    self.registers[dst.0 as usize] = self.registers[src.0 as usize];
                }
            }

            Opcode::MoveZero => {
                if let Operands::RR { dst, src: _ } = &instruction.operands {
                    self.registers[dst.0 as usize] = 0;
                }
            }

            Opcode::MoveOne => {
                if let Operands::RR { dst, src: _ } = &instruction.operands {
                    self.registers[dst.0 as usize] = 1;
                }
            }

            Opcode::MoveMinusOne => {
                if let Operands::RR { dst, src: _ } = &instruction.operands {
                    self.registers[dst.0 as usize] = -1;
                }
            }

            Opcode::MoveImm8 => {
                if let Operands::RI8 { dst, src: _, imm } = &instruction.operands {
                    self.registers[dst.0 as usize] = *imm as i64;
                }
            }

            Opcode::MoveImm16 => {
                if let Operands::RI16 { dst, src: _, imm } = &instruction.operands {
                    self.registers[dst.0 as usize] = *imm as i64;
                }
            }

            Opcode::MoveImm32 => {
                if let Operands::RI64 { dst, imm } = &instruction.operands {
                    self.registers[dst.0 as usize] = *imm;
                }
            }

            Opcode::MoveImm64 => {
                if let Operands::RI64 { dst, imm } = &instruction.operands {
                    self.registers[dst.0 as usize] = *imm;
                }
            }

            Opcode::MoveTrue => {
                if let Operands::RR { dst, src: _ } = &instruction.operands {
                    self.registers[dst.0 as usize] = 1;
                }
            }

            Opcode::MoveFalse => {
                if let Operands::RR { dst, src: _ } = &instruction.operands {
                    self.registers[dst.0 as usize] = 0;
                }
            }

            // CONTROL FLOW
            Opcode::Return => {
                self.running = false;
            }

            Opcode::Jump => {
                if let Operands::Jump { offset } = &instruction.operands {
                    // Jump is relative to the next instruction (after this one)
                    let next_pc = self.pc + instruction.size;
                    let new_pc = (next_pc as i32 + *offset as i32) as usize;
                    if new_pc < self.bytecode.len() {
                        self.pc = new_pc;
                        return Ok(()); // Don't advance PC normally
                    } else {
                        return Err("Jump out of bounds".to_string());
                    }
                }
            }

            Opcode::JumpIf => {
                if let Operands::Branch { cond, offset } = &instruction.operands {
                    let cond_val = self.registers[cond.0 as usize];
                    if cond_val != 0 {
                        // if true
                        let next_pc = self.pc + instruction.size;
                        let new_pc = (next_pc as i32 + *offset as i32) as usize;
                        if new_pc < self.bytecode.len() {
                            self.pc = new_pc;
                            return Ok(()); // Don't advance PC normally
                        } else {
                            return Err("Conditional jump out of bounds".to_string());
                        }
                    }
                }
            }

            Opcode::JumpIfNot => {
                if let Operands::Branch { cond, offset } = &instruction.operands {
                    let cond_val = self.registers[cond.0 as usize];
                    if cond_val == 0 {
                        // if false
                        let next_pc = self.pc + instruction.size;
                        let new_pc = (next_pc as i32 + *offset as i32) as usize;
                        if new_pc < self.bytecode.len() {
                            self.pc = new_pc;
                            return Ok(()); // Don't advance PC normally
                        } else {
                            return Err("Conditional jump out of bounds".to_string());
                        }
                    }
                }
            }

            // FUNCTION CALLS
            Opcode::Call => {
                // Placeholder for function calls - return a default value for now
                // This is the issue that was causing inconsistent behavior
                if let Operands::Call {
                    func_id: _,
                    argc: _,
                    args: _,
                } = &instruction.operands
                {
                    // Just return a placeholder value since we can't execute function calls yet
                    // This prevents the program from crashing inconsistently
                    self.registers[0] = 0; // Put result in R0
                    // We should properly handle the call but for now just continue
                    eprintln!("Warning: Function calls not implemented, returning 0");
                }
            }

            // GC OPERATIONS
            Opcode::GCSafepoint => {
                // Just a placeholder - in real implementation, this would check for GC
            }

            // MEMORY OPERATIONS (simplified)
            Opcode::LoadLocal8
            | Opcode::LoadLocal16
            | Opcode::LoadLocal32
            | Opcode::LoadLocal64
            | Opcode::LoadLocalPtr => {
                // Simplified handling - these need proper stack implementation
                return Err("LoadLocal operations not fully implemented".to_string());
            }
            Opcode::StoreLocal8
            | Opcode::StoreLocal16
            | Opcode::StoreLocal32
            | Opcode::StoreLocal64
            | Opcode::StoreLocalPtr => {
                // Simplified handling - these need proper stack implementation
                return Err("StoreLocal operations not fully implemented".to_string());
            }

            _ => {
                // For unimplemented opcodes, return an error to make debugging clearer
                return Err(format!("Unimplemented opcode: {:?}", instruction.opcode));
            }
        }

        // Advance PC to next instruction
        self.pc += instruction.size;
        Ok(())
    }
}

/// Convenience function to run bytecode directly
pub fn run_bytecode(
    bytecode: &[u8],
    function_metadata: &[FunctionMetadata],
    constant_pool: Option<&[u8]>,
    struct_layouts: Option<&[StructLayout]>,
    enum_layouts: Option<&[EnumLayout]>,
) -> Result<i64, String> {
    let mut vm = VM::from_slices(
        bytecode,
        function_metadata,
        constant_pool,
        struct_layouts,
        enum_layouts,
    );

    vm.run()
}
