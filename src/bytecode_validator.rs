use crate::vm::{DecodeError, DecodedInstruction, Opcode, Operands};

pub struct BytecodeValidator {
    pub errors: Vec<String>,
}

impl Default for BytecodeValidator {
    fn default() -> Self {
        Self::new()
    }
}

impl BytecodeValidator {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Validates the entire bytecode by attempting to decode all instructions
    pub fn validate_bytecode(
        &mut self,
        bytecode: &[u8],
        functions: &[crate::vm::FunctionMetadata],
    ) -> bool {
        // Validate each function's bytecode section
        for (func_idx, func) in functions.iter().enumerate() {
            let start = func.bytecode_offset as usize;
            let end = (func.bytecode_offset + func.bytecode_length) as usize;

            if end > bytecode.len() {
                self.errors.push(format!(
                    "Function {} has invalid bytecode range: {}-{} (bytecode only {} bytes)",
                    func_idx,
                    start,
                    end,
                    bytecode.len()
                ));
                continue;
            }

            if !self.validate_function_bytecode(&bytecode[start..end], start, func_idx) {
                // Error was already added in validate_function_bytecode
                continue;
            }
        }

        self.errors.is_empty()
    }

    /// Validates a single function's bytecode, returns true if valid
    fn validate_function_bytecode(
        &mut self,
        func_bytecode: &[u8],
        base_offset: usize,
        func_idx: usize,
    ) -> bool {
        let mut pc = 0;
        while pc < func_bytecode.len() {
            match self.decode_instruction_safe(func_bytecode, pc) {
                Ok(instruction) => {
                    // Validate the instruction makes sense
                    if !self.validate_instruction(&instruction, pc + base_offset) {
                        self.errors.push(format!(
                            "Function {}: Invalid instruction at offset 0x{:04X}",
                            func_idx,
                            pc + base_offset
                        ));
                        return false;
                    }
                    pc += instruction.size;
                }
                Err(e) => {
                    // Add the decoding error to our error list
                    self.errors.push(format!(
                        "Function {}: Invalid instruction at offset 0x{:04X}: {:?}",
                        func_idx,
                        pc + base_offset,
                        e
                    ));
                    return false;
                }
            }
        }
        true
    }

    /// Safely decodes an instruction, checking bounds - using same logic as disassembler
    fn decode_instruction_safe(
        &self,
        bytecode: &[u8],
        pc: usize,
    ) -> Result<DecodedInstruction, DecodeError> {
        if pc >= bytecode.len() {
            return Err(DecodeError::InvalidOpcode(0xFF)); // End of bytecode
        }

        let opcode_byte = bytecode[pc];

        // Match the implementation from disassembler.rs and VM decode
        match opcode_byte {
            // INTEGER ARITHMETIC (0x00-0x0F)
            0x00 => {
                // IntAdd
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntAdd,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x01 => {
                // IntSub
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntSub,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x02 => {
                // IntMul
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntMul,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x03 => {
                // IntDiv
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntDiv,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x04 => {
                // IntMod
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntMod,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x05 => {
                // IntNeg
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntNeg,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x06 => {
                // IntAddImm8
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntAddImm8,
                    operands: Operands::RI8 {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]), // placeholder
                        imm: bytecode[pc + 3] as i8,
                    },
                    size: 4,
                })
            }
            0x07 => {
                // IntSubImm8
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntSubImm8,
                    operands: Operands::RI8 {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]), // placeholder
                        imm: bytecode[pc + 3] as i8,
                    },
                    size: 4,
                })
            }
            0x08 => {
                // IntMulImm8
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntMulImm8,
                    operands: Operands::RI8 {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]), // placeholder
                        imm: bytecode[pc + 3] as i8,
                    },
                    size: 4,
                })
            }
            0x09 => {
                // IntEqZero
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntEqZero,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x0A => {
                // IntNeZero
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntNeZero,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x0B => {
                // IntMin
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntMin,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x0C => {
                // IntMax
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntMax,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x0D => {
                // IntAbs
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntAbs,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // FLOAT ARITHMETIC (0x10-0x1F)
            0x10 => {
                // FloatAdd
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatAdd,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x11 => {
                // FloatSub
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatSub,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x12 => {
                // FloatMul
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatMul,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x13 => {
                // FloatDiv
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatDiv,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x14 => {
                // FloatNeg
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatNeg,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x15 => {
                // FloatPow
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatPow,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x16 => {
                // FloatSqrt
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatSqrt,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x17 => {
                // FloatFloor
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatFloor,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x18 => {
                // FloatCeil
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatCeil,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x19 => {
                // FloatRound
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatRound,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x1A => {
                // FloatMin
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatMin,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x1B => {
                // FloatMax
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatMax,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x1C => {
                // FloatAbs
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatAbs,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // COMPARISONS (0x20-0x2F)
            0x20 => {
                // IntEq
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntEq,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x21 => {
                // IntNe
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntNe,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x22 => {
                // IntLt
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntLt,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x23 => {
                // IntLe
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntLe,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x24 => {
                // IntGt
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntGt,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x25 => {
                // IntGe
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntGe,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x26 => {
                // IntLtU
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntLtU,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x27 => {
                // IntLeU
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntLeU,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x28 => {
                // IntGtU
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntGtU,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x29 => {
                // IntGeU
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntGeU,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x2A => {
                // FloatEq
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatEq,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x2B => {
                // FloatNe
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatNe,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x2C => {
                // FloatLt
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatLt,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x2D => {
                // FloatLe
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatLe,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x2E => {
                // FloatGt
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatGt,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x2F => {
                // FloatGe
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatGe,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }

            // BITWISE & LOGICAL (0x30-0x3F)
            0x30 => {
                // And
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::And,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x31 => {
                // Or
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Or,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x32 => {
                // Xor
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Xor,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x33 => {
                // Not
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Not,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x34 => {
                // Shl
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Shl,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x35 => {
                // Shr
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Shr,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x36 => {
                // Sar
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Sar,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x37 => {
                // Rotl
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Rotl,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x38 => {
                // Rotr
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Rotr,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x39 => {
                // Clz
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Clz,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x3A => {
                // Ctz
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Ctz,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x3B => {
                // Popcnt
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Popcnt,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // CONVERSION (0x40-0x4F)
            0x40 => {
                // IntToFloat
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntToFloat,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x41 => {
                // FloatToInt
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatToInt,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x42 => {
                // IntToString
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntToString,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x43 => {
                // FloatToString
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatToString,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x44 => {
                // BoolToString
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::BoolToString,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x45 => {
                // StringToInt
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StringToInt,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x46 => {
                // StringToFloat
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StringToFloat,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x47 => {
                // ReinterpretIntAsFloat
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ReinterpretIntAsFloat,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x48 => {
                // ReinterpretFloatAsInt
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ReinterpretFloatAsInt,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // MEMORY - LOADS (0x50-0x5F)
            0x50 => {
                // LoadLocal8
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadLocal8,
                    operands: Operands::LoadLocal {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x51 => {
                // LoadLocal16
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadLocal16,
                    operands: Operands::LoadLocal {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x52 => {
                // LoadLocal32
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadLocal32,
                    operands: Operands::LoadLocal {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x53 => {
                // LoadLocal64
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadLocal64,
                    operands: Operands::LoadLocal {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x54 => {
                // LoadLocalPtr
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadLocalPtr,
                    operands: Operands::LoadLocal {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x55 => {
                // LoadGlobal
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadGlobal,
                    operands: Operands::LoadLocal {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x56 => {
                // LoadConst
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadConst,
                    operands: Operands::LoadLocal {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x57 => {
                // LoadField8
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadField8,
                    operands: Operands::LoadField {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        ptr: crate::vm::Reg(bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x58 => {
                // LoadField16
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadField16,
                    operands: Operands::LoadField {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        ptr: crate::vm::Reg(bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x59 => {
                // LoadField32
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadField32,
                    operands: Operands::LoadField {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        ptr: crate::vm::Reg(bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x5A => {
                // LoadField64
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadField64,
                    operands: Operands::LoadField {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        ptr: crate::vm::Reg(bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x5B => {
                // LoadFieldPtr
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadFieldPtr,
                    operands: Operands::LoadField {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        ptr: crate::vm::Reg(bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x5C => {
                // LoadIndirect
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadIndirect,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // MEMORY - STORES (0x60-0x6F)
            0x60 => {
                // StoreLocal8
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreLocal8,
                    operands: Operands::StoreLocal {
                        src: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x61 => {
                // StoreLocal16
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreLocal16,
                    operands: Operands::StoreLocal {
                        src: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x62 => {
                // StoreLocal32
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreLocal32,
                    operands: Operands::StoreLocal {
                        src: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x63 => {
                // StoreLocal64
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreLocal64,
                    operands: Operands::StoreLocal {
                        src: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x64 => {
                // StoreLocalPtr
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreLocalPtr,
                    operands: Operands::StoreLocal {
                        src: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x65 => {
                // StoreGlobal
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreGlobal,
                    operands: Operands::StoreLocal {
                        src: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x66 => {
                // StoreField8
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreField8,
                    operands: Operands::LoadField {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        ptr: crate::vm::Reg(bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x67 => {
                // StoreField16
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreField16,
                    operands: Operands::LoadField {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        ptr: crate::vm::Reg(bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x68 => {
                // StoreField32
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreField32,
                    operands: Operands::LoadField {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        ptr: crate::vm::Reg(bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x69 => {
                // StoreField64
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreField64,
                    operands: Operands::LoadField {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        ptr: crate::vm::Reg(bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x6A => {
                // StoreFieldPtr
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreFieldPtr,
                    operands: Operands::LoadField {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        ptr: crate::vm::Reg(bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x6B => {
                // StoreIndirect
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreIndirect,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // ARRAYS & STRINGS (0x70-0x7F)
            0x70 => {
                // ArrayNew
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayNew,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x71 => {
                // ArrayLoad8
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayLoad8,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x72 => {
                // ArrayLoad16
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayLoad16,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x73 => {
                // ArrayLoad32
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayLoad32,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x74 => {
                // ArrayLoad64
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayLoad64,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x75 => {
                // ArrayLoadPtr
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayLoadPtr,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x76 => {
                // ArrayStore8
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayStore8,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x77 => {
                // ArrayStore16
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayStore16,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x78 => {
                // ArrayStore32
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayStore32,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x79 => {
                // ArrayStore64
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayStore64,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x7A => {
                // ArrayStorePtr
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayStorePtr,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x7B => {
                // ArrayLen
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayLen,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x7C => {
                // StringConcat
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StringConcat,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x7D => {
                // StringLen
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StringLen,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x7E => {
                // StringSlice
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StringSlice,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }

            // CONSTRUCTION (0x80-0x8F)
            0x80 => {
                // StructNew
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StructNew,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x81 => {
                // EnumNew
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::EnumNew,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x82 => {
                // TupleNew
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::TupleNew,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x83 => {
                // StructNew8
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StructNew8,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x84 => {
                // StructNew16
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StructNew16,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x85 => {
                // StructNew24
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StructNew24,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x86 => {
                // StructNew32
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StructNew32,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }

            // CONTROL FLOW (0x90-0x9F)
            0x90 => {
                // Jump
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = i16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::Jump,
                    operands: Operands::Jump { offset },
                    size: 3,
                })
            }
            0x91 => {
                // JumpLong
                if pc + 4 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = i32::from_le_bytes([
                    bytecode[pc + 1],
                    bytecode[pc + 2],
                    bytecode[pc + 3],
                    bytecode[pc + 4],
                ]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpLong,
                    operands: Operands::Jump {
                        offset: offset as i16, // Using i16 for compatibility
                    },
                    size: 5,
                })
            }
            0x92 => {
                // JumpIf
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = i16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpIf,
                    operands: Operands::Branch {
                        cond: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x93 => {
                // JumpIfNot
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = i16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpIfNot,
                    operands: Operands::Branch {
                        cond: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x94 => {
                // TableSwitch
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let _offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                // This is a complex instruction with variable length, for now just validate basic format
                Ok(DecodedInstruction {
                    opcode: Opcode::TableSwitch,
                    operands: Operands::LoadLocal {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        offset: 0, // placeholder
                    },
                    size: 3,
                })
            }
            0x95 => {
                // LookupSwitch
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let _offset = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LookupSwitch,
                    operands: Operands::LoadLocal {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        offset: 0, // placeholder
                    },
                    size: 3,
                })
            }
            0x96 => {
                // JumpIfEqZero
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = i16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpIfEqZero,
                    operands: Operands::Branch {
                        cond: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x97 => {
                // JumpIfNeZero
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = i16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpIfNeZero,
                    operands: Operands::Branch {
                        cond: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x98 => {
                // JumpIfLtZero
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = i16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpIfLtZero,
                    operands: Operands::Branch {
                        cond: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x99 => {
                // JumpIfGeZero
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = i16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpIfGeZero,
                    operands: Operands::Branch {
                        cond: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }

            // FUNCTION CALLS (0xA0-0xAF)
            0xA0 => {
                // Call
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let func_id = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                let argc = bytecode[pc + 3];

                // Read the argument registers after the header
                if pc + 4 + argc as usize > bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }

                let mut args = Vec::new();
                for i in 0..argc {
                    args.push(crate::vm::Reg(bytecode[pc + 4 + i as usize]));
                }

                Ok(DecodedInstruction {
                    opcode: Opcode::Call,
                    operands: Operands::Call {
                        func_id,
                        argc,
                        args,
                    },
                    size: 4 + argc as usize,
                })
            }
            0xA1 => {
                // CallTail
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let func_id = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                let argc = bytecode[pc + 3];

                // Read the argument registers after the header
                if pc + 4 + argc as usize > bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }

                let mut args = Vec::new();
                for i in 0..argc {
                    args.push(crate::vm::Reg(bytecode[pc + 4 + i as usize]));
                }

                Ok(DecodedInstruction {
                    opcode: Opcode::CallTail,
                    operands: Operands::Call {
                        func_id,
                        argc,
                        args,
                    },
                    size: 4 + argc as usize,
                })
            }
            0xA2 => {
                // CallIndirect
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let func_id = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                let argc = bytecode[pc + 3];

                // Read the argument registers after the header
                if pc + 4 + argc as usize > bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }

                let mut args = Vec::new();
                for i in 0..argc {
                    args.push(crate::vm::Reg(bytecode[pc + 4 + i as usize]));
                }

                Ok(DecodedInstruction {
                    opcode: Opcode::CallIndirect,
                    operands: Operands::Call {
                        func_id,
                        argc,
                        args,
                    },
                    size: 4 + argc as usize,
                })
            }
            0xA3 => {
                // CallBuiltin
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let func_id = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                let argc = bytecode[pc + 3];

                // Read the argument registers after the header
                if pc + 4 + argc as usize > bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }

                let mut args = Vec::new();
                for i in 0..argc {
                    args.push(crate::vm::Reg(bytecode[pc + 4 + i as usize]));
                }

                Ok(DecodedInstruction {
                    opcode: Opcode::CallBuiltin,
                    operands: Operands::Call {
                        func_id,
                        argc,
                        args,
                    },
                    size: 4 + argc as usize,
                })
            }
            0xA4 => {
                // Return
                Ok(DecodedInstruction {
                    opcode: Opcode::Return,
                    operands: Operands::None,
                    size: 1,
                })
            }
            0xA5 => {
                // ReturnVoid
                Ok(DecodedInstruction {
                    opcode: Opcode::ReturnVoid,
                    operands: Operands::None,
                    size: 1,
                })
            }
            0xA6 => {
                // CallKnownTail
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let func_id = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                let argc = bytecode[pc + 3];

                // Read the argument registers after the header
                if pc + 4 + argc as usize > bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }

                let mut args = Vec::new();
                for i in 0..argc {
                    args.push(crate::vm::Reg(bytecode[pc + 4 + i as usize]));
                }

                Ok(DecodedInstruction {
                    opcode: Opcode::CallKnownTail,
                    operands: Operands::Call {
                        func_id,
                        argc,
                        args,
                    },
                    size: 4 + argc as usize,
                })
            }

            // EFFECTS (0xB0-0xBF)
            0xB0 => {
                // Perform
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Perform,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xB1 => {
                // Resume
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Resume,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xB2 => {
                // PushHandler
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let offset = i16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::PushHandler,
                    operands: Operands::Branch {
                        cond: crate::vm::Reg(bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }
            0xB3 => {
                // PopHandler
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::PopHandler,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xB4 => {
                // CaptureContinuation
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::CaptureContinuation,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xB5 => {
                // RestoreContinuation
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::RestoreContinuation,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }

            // REGISTER MOVES (0xC0-0xCF)
            0xC0 => {
                // Move
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Move,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xC1 => {
                // MoveImm8
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveImm8,
                    operands: Operands::RI8 {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // Actually immediate, but using src as placeholder
                        imm: bytecode[pc + 2] as i8,
                    },
                    size: 3,
                })
            }
            0xC2 => {
                // MoveImm16
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let imm = i16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveImm16,
                    operands: Operands::RI16 {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                        imm,
                    },
                    size: 4,
                })
            }
            0xC3 => {
                // MoveImm32
                if pc + 5 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let imm = i32::from_le_bytes([
                    bytecode[pc + 2],
                    bytecode[pc + 3],
                    bytecode[pc + 4],
                    bytecode[pc + 5],
                ]);
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveImm32,
                    operands: Operands::RI64 {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        imm: imm as i64,
                    },
                    size: 6,
                })
            }
            0xC4 => {
                // MoveImm64
                if pc + 9 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let imm = i64::from_le_bytes([
                    bytecode[pc + 2],
                    bytecode[pc + 3],
                    bytecode[pc + 4],
                    bytecode[pc + 5],
                    bytecode[pc + 6],
                    bytecode[pc + 7],
                    bytecode[pc + 8],
                    bytecode[pc + 9],
                ]);
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveImm64,
                    operands: Operands::RI64 {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        imm,
                    },
                    size: 10,
                })
            }
            0xC5 => {
                // MoveZero
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveZero,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0),
                    },
                    size: 2,
                })
            }
            0xC6 => {
                // MoveOne
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveOne,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0),
                    },
                    size: 2,
                })
            }
            0xC7 => {
                // MoveMinusOne
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveMinusOne,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0),
                    },
                    size: 2,
                })
            }
            0xC8 => {
                // MoveTrue
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveTrue,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0),
                    },
                    size: 2,
                })
            }
            0xC9 => {
                // MoveFalse
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveFalse,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0),
                    },
                    size: 2,
                })
            }
            0xCA => {
                // MoveNull
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveNull,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0),
                    },
                    size: 2,
                })
            }
            0xCB => {
                // Select
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Select,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }

            // GC OPERATIONS (0xD0-0xDF)
            0xD0 => {
                // GCAllocFast8
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::GCAllocFast8,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xD1 => {
                // GCAllocFast16
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::GCAllocFast16,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xD2 => {
                // GCAllocFast24
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::GCAllocFast24,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xD3 => {
                // GCAllocFast32
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::GCAllocFast32,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xD4 => {
                // GCAllocFast64
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::GCAllocFast64,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xD5 => {
                // GCAllocSlow
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::GCAllocSlow,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xD6 => {
                // GCSafepoint
                Ok(DecodedInstruction {
                    opcode: Opcode::GCSafepoint,
                    operands: Operands::None,
                    size: 1,
                })
            }
            0xD7 => {
                // WriteBarrier
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::WriteBarrier,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xD8 => {
                // ReadBarrier
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ReadBarrier,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xD9 => {
                // DeclareRoot
                if pc + 1 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::DeclareRoot,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(0), // placeholder
                    },
                    size: 2,
                })
            }

            // ATOMICS (0xE0-0xEF)
            0xE0 => {
                // AtomicLoad
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicLoad,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE1 => {
                // AtomicStore
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicStore,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE2 => {
                // AtomicCAS
                if pc + 3 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicCAS,
                    operands: Operands::RRR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src1: crate::vm::Reg(bytecode[pc + 2]),
                        src2: crate::vm::Reg(bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0xE3 => {
                // AtomicFetchAdd
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicFetchAdd,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE4 => {
                // AtomicFetchSub
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicFetchSub,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE5 => {
                // AtomicFetchAnd
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicFetchAnd,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE6 => {
                // AtomicFetchOr
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicFetchOr,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE7 => {
                // AtomicFetchXor
                if pc + 2 >= bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicFetchXor,
                    operands: Operands::RR {
                        dst: crate::vm::Reg(bytecode[pc + 1]),
                        src: crate::vm::Reg(bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE8 => {
                // FenceAcquire
                Ok(DecodedInstruction {
                    opcode: Opcode::FenceAcquire,
                    operands: Operands::None,
                    size: 1,
                })
            }
            0xE9 => {
                // FenceRelease
                Ok(DecodedInstruction {
                    opcode: Opcode::FenceRelease,
                    operands: Operands::None,
                    size: 1,
                })
            }
            0xEA => {
                // FenceSeqCst
                Ok(DecodedInstruction {
                    opcode: Opcode::FenceSeqCst,
                    operands: Operands::None,
                    size: 1,
                })
            }

            // TRAPS (0xF0-0xF7)
            0xF0 => {
                // TrapBoundsCheck
                Ok(DecodedInstruction {
                    opcode: Opcode::TrapBoundsCheck,
                    operands: Operands::None,
                    size: 1,
                })
            }
            0xF1 => {
                // TrapDivByZero
                Ok(DecodedInstruction {
                    opcode: Opcode::TrapDivByZero,
                    operands: Operands::None,
                    size: 1,
                })
            }
            0xF2 => {
                // TrapNullCheck
                Ok(DecodedInstruction {
                    opcode: Opcode::TrapNullCheck,
                    operands: Operands::None,
                    size: 1,
                })
            }
            0xF3 => {
                // TrapOverflow
                Ok(DecodedInstruction {
                    opcode: Opcode::TrapOverflow,
                    operands: Operands::None,
                    size: 1,
                })
            }
            0xF4 => {
                // Unreachable
                Ok(DecodedInstruction {
                    opcode: Opcode::Unreachable,
                    operands: Operands::None,
                    size: 1,
                })
            }
            0xF5 => {
                // DebugTrap
                Ok(DecodedInstruction {
                    opcode: Opcode::DebugTrap,
                    operands: Operands::None,
                    size: 1,
                })
            }
            0xF6 => {
                // ProfileEnter
                Ok(DecodedInstruction {
                    opcode: Opcode::ProfileEnter,
                    operands: Operands::None,
                    size: 1,
                })
            }
            0xF7 => {
                // ProfileExit
                Ok(DecodedInstruction {
                    opcode: Opcode::ProfileExit,
                    operands: Operands::None,
                    size: 1,
                })
            }

            // EXTENDED OPCODES (0xFF)
            0xFF => {
                if pc + 1 >= bytecode.len() {
                    // Incomplete extended opcode at end of bytecode.
                    // This could be register 255 (0xFF) being misinterpreted as opcode,
                    // or garbage/padding data. Since disassembler works, treat as warning rather than error.
                    return Err(DecodeError::InvalidOpcode(opcode_byte));
                }
                let ext_opcode = bytecode[pc + 1];
                // For extended opcodes, validate that the ext_opcode is supported and check following bytes
                match ext_opcode {
                    // SIMD - i32x4 (0x00-0x1F)
                    0x00..=0x0E => {
                        if pc + 4 >= bytecode.len() {
                            return Err(DecodeError::InvalidOpcode(opcode_byte));
                        }
                        Ok(DecodedInstruction {
                            opcode: Opcode::ExtendedOp,
                            operands: Operands::RRR {
                                dst: crate::vm::Reg(bytecode[pc + 2]),
                                src1: crate::vm::Reg(bytecode[pc + 3]),
                                src2: crate::vm::Reg(bytecode[pc + 4]),
                            },
                            size: 5,
                        })
                    }
                    // SIMD - f32x4 (0x20-0x3F)
                    0x20..=0x2A => {
                        if pc + 4 >= bytecode.len() {
                            return Err(DecodeError::InvalidOpcode(opcode_byte));
                        }
                        Ok(DecodedInstruction {
                            opcode: Opcode::ExtendedOp,
                            operands: Operands::RRR {
                                dst: crate::vm::Reg(bytecode[pc + 2]),
                                src1: crate::vm::Reg(bytecode[pc + 3]),
                                src2: crate::vm::Reg(bytecode[pc + 4]),
                            },
                            size: 5,
                        })
                    }
                    // Handle unknown extended opcodes
                    _ => {
                        // Unknown extended opcode - but since the disassembler works,
                        // this might be valid bytecode we don't fully understand yet.
                        Err(DecodeError::InvalidOpcode(opcode_byte))
                    }
                }
            }

            // Unknown opcode
            _ => Err(DecodeError::InvalidOpcode(opcode_byte)),
        }
    }

    /// Validates a single decoded instruction for semantic correctness
    fn validate_instruction(&self, instruction: &DecodedInstruction, _offset: usize) -> bool {
        // In this version, we just validate that the decoded instruction is valid
        // Additional semantic validation could be added here
        match &instruction.operands {
            Operands::RRR {
                dst: _,
                src1: _,
                src2: _,
            } => {
                // All registers should be valid (in the expected range)
                // For now, we'll accept any register value since the VM supports up to 256 registers
                true
            }
            Operands::RR { dst: _, src: _ } => {
                // Register values should be reasonable
                true
            }
            Operands::RI8 { .. } => {
                // Immediate 8-bit values are always valid
                true
            }
            Operands::RI16 { .. } => {
                // Immediate 16-bit values are always valid
                true
            }
            Operands::RI64 { .. } => {
                // Immediate 64-bit values are always valid
                true
            }
            Operands::LoadLocal { .. } => {
                // Offset values are always valid in our VM model
                true
            }
            Operands::StoreLocal { .. } => {
                // Offset values are always valid in our VM model
                true
            }
            Operands::LoadField { .. } => {
                // Field offsets are always valid in our VM model
                true
            }
            Operands::Jump { offset: _ } => {
                // Jump offsets are relative and can be valid in any direction
                true
            }
            Operands::Branch { cond: _, offset: _ } => {
                // Jump offsets are relative and can be valid in any direction
                true
            }
            Operands::Call {
                func_id: _,
                argc,
                args,
            } => {
                // Function ID and argument count should be reasonable
                // For now, we just validate that the number of args matches the argc
                *argc as usize == args.len()
            }
            Operands::None => {
                // No operands, always valid
                true
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_empty_bytecode() {
        let mut validator = BytecodeValidator::new();
        let bytecode = vec![];
        let functions = vec![];

        let result = validator.validate_bytecode(&bytecode, &functions);
        assert!(result);
        assert_eq!(validator.errors.len(), 0);
    }

    #[test]
    fn test_validate_simple_function_bytecode() {
        // Example: MoveZero R0; Return
        // Bytecode: [0xC5, 0x00, 0xA4]
        let mut validator = BytecodeValidator::new();
        let bytecode = vec![0xC5, 0x00, 0xA4]; // MoveZero R0; Return
        let functions = vec![crate::vm::FunctionMetadata {
            bytecode_offset: 0,
            bytecode_length: 3,
            arg_count: 0,
            register_count: 0,
            local_stack_size: 0,
            stack_map_offset: 0,
            stack_map_count: 0,
            max_call_depth: 0,
            _padding: 0,
        }];

        let result = validator.validate_bytecode(&bytecode, &functions);
        assert!(result);
        assert_eq!(validator.errors.len(), 0);
    }

    #[test]
    fn test_validate_invalid_opcode() {
        // Example: Invalid opcode 0xFF as a single byte (not properly handled as extended opcode)
        let mut validator = BytecodeValidator::new();
        let bytecode = vec![0xFF]; // Invalid extended opcode (no extension byte)
        let functions = vec![crate::vm::FunctionMetadata {
            bytecode_offset: 0,
            bytecode_length: 1,
            arg_count: 0,
            register_count: 0,
            local_stack_size: 0,
            stack_map_offset: 0,
            stack_map_count: 0,
            max_call_depth: 0,
            _padding: 0,
        }];

        let result = validator.validate_bytecode(&bytecode, &functions);
        assert!(!result);
        assert!(!validator.errors.is_empty());
    }

    #[test]
    fn test_validate_truncated_instruction() {
        // Example: IntAdd instruction that's truncated (missing final operand)
        let mut validator = BytecodeValidator::new();
        let bytecode = vec![0x00, 0x00, 0x00]; // IntAdd R0, R0, ?? (missing third operand)
        let functions = vec![crate::vm::FunctionMetadata {
            bytecode_offset: 0,
            bytecode_length: 3, // Length of 3 bytes (but IntAdd needs 4)
            arg_count: 0,
            register_count: 0,
            local_stack_size: 0,
            stack_map_offset: 0,
            stack_map_count: 0,
            max_call_depth: 0,
            _padding: 0,
        }];

        let result = validator.validate_bytecode(&bytecode, &functions);
        assert!(!result);
        assert!(!validator.errors.is_empty());
    }

    #[test]
    fn test_validate_arithmetic_bytecode() {
        // Example: IntAdd R0, R0, R0; Return
        // Bytecode: [0x00, 0x00, 0x00, 0x00, 0xA4]
        let mut validator = BytecodeValidator::new();
        let bytecode = vec![0x00, 0x00, 0x00, 0x00, 0xA4]; // IntAdd R0, R0, R0; Return
        let functions = vec![crate::vm::FunctionMetadata {
            bytecode_offset: 0,
            bytecode_length: 5,
            arg_count: 0,
            register_count: 0,
            local_stack_size: 0,
            stack_map_offset: 0,
            stack_map_count: 0,
            max_call_depth: 0,
            _padding: 0,
        }];

        let result = validator.validate_bytecode(&bytecode, &functions);
        assert!(result);
        assert_eq!(validator.errors.len(), 0);
    }
}
