use super::{
    DecodedInstruction, EnumLayout, FunctionMetadata, Opcode, Operands, Reg, StructLayout,
};
use std::fmt::Write;

/// Convenience function to disassemble bytecode and return formatted string
pub fn pretty_dump_bytecode(
    bytecode: &[u8],
    functions: &[FunctionMetadata],
    _constant_pool: Option<&[u8]>,
    _struct_layouts: Option<&[StructLayout]>,
    _enum_layouts: Option<&[EnumLayout]>,
) -> String {
    let mut result = String::new();

    // Add header information
    writeln!(result, "=== Vial VM Bytecode Disassembly ===").unwrap();
    writeln!(result, "Total bytecode size: {} bytes", bytecode.len()).unwrap();
    writeln!(result, "Functions: {}", functions.len()).unwrap();
    writeln!(result).unwrap();

    // Disassemble each function
    for (idx, func) in functions.iter().enumerate() {
        let mut current_offset = func.bytecode_offset as usize;
        let end = (func.bytecode_offset + func.bytecode_length) as usize;

        writeln!(result, "Function #{}:", idx).unwrap();
        writeln!(
            result,
            "  Args: {}, Registers: {}, Local Stack: {}",
            func.arg_count, func.register_count, func.local_stack_size
        )
        .unwrap();
        writeln!(
            result,
            "  Bytecode offset: 0x{:04X}, length: {} bytes",
            func.bytecode_offset, func.bytecode_length
        )
        .unwrap();
        writeln!(result).unwrap();

        // Disassemble instructions in this function
        while current_offset < end && current_offset < bytecode.len() {
            let instruction = match decode_instruction_at(bytecode, current_offset) {
                Ok(instr) => instr,
                Err(e) => {
                    writeln!(
                        result,
                        "ERROR: Failed to decode instruction at offset 0x{:04X}: {:?}",
                        current_offset, e
                    )
                    .unwrap();
                    break;
                }
            };

            let addr = current_offset;
            let formatted = format_instruction(&instruction, addr, bytecode);
            writeln!(result, "  0x{:04X}: {}", addr, formatted).unwrap();

            current_offset += instruction.size;
        }
        writeln!(result).unwrap();
    }

    result
}

fn decode_instruction_at(bytecode: &[u8], offset: usize) -> Result<DecodedInstruction, String> {
    if offset >= bytecode.len() {
        return Err("End of bytecode".to_string());
    }

    let opcode_byte = bytecode[offset];

    let instruction = match opcode_byte {
        // INTEGER ARITHMETIC (0x00-0x0F)
        0x00 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntAdd".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntAdd,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x01 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntSub".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntSub,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x02 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntMul".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntMul,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x03 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntDiv".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntDiv,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x04 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntMod".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntMod,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x05 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for IntNeg".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntNeg,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }

        // IMMEDIATE ARITHMETIC (0x06-0x0A)
        0x06 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntAddImm8".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntAddImm8,
                operands: Operands::RI8 {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                    imm: bytecode[offset + 3] as i8,
                },
                size: 4,
            }
        }
        0x07 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntSubImm8".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntSubImm8,
                operands: Operands::RI8 {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                    imm: bytecode[offset + 3] as i8,
                },
                size: 4,
            }
        }
        0x08 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntMulImm8".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntMulImm8,
                operands: Operands::RI8 {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                    imm: bytecode[offset + 3] as i8,
                },
                size: 4,
            }
        }
        0x09 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for IntEqZero".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntEqZero,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x0A => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for IntNeZero".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntNeZero,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }

        // FLOAT ARITHMETIC (0x10-0x1F)
        0x10 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatAdd".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatAdd,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x11 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatSub".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatSub,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x12 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatMul".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatMul,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x13 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatDiv".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatDiv,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x14 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for FloatNeg".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatNeg,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }

        // COMPARISONS (0x20-0x2F)
        0x20 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntEq".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntEq,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x21 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntNe".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntNe,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x22 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntLt".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntLt,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x24 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntGt".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntGt,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }

        // BITWISE & LOGICAL (0x30-0x3F)
        0x30 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for And".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::And,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x31 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Or".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Or,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x32 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Xor".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Xor,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x33 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Not".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Not,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }

        // MEMORY - LOADS (0x50-0x5F)
        0x50 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for LoadLocal8".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::LoadLocal8,
                operands: Operands::LoadLocal {
                    dst: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3, // Fixed: should be 3 bytes, not 4
            }
        }
        0x51 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for LoadLocal16".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::LoadLocal16,
                operands: Operands::LoadLocal {
                    dst: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3, // Fixed: should be 3 bytes, not 4
            }
        }

        // MEMORY - STORES (0x60-0x6F)
        0x60 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for StoreLocal8".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::StoreLocal8,
                operands: Operands::StoreLocal {
                    src: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3, // Fixed: should be 3 bytes, not 4
            }
        }

        // CONTROL FLOW (0x90-0x9F)
        0x90 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Jump".to_string());
            }
            let jump_offset = i16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::Jump,
                operands: Operands::Jump {
                    offset: jump_offset,
                },
                size: 3,
            }
        }
        0x92 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for JumpIf".to_string());
            }
            let jump_offset = i16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::JumpIf,
                operands: Operands::Branch {
                    cond: Reg(bytecode[offset + 1]),
                    offset: jump_offset,
                },
                size: 4,
            }
        }
        0x93 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for JumpIfNot".to_string());
            }
            let jump_offset = i16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::JumpIfNot,
                operands: Operands::Branch {
                    cond: Reg(bytecode[offset + 1]),
                    offset: jump_offset,
                },
                size: 4,
            }
        }

        // FUNCTION CALLS (0xA0-0xAF)
        0xA0 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for Call".to_string());
            }
            let func_id = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            let argc = bytecode[offset + 3];

            // Read the argument registers after the header
            let mut args = Vec::new();
            for i in 0..argc {
                if offset + 4 + i as usize >= bytecode.len() {
                    break;
                }
                args.push(Reg(bytecode[offset + 4 + i as usize]));
            }

            DecodedInstruction {
                opcode: Opcode::Call,
                operands: Operands::Call {
                    func_id,
                    argc,
                    args,
                },
                size: 4 + argc as usize,
            }
        }
        0xA4 => DecodedInstruction {
            opcode: Opcode::Return,
            operands: Operands::None,
            size: 1,
        },

        // REGISTER MOVES (0xC0-0xCF)
        0xC0 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Move".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Move,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xC1 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for MoveImm8".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::MoveImm8,
                operands: Operands::RI8 {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // Actually immediate, but using src as placeholder
                    imm: bytecode[offset + 2] as i8,
                },
                size: 3,
            }
        }
        0xC4 => {
            if offset + 9 >= bytecode.len() {
                return Err("Not enough bytes for MoveImm64".to_string());
            }
            let imm = i64::from_le_bytes([
                bytecode[offset + 2],
                bytecode[offset + 3],
                bytecode[offset + 4],
                bytecode[offset + 5],
                bytecode[offset + 6],
                bytecode[offset + 7],
                bytecode[offset + 8],
                bytecode[offset + 9],
            ]);
            DecodedInstruction {
                opcode: Opcode::MoveImm64,
                operands: Operands::RI64 {
                    dst: Reg(bytecode[offset + 1]),
                    imm,
                },
                size: 10,
            }
        }
        0xC5 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for MoveZero".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::MoveZero,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0),
                },
                size: 2,
            }
        }
        0xC6 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for MoveOne".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::MoveOne,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0),
                },
                size: 2,
            }
        }

        // GC OPERATIONS (0xD0-0xDF)
        0xD0 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for GCAllocFast8".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::GCAllocFast8,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0xD1 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for GCAllocFast16".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::GCAllocFast16,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }

        // EXTENDED OPCODES (0xFF)
        0xFF => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for ExtendedOp".to_string());
            }
            // For now, let's handle a few extended opcodes
            let ext_opcode = bytecode[offset + 1];
            match ext_opcode {
                0x00 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Add".to_string());
                    }
                    DecodedInstruction {
                        opcode: Opcode::ExtendedOp,
                        operands: Operands::RRR {
                            dst: Reg(bytecode[offset + 2]),
                            src1: Reg(bytecode[offset + 3]),
                            src2: Reg(bytecode[offset + 4]),
                        },
                        size: 5,
                    }
                }
                _ => {
                    return Err(format!("Unknown extended opcode 0x{:02X}", ext_opcode));
                }
            }
        }

        _ => {
            return Err(format!("Unknown opcode 0x{:02X}", opcode_byte));
        }
    };

    Ok(instruction)
}

fn format_instruction(instruction: &DecodedInstruction, addr: usize, bytecode: &[u8]) -> String {
    let mut result = String::new();

    // Add the opcode name
    write!(result, "{:?}", instruction.opcode).unwrap();

    // Add operands based on the operand type
    match &instruction.operands {
        Operands::RRR { dst, src1, src2 } => {
            write!(result, " R{}, R{}, R{}", dst.0, src1.0, src2.0).unwrap();
        }
        Operands::RR { dst, src } => match instruction.opcode {
            Opcode::MoveZero => {
                write!(result, " R{}", dst.0).unwrap();
            }
            Opcode::MoveOne => {
                write!(result, " R{}", dst.0).unwrap();
            }
            _ => {
                write!(result, " R{}, R{}", dst.0, src.0).unwrap();
            }
        },
        Operands::RI8 { dst, src: _, imm } => {
            write!(result, " R{}, {}", dst.0, imm).unwrap(); // src is unused for immediates
        }
        Operands::RI16 { dst, src: _, imm } => {
            write!(result, " R{}, {}", dst.0, imm).unwrap();
        }
        Operands::RI64 { dst, imm } => {
            write!(result, " R{}, 0x{:X}", dst.0, imm).unwrap();
        }
        Operands::LoadLocal { dst, offset } => {
            write!(result, " R{}, [fp+{}]", dst.0, offset).unwrap();
        }
        Operands::StoreLocal { src, offset } => {
            write!(result, " [fp+{}], R{}", offset, src.0).unwrap();
        }
        Operands::LoadField { dst, ptr, offset } => {
            write!(result, " R{}, [R{}+{}]", dst.0, ptr.0, offset).unwrap();
        }
        Operands::Jump { offset } => {
            let target_addr = addr as i32 + instruction.size as i32 + *offset as i32;
            write!(result, " 0x{:04X} (rel: {})", target_addr as usize, offset).unwrap();
        }
        Operands::Branch { cond, offset } => {
            let target_addr = addr as i32 + instruction.size as i32 + *offset as i32;
            write!(
                result,
                " R{}, 0x{:04X} (rel: {})",
                cond.0, target_addr as usize, offset
            )
            .unwrap();
        }
        Operands::Call {
            func_id,
            argc,
            args,
        } => {
            write!(result, " {}, argc={} (", func_id, argc).unwrap();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(result, ", ").unwrap();
                }
                write!(result, "R{}", arg.0).unwrap();
            }
            write!(result, ")").unwrap();
        }
        Operands::None => {
            // No operands
        }
    }

    // Add hex representation of the bytecode
    let end_offset = addr + instruction.size;
    let mut hex_bytes = String::new();
    for i in addr..end_offset {
        if i < bytecode.len() {
            write!(&mut hex_bytes, "{:02X} ", bytecode[i]).unwrap();
        }
    }

    format!("{}  ; {}", result, hex_bytes.trim_end())
}
