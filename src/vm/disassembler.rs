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

pub(super) fn decode_instruction_at(
    bytecode: &[u8],
    offset: usize,
) -> Result<DecodedInstruction, String> {
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
        0x0B => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntMin".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntMin,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x0C => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntMax".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntMax,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x0D => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for IntAbs".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntAbs,
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
        0x15 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatPow".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatPow,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x16 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for FloatSqrt".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatSqrt,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x17 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for FloatFloor".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatFloor,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x18 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for FloatCeil".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatCeil,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x19 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for FloatRound".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatRound,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x1A => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatMin".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatMin,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x1B => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatMax".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatMax,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x1C => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for FloatAbs".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatAbs,
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
        0x23 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntLe".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntLe,
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
        0x25 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntGe".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntGe,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x26 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntLtU".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntLtU,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x27 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntLeU".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntLeU,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x28 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntGtU".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntGtU,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x29 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for IntGeU".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntGeU,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x2A => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatEq".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatEq,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x2B => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatNe".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatNe,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x2C => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatLt".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatLt,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x2D => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatLe".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatLe,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x2E => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatGt".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatGt,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x2F => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for FloatGe".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatGe,
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
        0x34 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Shl".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Shl,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x35 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Shr".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Shr,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x36 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Sar".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Sar,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x37 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Rotl".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Rotl,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x38 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Rotr".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Rotr,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x39 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Clz".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Clz,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x3A => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Ctz".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Ctz,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x3B => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Popcnt".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Popcnt,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }

        // CONVERSION (0x40-0x4F)
        0x40 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for IntToFloat".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntToFloat,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x41 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for FloatToInt".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatToInt,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x42 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for IntToString".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::IntToString,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x43 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for FloatToString".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::FloatToString,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x44 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for BoolToString".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::BoolToString,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x45 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for StringToInt".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::StringToInt,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x46 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for StringToFloat".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::StringToFloat,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x47 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ReinterpretIntAsFloat".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ReinterpretIntAsFloat,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x48 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ReinterpretFloatAsInt".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ReinterpretFloatAsInt,
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
        0x52 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for LoadLocal32".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::LoadLocal32,
                operands: Operands::LoadLocal {
                    dst: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3,
            }
        }
        0x53 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for LoadLocal64".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::LoadLocal64,
                operands: Operands::LoadLocal {
                    dst: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3,
            }
        }
        0x54 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for LoadLocalPtr".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::LoadLocalPtr,
                operands: Operands::LoadLocal {
                    dst: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3,
            }
        }
        0x55 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for LoadGlobal".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::LoadGlobal,
                operands: Operands::LoadLocal {
                    dst: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3,
            }
        }
        0x56 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for LoadConst".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::LoadConst,
                operands: Operands::LoadLocal {
                    dst: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3,
            }
        }
        0x57 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for LoadField8".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::LoadField8,
                operands: Operands::LoadField {
                    dst: Reg(bytecode[offset + 1]),
                    ptr: Reg(bytecode[offset + 2]),
                    offset: offset_val,
                },
                size: 4,
            }
        }
        0x58 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for LoadField16".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::LoadField16,
                operands: Operands::LoadField {
                    dst: Reg(bytecode[offset + 1]),
                    ptr: Reg(bytecode[offset + 2]),
                    offset: offset_val,
                },
                size: 4,
            }
        }
        0x59 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for LoadField32".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::LoadField32,
                operands: Operands::LoadField {
                    dst: Reg(bytecode[offset + 1]),
                    ptr: Reg(bytecode[offset + 2]),
                    offset: offset_val,
                },
                size: 4,
            }
        }
        0x5A => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for LoadField64".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::LoadField64,
                operands: Operands::LoadField {
                    dst: Reg(bytecode[offset + 1]),
                    ptr: Reg(bytecode[offset + 2]),
                    offset: offset_val,
                },
                size: 4,
            }
        }
        0x5B => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for LoadFieldPtr".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::LoadFieldPtr,
                operands: Operands::LoadField {
                    dst: Reg(bytecode[offset + 1]),
                    ptr: Reg(bytecode[offset + 2]),
                    offset: offset_val,
                },
                size: 4,
            }
        }
        0x5C => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for LoadIndirect".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::LoadIndirect,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
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
        0x61 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for StoreLocal16".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::StoreLocal16,
                operands: Operands::StoreLocal {
                    src: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3,
            }
        }
        0x62 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for StoreLocal32".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::StoreLocal32,
                operands: Operands::StoreLocal {
                    src: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3,
            }
        }
        0x63 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for StoreLocal64".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::StoreLocal64,
                operands: Operands::StoreLocal {
                    src: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3,
            }
        }
        0x64 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for StoreLocalPtr".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::StoreLocalPtr,
                operands: Operands::StoreLocal {
                    src: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3,
            }
        }
        0x65 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for StoreGlobal".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::StoreGlobal,
                operands: Operands::StoreLocal {
                    src: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3,
            }
        }
        0x66 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for StoreField8".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::StoreField8,
                operands: Operands::LoadField {
                    // Note: Using LoadField because it has the same fields
                    dst: Reg(bytecode[offset + 1]),
                    ptr: Reg(bytecode[offset + 2]),
                    offset: offset_val,
                },
                size: 4,
            }
        }
        0x67 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for StoreField16".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::StoreField16,
                operands: Operands::LoadField {
                    dst: Reg(bytecode[offset + 1]),
                    ptr: Reg(bytecode[offset + 2]),
                    offset: offset_val,
                },
                size: 4,
            }
        }
        0x68 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for StoreField32".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::StoreField32,
                operands: Operands::LoadField {
                    dst: Reg(bytecode[offset + 1]),
                    ptr: Reg(bytecode[offset + 2]),
                    offset: offset_val,
                },
                size: 4,
            }
        }
        0x69 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for StoreField64".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::StoreField64,
                operands: Operands::LoadField {
                    dst: Reg(bytecode[offset + 1]),
                    ptr: Reg(bytecode[offset + 2]),
                    offset: offset_val,
                },
                size: 4,
            }
        }
        0x6A => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for StoreFieldPtr".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::StoreFieldPtr,
                operands: Operands::LoadField {
                    dst: Reg(bytecode[offset + 1]),
                    ptr: Reg(bytecode[offset + 2]),
                    offset: offset_val,
                },
                size: 4,
            }
        }
        0x6B => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for StoreIndirect".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::StoreIndirect,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x70 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for ArrayNew".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ArrayNew,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0x71 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ArrayLoad8".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ArrayLoad8,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x72 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ArrayLoad16".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ArrayLoad16,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x73 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ArrayLoad32".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ArrayLoad32,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x74 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ArrayLoad64".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ArrayLoad64,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x75 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ArrayLoadPtr".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ArrayLoadPtr,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x76 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ArrayStore8".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ArrayStore8,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x77 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ArrayStore16".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ArrayStore16,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x78 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ArrayStore32".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ArrayStore32,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x79 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ArrayStore64".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ArrayStore64,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x7A => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ArrayStorePtr".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ArrayStorePtr,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x7B => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ArrayLen".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ArrayLen,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x7C => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for StringConcat".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::StringConcat,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0x7D => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for StringLen".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::StringLen,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0x7E => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for StringSlice".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::StringSlice,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }

        // CONSTRUCTION (0x80-0x8F)
        0x80 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for StructNew".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::StructNew,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0x81 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for EnumNew".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::EnumNew,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0x82 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for TupleNew".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::TupleNew,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0x83 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for StructNew8".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::StructNew8,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0x84 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for StructNew16".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::StructNew16,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0x85 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for StructNew24".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::StructNew24,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0x86 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for StructNew32".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::StructNew32,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
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
        0x94 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for TableSwitch".to_string());
            }
            // This is a complex instruction with variable length, for now treat as basic
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::TableSwitch,
                operands: Operands::LoadLocal {
                    // Using as placeholder
                    dst: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3,
            }
        }
        0x95 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for LookupSwitch".to_string());
            }
            let offset_val = u16::from_le_bytes([bytecode[offset + 1], bytecode[offset + 2]]);
            DecodedInstruction {
                opcode: Opcode::LookupSwitch,
                operands: Operands::LoadLocal {
                    // Using as placeholder
                    dst: Reg(bytecode[offset + 1]),
                    offset: offset_val,
                },
                size: 3,
            }
        }
        0x96 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for JumpIfEqZero".to_string());
            }
            let jump_offset = i16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::JumpIfEqZero,
                operands: Operands::Branch {
                    cond: Reg(bytecode[offset + 1]),
                    offset: jump_offset,
                },
                size: 4,
            }
        }
        0x97 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for JumpIfNeZero".to_string());
            }
            let jump_offset = i16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::JumpIfNeZero,
                operands: Operands::Branch {
                    cond: Reg(bytecode[offset + 1]),
                    offset: jump_offset,
                },
                size: 4,
            }
        }
        0x98 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for JumpIfLtZero".to_string());
            }
            let jump_offset = i16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::JumpIfLtZero,
                operands: Operands::Branch {
                    cond: Reg(bytecode[offset + 1]),
                    offset: jump_offset,
                },
                size: 4,
            }
        }
        0x99 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for JumpIfGeZero".to_string());
            }
            let jump_offset = i16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::JumpIfGeZero,
                operands: Operands::Branch {
                    cond: Reg(bytecode[offset + 1]),
                    offset: jump_offset,
                },
                size: 4,
            }
        }
        0x91 => {
            if offset + 4 >= bytecode.len() {
                return Err("Not enough bytes for JumpLong".to_string());
            }
            let jump_offset = i32::from_le_bytes([
                bytecode[offset + 1],
                bytecode[offset + 2],
                bytecode[offset + 3],
                bytecode[offset + 4],
            ]);
            DecodedInstruction {
                opcode: Opcode::JumpLong,
                operands: Operands::Jump {
                    offset: jump_offset as i16, // Using as i16 for compatibility
                },
                size: 5,
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
        0xA1 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for CallTail".to_string());
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
                opcode: Opcode::CallTail,
                operands: Operands::Call {
                    func_id,
                    argc,
                    args,
                },
                size: 4 + argc as usize,
            }
        }
        0xA2 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for CallIndirect".to_string());
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
                opcode: Opcode::CallIndirect,
                operands: Operands::Call {
                    func_id,
                    argc,
                    args,
                },
                size: 4 + argc as usize,
            }
        }
        0xA3 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for CallBuiltin".to_string());
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
                opcode: Opcode::CallBuiltin,
                operands: Operands::Call {
                    func_id,
                    argc,
                    args,
                },
                size: 4 + argc as usize,
            }
        }
        0xA5 => DecodedInstruction {
            opcode: Opcode::ReturnVoid,
            operands: Operands::None,
            size: 1,
        },
        0xA6 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for CallKnownTail".to_string());
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
                opcode: Opcode::CallKnownTail,
                operands: Operands::Call {
                    func_id,
                    argc,
                    args,
                },
                size: 4 + argc as usize,
            }
        }

        // EFFECT SYSTEM (0xB0-0xB5)
        0xB0 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Perform".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Perform,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xB1 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for Resume".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Resume,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xB2 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for PushHandler".to_string());
            }
            let jump_offset = i16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::PushHandler,
                operands: Operands::Branch {
                    cond: Reg(bytecode[offset + 1]),
                    offset: jump_offset,
                },
                size: 4,
            }
        }
        0xB3 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for PopHandler".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::PopHandler,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0xB4 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for CaptureContinuation".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::CaptureContinuation,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0xB5 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for RestoreContinuation".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::RestoreContinuation,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }

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
        0xC2 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for MoveImm16".to_string());
            }
            let imm = i16::from_le_bytes([bytecode[offset + 2], bytecode[offset + 3]]);
            DecodedInstruction {
                opcode: Opcode::MoveImm16,
                operands: Operands::RI16 {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                    imm,
                },
                size: 4,
            }
        }
        0xC3 => {
            if offset + 5 >= bytecode.len() {
                return Err("Not enough bytes for MoveImm32".to_string());
            }
            let imm = i32::from_le_bytes([
                bytecode[offset + 2],
                bytecode[offset + 3],
                bytecode[offset + 4],
                bytecode[offset + 5],
            ]);
            DecodedInstruction {
                opcode: Opcode::MoveImm32,
                operands: Operands::RI64 {
                    dst: Reg(bytecode[offset + 1]),
                    imm: imm as i64,
                },
                size: 6,
            }
        }
        0xC7 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for MoveMinusOne".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::MoveMinusOne,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0),
                },
                size: 2,
            }
        }
        0xC8 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for MoveTrue".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::MoveTrue,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0),
                },
                size: 2,
            }
        }
        0xC9 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for MoveFalse".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::MoveFalse,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0),
                },
                size: 2,
            }
        }
        0xCA => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for MoveNull".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::MoveNull,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0),
                },
                size: 2,
            }
        }
        0xCB => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for Select".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::Select,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
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
        0xD2 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for GCAllocFast24".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::GCAllocFast24,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0xD3 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for GCAllocFast32".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::GCAllocFast32,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0xD4 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for GCAllocFast64".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::GCAllocFast64,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }
        0xD5 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for GCAllocSlow".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::GCAllocSlow,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xD6 => DecodedInstruction {
            opcode: Opcode::GCSafepoint,
            operands: Operands::None,
            size: 1,
        },
        0xD7 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for WriteBarrier".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::WriteBarrier,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xD8 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for ReadBarrier".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::ReadBarrier,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xD9 => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for DeclareRoot".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::DeclareRoot,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(0), // placeholder
                },
                size: 2,
            }
        }

        // ATOMICS (0xE0-0xEF)
        0xE0 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for AtomicLoad".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::AtomicLoad,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xE1 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for AtomicStore".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::AtomicStore,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xE2 => {
            if offset + 3 >= bytecode.len() {
                return Err("Not enough bytes for AtomicCAS".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::AtomicCAS,
                operands: Operands::RRR {
                    dst: Reg(bytecode[offset + 1]),
                    src1: Reg(bytecode[offset + 2]),
                    src2: Reg(bytecode[offset + 3]),
                },
                size: 4,
            }
        }
        0xE3 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for AtomicFetchAdd".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::AtomicFetchAdd,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xE4 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for AtomicFetchSub".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::AtomicFetchSub,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xE5 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for AtomicFetchAnd".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::AtomicFetchAnd,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xE6 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for AtomicFetchOr".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::AtomicFetchOr,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xE7 => {
            if offset + 2 >= bytecode.len() {
                return Err("Not enough bytes for AtomicFetchXor".to_string());
            }
            DecodedInstruction {
                opcode: Opcode::AtomicFetchXor,
                operands: Operands::RR {
                    dst: Reg(bytecode[offset + 1]),
                    src: Reg(bytecode[offset + 2]),
                },
                size: 3,
            }
        }
        0xE8 => DecodedInstruction {
            opcode: Opcode::FenceAcquire,
            operands: Operands::None,
            size: 1,
        },
        0xE9 => DecodedInstruction {
            opcode: Opcode::FenceRelease,
            operands: Operands::None,
            size: 1,
        },
        0xEA => DecodedInstruction {
            opcode: Opcode::FenceSeqCst,
            operands: Operands::None,
            size: 1,
        },

        // TRAPS (0xF0-0xF7)
        0xF0 => DecodedInstruction {
            opcode: Opcode::TrapBoundsCheck,
            operands: Operands::None,
            size: 1,
        },
        0xF1 => DecodedInstruction {
            opcode: Opcode::TrapDivByZero,
            operands: Operands::None,
            size: 1,
        },
        0xF2 => DecodedInstruction {
            opcode: Opcode::TrapNullCheck,
            operands: Operands::None,
            size: 1,
        },
        0xF3 => DecodedInstruction {
            opcode: Opcode::TrapOverflow,
            operands: Operands::None,
            size: 1,
        },
        0xF4 => DecodedInstruction {
            opcode: Opcode::Unreachable,
            operands: Operands::None,
            size: 1,
        },
        0xF5 => DecodedInstruction {
            opcode: Opcode::DebugTrap,
            operands: Operands::None,
            size: 1,
        },
        0xF6 => DecodedInstruction {
            opcode: Opcode::ProfileEnter,
            operands: Operands::None,
            size: 1,
        },
        0xF7 => DecodedInstruction {
            opcode: Opcode::ProfileExit,
            operands: Operands::None,
            size: 1,
        },

        // EXTENDED OPCODES (0xFF)
        0xFF => {
            if offset + 1 >= bytecode.len() {
                return Err("Not enough bytes for ExtendedOp".to_string());
            }
            // Handle SIMD extended opcodes
            let ext_opcode = bytecode[offset + 1];
            match ext_opcode {
                // SIMD - i32x4 (0x00-0x1F)
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
                0x01 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Sub".to_string());
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
                0x02 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Mul".to_string());
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
                0x03 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Min".to_string());
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
                0x04 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Max".to_string());
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
                0x05 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Eq".to_string());
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
                0x06 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Lt".to_string());
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
                0x07 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Le".to_string());
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
                0x08 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Gt".to_string());
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
                0x09 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Ge".to_string());
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
                0x0A => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Load".to_string());
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
                0x0B => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Store".to_string());
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
                0x0C => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Splat".to_string());
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
                0x0D => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Extract".to_string());
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
                0x0E => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for I32x4Replace".to_string());
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
                // SIMD - f32x4 (0x20-0x3F)
                0x20 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for F32x4Add".to_string());
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
                0x21 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for F32x4Sub".to_string());
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
                0x22 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for F32x4Mul".to_string());
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
                0x23 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for F32x4Div".to_string());
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
                0x24 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for F32x4Min".to_string());
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
                0x25 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for F32x4Max".to_string());
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
                0x26 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for F32x4Sqrt".to_string());
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
                0x27 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for F32x4Fma".to_string());
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
                0x28 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for F32x4Eq".to_string());
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
                0x29 => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for F32x4Lt".to_string());
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
                0x2A => {
                    if offset + 4 >= bytecode.len() {
                        return Err("Not enough bytes for F32x4Le".to_string());
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
                // More SIMD opcodes can be added as needed
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
