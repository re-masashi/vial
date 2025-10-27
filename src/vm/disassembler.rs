use crate::vm::{EnumLayout, FunctionMetadata, StructLayout};

pub fn pretty_dump_bytecode(
    bytecode: &[u8],
    functions: &[FunctionMetadata],
    constant_pool: Option<&[u8]>,
    struct_layouts: Option<&[StructLayout]>,
    enum_layouts: Option<&[EnumLayout]>,
) -> String {
    let mut output = String::new();

    if functions.is_empty() {
        let mut current_pc = 0;
        while current_pc < bytecode.len() {
            // Decode and format the current instruction
            let (instruction_str, size) = decode_instruction(
                bytecode,
                current_pc,
                constant_pool,
                struct_layouts,
                enum_layouts,
            );

            output.push_str(&format!("0x{:04X}: ", current_pc));
            output.push_str(&format_raw_bytes(
                &bytecode[current_pc..current_pc + size.min(bytecode.len() - current_pc)],
            ));
            output.push_str(&format!("{:20} {}\n", instruction_str.0, instruction_str.1));

            current_pc += size;
        }
        return output;
    }

    for (func_idx, func) in functions.iter().enumerate() {
        output.push_str("═══════════════════════════════════════════════════════════\n");
        output.push_str(&format!("FUNCTION: func_{} (id={})\n", func_idx, func_idx));
        output.push_str(&format!(
            "  Bytecode: 0x{:04X}..0x{:04X} ({} bytes)\n",
            func.bytecode_offset,
            func.bytecode_offset + func.bytecode_length,
            func.bytecode_length
        ));
        output.push_str(&format!(
            "  Args: {}, Registers: {}, Stack: {} bytes\n",
            func.arg_count, func.register_count, func.local_stack_size
        ));
        output.push_str(&format!(
            "  Stack maps: {} @ offset 0x{:04X}\n",
            func.stack_map_count, func.stack_map_offset
        ));
        output.push_str("═══════════════════════════════════════════════════════════\n\n");

        let func_start = func.bytecode_offset as usize;
        let func_end = (func.bytecode_offset + func.bytecode_length) as usize;
        let func_bytecode = &bytecode[func_start..func_end.min(bytecode.len())];

        let mut current_pc = 0;
        while current_pc < func_bytecode.len() {
            let (instruction_str, size) = decode_instruction(
                func_bytecode,
                current_pc,
                constant_pool,
                struct_layouts,
                enum_layouts,
            );

            // Add the formatted instruction to output
            output.push_str(&format!("0x{:04X}: ", func_start + current_pc));
            output.push_str(&format_raw_bytes(
                &func_bytecode[current_pc..current_pc + size.min(func_bytecode.len() - current_pc)],
            ));
            output.push_str(&format!("{:20} {}\n", instruction_str.0, instruction_str.1));

            current_pc += size;
        }

        output.push('\n');
    }

    output
}

struct FormattedInstruction(String, String); // (mnemonic, operands)

// Instruction formats
#[derive(Debug, Clone, Copy)]
#[allow(unused)]
#[allow(clippy::upper_case_acronyms)]
enum InstructionFormat {
    RRR,          // 3 registers: [opcode][dst][src1][src2] (4 bytes)
    RR,           // 2 registers: [opcode][dst][src] (3 bytes)
    RRI8,         // 2 registers + i8: [opcode][dst][src][imm8] (4 bytes)
    RI8,          // 1 register + i8: [opcode][dst][imm8] (3 bytes)
    RI16,         // 1 register + i16: [opcode][dst][imm16_le] (4 bytes)
    RI32,         // 1 register + i32: [opcode][dst][imm32_le] (6 bytes)
    RI64,         // 1 register + i64: [opcode][dst][imm64_le] (10 bytes)
    LoadLocal,    // [opcode][dst][offset16_le] (4 bytes)
    LoadField,    // [opcode][dst][ptr][offset16_le] (5 bytes)
    LoadGlobal,   // [opcode][dst][global_id16_le] (4 bytes)
    LoadConst,    // [opcode][dst][const_id16_le] (4 bytes)
    StoreLocal,   // [opcode][src][offset16_le] (4 bytes)
    StoreField,   // [opcode][ptr][offset16_le][src] (5 bytes)
    Jump,         // [opcode][offset16_le] (3 bytes)
    JumpLong,     // [opcode][offset32_le] (5 bytes)
    JumpCond,     // [opcode][cond][offset16_le] (4 bytes)
    JumpIfEqZero, // [opcode][reg][offset16_le] (4 bytes)
    Call,         // [opcode][func_id16_le][argc][args...] (variable)
    CallTail,     // [opcode][func_id16_le][argc][args...] (variable)
    CallIndirect, // [opcode][func_id16_le][argc][args...] (variable)
    CallBuiltin,  // [opcode][builtin_id16_le][argc][args...] (variable)
    Return,       // [opcode] (1 byte)
    ReturnVoid,   // [opcode] (1 byte)
    Move,         // [opcode][dst][src] (3 bytes)
    MoveImm8,     // [opcode][dst][imm8] (3 bytes)
    MoveImm16,    // [opcode][dst][imm16_le] (4 bytes)
    MoveImm32,    // [opcode][dst][imm32_le] (6 bytes)
    MoveImm64,    // [opcode][dst][imm64_le] (10 bytes)
    MoveSpecial,  // [opcode][dst] (2 bytes) - MoveZero, MoveOne, etc.
    GCAllocFast,  // [opcode][dst] (2 bytes)
    GCAllocSlow,  // [opcode][dst][size][align] (4 bytes)
    ArrayNew,     // [opcode][dst][elem_type16_le][length_reg] (5 bytes)
    ArrayLoad,    // [opcode][dst][array][index] (4 bytes)
    ArrayStore,   // [opcode][array][index][value] (4 bytes)
    StructNew,    // [opcode][dst][struct_id16_le] (4 bytes)
    EnumNew,      // [opcode][dst][enum_id16_le][variant] (5 bytes)
    Extended,     // [0xFF][ext_opcode][dst][src1][src2] (5 bytes)
    None,         // No operands: [opcode] (1 byte)
} // PHEW FINALLY

const INSTRUCTION_TABLE: [Option<(&'static str, InstructionFormat)>; 256] = [
    // 0x00-0x0F - INTEGER ARITHMETIC
    Some(("IntAdd", InstructionFormat::RRR)),      // 0x00
    Some(("IntSub", InstructionFormat::RRR)),      // 0x01
    Some(("IntMul", InstructionFormat::RRR)),      // 0x02
    Some(("IntDiv", InstructionFormat::RRR)),      // 0x03
    Some(("IntMod", InstructionFormat::RRR)),      // 0x04
    Some(("IntNeg", InstructionFormat::RR)),       // 0x05
    Some(("IntAddImm8", InstructionFormat::RRI8)), // 0x06
    Some(("IntSubImm8", InstructionFormat::RRI8)), // 0x07
    Some(("IntMulImm8", InstructionFormat::RRI8)), // 0x08
    Some(("IntEqZero", InstructionFormat::RR)),    // 0x09
    Some(("IntNeZero", InstructionFormat::RR)),    // 0x0A
    Some(("IntMin", InstructionFormat::RRR)),      // 0x0B
    Some(("IntMax", InstructionFormat::RRR)),      // 0x0C
    Some(("IntAbs", InstructionFormat::RR)),       // 0x0D
    None,
    None,
    None,
    None, // 0x0E-0x0F
    // 0x10-0x1F - FLOAT ARITHMETIC
    Some(("FloatAdd", InstructionFormat::RRR)),  // 0x10
    Some(("FloatSub", InstructionFormat::RRR)),  // 0x11
    Some(("FloatMul", InstructionFormat::RRR)),  // 0x12
    Some(("FloatDiv", InstructionFormat::RRR)),  // 0x13
    Some(("FloatNeg", InstructionFormat::RR)),   // 0x14
    Some(("FloatPow", InstructionFormat::RRR)),  // 0x15
    Some(("FloatSqrt", InstructionFormat::RR)),  // 0x16
    Some(("FloatFloor", InstructionFormat::RR)), // 0x17
    Some(("FloatCeil", InstructionFormat::RR)),  // 0x18
    Some(("FloatRound", InstructionFormat::RR)), // 0x19
    Some(("FloatMin", InstructionFormat::RRR)),  // 0x1A
    Some(("FloatMax", InstructionFormat::RRR)),  // 0x1B
    Some(("FloatAbs", InstructionFormat::RR)),   // 0x1C
    None,
    None,
    None,
    None, // 0x1D-0x1F
    // 0x20-0x2F - COMPARISONS
    Some(("IntEq", InstructionFormat::RRR)),   // 0x20
    Some(("IntNe", InstructionFormat::RRR)),   // 0x21
    Some(("IntLt", InstructionFormat::RRR)),   // 0x22
    Some(("IntLe", InstructionFormat::RRR)),   // 0x23
    Some(("IntGt", InstructionFormat::RRR)),   // 0x24
    Some(("IntGe", InstructionFormat::RRR)),   // 0x25
    Some(("IntLtU", InstructionFormat::RRR)),  // 0x26
    Some(("IntLeU", InstructionFormat::RRR)),  // 0x27
    Some(("IntGtU", InstructionFormat::RRR)),  // 0x28
    Some(("IntGeU", InstructionFormat::RRR)),  // 0x29
    Some(("FloatEq", InstructionFormat::RRR)), // 0x2A
    Some(("FloatNe", InstructionFormat::RRR)), // 0x2B
    Some(("FloatLt", InstructionFormat::RRR)), // 0x2C
    Some(("FloatLe", InstructionFormat::RRR)), // 0x2D
    Some(("FloatGt", InstructionFormat::RRR)), // 0x2E
    Some(("FloatGe", InstructionFormat::RRR)), // 0x2F
    // 0x30-0x3F - BITWISE & LOGICAL
    Some(("And", InstructionFormat::RRR)),   // 0x30
    Some(("Or", InstructionFormat::RRR)),    // 0x31
    Some(("Xor", InstructionFormat::RRR)),   // 0x32
    Some(("Not", InstructionFormat::RR)),    // 0x33
    Some(("Shl", InstructionFormat::RRR)),   // 0x34
    Some(("Shr", InstructionFormat::RRR)),   // 0x35
    Some(("Sar", InstructionFormat::RRR)),   // 0x36
    Some(("Rotl", InstructionFormat::RRR)),  // 0x37
    Some(("Rotr", InstructionFormat::RRR)),  // 0x38
    Some(("Clz", InstructionFormat::RR)),    // 0x39
    Some(("Ctz", InstructionFormat::RR)),    // 0x3A
    Some(("Popcnt", InstructionFormat::RR)), // 0x3B
    None,
    None,
    None,
    None, // 0x3C-0x3F
    // 0x40-0x4F - CONVERSION
    Some(("IntToFloat", InstructionFormat::RR)),  // 0x40
    Some(("FloatToInt", InstructionFormat::RR)),  // 0x41
    Some(("IntToString", InstructionFormat::RR)), // 0x42
    Some(("FloatToString", InstructionFormat::RR)), // 0x43
    Some(("BoolToString", InstructionFormat::RR)), // 0x44
    Some(("StringToInt", InstructionFormat::RR)), // 0x45
    Some(("StringToFloat", InstructionFormat::RR)), // 0x46
    Some(("ReinterpretIntAsFloat", InstructionFormat::RR)), // 0x47
    Some(("ReinterpretFloatAsInt", InstructionFormat::RR)), // 0x48
    None,
    None,
    None,
    None,
    None,
    None, // 0x49-0x4F
    // 0x50-0x5F - MEMORY LOADS
    Some(("LoadLocal8", InstructionFormat::LoadLocal)), // 0x50
    Some(("LoadLocal16", InstructionFormat::LoadLocal)), // 0x51
    Some(("LoadLocal32", InstructionFormat::LoadLocal)), // 0x52
    Some(("LoadLocal64", InstructionFormat::LoadLocal)), // 0x53
    Some(("LoadLocalPtr", InstructionFormat::LoadLocal)), // 0x54
    Some(("LoadGlobal", InstructionFormat::LoadGlobal)), // 0x55
    Some(("LoadConst", InstructionFormat::LoadConst)),  // 0x56
    Some(("LoadField8", InstructionFormat::LoadField)), // 0x57
    Some(("LoadField16", InstructionFormat::LoadField)), // 0x58
    Some(("LoadField32", InstructionFormat::LoadField)), // 0x59
    Some(("LoadField64", InstructionFormat::LoadField)), // 0x5A
    Some(("LoadFieldPtr", InstructionFormat::LoadField)), // 0x5B
    Some(("LoadIndirect", InstructionFormat::RR)),      // 0x5C
    None,
    None,
    None, // 0x5D-0x5F
    // 0x60-0x6F - MEMORY STORES
    Some(("StoreLocal8", InstructionFormat::StoreLocal)), // 0x60
    Some(("StoreLocal16", InstructionFormat::StoreLocal)), // 0x61
    Some(("StoreLocal32", InstructionFormat::StoreLocal)), // 0x62
    Some(("StoreLocal64", InstructionFormat::StoreLocal)), // 0x63
    Some(("StoreLocalPtr", InstructionFormat::StoreLocal)), // 0x64
    Some(("StoreGlobal", InstructionFormat::RR)),         // 0x65
    Some(("StoreField8", InstructionFormat::StoreField)), // 0x66
    Some(("StoreField16", InstructionFormat::StoreField)), // 0x67
    Some(("StoreField32", InstructionFormat::StoreField)), // 0x68
    Some(("StoreField64", InstructionFormat::StoreField)), // 0x69
    Some(("StoreFieldPtr", InstructionFormat::StoreField)), // 0x6A
    Some(("StoreIndirect", InstructionFormat::RRR)),      // 0x6B
    None,
    None,
    None,
    None, // 0x6C-0x6F
    // 0x70-0x7F - ARRAYS & STRINGS
    Some(("ArrayNew", InstructionFormat::ArrayNew)), // 0x70
    Some(("ArrayLoad8", InstructionFormat::ArrayLoad)), // 0x71
    Some(("ArrayLoad16", InstructionFormat::ArrayLoad)), // 0x72
    Some(("ArrayLoad32", InstructionFormat::ArrayLoad)), // 0x73
    Some(("ArrayLoad64", InstructionFormat::ArrayLoad)), // 0x74
    Some(("ArrayLoadPtr", InstructionFormat::ArrayLoad)), // 0x75
    Some(("ArrayStore8", InstructionFormat::ArrayStore)), // 0x76
    Some(("ArrayStore16", InstructionFormat::ArrayStore)), // 0x77
    Some(("ArrayStore32", InstructionFormat::ArrayStore)), // 0x78
    Some(("ArrayStore64", InstructionFormat::ArrayStore)), // 0x79
    Some(("ArrayStorePtr", InstructionFormat::ArrayStore)), // 0x7A
    Some(("ArrayLen", InstructionFormat::RR)),       // 0x7B
    Some(("StringConcat", InstructionFormat::RRR)),  // 0x7C
    Some(("StringLen", InstructionFormat::RR)),      // 0x7D
    Some(("StringSlice", InstructionFormat::RRR)),   // 0x7E
    None,                                            // 0x7F
    // 0x80-0x8F - CONSTRUCTION
    Some(("StructNew", InstructionFormat::StructNew)), // 0x80
    Some(("EnumNew", InstructionFormat::EnumNew)),     // 0x81
    Some(("TupleNew", InstructionFormat::RR)),         // 0x82
    Some(("StructNew8", InstructionFormat::RR)),       // 0x83
    Some(("StructNew16", InstructionFormat::RR)),      // 0x84
    Some(("StructNew24", InstructionFormat::RR)),      // 0x85
    Some(("StructNew32", InstructionFormat::RR)),      // 0x86
    None,
    None,
    None,
    None,
    None, // 0x87-0x8B
    None,
    None,
    None, // 0x8C-0x8E
    None, // 0x8F
    // 0x90-0x9F - CONTROL FLOW
    Some(("Jump", InstructionFormat::Jump)),          // 0x90
    Some(("JumpLong", InstructionFormat::JumpLong)),  // 0x91
    Some(("JumpIf", InstructionFormat::JumpCond)),    // 0x92
    Some(("JumpIfNot", InstructionFormat::JumpCond)), // 0x93
    Some(("TableSwitch", InstructionFormat::RR)),     // 0x94
    Some(("LookupSwitch", InstructionFormat::RR)),    // 0x95
    Some(("JumpIfEqZero", InstructionFormat::JumpIfEqZero)), // 0x96
    Some(("JumpIfNeZero", InstructionFormat::JumpIfEqZero)), // 0x97
    Some(("JumpIfLtZero", InstructionFormat::JumpIfEqZero)), // 0x98
    Some(("JumpIfGeZero", InstructionFormat::JumpIfEqZero)), // 0x99
    None,
    None, // 0x9A-0x9B
    None,
    None,
    None,
    None, // 0x9C-0x9F
    // 0xA0-0xAF - FUNCTION CALLS
    Some(("Call", InstructionFormat::Call)),         // 0xA0
    Some(("CallTail", InstructionFormat::CallTail)), // 0xA1
    Some(("CallIndirect", InstructionFormat::CallIndirect)), // 0xA2
    Some(("CallBuiltin", InstructionFormat::CallBuiltin)), // 0xA3
    Some(("Return", InstructionFormat::Return)),     // 0xA4
    Some(("ReturnVoid", InstructionFormat::ReturnVoid)), // 0xA5
    Some(("CallKnownTail", InstructionFormat::Call)), // 0xA6
    None,
    None,
    None, // 0xA7-0xAF
    // 0xB0-0xBF - EFFECTS
    Some(("Perform", InstructionFormat::RR)),      // 0xB0
    Some(("Resume", InstructionFormat::RR)),       // 0xB1
    Some(("PushHandler", InstructionFormat::RR)),  // 0xB2
    Some(("PopHandler", InstructionFormat::None)), // 0xB3
    Some(("CaptureContinuation", InstructionFormat::RR)), // 0xB4
    Some(("RestoreContinuation", InstructionFormat::RR)), // 0xB5
    None,
    None,
    None,
    None, // 0xB6-0xBF
    // 0xC0-0xCF - REGISTER MOVES
    Some(("Move", InstructionFormat::Move)),           // 0xC0
    Some(("MoveImm8", InstructionFormat::MoveImm8)),   // 0xC1
    Some(("MoveImm16", InstructionFormat::MoveImm16)), // 0xC2
    Some(("MoveImm32", InstructionFormat::MoveImm32)), // 0xC3
    Some(("MoveImm64", InstructionFormat::MoveImm64)), // 0xC4
    Some(("MoveZero", InstructionFormat::MoveSpecial)), // 0xC5
    Some(("MoveOne", InstructionFormat::MoveSpecial)), // 0xC6
    Some(("MoveMinusOne", InstructionFormat::MoveSpecial)), // 0xC7
    Some(("MoveTrue", InstructionFormat::MoveSpecial)), // 0xC8
    Some(("MoveFalse", InstructionFormat::MoveSpecial)), // 0xC9
    Some(("MoveNull", InstructionFormat::MoveSpecial)), // 0xCA
    Some(("Select", InstructionFormat::RRR)),          // 0xCB
    None,
    None,
    None, // 0xCC-0xCF
    // 0xD0-0xDF - GC OPERATIONS
    Some(("GCAllocFast8", InstructionFormat::GCAllocFast)), // 0xD0
    Some(("GCAllocFast16", InstructionFormat::GCAllocFast)), // 0xD1
    Some(("GCAllocFast24", InstructionFormat::GCAllocFast)), // 0xD2
    Some(("GCAllocFast32", InstructionFormat::GCAllocFast)), // 0xD3
    Some(("GCAllocFast64", InstructionFormat::GCAllocFast)), // 0xD4
    Some(("GCAllocSlow", InstructionFormat::GCAllocSlow)),  // 0xD5
    Some(("GCSafepoint", InstructionFormat::None)),         // 0xD6
    Some(("WriteBarrier", InstructionFormat::RRR)),         // 0xD7
    Some(("ReadBarrier", InstructionFormat::RR)),           // 0xD8
    Some(("DeclareRoot", InstructionFormat::RR)),           // 0xD9
    None,
    None, // 0xDA-0xDB
    None,
    None,
    None,
    None, // 0xDC-0xDF
    // 0xE0-0xEF - ATOMICS
    Some(("AtomicLoad", InstructionFormat::RR)),   // 0xE0
    Some(("AtomicStore", InstructionFormat::RRR)), // 0xE1
    Some(("AtomicCAS", InstructionFormat::RRR)),   // 0xE2
    Some(("AtomicFetchAdd", InstructionFormat::RRR)), // 0xE3
    Some(("AtomicFetchSub", InstructionFormat::RRR)), // 0xE4
    Some(("AtomicFetchAnd", InstructionFormat::RRR)), // 0xE5
    Some(("AtomicFetchOr", InstructionFormat::RRR)), // 0xE6
    Some(("AtomicFetchXor", InstructionFormat::RRR)), // 0xE7
    Some(("FenceAcquire", InstructionFormat::None)), // 0xE8
    Some(("FenceRelease", InstructionFormat::None)), // 0xE9
    Some(("FenceSeqCst", InstructionFormat::None)), // 0xEA
    None,
    None,
    None,
    None, // 0xEB-0xEF
    // 0xF0-0xFF - TRAPS and Extended Opcodes
    Some(("TrapBoundsCheck", InstructionFormat::RR)), // 0xF0
    Some(("TrapDivByZero", InstructionFormat::RR)),   // 0xF1
    Some(("TrapNullCheck", InstructionFormat::RR)),   // 0xF2
    Some(("TrapOverflow", InstructionFormat::RR)),    // 0xF3
    Some(("Unreachable", InstructionFormat::None)),   // 0xF4
    Some(("DebugTrap", InstructionFormat::None)),     // 0xF5
    Some(("ProfileEnter", InstructionFormat::RR)),    // 0xF6
    Some(("ProfileExit", InstructionFormat::RR)),     // 0xF7
    None,
    None,
    None,
    None, // 0xF8-0xFB
    None,
    None,
    None,                                              // 0xFC-0xFE
    Some(("ExtendedOp", InstructionFormat::Extended)), // 0xFF
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None, // 12 more to reach 256 (uhh i think i covered everything though. idk)
    None,
    None,
    None,
    None,
];

fn decode_instruction(
    bytecode: &[u8],
    pc: usize,
    constant_pool: Option<&[u8]>,
    struct_layouts: Option<&[StructLayout]>,
    enum_layouts: Option<&[EnumLayout]>,
) -> (FormattedInstruction, usize) {
    if pc >= bytecode.len() {
        return (
            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
            1,
        );
    }

    let opcode = bytecode[pc];

    match &INSTRUCTION_TABLE[opcode as usize] {
        Some((mnemonic, format)) => {
            match format {
                InstructionFormat::RRR => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let src1 = bytecode[pc + 2];
                    let src2 = bytecode[pc + 3];

                    let mut operands = format!(
                        "{}, {}, {}",
                        format_reg(dst),
                        format_reg(src1),
                        format_reg(src2)
                    );

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        // This would require function metadata to know argument count
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::RR => {
                    if pc + 2 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let src = bytecode[pc + 2];

                    let mut operands = format!("{}, {}", format_reg(dst), format_reg(src));

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 3)
                }

                InstructionFormat::RRI8 => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let src = bytecode[pc + 2];
                    let imm = bytecode[pc + 3] as i8;

                    let mut operands = format!(
                        "{}, {}, {}",
                        format_reg(dst),
                        format_reg(src),
                        format_imm(imm as i64)
                    );

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::RI8 => {
                    if pc + 2 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let imm = bytecode[pc + 2] as i8;

                    let mut operands = format!("{}, {}", format_reg(dst), format_imm(imm as i64));

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 3)
                }

                InstructionFormat::RI16 => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let imm = i16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);

                    let mut operands = format!("{}, {}", format_reg(dst), format_imm(imm as i64));

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::RI32 => {
                    if pc + 5 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let imm = i32::from_le_bytes([
                        bytecode[pc + 2],
                        bytecode[pc + 3],
                        bytecode[pc + 4],
                        bytecode[pc + 5],
                    ]);

                    let mut operands = format!("{}, {}", format_reg(dst), format_imm(imm as i64));

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 6)
                }

                InstructionFormat::RI64 => {
                    if pc + 9 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
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

                    let mut operands = format!("{}, {}", format_reg(dst), format_imm(imm));

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 10)
                }

                InstructionFormat::LoadLocal => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);

                    let mut operands =
                        format!("{}, {}", format_reg(dst), format_uimm(offset as u64));

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::LoadField => {
                    if pc + 4 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let ptr = bytecode[pc + 2];
                    let offset = u16::from_le_bytes([bytecode[pc + 3], bytecode[pc + 4]]);

                    let mut operands = format!(
                        "{}, {}, {}",
                        format_reg(dst),
                        format_reg(ptr),
                        format_uimm(offset as u64)
                    );

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 5)
                }

                InstructionFormat::LoadGlobal => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let id = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);

                    let mut operands = format!("{}, global_{}", format_reg(dst), id);

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::LoadConst => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let id = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);

                    let mut operands = format!("{}, const_{}", format_reg(dst), id);

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    // If we have a constant pool, try to get the value
                    if let Some(pool) = constant_pool
                        && let Some(value_str) = get_constant_value(pool, id)
                    {
                        operands.push_str(&format!("  ; = {}", value_str));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::StoreLocal => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let src = bytecode[pc + 1];
                    let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);

                    let operands = format!("{}, {}", format_reg(src), format_uimm(offset as u64));

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::StoreField => {
                    if pc + 4 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let ptr = bytecode[pc + 1];
                    let offset = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                    let val = bytecode[pc + 4];

                    let operands = format!(
                        "{}, {}, {}",
                        format_reg(ptr),
                        format_uimm(offset as u64),
                        format_reg(val)
                    );

                    (FormattedInstruction(mnemonic.to_string(), operands), 5)
                }

                InstructionFormat::Jump => {
                    if pc + 2 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let offset = i16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
                    let target = (pc as i32 + 3 + offset as i32) as usize;

                    let operands = format!("{:+} (-> 0x{:04X})", offset, target);

                    (FormattedInstruction(mnemonic.to_string(), operands), 3)
                }

                InstructionFormat::JumpLong => {
                    if pc + 4 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let offset = i32::from_le_bytes([
                        bytecode[pc + 1],
                        bytecode[pc + 2],
                        bytecode[pc + 3],
                        bytecode[pc + 4],
                    ]);
                    let target = (pc as i32 + 5 + offset) as usize;

                    let operands = format!("{:+} (-> 0x{:04X})", offset, target);

                    (FormattedInstruction(mnemonic.to_string(), operands), 5)
                }

                InstructionFormat::JumpCond => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let cond = bytecode[pc + 1];
                    let offset = i16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                    let target = (pc as i32 + 4 + offset as i32) as usize;

                    let operands =
                        format!("{}, {:+} (-> 0x{:04X})", format_reg(cond), offset, target);

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::JumpIfEqZero => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let reg = bytecode[pc + 1];
                    let offset = i16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                    let target = (pc as i32 + 4 + offset as i32) as usize;

                    let operands =
                        format!("{}, {:+} (-> 0x{:04X})", format_reg(reg), offset, target);

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::Call => decode_call_instruction(bytecode, pc, "Call"),

                InstructionFormat::CallTail => decode_call_instruction(bytecode, pc, "CallTail"),

                InstructionFormat::CallIndirect => {
                    decode_call_instruction(bytecode, pc, "CallIndirect")
                }

                InstructionFormat::CallBuiltin => {
                    decode_call_instruction(bytecode, pc, "CallBuiltin")
                }

                InstructionFormat::Return => (
                    FormattedInstruction(mnemonic.to_string(), "".to_string()),
                    1,
                ),

                InstructionFormat::ReturnVoid => (
                    FormattedInstruction(mnemonic.to_string(), "".to_string()),
                    1,
                ),

                InstructionFormat::Move => {
                    if pc + 2 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let src = bytecode[pc + 2];

                    let mut operands = format!("{}, {}", format_reg(dst), format_reg(src));

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 3)
                }

                InstructionFormat::MoveImm8 => {
                    if pc + 2 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let imm = bytecode[pc + 2] as i8;

                    let mut operands = format!("{}, {}", format_reg(dst), format_imm(imm as i64));

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 3)
                }

                InstructionFormat::MoveImm16 => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let imm = i16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);

                    let mut operands = format!("{}, {}", format_reg(dst), format_imm(imm as i64));

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::MoveImm32 => {
                    if pc + 5 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let imm = i32::from_le_bytes([
                        bytecode[pc + 2],
                        bytecode[pc + 3],
                        bytecode[pc + 4],
                        bytecode[pc + 5],
                    ]);

                    let mut operands = format!("{}, {}", format_reg(dst), format_imm(imm as i64));

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 6)
                }

                InstructionFormat::MoveImm64 => {
                    if pc + 9 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
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

                    let mut operands = format!("{}, {}", format_reg(dst), format_imm(imm));

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 10)
                }

                InstructionFormat::MoveSpecial => {
                    if pc + 1 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];

                    let mut operands = format_reg(dst).to_string();

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 2)
                }

                InstructionFormat::GCAllocFast => {
                    if pc + 1 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];

                    let mut operands = format_reg(dst).to_string();

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 2)
                }

                InstructionFormat::GCAllocSlow => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let size = bytecode[pc + 2];
                    let align = bytecode[pc + 3];

                    let mut operands = format!("{}, {}, {}", format_reg(dst), size, align);

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::ArrayNew => {
                    if pc + 4 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let elem_type = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                    let length_reg = bytecode[pc + 4];

                    let mut operands = format!(
                        "{}, {}, {}",
                        format_reg(dst),
                        elem_type,
                        format_reg(length_reg)
                    );

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 5)
                }

                InstructionFormat::ArrayLoad => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let array = bytecode[pc + 2];
                    let index = bytecode[pc + 3];

                    let mut operands = format!(
                        "{}, {}, {}",
                        format_reg(dst),
                        format_reg(array),
                        format_reg(index)
                    );

                    // Add annotation for R0
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::ArrayStore => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let array = bytecode[pc + 1];
                    let index = bytecode[pc + 2];
                    let val = bytecode[pc + 3];

                    let operands = format!(
                        "{}, {}, {}",
                        format_reg(array),
                        format_reg(index),
                        format_reg(val)
                    );

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::StructNew => {
                    if pc + 3 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let struct_id = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);

                    let mut operands = format!("{}, struct_{}", format_reg(dst), struct_id);

                    // Add annotation for R0 and type info if available
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    if let Some(layouts) = struct_layouts
                        && (struct_id as usize) < layouts.len()
                    {
                        operands.push_str(&format!("  ; struct_{}", struct_id)); // Basic info for now
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 4)
                }

                InstructionFormat::EnumNew => {
                    if pc + 4 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    let dst = bytecode[pc + 1];
                    let enum_id = u16::from_le_bytes([bytecode[pc + 2], bytecode[pc + 3]]);
                    let variant = bytecode[pc + 4];

                    let mut operands = format!("{}, {}, {}", format_reg(dst), enum_id, variant);

                    // Add annotation for R0 and type info if available
                    if dst == 0 {
                        operands.push_str("  ; R0 = return value");
                    } else if (1..=8).contains(&dst) {
                        operands.push_str(&format!("  ; arg{}", dst - 1));
                    }

                    if let Some(layouts) = enum_layouts
                        && (enum_id as usize) < layouts.len()
                    {
                        operands.push_str(&format!("  ; enum_{}", enum_id)); // Basic info for now
                    }

                    (FormattedInstruction(mnemonic.to_string(), operands), 5)
                }

                InstructionFormat::Extended => {
                    if pc + 1 >= bytecode.len() {
                        return (
                            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                            1,
                        );
                    }
                    // Extended opcode: [0xFF][ext_opcode][dst][src1][src2] (5 bytes total)
                    let ext_opcode = bytecode[pc + 1];

                    match get_extended_opcode_mnemonic(ext_opcode) {
                        Some((ext_mnemonic, _)) => {
                            if pc + 4 >= bytecode.len() {
                                return (
                                    FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
                                    1,
                                );
                            }
                            let dst = bytecode[pc + 2];
                            let src1 = bytecode[pc + 3];
                            let src2 = bytecode[pc + 4];

                            let mut operands = format!(
                                "{}, {}, {}",
                                format_reg(dst),
                                format_reg(src1),
                                format_reg(src2)
                            );

                            // Add annotation for R0
                            if dst == 0 {
                                operands.push_str("  ; R0 = return value");
                            } else if (1..=8).contains(&dst) {
                                operands.push_str(&format!("  ; arg{}", dst - 1));
                            }

                            (FormattedInstruction(ext_mnemonic.to_string(), operands), 5)
                        }
                        None => (
                            FormattedInstruction(
                                format!("INVALID(0xFF_{:02X})", ext_opcode),
                                "TRUNCATED".to_string(),
                            ),
                            2,
                        ),
                    }
                }

                InstructionFormat::None => (
                    FormattedInstruction(mnemonic.to_string(), "".to_string()),
                    1,
                ),
            }
        }
        None => (
            FormattedInstruction(format!("INVALID(0x{:02X})", opcode), "".to_string()),
            1,
        ),
    }
}

fn decode_call_instruction(
    bytecode: &[u8],
    pc: usize,
    mnemonic: &str,
) -> (FormattedInstruction, usize) {
    if pc + 2 >= bytecode.len() {
        return (
            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
            1,
        );
    }

    let func_id = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]);
    if pc + 3 >= bytecode.len() {
        return (
            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
            1,
        );
    }

    let argc = bytecode[pc + 3];
    let base_size = 4; // opcode + func_id + argc
    let total_size = base_size + argc as usize;

    if pc + total_size > bytecode.len() {
        return (
            FormattedInstruction("TRUNCATED".to_string(), "".to_string()),
            1,
        );
    }

    let mut args = Vec::new();
    for i in 0..argc {
        args.push(format_reg(bytecode[pc + 4 + i as usize]));
    }

    let args_str = args.join(", ");
    let operands = format!("func_{}, {}, [{}]", func_id, argc, args_str);

    (
        FormattedInstruction(mnemonic.to_string(), operands),
        total_size,
    )
}

fn format_reg(reg: u8) -> String {
    format!("R{}", reg)
}

fn format_imm(val: i64) -> String {
    if (-128..=127).contains(&val) {
        format!("{}", val)
    } else {
        format!("0x{:X}", val)
    }
}

fn format_uimm(val: u64) -> String {
    if val <= 127 {
        format!("{}", val)
    } else {
        format!("0x{:X}", val)
    }
}

fn format_raw_bytes(bytes: &[u8]) -> String {
    let hex: String = bytes
        .iter()
        .map(|b| format!("{:02X}", b))
        .collect::<Vec<_>>()
        .join(" ");

    // Pad to 27 characters for alignment (13 bytes * 3 chars each, plus spaces)
    let padded_hex = format!("{:27}", hex);
    format!("{}    ", padded_hex)
}

fn get_extended_opcode_mnemonic(ext_opcode: u8) -> Option<(&'static str, InstructionFormat)> {
    match ext_opcode {
        0x00 => Some(("I32x4Add", InstructionFormat::RRR)),
        0x01 => Some(("I32x4Sub", InstructionFormat::RRR)),
        0x02 => Some(("I32x4Mul", InstructionFormat::RRR)),
        0x03 => Some(("I32x4Min", InstructionFormat::RRR)),
        0x04 => Some(("I32x4Max", InstructionFormat::RRR)),
        0x05 => Some(("I32x4Eq", InstructionFormat::RRR)),
        0x06 => Some(("I32x4Lt", InstructionFormat::RRR)),
        0x07 => Some(("I32x4Le", InstructionFormat::RRR)),
        0x08 => Some(("I32x4Gt", InstructionFormat::RRR)),
        0x09 => Some(("I32x4Ge", InstructionFormat::RRR)),
        0x0A => Some(("I32x4Load", InstructionFormat::RRR)),
        0x0B => Some(("I32x4Store", InstructionFormat::RRR)),
        0x0C => Some(("I32x4Splat", InstructionFormat::RRR)),
        0x0D => Some(("I32x4Extract", InstructionFormat::RRR)),
        0x0E => Some(("I32x4Replace", InstructionFormat::RRR)),
        0x20 => Some(("F32x4Add", InstructionFormat::RRR)),
        0x21 => Some(("F32x4Sub", InstructionFormat::RRR)),
        0x22 => Some(("F32x4Mul", InstructionFormat::RRR)),
        0x23 => Some(("F32x4Div", InstructionFormat::RRR)),
        0x24 => Some(("F32x4Min", InstructionFormat::RRR)),
        0x25 => Some(("F32x4Max", InstructionFormat::RRR)),
        0x26 => Some(("F32x4Sqrt", InstructionFormat::RRR)),
        0x27 => Some(("F32x4Fma", InstructionFormat::RRR)),
        0x28 => Some(("F32x4Eq", InstructionFormat::RRR)),
        0x29 => Some(("F32x4Lt", InstructionFormat::RRR)),
        0x2A => Some(("F32x4Le", InstructionFormat::RRR)),
        0x2B => Some(("F32x4Load", InstructionFormat::RRR)),
        0x2C => Some(("F32x4Store", InstructionFormat::RRR)),
        0x2D => Some(("F32x4Splat", InstructionFormat::RRR)),
        0x2E => Some(("F32x4Extract", InstructionFormat::RRR)),
        0x2F => Some(("F32x4Replace", InstructionFormat::RRR)),
        0x40 => Some(("F64x2Add", InstructionFormat::RRR)),
        0x41 => Some(("F64x2Sub", InstructionFormat::RRR)),
        0x42 => Some(("F64x2Mul", InstructionFormat::RRR)),
        0x43 => Some(("F64x2Div", InstructionFormat::RRR)),
        0x44 => Some(("F64x2Min", InstructionFormat::RRR)),
        0x45 => Some(("F64x2Max", InstructionFormat::RRR)),
        0x46 => Some(("F64x2Sqrt", InstructionFormat::RRR)),
        0x47 => Some(("F64x2Fma", InstructionFormat::RRR)),
        0x48 => Some(("F64x2Load", InstructionFormat::RRR)),
        0x49 => Some(("F64x2Store", InstructionFormat::RRR)),
        0x4A => Some(("F64x2Splat", InstructionFormat::RRR)),
        0x4B => Some(("F64x2Extract", InstructionFormat::RRR)),
        0x4C => Some(("F64x2Replace", InstructionFormat::RRR)),
        0x60 => Some(("I64x2Add", InstructionFormat::RRR)),
        0x61 => Some(("I64x2Sub", InstructionFormat::RRR)),
        0x62 => Some(("I64x2Mul", InstructionFormat::RRR)),
        0x63 => Some(("I64x2Load", InstructionFormat::RRR)),
        0x64 => Some(("I64x2Store", InstructionFormat::RRR)),
        0x65 => Some(("I64x2Splat", InstructionFormat::RRR)),
        0x66 => Some(("I64x2Extract", InstructionFormat::RRR)),
        0x67 => Some(("I64x2Replace", InstructionFormat::RRR)),
        _ => None,
    }
}

fn get_constant_value(_pool: &[u8], id: u16) -> Option<String> {
    // todo: extract from constant pool
    Some(format!("#{}", id))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::FunctionMetadata;

    #[test]
    fn test_pretty_dump() {
        let bytecode = vec![
            0x00, 0x00, 0x01, 0x02, // IntAdd R0, R1, R2
            0xA4, // Return
        ];

        let functions = vec![FunctionMetadata {
            bytecode_offset: 0,
            bytecode_length: 5,
            arg_count: 2,
            register_count: 3,
            local_stack_size: 0,
            stack_map_offset: 0,
            stack_map_count: 0,
            max_call_depth: 1,
            _padding: 0,
        }];

        let output = pretty_dump_bytecode(&bytecode, &functions, None, None, None);

        println!("{}", output);

        assert!(output.contains("IntAdd"));
        assert!(output.contains("R0, R1, R2"));
        assert!(output.contains("Return"));
        assert!(output.contains("FUNCTION: func_0"));
        assert!(output.contains("R0 = return value"));
    }

    #[test]
    fn test_various_instructions() {
        let bytecode = vec![
            0x00, 0x00, 0x01, 0x02, // IntAdd R0, R1, R2
            0xC4, 0x01, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // MoveImm64 R1, 5
            0x90, 0x06, 0x00, // Jump +6
            0xA4, // Return
        ];

        let functions = vec![FunctionMetadata {
            bytecode_offset: 0,
            bytecode_length: 18,
            arg_count: 0,
            register_count: 3,
            local_stack_size: 0,
            stack_map_offset: 0,
            stack_map_count: 0,
            max_call_depth: 1,
            _padding: 0,
        }];

        let output = pretty_dump_bytecode(&bytecode, &functions, None, None, None);

        println!("{}", output);

        assert!(output.contains("IntAdd"));
        assert!(output.contains("MoveImm64"));
        assert!(output.contains("Jump"));
        assert!(output.contains("5"));
    }
}
