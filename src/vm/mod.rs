pub const REGISTER_COUNT: usize = 256;

pub mod disassembler;

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    // INTEGER ARITHMETIC (0x00-0x0F)
    IntAdd = 0x00, // dst = src1 + src2
    IntSub = 0x01, // dst = src1 - src2
    IntMul = 0x02, // dst = src1 * src2
    IntDiv = 0x03, // dst = src1 / src2 (trap on zero)
    IntMod = 0x04, // dst = src1 % src2
    IntNeg = 0x05, // dst = -src

    IntAddImm8 = 0x06, // dst = src + imm8 (FAST PATH for +1, -1, etc)
    IntSubImm8 = 0x07, // dst = src - imm8
    IntMulImm8 = 0x08, // dst = src * imm8 (for array indexing)
    IntEqZero = 0x09,  // dst = (src == 0) - common case
    IntNeZero = 0x0A,  // dst = (src != 0)

    IntMin = 0x0B, // dst = min(src1, src2)
    IntMax = 0x0C, // dst = max(src1, src2)
    IntAbs = 0x0D, // dst = abs(src)

    // FLOAT ARITHMETIC (0x10-0x1F)
    FloatAdd = 0x10,
    FloatSub = 0x11,
    FloatMul = 0x12,
    FloatDiv = 0x13,
    FloatNeg = 0x14,
    FloatPow = 0x15,

    FloatSqrt = 0x16,  // dst = sqrt(src)
    FloatFloor = 0x17, // dst = floor(src)
    FloatCeil = 0x18,  // dst = ceil(src)
    FloatRound = 0x19, // dst = round(src)

    FloatMin = 0x1A,
    FloatMax = 0x1B,
    FloatAbs = 0x1C,

    // COMPARISONS (0x20-0x2F)
    IntEq = 0x20, // dst = (src1 == src2)
    IntNe = 0x21,
    IntLt = 0x22,
    IntLe = 0x23,
    IntGt = 0x24,
    IntGe = 0x25,

    IntLtU = 0x26, // Unsigned comparisons
    IntLeU = 0x27,
    IntGtU = 0x28,
    IntGeU = 0x29,

    FloatEq = 0x2A,
    FloatNe = 0x2B,
    FloatLt = 0x2C,
    FloatLe = 0x2D,
    FloatGt = 0x2E,
    FloatGe = 0x2F,

    // BITWISE & LOGICAL (0x30-0x3F)
    And = 0x30, // Bitwise AND
    Or = 0x31,  // Bitwise OR
    Xor = 0x32, // Bitwise XOR
    Not = 0x33, // Bitwise NOT

    Shl = 0x34,  // Shift left
    Shr = 0x35,  // Shift right logical
    Sar = 0x36,  // Shift right arithmetic
    Rotl = 0x37, // Rotate left
    Rotr = 0x38, // Rotate right

    Clz = 0x39,    // Count leading zeros (single-cycle on modern CPUs)
    Ctz = 0x3A,    // Count trailing zeros
    Popcnt = 0x3B, // Population count (number of 1 bits)

    // CONVERSION (0x40-0x4F)
    IntToFloat = 0x40,    // dst_float = (float)src_int
    FloatToInt = 0x41,    // dst_int = (int)src_float
    IntToString = 0x42,   // dst_ptr = int_to_string(src_int)
    FloatToString = 0x43, // dst_ptr = float_to_string(src_float)
    BoolToString = 0x44,

    StringToInt = 0x45, // dst_int = parse_int(src_ptr)
    StringToFloat = 0x46,

    ReinterpretIntAsFloat = 0x47, // Bitcast
    ReinterpretFloatAsInt = 0x48,

    // MEMORY - LOADS (0x50-0x5F)
    LoadLocal8 = 0x50, // Load from stack frame offset
    LoadLocal16 = 0x51,
    LoadLocal32 = 0x52,
    LoadLocal64 = 0x53,
    LoadLocalPtr = 0x54,

    LoadGlobal = 0x55, // Load from global table
    LoadConst = 0x56,  // Load from constant pool

    LoadField8 = 0x57, // Load field at offset from ptr in register
    LoadField16 = 0x58,
    LoadField32 = 0x59,
    LoadField64 = 0x5A,
    LoadFieldPtr = 0x5B,

    LoadIndirect = 0x5C, // dst = *src (generic pointer load)

    // MEMORY - STORES (0x60-0x6F)
    StoreLocal8 = 0x60, // Store to stack frame offset
    StoreLocal16 = 0x61,
    StoreLocal32 = 0x62,
    StoreLocal64 = 0x63,
    StoreLocalPtr = 0x64,

    StoreGlobal = 0x65,

    StoreField8 = 0x66, // Store to field at offset from ptr
    StoreField16 = 0x67,
    StoreField32 = 0x68,
    StoreField64 = 0x69,
    StoreFieldPtr = 0x6A,

    StoreIndirect = 0x6B, // *dst = src

    // ARRAYS & STRINGS (0x70-0x7F)
    ArrayNew = 0x70,   // dst = alloc_array(elem_type_id, length_reg)
    ArrayLoad8 = 0x71, // dst = array[index] (1 byte elem)
    ArrayLoad16 = 0x72,
    ArrayLoad32 = 0x73,
    ArrayLoad64 = 0x74,
    ArrayLoadPtr = 0x75,

    ArrayStore8 = 0x76, // array[index] = src
    ArrayStore16 = 0x77,
    ArrayStore32 = 0x78,
    ArrayStore64 = 0x79,
    ArrayStorePtr = 0x7A,

    ArrayLen = 0x7B, // dst = array.length

    StringConcat = 0x7C, // dst = concat(src1, src2)
    StringLen = 0x7D,    // dst = strlen(src)
    StringSlice = 0x7E,  // dst = slice(src, start_reg, end_reg)

    // CONSTRUCTION (0x80-0x8F)
    StructNew = 0x80, // dst = alloc_struct(struct_id)
    EnumNew = 0x81,   // dst = alloc_enum(enum_id, variant_id)
    TupleNew = 0x82,  // dst = alloc_tuple(size)

    // Fast paths for common sizes
    StructNew8 = 0x83, // Struct with 8-byte size
    StructNew16 = 0x84,
    StructNew24 = 0x85,
    StructNew32 = 0x86,

    // CONTROL FLOW (0x90-0x9F)
    Jump = 0x90,     // Unconditional jump (i16 offset)
    JumpLong = 0x91, // Jump with i32 offset

    JumpIf = 0x92, // if (cond_reg) jump
    JumpIfNot = 0x93,

    // Switch/Match compilation
    TableSwitch = 0x94,  // Jump table for dense cases [opcode][reg][table_offset]
    LookupSwitch = 0x95, // Binary search for sparse cases

    // Specialized comparisons + jump (branch fusion)
    JumpIfEqZero = 0x96, // if (reg == 0) jump - COMMON in pattern matching
    JumpIfNeZero = 0x97,
    JumpIfLtZero = 0x98,
    JumpIfGeZero = 0x99,

    // FUNCTION CALLS (0xA0-0xAF)
    Call = 0xA0,         // Call function (ghccc)
    CallTail = 0xA1,     // Tail call (reuses current frame)
    CallIndirect = 0xA2, // Call function pointer
    CallBuiltin = 0xA3,  // Call VM builtin (print, etc)

    Return = 0xA4,     // Return value in R0
    ReturnVoid = 0xA5, // Return without value

    CallKnownTail = 0xA6, // Compiler-proven tail call (skip frame setup)

    // EFFECTS (0xB0-0xBF)
    Perform = 0xB0,     // Perform effect operation
    Resume = 0xB1,      // Resume continuation
    PushHandler = 0xB2, // Install effect handler
    PopHandler = 0xB3,  // Remove effect handler

    // Capture continuation for effects
    CaptureContinuation = 0xB4,
    RestoreContinuation = 0xB5,

    // REGISTER MOVES (0xC0-0xCF)
    Move = 0xC0, // dst = src (register to register)

    // Immediate loads
    MoveImm8 = 0xC1, // dst = imm8
    MoveImm16 = 0xC2,
    MoveImm32 = 0xC3,
    MoveImm64 = 0xC4,

    // Special immediates (common constants)
    MoveZero = 0xC5,     // dst = 0
    MoveOne = 0xC6,      // dst = 1
    MoveMinusOne = 0xC7, // dst = -1
    MoveTrue = 0xC8,     // dst = true
    MoveFalse = 0xC9,    // dst = false
    MoveNull = 0xCA,     // dst = null

    // Conditional move (no branching!)
    Select = 0xCB, // dst = cond ? src1 : src2

    // GC OPERATIONS (0xD0-0xDF)
    // Fast-path allocations (size-specialized)
    GCAllocFast8 = 0xD0, // dst = TLAB_alloc(8) - bump pointer only
    GCAllocFast16 = 0xD1,
    GCAllocFast24 = 0xD2,
    GCAllocFast32 = 0xD3,
    GCAllocFast64 = 0xD4,

    // Slow path
    GCAllocSlow = 0xD5, // dst = gc_alloc(size_reg, align_reg)

    GCSafepoint = 0xD6,  // Cooperative GC point
    WriteBarrier = 0xD7, // *ptr = val; record_write(ptr) for concurrent GC
    ReadBarrier = 0xD8,  // val = *ptr; check_forwarded(val) for moving GC

    // Mark value as GC root
    DeclareRoot = 0xD9,

    // ATOMICS (0xE0-0xEF) - For concurrency
    AtomicLoad = 0xE0,  // dst = atomic_load(ptr, ordering)
    AtomicStore = 0xE1, // atomic_store(ptr, val, ordering)
    AtomicCAS = 0xE2,   // compare-and-swap
    AtomicFetchAdd = 0xE3,
    AtomicFetchSub = 0xE4,
    AtomicFetchAnd = 0xE5,
    AtomicFetchOr = 0xE6,
    AtomicFetchXor = 0xE7,

    // Memory ordering fence
    FenceAcquire = 0xE8,
    FenceRelease = 0xE9,
    FenceSeqCst = 0xEA,

    // TRAPS (0xF0-0xF7) - Only what static analysis can't prove
    TrapBoundsCheck = 0xF0, // Trap if index >= length
    TrapDivByZero = 0xF1,   // Trap if divisor == 0
    TrapNullCheck = 0xF2,   // Trap if ptr == null
    TrapOverflow = 0xF3,    // Trap on integer overflow

    Unreachable = 0xF4, // Marks unreachable code (optimizer hint)

    // Debug/profiling
    DebugTrap = 0xF5,    // Breakpoint for debugger
    ProfileEnter = 0xF6, // Function entry profiling
    ProfileExit = 0xF7,  // Function exit profiling

    // SIMD OPERATIONS (0x01XX - 0x03XX) - Two-byte opcodes
    // Use 0xFF as escape prefix for extended opcodes
    ExtendedOp = 0xFF, // Next byte is extended opcode
}

// Extended opcodes (after 0xFF prefix)
#[repr(u8)]
pub enum ExtendedOpcode {
    // SIMD - i32x4 (0x00-0x1F)
    I32x4Add = 0x00, // dst_vec = src1_vec + src2_vec (4x i32)
    I32x4Sub = 0x01,
    I32x4Mul = 0x02,
    I32x4Min = 0x03,
    I32x4Max = 0x04,

    I32x4Eq = 0x05, // Compare, result is mask
    I32x4Lt = 0x06,
    I32x4Le = 0x07,
    I32x4Gt = 0x08,
    I32x4Ge = 0x09,

    I32x4Load = 0x0A, // Load 4x i32 from aligned address
    I32x4Store = 0x0B,
    I32x4Splat = 0x0C,   // dst_vec = {src, src, src, src}
    I32x4Extract = 0x0D, // dst_scalar = src_vec[lane]
    I32x4Replace = 0x0E, // dst_vec = src_vec with [lane] = scalar

    // SIMD - f32x4 (0x20-0x3F)
    F32x4Add = 0x20,
    F32x4Sub = 0x21,
    F32x4Mul = 0x22,
    F32x4Div = 0x23,
    F32x4Min = 0x24,
    F32x4Max = 0x25,
    F32x4Sqrt = 0x26,
    F32x4Fma = 0x27, // Fused multiply-add (FAST on modern CPUs)

    F32x4Eq = 0x28,
    F32x4Lt = 0x29,
    F32x4Le = 0x2A,

    F32x4Load = 0x2B,
    F32x4Store = 0x2C,
    F32x4Splat = 0x2D,
    F32x4Extract = 0x2E,
    F32x4Replace = 0x2F,

    // SIMD - f64x2 (0x40-0x5F)
    F64x2Add = 0x40,
    F64x2Sub = 0x41,
    F64x2Mul = 0x42,
    F64x2Div = 0x43,
    F64x2Min = 0x44,
    F64x2Max = 0x45,
    F64x2Sqrt = 0x46,
    F64x2Fma = 0x47,

    F64x2Load = 0x48,
    F64x2Store = 0x49,
    F64x2Splat = 0x4A,
    F64x2Extract = 0x4B,
    F64x2Replace = 0x4C,

    // SIMD - i64x2 (0x60-0x7F)
    I64x2Add = 0x60,
    I64x2Sub = 0x61,
    I64x2Mul = 0x62,

    I64x2Load = 0x63,
    I64x2Store = 0x64,
    I64x2Splat = 0x65,
    I64x2Extract = 0x66,
    I64x2Replace = 0x67, // HEHEHEHEHHEHEHEHE 67
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Reg(pub u8);

pub struct VM<'a> {
    pub bytecode: &'a [u8],
    pub debug_info: &'a [DebugEntry],
    pub functions: &'a [FunctionMetadata],
    pub struct_layouts: &'a [StructLayout],
    pub enum_layouts: &'a [EnumLayout],
    pub constant_pool: &'a [u8],
    pub stack_maps: &'a [u8],
    pub globals: &'a [u8],
    pub entry_point: u32,

    pub registers: [Value; 256],
    pub pc: u32,   // Program counter (bytecode offset)
    pub sp: usize, // Stack pointer
    pub fp: usize, // Frame pointer

    pub stack: &'a [u8],
    pub call_frames: Vec<CallFrame>,
    pub effect_handlers: Vec<EffectHandler>,

    pub heap_start: *mut u8,
    pub heap_size: usize,
    pub tlab_ptr: *mut u8,
    pub tlab_end: *mut u8,

    pub running: bool,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct DebugEntry {
    pub span_start: usize,
    pub span_end: usize,
    pub file_id: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FunctionMetadata {
    pub bytecode_offset: u32,
    pub bytecode_length: u32,

    pub arg_count: u8,
    pub register_count: u8,
    pub local_stack_size: u16,

    pub stack_map_offset: u32,
    pub stack_map_count: u16,

    // For your static analysis results
    pub max_call_depth: u8,

    pub _padding: u8,
}

#[repr(C)]
pub struct StackMapEntry {
    pub bytecode_offset: u16,
    pub live_ptr_count: u8,
    // live_ptr_regs follows immediately in memory
    // Access via: unsafe { slice::from_raw_parts(ptr.add(1), live_ptr_count) }
}

impl StackMapEntry {
    pub fn live_regs(&self) -> &[u8] {
        unsafe {
            let ptr = (self as *const Self).add(1) as *const u8;
            std::slice::from_raw_parts(ptr, self.live_ptr_count as usize)
        }
    }

    pub fn size_bytes(&self) -> usize {
        3 + self.live_ptr_count as usize // header + register list
    }
}

#[repr(C)]
pub struct StructLayout {
    pub size: u32,
    pub alignment: u16,
    pub gc_ptr_offsets: Box<[u16]>, // Where the GC pointers are
}

#[repr(C)]
pub struct EnumLayout {
    pub size: u32,
    pub alignment: u16,
    pub discriminant_offset: u16,
    pub discriminant_size: u8,
    pub variant_layouts: Box<[VariantLayout]>,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct VariantLayout {
    pub discriminant: u32,
    pub gc_ptr_offsets_start: u16, // Index into shared array
    pub gc_ptr_offsets_count: u16,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union Value {
    pub int: i64,
    pub float: f64,
    pub ptr: *mut u8,
    pub raw: u64, // For bit manipulation
}

#[repr(C)]
pub struct CallFrame {
    pub return_pc: u32,   // Where to return to
    pub return_fp: usize, // Caller's frame pointer
    pub function_id: u16, // Which function
    pub register_base: u8,
    pub _padding: u8,
}

pub struct EffectHandler {
    pub effect_id: u16,
    pub operation_id: u8,
    pub handler_pc: u32,
    pub captured_fp: usize,
}

/*
INSTRUCTION ENCODING REFERENCE
==============================

3-OPERAND ARITHMETIC (4 bytes):
  IntAdd/Sub/Mul/Div/Mod, FloatAdd/Sub/Mul/Div, etc.
  [opcode:u8][dst:u8][src1:u8][src2:u8]

2-OPERAND (3 bytes):
  IntNeg, FloatNeg, Clz, Ctz, Move, etc.
  [opcode:u8][dst:u8][src:u8]

IMMEDIATE VARIANTS (4 bytes):
  IntAddImm8, IntSubImm8, IntMulImm8
  [opcode:u8][dst:u8][src:u8][imm:i8]

MEMORY LOADS (4-5 bytes):
  LoadLocal*: [opcode:u8][dst:u8][offset:u16]
  LoadField*: [opcode:u8][dst:u8][ptr:u8][offset:u16]

MEMORY STORES (4-5 bytes):
  StoreLocal*: [opcode:u8][src:u8][offset:u16]
  StoreField*: [opcode:u8][ptr:u8][offset:u16][src:u8]

CONTROL FLOW:
  Jump: [opcode:u8][offset:i16] = 3 bytes
  JumpIf: [opcode:u8][cond:u8][offset:i16] = 4 bytes

CALLS (variable):
  Call: [opcode:u8][func_id:u16][argc:u8][args...] = 4+argc bytes
  Return: [opcode:u8] = 1 byte

MOVES:
  Move: [opcode:u8][dst:u8][src:u8] = 3 bytes
  MoveImm8: [opcode:u8][dst:u8][imm:i8] = 3 bytes
  MoveImm64: [opcode:u8][dst:u8][imm:i64] = 10 bytes
  MoveZero/One: [opcode:u8][dst:u8] = 2 bytes

GC OPERATIONS:
  GCAllocFast*: [opcode:u8][dst:u8] = 2 bytes
  GCAllocSlow: [opcode:u8][dst:u8][size:u8][align:u8] = 4 bytes

SIMD (5 bytes):
  [0xFF][ext_opcode:u8][dst:u8][src1:u8][src2:u8]
*/

pub struct DecodedInstruction {
    pub opcode: Opcode,
    pub operands: Operands,
    pub size: usize, // How many bytes this instruction took
}

pub enum Operands {
    // 3-register operations (IntAdd, FloatMul, etc.)
    RRR {
        dst: Reg,
        src1: Reg,
        src2: Reg,
    },

    // 2-register operations (IntNeg, Move, etc.)
    RR {
        dst: Reg,
        src: Reg,
    },

    // Register + immediate
    RI8 {
        dst: Reg,
        src: Reg,
        imm: i8,
    },
    RI16 {
        dst: Reg,
        src: Reg,
        imm: i16,
    },
    RI64 {
        dst: Reg,
        imm: i64,
    },

    // Memory operations
    LoadLocal {
        dst: Reg,
        offset: u16,
    },
    StoreLocal {
        src: Reg,
        offset: u16,
    },
    LoadField {
        dst: Reg,
        ptr: Reg,
        offset: u16,
    },

    // Control flow
    Jump {
        offset: i16,
    },
    Branch {
        cond: Reg,
        offset: i16,
    },

    // Calls
    Call {
        func_id: u16,
        argc: u8,
        args: Vec<Reg>,
    },

    // No operands
    None,
}

impl VM<'_> {
    pub fn decode(&self, pc: usize) -> Result<DecodedInstruction, DecodeError> {
        if pc >= self.bytecode.len() {
            return Err(DecodeError::InvalidOpcode(0xFF)); // End of bytecode
        }

        let opcode = self.bytecode[pc];

        // Match the implementation from disassembler.rs
        match opcode {
            // INTEGER ARITHMETIC (0x00-0x0F)
            0x00 => {
                // IntAdd
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntAdd,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x01 => {
                // IntSub
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntSub,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x02 => {
                // IntMul
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntMul,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x03 => {
                // IntDiv
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntDiv,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x04 => {
                // IntMod
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntMod,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x05 => {
                // IntNeg
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntNeg,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // IMMEDIATE ARITHMETIC (0x06-0x0A)
            0x06 => {
                // IntAddImm8
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntAddImm8,
                    operands: Operands::RI8 {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                        imm: self.bytecode[pc + 3] as i8,
                    },
                    size: 4,
                })
            }
            0x07 => {
                // IntSubImm8
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntSubImm8,
                    operands: Operands::RI8 {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                        imm: self.bytecode[pc + 3] as i8,
                    },
                    size: 4,
                })
            }
            0x08 => {
                // IntMulImm8
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntMulImm8,
                    operands: Operands::RI8 {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                        imm: self.bytecode[pc + 3] as i8,
                    },
                    size: 4,
                })
            }
            0x09 => {
                // IntEqZero
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntEqZero,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x0A => {
                // IntNeZero
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntNeZero,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x0B => {
                // IntMin
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntMin,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x0C => {
                // IntMax
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntMax,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x0D => {
                // IntAbs
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntAbs,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // FLOAT ARITHMETIC (0x10-0x1F)
            0x10 => {
                // FloatAdd
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatAdd,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x11 => {
                // FloatSub
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatSub,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x12 => {
                // FloatMul
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatMul,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x13 => {
                // FloatDiv
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatDiv,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x14 => {
                // FloatNeg
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatNeg,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x15 => {
                // FloatPow
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatPow,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x16 => {
                // FloatSqrt
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatSqrt,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x17 => {
                // FloatFloor
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatFloor,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x18 => {
                // FloatCeil
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatCeil,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x19 => {
                // FloatRound
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatRound,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x1A => {
                // FloatMin
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatMin,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x1B => {
                // FloatMax
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatMax,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x1C => {
                // FloatAbs
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatAbs,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // COMPARISONS (0x20-0x2F)
            0x20 => {
                // IntEq
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntEq,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x21 => {
                // IntNe
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntNe,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x22 => {
                // IntLt
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntLt,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x23 => {
                // IntLe
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntLe,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x24 => {
                // IntGt
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntGt,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x25 => {
                // IntGe
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntGe,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x26 => {
                // IntLtU
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntLtU,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x27 => {
                // IntLeU
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntLeU,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x28 => {
                // IntGtU
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntGtU,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x29 => {
                // IntGeU
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntGeU,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x2A => {
                // FloatEq
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatEq,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x2B => {
                // FloatNe
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatNe,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x2C => {
                // FloatLt
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatLt,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x2D => {
                // FloatLe
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatLe,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x2E => {
                // FloatGt
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatGt,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x2F => {
                // FloatGe
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatGe,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }

            // BITWISE & LOGICAL (0x30-0x3F)
            0x30 => {
                // And
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::And,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x31 => {
                // Or
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Or,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x32 => {
                // Xor
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Xor,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x33 => {
                // Not
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Not,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x34 => {
                // Shl
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Shl,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x35 => {
                // Shr
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Shr,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x36 => {
                // Sar
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Sar,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x37 => {
                // Rotl
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Rotl,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x38 => {
                // Rotr
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Rotr,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x39 => {
                // Clz
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Clz,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x3A => {
                // Ctz
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Ctz,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x3B => {
                // Popcnt
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Popcnt,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // CONVERSION (0x40-0x4F)
            0x40 => {
                // IntToFloat
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntToFloat,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x41 => {
                // FloatToInt
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatToInt,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x42 => {
                // IntToString
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::IntToString,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x43 => {
                // FloatToString
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::FloatToString,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x44 => {
                // BoolToString
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::BoolToString,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x45 => {
                // StringToInt
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StringToInt,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x46 => {
                // StringToFloat
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StringToFloat,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x47 => {
                // ReinterpretIntAsFloat
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ReinterpretIntAsFloat,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x48 => {
                // ReinterpretFloatAsInt
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ReinterpretFloatAsInt,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // MEMORY - LOADS (0x50-0x5F)
            0x50 => {
                // LoadLocal8
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadLocal8,
                    operands: Operands::LoadLocal {
                        dst: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x51 => {
                // LoadLocal16
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadLocal16,
                    operands: Operands::LoadLocal {
                        dst: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x52 => {
                // LoadLocal32
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadLocal32,
                    operands: Operands::LoadLocal {
                        dst: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x53 => {
                // LoadLocal64
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadLocal64,
                    operands: Operands::LoadLocal {
                        dst: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x54 => {
                // LoadLocalPtr
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadLocalPtr,
                    operands: Operands::LoadLocal {
                        dst: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x55 => {
                // LoadGlobal
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadGlobal,
                    operands: Operands::LoadLocal {
                        dst: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x56 => {
                // LoadConst
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadConst,
                    operands: Operands::LoadLocal {
                        dst: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x57 => {
                // LoadField8
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadField8,
                    operands: Operands::LoadField {
                        dst: Reg(self.bytecode[pc + 1]),
                        ptr: Reg(self.bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x58 => {
                // LoadField16
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadField16,
                    operands: Operands::LoadField {
                        dst: Reg(self.bytecode[pc + 1]),
                        ptr: Reg(self.bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x59 => {
                // LoadField32
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadField32,
                    operands: Operands::LoadField {
                        dst: Reg(self.bytecode[pc + 1]),
                        ptr: Reg(self.bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x5A => {
                // LoadField64
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadField64,
                    operands: Operands::LoadField {
                        dst: Reg(self.bytecode[pc + 1]),
                        ptr: Reg(self.bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x5B => {
                // LoadFieldPtr
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadFieldPtr,
                    operands: Operands::LoadField {
                        dst: Reg(self.bytecode[pc + 1]),
                        ptr: Reg(self.bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x5C => {
                // LoadIndirect
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::LoadIndirect,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // MEMORY - STORES (0x60-0x6F)
            0x60 => {
                // StoreLocal8
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreLocal8,
                    operands: Operands::StoreLocal {
                        src: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x61 => {
                // StoreLocal16
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreLocal16,
                    operands: Operands::StoreLocal {
                        src: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x62 => {
                // StoreLocal32
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreLocal32,
                    operands: Operands::StoreLocal {
                        src: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x63 => {
                // StoreLocal64
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreLocal64,
                    operands: Operands::StoreLocal {
                        src: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x64 => {
                // StoreLocalPtr
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreLocalPtr,
                    operands: Operands::StoreLocal {
                        src: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x65 => {
                // StoreGlobal
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreGlobal,
                    operands: Operands::StoreLocal {
                        src: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 3,
                })
            }
            0x66 => {
                // StoreField8
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreField8,
                    operands: Operands::LoadField {
                        dst: Reg(self.bytecode[pc + 1]),
                        ptr: Reg(self.bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x67 => {
                // StoreField16
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreField16,
                    operands: Operands::LoadField {
                        dst: Reg(self.bytecode[pc + 1]),
                        ptr: Reg(self.bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x68 => {
                // StoreField32
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreField32,
                    operands: Operands::LoadField {
                        dst: Reg(self.bytecode[pc + 1]),
                        ptr: Reg(self.bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x69 => {
                // StoreField64
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreField64,
                    operands: Operands::LoadField {
                        dst: Reg(self.bytecode[pc + 1]),
                        ptr: Reg(self.bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x6A => {
                // StoreFieldPtr
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreFieldPtr,
                    operands: Operands::LoadField {
                        dst: Reg(self.bytecode[pc + 1]),
                        ptr: Reg(self.bytecode[pc + 2]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x6B => {
                // StoreIndirect
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StoreIndirect,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }

            // ARRAYS & STRINGS (0x70-0x7F)
            0x70 => {
                // ArrayNew
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayNew,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x71 => {
                // ArrayLoad8
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayLoad8,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x72 => {
                // ArrayLoad16
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayLoad16,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x73 => {
                // ArrayLoad32
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayLoad32,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x74 => {
                // ArrayLoad64
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayLoad64,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x75 => {
                // ArrayLoadPtr
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayLoadPtr,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x76 => {
                // ArrayStore8
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayStore8,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x77 => {
                // ArrayStore16
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayStore16,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x78 => {
                // ArrayStore32
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayStore32,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x79 => {
                // ArrayStore64
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayStore64,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x7A => {
                // ArrayStorePtr
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayStorePtr,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x7B => {
                // ArrayLen
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ArrayLen,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x7C => {
                // StringConcat
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StringConcat,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0x7D => {
                // StringLen
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StringLen,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0x7E => {
                // StringSlice
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StringSlice,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }

            // CONSTRUCTION (0x80-0x8F)
            0x80 => {
                // StructNew
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StructNew,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x81 => {
                // EnumNew
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::EnumNew,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x82 => {
                // TupleNew
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::TupleNew,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x83 => {
                // StructNew8
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StructNew8,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x84 => {
                // StructNew16
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StructNew16,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x85 => {
                // StructNew24
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StructNew24,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0x86 => {
                // StructNew32
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::StructNew32,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }

            // CONTROL FLOW (0x90-0x9F)
            0x90 => {
                // Jump
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = i16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::Jump,
                    operands: Operands::Jump { offset },
                    size: 3,
                })
            }
            0x91 => {
                // JumpLong
                if pc + 4 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = i32::from_le_bytes([
                    self.bytecode[pc + 1],
                    self.bytecode[pc + 2],
                    self.bytecode[pc + 3],
                    self.bytecode[pc + 4],
                ]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpLong,
                    operands: Operands::RR {
                        dst: Reg(offset as u8), // Using dst as a temporary to store offset
                        src: Reg(0),            // placeholder
                    },
                    size: 5,
                })
            }
            0x92 => {
                // JumpIf
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = i16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpIf,
                    operands: Operands::Branch {
                        cond: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x93 => {
                // JumpIfNot
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = i16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpIfNot,
                    operands: Operands::Branch {
                        cond: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x94 => {
                // TableSwitch
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::TableSwitch,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(offset as u8), // Using src to store part of the offset
                    },
                    size: 3,
                })
            }
            0x95 => {
                // LookupSwitch
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::LookupSwitch,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(offset as u8), // Using src to store part of the offset
                    },
                    size: 3,
                })
            }
            0x96 => {
                // JumpIfEqZero
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = i16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpIfEqZero,
                    operands: Operands::Branch {
                        cond: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x97 => {
                // JumpIfNeZero
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = i16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpIfNeZero,
                    operands: Operands::Branch {
                        cond: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x98 => {
                // JumpIfLtZero
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = i16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpIfLtZero,
                    operands: Operands::Branch {
                        cond: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }
            0x99 => {
                // JumpIfGeZero
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = i16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::JumpIfGeZero,
                    operands: Operands::Branch {
                        cond: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }

            // FUNCTION CALLS (0xA0-0xAF)
            0xA0 => {
                // Call
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let func_id = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                let argc = self.bytecode[pc + 3];

                // Read the argument registers after the header
                if pc + 4 + argc as usize > self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }

                let mut args = Vec::new();
                for i in 0..argc {
                    args.push(Reg(self.bytecode[pc + 4 + i as usize]));
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
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let func_id = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                let argc = self.bytecode[pc + 3];

                // Read the argument registers after the header
                if pc + 4 + argc as usize > self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }

                let mut args = Vec::new();
                for i in 0..argc {
                    args.push(Reg(self.bytecode[pc + 4 + i as usize]));
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
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let func_id = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                let argc = self.bytecode[pc + 3];

                // Read the argument registers after the header
                if pc + 4 + argc as usize > self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }

                let mut args = Vec::new();
                for i in 0..argc {
                    args.push(Reg(self.bytecode[pc + 4 + i as usize]));
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
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let func_id = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                let argc = self.bytecode[pc + 3];

                // Read the argument registers after the header
                if pc + 4 + argc as usize > self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }

                let mut args = Vec::new();
                for i in 0..argc {
                    args.push(Reg(self.bytecode[pc + 4 + i as usize]));
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
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let func_id = u16::from_le_bytes([self.bytecode[pc + 1], self.bytecode[pc + 2]]);
                let argc = self.bytecode[pc + 3];

                // Read the argument registers after the header
                if pc + 4 + argc as usize > self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }

                let mut args = Vec::new();
                for i in 0..argc {
                    args.push(Reg(self.bytecode[pc + 4 + i as usize]));
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
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Perform,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xB1 => {
                // Resume
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Resume,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xB2 => {
                // PushHandler
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let offset = i16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::PushHandler,
                    operands: Operands::Branch {
                        cond: Reg(self.bytecode[pc + 1]),
                        offset,
                    },
                    size: 4,
                })
            }
            0xB3 => {
                // PopHandler
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::PopHandler,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xB4 => {
                // CaptureContinuation
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::CaptureContinuation,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xB5 => {
                // RestoreContinuation
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::RestoreContinuation,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }

            // REGISTER MOVES (0xC0-0xCF)
            0xC0 => {
                // Move
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Move,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xC1 => {
                // MoveImm8
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveImm8,
                    operands: Operands::RI8 {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                        imm: self.bytecode[pc + 2] as i8,
                    },
                    size: 3,
                })
            }
            0xC2 => {
                // MoveImm16
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let imm = i16::from_le_bytes([self.bytecode[pc + 2], self.bytecode[pc + 3]]);
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveImm16,
                    operands: Operands::RI16 {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                        imm,
                    },
                    size: 4,
                })
            }
            0xC3 => {
                // MoveImm32
                if pc + 5 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let imm = i32::from_le_bytes([
                    self.bytecode[pc + 2],
                    self.bytecode[pc + 3],
                    self.bytecode[pc + 4],
                    self.bytecode[pc + 5],
                ]);
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveImm32,
                    operands: Operands::RI64 {
                        dst: Reg(self.bytecode[pc + 1]),
                        imm: imm as i64,
                    },
                    size: 6,
                })
            }
            0xC4 => {
                // MoveImm64
                if pc + 9 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let imm = i64::from_le_bytes([
                    self.bytecode[pc + 2],
                    self.bytecode[pc + 3],
                    self.bytecode[pc + 4],
                    self.bytecode[pc + 5],
                    self.bytecode[pc + 6],
                    self.bytecode[pc + 7],
                    self.bytecode[pc + 8],
                    self.bytecode[pc + 9],
                ]);
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveImm64,
                    operands: Operands::RI64 {
                        dst: Reg(self.bytecode[pc + 1]),
                        imm,
                    },
                    size: 10,
                })
            }
            0xC5 => {
                // MoveZero
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveZero,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0),
                    },
                    size: 2,
                })
            }
            0xC6 => {
                // MoveOne
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveOne,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0),
                    },
                    size: 2,
                })
            }
            0xC7 => {
                // MoveMinusOne
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveMinusOne,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0),
                    },
                    size: 2,
                })
            }
            0xC8 => {
                // MoveTrue
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveTrue,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0),
                    },
                    size: 2,
                })
            }
            0xC9 => {
                // MoveFalse
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveFalse,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0),
                    },
                    size: 2,
                })
            }
            0xCA => {
                // MoveNull
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveNull,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0),
                    },
                    size: 2,
                })
            }
            0xCB => {
                // Select
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::Select,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }

            // GC OPERATIONS (0xD0-0xDF)
            0xD0 => {
                // GCAllocFast8
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::GCAllocFast8,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xD1 => {
                // GCAllocFast16
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::GCAllocFast16,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xD2 => {
                // GCAllocFast24
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::GCAllocFast24,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xD3 => {
                // GCAllocFast32
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::GCAllocFast32,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xD4 => {
                // GCAllocFast64
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::GCAllocFast64,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }
            0xD5 => {
                // GCAllocSlow
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::GCAllocSlow,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
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
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::WriteBarrier,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xD8 => {
                // ReadBarrier
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ReadBarrier,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xD9 => {
                // DeclareRoot
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::DeclareRoot,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // placeholder
                    },
                    size: 2,
                })
            }

            // ATOMICS (0xE0-0xEF)
            0xE0 => {
                // AtomicLoad
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicLoad,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE1 => {
                // AtomicStore
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicStore,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE2 => {
                // AtomicCAS
                if pc + 3 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicCAS,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src1: Reg(self.bytecode[pc + 2]),
                        src2: Reg(self.bytecode[pc + 3]),
                    },
                    size: 4,
                })
            }
            0xE3 => {
                // AtomicFetchAdd
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicFetchAdd,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE4 => {
                // AtomicFetchSub
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicFetchSub,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE5 => {
                // AtomicFetchAnd
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicFetchAnd,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE6 => {
                // AtomicFetchOr
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicFetchOr,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
                    },
                    size: 3,
                })
            }
            0xE7 => {
                // AtomicFetchXor
                if pc + 2 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::AtomicFetchXor,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(self.bytecode[pc + 2]),
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
                if pc + 1 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                let _ext_opcode = self.bytecode[pc + 1];
                // For now, just validate that it's a valid extended opcode by trying to read expected operands
                // In a real implementation, we'd have complete extended opcode validation
                if pc + 4 >= self.bytecode.len() {
                    return Err(DecodeError::InvalidOpcode(opcode));
                }
                Ok(DecodedInstruction {
                    opcode: Opcode::ExtendedOp,
                    operands: Operands::RRR {
                        dst: Reg(self.bytecode[pc + 2]),
                        src1: Reg(self.bytecode[pc + 3]),
                        src2: Reg(self.bytecode[pc + 4]),
                    },
                    size: 5,
                })
            }

            // Unknown opcode
            _ => Err(DecodeError::InvalidOpcode(opcode)),
        }
    }
}

#[derive(Debug)]
pub enum DecodeError {
    InvalidOpcode(u8),
}
