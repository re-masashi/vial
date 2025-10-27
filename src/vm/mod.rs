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
        let opcode = self.bytecode[pc];

        match opcode {
            0x00 => {
                // IntAdd
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

            0xC5 => {
                // MoveZero
                Ok(DecodedInstruction {
                    opcode: Opcode::MoveZero,
                    operands: Operands::RR {
                        dst: Reg(self.bytecode[pc + 1]),
                        src: Reg(0), // dummy
                    },
                    size: 2,
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

            // I AM NOT DOING ALL THAT BRUH WTF
            _ => Err(DecodeError::InvalidOpcode(opcode)),
        }
    }
}

#[derive(Debug)]
pub enum DecodeError {
    InvalidOpcode(u8),
}
