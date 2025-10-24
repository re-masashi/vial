use std::collections::HashMap;
use std::ops::Range;

use crate::ast::{AssignOp, BinOp, Kind, UnOp, Visibility};
use crate::typechecker::EffectSet;

pub mod builder;

// Memory management related types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MemorySlotId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AllocationId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum MemoryKind {
    Stack,     // Stack allocated (no GC)
    Heap,      // Heap allocated (GC'd)
    Static,    // Static lifetime
    Escaping,  // Escapes current scope (needs allocation)
    Temporary, // Temporary value (may be optimized away)
}

// Unique IDs for IR elements
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EffectId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariantId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BasicBlockId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(pub usize);

// Target information for cross-platform compilation
#[derive(Debug, Clone)]
pub struct TargetInfo {
    pub pointer_size: usize, // 4 (32-bit) or 8 (64-bit)
    pub int_size: usize,     // Target int size
    pub float_size: usize,   // Target float size
    pub bool_size: usize,    // Usually 1
    pub alignment_rules: AlignmentRules,
}

#[derive(Debug, Clone)]
pub enum AlignmentRules {
    Packed,  // No padding (smaller but slower)
    Natural, // Align to size (fast but bigger)
    SystemV, // System V ABI
    MsvcABI, // MSVC ABI
}

impl TargetInfo {
    // VM target - aligned for performance
    pub fn vm_target() -> Self {
        Self {
            pointer_size: 8,
            int_size: 8,
            float_size: 8,
            bool_size: 1,
            alignment_rules: AlignmentRules::Natural, // Performance!
        }
    }

    // Host target for JIT
    pub fn host_target() -> Self {
        Self {
            pointer_size: std::mem::size_of::<*const ()>(),
            int_size: std::mem::size_of::<i64>(),
            float_size: std::mem::size_of::<f64>(),
            bool_size: std::mem::size_of::<bool>(),
            alignment_rules: AlignmentRules::Natural,
        }
    }

    // For AOT backend
    pub fn from_triple(_triple: &str) -> Self {
        // use defaults based on triplet
        todo!("Implement LLVM target info")
    }
}

impl TargetInfo {
    pub fn size_of(&self, type_: &IRType, module: &IRModule) -> Option<usize> {
        match type_ {
            IRType::Bool => Some(self.bool_size),
            IRType::Int => Some(self.int_size),
            IRType::Float => Some(self.float_size),
            IRType::Unit => Some(0),
            IRType::String => Some(self.pointer_size),
            IRType::Never => None,
            IRType::Pointer(_) => Some(self.pointer_size),
            IRType::Function { .. } => Some(self.pointer_size),

            IRType::StructRef(struct_id) => {
                let idx = *module.struct_map.get(struct_id)?;
                Some(module.structs.get(idx)?.memory_layout.size)
            }

            IRType::EnumRef(enum_id) => {
                let idx = *module.enum_map.get(enum_id)?;
                Some(module.enums.get(idx)?.memory_layout.size)
            }

            IRType::Tuple(types) => self
                .compute_tuple_layout(types, module)
                .map(|layout| layout.size),

            IRType::Variable(_, _) => None,
            IRType::Union(_) => None,
            IRType::Named(_, _) => None,
            IRType::Error => None,
        }
    }

    pub fn align_of(&self, type_: &IRType, module: &IRModule) -> Option<usize> {
        match type_ {
            IRType::Bool => Some(self.bool_size),
            IRType::Int => Some(self.int_size),
            IRType::Float => Some(self.float_size),
            IRType::Unit => Some(1),
            IRType::String => Some(self.pointer_size),
            IRType::Never => None,
            IRType::Pointer(_) => Some(self.pointer_size),
            IRType::Function { .. } => Some(self.pointer_size),

            IRType::StructRef(struct_id) => {
                let idx = *module.struct_map.get(struct_id)?;
                Some(module.structs.get(idx)?.memory_layout.alignment)
            }

            IRType::EnumRef(enum_id) => {
                let idx = *module.enum_map.get(enum_id)?;
                Some(module.enums.get(idx)?.memory_layout.alignment)
            }

            IRType::Tuple(types) => {
                // Max alignment of all fields
                types
                    .iter()
                    .filter_map(|t| self.align_of(&t.type_, module))
                    .max()
            }

            IRType::Variable(_, _) => None,
            IRType::Union(_) => None,
            IRType::Named(_, _) => None,
            IRType::Error => None,
        }
    }

    // Compute tuple layout with padding (for performance)
    fn compute_tuple_layout(
        &self,
        types: &[IRTypeWithMemory],
        module: &IRModule,
    ) -> Option<TupleLayout> {
        match self.alignment_rules {
            AlignmentRules::Packed => {
                // No padding - just sum sizes
                let size = types
                    .iter()
                    .filter_map(|t| self.size_of(&t.type_, module))
                    .sum();
                Some(TupleLayout {
                    size,
                    alignment: 1,
                    field_offsets: vec![],
                })
            }
            AlignmentRules::Natural | AlignmentRules::SystemV | AlignmentRules::MsvcABI => {
                // With padding for alignment (PERF!)
                let mut offset = 0;
                let mut max_align = 1;
                let mut field_offsets = Vec::new();

                for type_with_mem in types {
                    let size = self.size_of(&type_with_mem.type_, module)?;
                    let align = self.align_of(&type_with_mem.type_, module)?;

                    // Align current offset to this field's alignment
                    offset = (offset + align - 1) & !(align - 1);
                    field_offsets.push(offset);
                    offset += size;
                    max_align = max_align.max(align);
                }

                // Align total size to max alignment (tail padding)
                offset = (offset + max_align - 1) & !(max_align - 1);

                Some(TupleLayout {
                    size: offset,
                    alignment: max_align,
                    field_offsets,
                })
            }
        }
    }

    // Compute struct/enum layout
    pub fn compute_aggregate_layout(
        &self,
        fields: &[(FieldId, &IRType)],
        module: &IRModule,
    ) -> Option<MemoryLayout> {
        let mut offset = 0;
        let mut max_align = 1;
        let mut field_offsets = Vec::new();
        let mut padding_bytes = Vec::new();

        for (field_id, field_type) in fields {
            let size = self.size_of(field_type, module)?;
            let align = self.align_of(field_type, module)?;

            // Check if padding needed
            let aligned_offset = (offset + align - 1) & !(align - 1);
            if aligned_offset > offset {
                padding_bytes.push(PaddingInfo {
                    offset,
                    size: aligned_offset - offset,
                    reason: PaddingReason::FieldAlignment,
                });
            }

            offset = aligned_offset;
            field_offsets.push((*field_id, offset));
            offset += size;
            max_align = max_align.max(align);
        }

        // Tail padding
        let final_size = (offset + max_align - 1) & !(max_align - 1);
        if final_size > offset {
            padding_bytes.push(PaddingInfo {
                offset,
                size: final_size - offset,
                reason: PaddingReason::TailPadding,
            });
        }

        Some(MemoryLayout {
            size: final_size,
            alignment: max_align,
            field_offsets,
            discriminant_offset: None,
            discriminant_size: None,
            discriminant_encoding: None,
            padding_bytes,
        })
    }
}

pub struct TupleLayout {
    pub size: usize,
    pub alignment: usize,
    pub field_offsets: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct PaddingInfo {
    pub offset: usize, // Where padding starts
    pub size: usize,   // How many padding bytes
    pub reason: PaddingReason,
}

#[derive(Debug, Clone)]
pub enum PaddingReason {
    FieldAlignment, // Between fields for alignment
    TailPadding,    // At end for struct alignment
}

// Main IR module
#[derive(Debug, Clone)]
pub struct IRModule {
    pub functions: Vec<IRFunction>,
    pub structs: Vec<IRStruct>,
    pub enums: Vec<IREnum>,
    pub effects: Vec<IREffect>,

    // Maps for quick lookups by ID
    pub function_map: HashMap<FunctionId, usize>,
    pub struct_map: HashMap<StructId, usize>,
    pub enum_map: HashMap<EnumId, usize>,
    pub effect_map: HashMap<EffectId, usize>,

    // ADD THIS
    pub target: TargetInfo, // Target architecture info
}

impl IRModule {
    pub fn new(target: TargetInfo) -> Self {
        Self {
            functions: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            effects: Vec::new(),
            function_map: HashMap::new(),
            struct_map: HashMap::new(),
            enum_map: HashMap::new(),
            effect_map: HashMap::new(),
            target,
        }
    }

    pub fn size_of(&self, type_: &IRType) -> Option<usize> {
        self.target.size_of(type_, self)
    }

    pub fn align_of(&self, type_: &IRType) -> Option<usize> {
        self.target.align_of(type_, self)
    }
}

#[derive(Debug, Clone)]
pub struct IRTypeWithMemory {
    pub type_: IRType,
    pub span: Range<usize>,
    pub file: String,
    pub memory_kind: MemoryKind,
    pub allocation_id: Option<AllocationId>, // For tracking allocations
}

#[derive(Debug, Clone)]
pub enum IRType {
    // Primitives (typically stack allocated)
    Bool,
    Int,
    Float,
    String,
    Unit,
    Never,

    // Named types
    Named(String, Vec<IRTypeWithMemory>), // Generic named types

    // Function type with effects
    Function {
        params: Vec<IRTypeWithMemory>,
        return_type: Box<IRTypeWithMemory>,
        effects: EffectSet,
    },

    // Tuple type
    Tuple(Vec<IRTypeWithMemory>),

    // Union type
    Union(Vec<IRTypeWithMemory>),

    // Struct and Enum references
    StructRef(StructId),
    EnumRef(EnumId),

    // Pointer type for FFI
    Pointer(Box<IRTypeWithMemory>),

    // Type variable (for generics)
    Variable(usize, Kind),

    // Error type
    Error,
}

// Instruction metadata for memory tracking
#[derive(Debug, Clone)]
pub struct InstructionMetadata {
    pub memory_slot: Option<MemorySlotId>,
    pub allocation_site: Option<AllocationId>,
}

// IR Value - represents SSA values (separate from memory locations)
#[derive(Debug, Clone)]
pub enum IRValue {
    // Immediate values (stack allocated)
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),

    // SSA values (each ValueId is defined exactly once)
    SSA(ValueId),

    // Function references
    FunctionRef(FunctionId),

    // Constant expressions
    Constant(Constant),

    // Memory addresses
    MemoryAddress(MemorySlotId),

    // Unit value (for expressions that return nothing)
    Unit,

    // Error value
    Error,
}

#[derive(Debug, Clone)]
pub enum Constant {
    // Simple constants (stack allocated)
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),

    // Complex constants with memory information
    Array(Vec<IRValue>),
    Tuple(Vec<IRValue>),

    // Error
    Error,
}

// Basic Block - represents a sequence of instructions
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub phi_nodes: Vec<IRInstruction>, // Only PHI instructions (must be at start)
    pub instructions: Vec<IRInstruction>, // All other instructions
    pub terminator: Option<IRTerminator>,
}

impl BasicBlock {
    pub fn new(id: BasicBlockId) -> Self {
        Self {
            id,
            phi_nodes: Vec::new(),
            instructions: Vec::new(),
            terminator: None,
        }
    }

    // Helper to add PHI (validates it's actually a PHI)
    pub fn add_phi(&mut self, phi: IRInstruction) {
        match phi {
            IRInstruction::Phi { .. } => self.phi_nodes.push(phi),
            _ => panic!("Only PHI instructions allowed in phi_nodes"),
        }
    }

    // Helper to add regular instruction (validates it's NOT a PHI)
    pub fn add_instruction(&mut self, inst: IRInstruction) {
        match inst {
            IRInstruction::Phi { .. } => panic!("PHI nodes must be added via add_phi()"),
            _ => self.instructions.push(inst),
        }
    }
}

// SSA instruction - with memory management
#[derive(Debug, Clone)]
pub enum IRInstruction {
    // Binary operations
    BinOp {
        result: ValueId,
        metadata: InstructionMetadata,
        left: IRValue,
        op: BinOp,
        right: IRValue,
        span: Range<usize>,
        file: String,
    },

    // Unary operations
    UnOp {
        result: ValueId,
        metadata: InstructionMetadata,
        op: UnOp,
        operand: IRValue,
        span: Range<usize>,
        file: String,
    },

    // Assignment operations
    Assign {
        result: ValueId,
        metadata: InstructionMetadata,
        l_val: IRValue,
        r_val: IRValue,
        op: AssignOp,
        span: Range<usize>,
        file: String,
    },

    // Variable binding
    Let {
        result: ValueId,
        metadata: InstructionMetadata,
        var: String,
        value: IRValue,
        var_type: IRTypeWithMemory,
        span: Range<usize>,
        file: String,
    },

    // Array construction (heap allocation)
    Array {
        result: ValueId,
        metadata: InstructionMetadata,
        elements: Vec<IRValue>,
        element_type: IRTypeWithMemory,
        span: Range<usize>,
        file: String,
    },

    // Tuple construction (stack or heap based on size/escape)
    Tuple {
        result: ValueId,
        metadata: InstructionMetadata,
        elements: Vec<IRValue>,
        span: Range<usize>,
        file: String,
    },

    // Map construction (heap allocation)
    Map {
        result: ValueId,
        metadata: InstructionMetadata,
        entries: Vec<(IRValue, IRValue)>,
        key_type: IRTypeWithMemory,
        value_type: IRTypeWithMemory,
        span: Range<usize>,
        file: String,
    },

    // Enum construction
    EnumConstruct {
        result: ValueId,
        metadata: InstructionMetadata,
        enum_id: EnumId,
        variant_id: VariantId,
        args: Vec<IRValue>,
        span: Range<usize>,
        file: String,
    },

    // Struct construction
    StructConstruct {
        result: ValueId,
        metadata: InstructionMetadata,
        struct_id: StructId,
        fields: Vec<(FieldId, IRValue)>,
        span: Range<usize>,
        file: String,
    },

    // Effect operations
    Perform {
        result: ValueId,
        metadata: InstructionMetadata,
        effect_id: EffectId,
        operation_id: usize, // Which operation of this effect
        args: Vec<IRValue>,
        span: Range<usize>,
        file: String,
    },

    // Function call
    Call {
        result: ValueId,
        metadata: InstructionMetadata,
        function: IRValue,
        args: Vec<IRValue>,
        type_args: Vec<IRTypeWithMemory>,
        span: Range<usize>,
        file: String,
    },

    // Cast
    Cast {
        result: ValueId,
        metadata: InstructionMetadata,
        expr: IRValue,
        target_type: IRTypeWithMemory,
        span: Range<usize>,
        file: String,
    },

    // Index access
    Index {
        result: ValueId,
        metadata: InstructionMetadata,
        target: IRValue,
        index: IRValue,
        element_type: IRTypeWithMemory,
        span: Range<usize>,
        file: String,
    },

    // Field access
    FieldAccess {
        result: ValueId,
        metadata: InstructionMetadata,
        target: IRValue,
        field_id: FieldId,
        field_type: IRTypeWithMemory,
        span: Range<usize>,
        file: String,
    },

    // Optional chain
    OptionalChain {
        result: ValueId,
        metadata: InstructionMetadata,
        target: IRValue,
        field_id: FieldId,
        field_type: IRTypeWithMemory,
        span: Range<usize>,
        file: String,
    },

    // Copy/Move operations
    Copy {
        result: ValueId,
        metadata: InstructionMetadata,
        source: IRValue,
        span: Range<usize>,
        file: String,
    },

    // PHI node for SSA form
    Phi {
        result: ValueId,
        metadata: InstructionMetadata,
        incoming: Vec<(IRValue, BasicBlockId)>, // (value, source_block)
        span: Range<usize>,
        file: String,
    },

    // Memory allocation
    Allocate {
        result: ValueId,
        metadata: InstructionMetadata,
        allocation_id: AllocationId,
        size: AllocationSize,
        memory_kind: MemoryKind,
        type_info: IRTypeWithMemory,
        span: Range<usize>,
        file: String,
    },

    // Memory deallocation (for non-GC systems)
    Deallocate {
        memory_slot: MemorySlotId,
        allocation_id: AllocationId,
        span: Range<usize>,
        file: String,
    },

    // Memory copy
    MemoryCopy {
        dest: IRValue,        // Can be any address
        src: IRValue,         // Can be any address
        size: AllocationSize, // Can be dynamic
        span: Range<usize>,
        file: String,
    },

    // Load from memory
    Load {
        result: ValueId,
        address: IRValue,
        address_kind: AddressKind,
        type_info: IRTypeWithMemory,
        span: Range<usize>,
        file: String,
    },

    // Store to memory
    Store {
        address: IRValue,
        address_kind: AddressKind,
        value: IRValue,
        type_info: IRTypeWithMemory,
        span: Range<usize>,
        file: String,
    },

    // Closure construction
    ConstructClosure {
        result: ValueId,
        metadata: InstructionMetadata,
        function_id: FunctionId,
        captures: Vec<(ValueId, CaptureMode)>,
        allocation_kind: MemoryKind,
        span: Range<usize>,
        file: String,
    },

    // GC safepoint
    GCSafepoint {
        live_values: Vec<ValueId>,
        span: Range<usize>,
        file: String,
    },

    // Declare GC root
    DeclareRoot {
        value: ValueId,
        is_root: bool,
        span: Range<usize>,
        file: String,
    },

    // Trap instruction for runtime checks
    Trap {
        kind: TrapKind,
        span: Range<usize>,
        file: String,
    },

    // Select/Conditional move instruction
    Select {
        result: ValueId,
        metadata: InstructionMetadata,
        condition: IRValue,
        if_true: IRValue,
        if_false: IRValue,
        span: Range<usize>,
        file: String,
    },

    // Error instruction
    Error {
        span: Range<usize>,
        file: String,
    },
}

#[derive(Debug, Clone)]
pub enum TrapKind {
    BoundsCheck {
        index: IRValue,
        length: IRValue,
    },
    NullCheck {
        value: IRValue,
    },
    DivideByZero {
        divisor: IRValue,
    },
    IntegerOverflow {
        operation: BinOp,
        left: IRValue,
        right: IRValue,
    },
    Unreachable,
}

// Address kind for load/store operations
#[derive(Debug, Clone)]
pub enum AddressKind {
    StackSlot(MemorySlotId),
    HeapObject(AllocationId),
    StructField { base: ValueId, field: FieldId },
    ArrayElement { base: ValueId, index: ValueId },
    Computed, // Generic pointer arithmetic
}

// Capture mode for closures
#[derive(Debug, Clone)]
pub enum CaptureMode {
    ByValue,
    ByReference,
    ByMutableReference,
}

// Allocation size (static or dynamic)
#[derive(Debug, Clone)]
pub enum AllocationSize {
    Static(usize),
    Dynamic(IRValue), // Runtime-computed size
}

// Basic block terminators
#[derive(Debug, Clone)]
pub enum IRTerminator {
    // Return statement
    Return {
        value: Option<IRValue>,
        span: Range<usize>,
        file: String,
    },

    // Unconditional jump
    Jump {
        target: BasicBlockId,
        span: Range<usize>,
        file: String,
    },

    // Conditional branch
    Branch {
        condition: IRValue,
        then_block: BasicBlockId,
        else_block: BasicBlockId,
        span: Range<usize>,
        file: String,
    },

    // Switch/Match statement
    Switch {
        value: IRValue,
        cases: Vec<(IRValue, BasicBlockId)>, // (pattern, target_block)
        default: Option<BasicBlockId>,       // None if exhaustive
        is_exhaustive: bool,                 // New field
        span: Range<usize>,
        file: String,
    },

    // Loop back edge
    Loop {
        condition: IRValue,
        body: BasicBlockId,
        continue_block: BasicBlockId,
        span: Range<usize>,
        file: String,
    },

    // Unreachable
    Unreachable {
        span: Range<usize>,
        file: String,
    },

    // Effect handler
    Handle {
        body: BasicBlockId,
        handlers: Vec<EffectHandlerBlock>,
        return_type: IRTypeWithMemory,
        span: Range<usize>,
        file: String,
    },

    // Error terminator
    Error {
        span: Range<usize>,
        file: String,
    },
}

#[derive(Debug, Clone)]
pub struct EffectHandlerBlock {
    pub effect_id: EffectId,
    pub params: Vec<(ValueId, MemorySlotId, IRTypeWithMemory)>, // (binding_id, memory_slot, type)
    pub resume_param: ValueId,
    pub resume_memory: MemorySlotId,
    pub resume_type: IRTypeWithMemory,
    pub continuation_type: ContinuationType, // New field
    pub body: BasicBlockId,
    pub span: Range<usize>,
    pub file: String,
}

#[derive(Debug, Clone)]
pub struct ContinuationType {
    pub input_type: IRTypeWithMemory,  // What resume takes
    pub output_type: IRTypeWithMemory, // What resume returns
    pub captured_effects: EffectSet,   // Effects in scope
}

// IR Function definition with memory information
#[derive(Debug, Clone)]
pub struct IRFunction {
    pub id: FunctionId,
    pub name: String,
    pub vis: Visibility,
    pub args: Vec<IRFunctionArg>,
    pub return_type: IRTypeWithMemory,
    pub effects: EffectSet,
    pub function_type: IRTypeWithMemory,
    pub basic_blocks: Vec<BasicBlock>,
    pub cfg: ControlFlowGraph,
    pub span: Range<usize>,
    pub file: String,
    pub body: Option<BasicBlockId>,            // ID of the entry block
    pub memory_usage: MemoryUsage,             // Information about memory usage for analysis
    pub optimization_hints: OptimizationHints, // Optimization hints
}

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    pub blocks: HashMap<BasicBlockId, BasicBlockInfo>,
    pub entry: BasicBlockId,
    pub exits: Vec<BasicBlockId>,
}

#[derive(Debug, Clone)]
pub struct BasicBlockInfo {
    pub id: BasicBlockId,
    pub predecessors: Vec<BasicBlockId>,
    pub successors: Vec<BasicBlockId>,
    pub dominators: Option<DominatorInfo>,
}

#[derive(Debug, Clone)]
pub struct DominatorInfo {
    pub immediate_dominator: Option<BasicBlockId>,
    pub dominated_blocks: Vec<BasicBlockId>,
    pub dominance_frontier: Vec<BasicBlockId>,
}

#[derive(Debug, Clone)]
pub struct MemoryUsage {
    pub max_stack_size: usize,
    pub heap_allocations: Vec<AllocationId>,
    pub escape_analysis_info: EscapeAnalysisInfo,
}

#[derive(Debug, Clone)]
pub struct OptimizationHints {
    pub inline: InlineHint,
    pub is_pure: bool,                // No side effects
    pub is_cold: bool,                // Rarely executed
    pub should_unroll: Option<usize>, // Loop unrolling
}

#[derive(Debug, Clone)]
pub enum InlineHint {
    Always,
    Never,
    Heuristic,
}

#[derive(Debug, Clone)]
pub struct EscapeAnalysisInfo {
    // Results
    pub escaping_values: Vec<ValueId>,
    pub stack_allocated_values: Vec<ValueId>,
    pub heap_allocated_values: Vec<ValueId>,

    // Analysis data
    pub value_lifetimes: HashMap<ValueId, Lifetime>,
    pub escape_reasons: HashMap<ValueId, EscapeReason>,
    pub capture_sets: HashMap<ValueId, Vec<ValueId>>, // For closures
}

#[derive(Debug, Clone)]
pub struct Lifetime {
    pub first_def: BasicBlockId,
    pub last_use: BasicBlockId,
    pub scope_depth: usize,
    pub escapes_to_caller: bool,
    pub escapes_to_heap: bool,
}

#[derive(Debug, Clone)]
pub enum EscapeReason {
    ReturnedFromFunction,
    StoredInHeapObject,
    CapturedByClosure,
    PassedToUnknownFunction,
    ExceedsStackLimit,
}

#[derive(Debug, Clone)]
pub struct IRFunctionArg {
    pub name: String,
    pub binding_id: usize, // The binding ID from the AST
    pub type_: IRTypeWithMemory,
    pub memory_slot: MemorySlotId,
    pub span: Range<usize>,
    pub file: String,
}

// IR Struct definition
#[derive(Debug, Clone)]
pub struct IRStruct {
    pub id: StructId,
    pub name: String,
    pub vis: Visibility,
    pub fields: Vec<IRStructField>,
    pub span: Range<usize>,
    pub file: String,
    pub struct_type: IRTypeWithMemory,
    pub memory_layout: MemoryLayout, // Information about memory layout
}

#[derive(Debug, Clone)]
pub struct MemoryLayout {
    pub size: usize,
    pub alignment: usize,
    pub field_offsets: Vec<(FieldId, usize)>, // (field_id, offset_in_bytes)
    pub discriminant_offset: Option<usize>,   // For enums
    pub discriminant_size: Option<usize>,     // u8, u16, u32, etc.
    pub discriminant_encoding: Option<DiscriminantEncoding>,

    pub padding_bytes: Vec<PaddingInfo>, // Where padding occurs
}

#[derive(Debug, Clone)]
pub enum DiscriminantEncoding {
    Explicit,       // Separate tag field
    NicheOptimized, // Using pointer null/values
}

#[derive(Debug, Clone)]
pub struct IRStructField {
    pub name: String,
    pub field_id: FieldId,
    pub type_: IRTypeWithMemory,
    pub vis: Visibility,
    pub span: Range<usize>,
    pub file: String,
    pub offset: usize, // Offset in the struct
}

// IR Enum definition
#[derive(Debug, Clone)]
pub struct IREnum {
    pub id: EnumId,
    pub name: String,
    pub vis: Visibility,
    pub variants: Vec<IREnumVariant>,
    pub span: Range<usize>,
    pub file: String,
    pub enum_type: IRTypeWithMemory,
    pub memory_layout: MemoryLayout, // Information about memory layout
}

#[derive(Debug, Clone)]
pub struct IREnumVariant {
    pub name: String,
    pub variant_id: VariantId,
    pub types: Vec<IRTypeWithMemory>,
    pub constructor_type: IRTypeWithMemory,
    pub span: Range<usize>,
    pub file: String,
    pub data_offset: usize, // Offset of THIS variant's data (after discriminant)
    pub discriminant_value: usize, // The tag value for this variant
}

// IR Effect definition
#[derive(Debug, Clone)]
pub struct IREffect {
    pub id: EffectId,
    pub name: String,
    pub vis: Visibility,
    pub operations: Vec<IREffectOperation>,
    pub span: Range<usize>,
    pub file: String,
}

#[derive(Debug, Clone)]
pub struct IREffectOperation {
    pub name: String,
    pub params: Vec<IRTypeWithMemory>,
    pub return_type: IRTypeWithMemory,
    pub operation_type: IRTypeWithMemory,
    pub span: Range<usize>,
    pub file: String,
}
