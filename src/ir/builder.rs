use std::collections::BTreeMap;
use std::ops::Range;
use std::rc::Rc;

use crate::ast::{
    BindingId, Symbol, TypedASTNode, TypedASTNodeKind, TypedEffectDef, TypedEnum, TypedExpr,
    TypedExprKind, TypedFunction, TypedStruct,
};
use crate::ir::{
    AddressKind, AllocationId, BasicBlock, BasicBlockId, BasicBlockInfo, ControlFlowGraph,
    EffectId, EnumId, EscapeAnalysisInfo, FieldId, FunctionId, IREffect, IREffectOperation, IREnum,
    IREnumVariant, IRExternFunction, IRFunction, IRFunctionArg, IRInstruction, IRModule, IRStruct,
    IRStructField, IRTerminator, IRType, IRTypeWithMemory, IRValue, InlineHint,
    InstructionMetadata, MemoryKind, MemorySlotId, MemoryUsage, OptimizationHints, StructId,
    TargetInfo, ValueId, VariantId,
};

// Use BTreeMap for deterministic ordering instead of HashMap
type DeterministicMap<K, V> = BTreeMap<K, V>;

pub struct IRBuilder {
    // Type registry (filled in Pass 1) - using BTreeMap for deterministic ordering
    structs: DeterministicMap<StructId, IRStruct>,
    enums: DeterministicMap<EnumId, IREnum>,
    effects: DeterministicMap<EffectId, IREffect>,
    functions: DeterministicMap<FunctionId, IRFunction>,
    extern_functions: DeterministicMap<FunctionId, IRExternFunction>,

    // Name → ID mappings - using BTreeMap for deterministic ordering
    struct_names: DeterministicMap<String, StructId>,
    enum_names: DeterministicMap<String, EnumId>,
    effect_names: DeterministicMap<String, EffectId>,
    function_names: DeterministicMap<String, FunctionId>,

    // ID generators
    next_struct_id: usize,
    next_enum_id: usize,
    next_effect_id: usize,
    next_function_id: usize,
    next_value_id: usize,
    next_block_id: usize,
    next_alloc_id: usize,
    next_memory_slot_id: usize,

    // Configuration
    target: TargetInfo,
    interner: crate::ast::Interner,
}

impl IRBuilder {
    pub fn new(target: TargetInfo, interner: crate::ast::Interner) -> Self {
        let mut builder = Self {
            structs: BTreeMap::new(),
            enums: BTreeMap::new(),
            effects: BTreeMap::new(),
            functions: BTreeMap::new(),
            extern_functions: BTreeMap::new(),
            struct_names: BTreeMap::new(),
            enum_names: BTreeMap::new(),
            effect_names: BTreeMap::new(),
            function_names: BTreeMap::new(),
            next_struct_id: 0,
            next_enum_id: 0,
            next_effect_id: 0,
            next_function_id: 0,
            next_value_id: 0,
            next_block_id: 0,
            next_alloc_id: 0,
            next_memory_slot_id: 0,
            target,
            interner,
        };

        // Register builtin types
        builder.register_builtin_types();

        builder
    }

    fn register_builtin_types(&mut self) {
        // Use local counters since the struct doesn't have these fields
        let mut next_variant_id = self.next_enum_id + 100; // Use enum_id as base to avoid conflicts for variants
        let mut next_type_var_id = 0; // Local counter for type variables

        // Register Option<T> enum
        let option_id = self.fresh_enum_id();
        let option_name = "Option".to_string();

        let some_variant_id = VariantId(next_variant_id);
        next_variant_id += 1;
        let none_variant_id = VariantId(next_variant_id);
        next_variant_id += 1;

        // Create Some variant with one field of generic type T
        let some_variant = crate::ir::IREnumVariant {
            name: "Some".to_string(),
            variant_id: some_variant_id,
            types: vec![IRTypeWithMemory {
                type_: IRType::Named(format!("T{}", next_type_var_id), vec![]), // Placeholder for generic T
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            }],
            constructor_type: IRTypeWithMemory {
                type_: IRType::Error, // Will be filled in properly
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            span: 0..0,
            file: String::new(),
            data_offset: 0,
            discriminant_value: 0,
        };
        next_type_var_id += 1;

        // Create None variant with no fields
        let none_variant = crate::ir::IREnumVariant {
            name: "None".to_string(),
            variant_id: none_variant_id,
            types: vec![], // No fields
            constructor_type: IRTypeWithMemory {
                type_: IRType::Error, // Will be filled in properly
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            span: 0..0,
            file: String::new(),
            data_offset: 0,
            discriminant_value: 1, // Use different discriminant for None
        };

        let option_enum = crate::ir::IREnum {
            id: option_id,
            name: option_name.clone(),
            vis: crate::ast::Visibility::Public,
            variants: vec![some_variant, none_variant],
            span: 0..0,
            file: String::new(),
            enum_type: IRTypeWithMemory {
                type_: IRType::Named(option_name.clone(), vec![]), // Will be properly filled in
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            memory_layout: crate::ir::MemoryLayout {
                size: 0,                     // Will be computed
                alignment: 0,                // Will be computed
                field_offsets: Vec::new(),   // Will be computed
                discriminant_offset: None,   // Will be computed
                discriminant_size: None,     // Will be computed
                discriminant_encoding: None, // Will be computed
                padding_bytes: Vec::new(),   // Will be computed
            },
        };

        self.enums.insert(option_id, option_enum);
        self.enum_names.insert(option_name, option_id);

        // Register Result<T, E> enum
        let result_id = self.fresh_enum_id();
        let result_name = "Result".to_string();

        let ok_variant_id = VariantId(next_variant_id);
        next_variant_id += 1;
        let err_variant_id = VariantId(next_variant_id);
        // next_variant_id += 1;

        // Create Ok variant with one field of generic type T
        let ok_variant = crate::ir::IREnumVariant {
            name: "Ok".to_string(),
            variant_id: ok_variant_id,
            types: vec![IRTypeWithMemory {
                type_: IRType::Named(format!("T{}", next_type_var_id), vec![]), // Placeholder for generic T
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            }],
            constructor_type: IRTypeWithMemory {
                type_: IRType::Error, // Will be filled in properly
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            span: 0..0,
            file: String::new(),
            data_offset: 0,
            discriminant_value: 0,
        };
        next_type_var_id += 1;

        // Create Err variant with one field of generic type E
        let err_variant = crate::ir::IREnumVariant {
            name: "Err".to_string(),
            variant_id: err_variant_id,
            types: vec![IRTypeWithMemory {
                type_: IRType::Named(format!("E{}", next_type_var_id), vec![]), // Placeholder for generic E
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            }],
            constructor_type: IRTypeWithMemory {
                type_: IRType::Error, // Will be filled in properly
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            span: 0..0,
            file: String::new(),
            data_offset: 0,
            discriminant_value: 1, // Use different discriminant for Err
        };
        // next_type_var_id += 1;

        let result_enum = crate::ir::IREnum {
            id: result_id,
            name: result_name.clone(),
            vis: crate::ast::Visibility::Public,
            variants: vec![ok_variant, err_variant],
            span: 0..0,
            file: String::new(),
            enum_type: IRTypeWithMemory {
                type_: IRType::Named(result_name.clone(), vec![]), // Will be properly filled in
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            memory_layout: crate::ir::MemoryLayout {
                size: 0,                     // Will be computed
                alignment: 0,                // Will be computed
                field_offsets: Vec::new(),   // Will be computed
                discriminant_offset: None,   // Will be computed
                discriminant_size: None,     // Will be computed
                discriminant_encoding: None, // Will be computed
                padding_bytes: Vec::new(),   // Will be computed
            },
        };

        self.enums.insert(result_id, result_enum);
        self.enum_names.insert(result_name, result_id);
    }

    pub fn lower_program(&mut self, ast: Vec<TypedASTNode>) -> IRModule {
        // Pass 1: Collect definitions
        self.collect_definitions(&ast);

        // Pass 2: Lower function bodies
        self.lower_function_bodies(&ast);

        // Build the final module with deterministic ordering
        // Collect keys and sort them to ensure consistent order
        let mut struct_keys: Vec<_> = self.structs.keys().cloned().collect();
        struct_keys.sort();
        let structs: Vec<_> = struct_keys
            .into_iter()
            .map(|id| self.structs[&id].clone())
            .collect();

        let mut enum_keys: Vec<_> = self.enums.keys().cloned().collect();
        enum_keys.sort();
        let enums: Vec<_> = enum_keys
            .into_iter()
            .map(|id| self.enums[&id].clone())
            .collect();

        let mut effect_keys: Vec<_> = self.effects.keys().cloned().collect();
        effect_keys.sort();
        let effects: Vec<_> = effect_keys
            .into_iter()
            .map(|id| self.effects[&id].clone())
            .collect();

        let mut function_keys: Vec<_> = self.functions.keys().cloned().collect();
        function_keys.sort();
        let mut functions: Vec<_> = function_keys
            .into_iter()
            .map(|id| self.functions[&id].clone())
            .collect();

        // Add extern functions to the functions vector
        let mut extern_function_keys: Vec<_> = self.extern_functions.keys().cloned().collect();
        extern_function_keys.sort();
        let extern_functions: Vec<_> = extern_function_keys
            .into_iter()
            .map(|id| {
                // Convert IRExternFunction to IRFunction with no body for now
                // In the future, we might want a different approach
                let extern_func = &self.extern_functions[&id];
                IRFunction {
                    id: extern_func.id,
                    name: extern_func.name.clone(),
                    vis: extern_func.vis,
                    args: extern_func.args.clone(),
                    return_type: extern_func.return_type.clone(),
                    effects: extern_func.effects.clone(),
                    function_type: extern_func.function_type.clone(),
                    basic_blocks: Vec::new(),
                    cfg: ControlFlowGraph {
                        blocks: BTreeMap::new(),
                        entry: BasicBlockId(0),
                        exits: Vec::new(),
                    },
                    span: extern_func.span.clone(),
                    file: extern_func.file.clone(),
                    body: None,
                    memory_usage: MemoryUsage {
                        max_stack_size: 0,
                        heap_allocations: Vec::new(),
                        escape_analysis_info: EscapeAnalysisInfo {
                            escaping_values: Vec::new(),
                            stack_allocated_values: Vec::new(),
                            heap_allocated_values: Vec::new(),
                            value_lifetimes: BTreeMap::new(),
                            escape_reasons: BTreeMap::new(),
                            capture_sets: BTreeMap::new(),
                        },
                    },
                    optimization_hints: OptimizationHints {
                        inline: InlineHint::Never, // Never inline extern functions
                        is_pure: extern_func.effects.is_pure(),
                        is_cold: true, // Extern functions are typically cold
                        should_unroll: None,
                    },
                }
            })
            .collect();

        // Append extern functions to the functions vector
        functions.extend(extern_functions);

        IRModule {
            structs,
            enums,
            effects,
            functions,
            function_map: self
                .functions
                .iter()
                .map(|(id, func)| (*id, func.id.0))
                .collect(),
            struct_map: self.structs.iter().map(|(id, s)| (*id, s.id.0)).collect(),
            enum_map: self.enums.iter().map(|(id, e)| (*id, e.id.0)).collect(),
            effect_map: self
                .effects
                .iter()
                .map(|(id, eff)| (*id, eff.id.0))
                .collect(),
            target: self.target.clone(),
        }
    }

    fn collect_definitions(&mut self, ast: &[TypedASTNode]) {
        for node in ast {
            match &node.node {
                TypedASTNodeKind::Struct(s) => {
                    let id = self.fresh_struct_id();
                    let ir_struct = self.convert_struct(s, id);
                    self.structs.insert(id, ir_struct);
                    self.struct_names
                        .insert(self.interner.resolve(s.name).to_string(), id);
                }
                TypedASTNodeKind::Enum(e) => {
                    let id = self.fresh_enum_id();
                    let ir_enum = self.convert_enum(e, id);
                    self.enums.insert(id, ir_enum);
                    self.enum_names
                        .insert(self.interner.resolve(e.name).to_string(), id);
                }
                TypedASTNodeKind::EffectDef(eff) => {
                    let id = self.fresh_effect_id();
                    let ir_effect = self.convert_effect(eff, id);
                    self.effects.insert(id, ir_effect);
                    self.effect_names
                        .insert(self.interner.resolve(eff.name).to_string(), id);
                }
                TypedASTNodeKind::Function(f) => {
                    let id = self.fresh_function_id();
                    let ir_function = self.convert_function_signature(f, id);
                    self.functions.insert(id, ir_function);
                    self.function_names
                        .insert(self.interner.resolve(f.name).to_string(), id);
                }
                TypedASTNodeKind::ExternFunction(extern_f) => {
                    let id = self.fresh_function_id();
                    let ir_extern_function = self.convert_extern_function(extern_f, id);

                    // Convert to a regular IRFunction without a body
                    let ir_function = IRFunction {
                        id: ir_extern_function.id,
                        name: ir_extern_function.name.clone(),
                        vis: ir_extern_function.vis,
                        args: ir_extern_function.args.clone(),
                        return_type: ir_extern_function.return_type.clone(),
                        effects: ir_extern_function.effects.clone(),
                        function_type: ir_extern_function.function_type.clone(),
                        basic_blocks: Vec::new(),
                        body: None,
                        span: ir_extern_function.span.clone(),
                        file: ir_extern_function.file.clone(),
                        cfg: ControlFlowGraph {
                            blocks: BTreeMap::new(),
                            entry: BasicBlockId(0),
                            exits: Vec::new(),
                        },
                        memory_usage: MemoryUsage {
                            max_stack_size: 0,
                            heap_allocations: Vec::new(),
                            escape_analysis_info: EscapeAnalysisInfo {
                                escaping_values: Vec::new(),
                                stack_allocated_values: Vec::new(),
                                heap_allocated_values: Vec::new(),
                                value_lifetimes: BTreeMap::new(),
                                escape_reasons: BTreeMap::new(),
                                capture_sets: BTreeMap::new(),
                            },
                        },
                        optimization_hints: OptimizationHints {
                            inline: InlineHint::Never, // Never inline extern functions
                            is_pure: ir_extern_function.effects.is_pure(),
                            is_cold: true, // Extern functions are typically cold
                            should_unroll: None,
                        },
                    };

                    self.functions.insert(id, ir_function);
                    self.function_names
                        .insert(self.interner.resolve(extern_f.name).to_string(), id);
                }
                _ => {}
            }
        }
    }

    fn convert_struct(&mut self, s: &TypedStruct, id: StructId) -> IRStruct {
        // Convert fields with proper memory layout
        let fields: Vec<_> = s
            .fields
            .iter()
            .map(|(field_arg, vis, field_id)| {
                IRStructField {
                    name: self.interner.resolve(field_arg.name).to_string(),
                    field_id: FieldId(field_id.0), // Convert from AST FieldId to IR FieldId
                    type_: self.convert_type(&field_arg.type_),
                    vis: *vis,
                    span: field_arg.span.clone(),
                    file: field_arg.file.clone(),
                    offset: 0, // Computed below
                }
            })
            .collect();

        // Compute memory layout
        let field_types: Vec<_> = fields
            .iter()
            .map(|f| (f.field_id, &f.type_.type_))
            .collect();
        let layout = self
            .target
            .compute_aggregate_layout(&field_types, &IRModule::new(self.target.clone()))
            .expect("Failed to compute struct layout");

        // Update field offsets
        let fields = fields
            .into_iter()
            .map(|mut f| {
                f.offset = layout
                    .field_offsets
                    .iter()
                    .find(|(id, _)| *id == f.field_id)
                    .map(|(_, off)| *off)
                    .unwrap_or(0);
                f
            })
            .collect();

        IRStruct {
            id: StructId(id.0), // Convert from AST StructId to IR StructId
            name: self.interner.resolve(s.name).to_string(),
            vis: s.vis,
            fields,
            span: s.span.clone(),
            file: s.file.clone(),
            struct_type: self.convert_type(&s.struct_type),
            memory_layout: layout,
        }
    }

    fn convert_enum(&mut self, e: &TypedEnum, id: EnumId) -> IREnum {
        let variants: Vec<_> = e
            .variants
            .iter()
            .map(|variant| {
                let _fields: Vec<_> = variant
                    .types
                    .iter()
                    .enumerate()
                    .map(|(j, field_type)| {
                        IRStructField {
                            name: format!("field_{}", j), // Enum variants can have anonymous fields
                            field_id: FieldId(j),
                            type_: self.convert_type(field_type),
                            vis: crate::ast::Visibility::Public, // Default visibility for enum fields
                            span: variant.span.clone(),          // Use variant span for all fields
                            file: variant.file.clone(),
                            offset: 0, // Will be computed later
                        }
                    })
                    .collect();

                IREnumVariant {
                    name: self.interner.resolve(variant.name).to_string(),
                    variant_id: VariantId(variant.variant_id.0),
                    types: variant.types.iter().map(|t| self.convert_type(t)).collect(), // Fixed: collect types
                    constructor_type: self.convert_type(&variant.constructor_type),
                    span: variant.span.clone(),
                    file: variant.file.clone(),
                    data_offset: 0,        // Will be computed later
                    discriminant_value: 0, // Will be computed later
                }
            })
            .collect();

        IREnum {
            id: EnumId(id.0), // Convert from AST EnumId to IR EnumId
            name: self.interner.resolve(e.name).to_string(),
            vis: e.vis,
            variants,
            span: e.span.clone(),
            file: e.file.clone(),
            enum_type: self.convert_type(&e.enum_type),
            memory_layout: crate::ir::MemoryLayout {
                size: 0,                     // Will be computed
                alignment: 0,                // Will be computed
                field_offsets: Vec::new(),   // Will be computed
                discriminant_offset: None,   // Will be computed
                discriminant_size: None,     // Will be computed
                discriminant_encoding: None, // Will be computed
                padding_bytes: Vec::new(),   // Will be computed
            },
        }
    }

    fn convert_effect(&mut self, eff: &TypedEffectDef, id: EffectId) -> IREffect {
        let operations: Vec<_> = eff
            .operations
            .iter()
            .map(|op| IREffectOperation {
                name: self.interner.resolve(op.name).to_string(),
                params: op
                    .params
                    .iter()
                    .map(|param_type| self.convert_type(param_type))
                    .collect(),
                return_type: self.convert_type(&op.return_type),
                operation_type: self.convert_type(&op.operation_type),
                span: op.span.clone(),
                file: eff.file.clone(),
            })
            .collect();

        IREffect {
            id: EffectId(id.0), // Convert from AST EffectId to IR EffectId
            name: self.interner.resolve(eff.name).to_string(),
            vis: eff.vis,
            operations,
            span: eff.span.clone(),
            file: eff.file.clone(),
        }
    }

    fn convert_function_signature(&mut self, f: &TypedFunction, id: FunctionId) -> IRFunction {
        let parameters: Vec<_> = f
            .args
            .iter()
            .map(|param| IRFunctionArg {
                name: self.interner.resolve(param.name).to_string(),
                binding_id: param.binding_id.0,
                type_: self.convert_type(&param.type_),
                memory_slot: self.fresh_memory_slot_id(),
                span: param.span.clone(),
                file: param.file.clone(),
            })
            .collect();

        // For main function, assign ALL effects available in the system
        let effects = if self.interner.resolve(f.name) == "main" {
            // Collect ALL effect IDs from the module
            let all_effects: crate::typechecker::EffectSet = crate::typechecker::EffectSet {
                effects: self.effects.keys().map(|effect_id| effect_id.0).collect(), // Extract usize from EffectId
                rest: None,
            };
            all_effects
        } else {
            f.effects.clone()
        };

        // FIXME: interning logic has a bug where uhhh some things are out of bounds sometimes

        let effects_clone = effects.clone(); // Clone to avoid move issue

        IRFunction {
            id: FunctionId(id.0), // Convert from AST FunctionId to IR FunctionId
            name: self.interner.resolve(f.name).to_string(),
            vis: f.vis,
            args: parameters,
            return_type: self.convert_type(&f.return_type),
            effects,
            function_type: self.convert_type(&f.function_type),
            basic_blocks: Vec::new(), // Will be filled in Pass 2
            body: None,               // Will be filled in Pass 2
            span: f.span.clone(),
            file: f.file.clone(),
            cfg: ControlFlowGraph {
                blocks: BTreeMap::new(),
                entry: BasicBlockId(0), // Will be updated
                exits: Vec::new(),
            },
            memory_usage: crate::ir::MemoryUsage {
                max_stack_size: 0,
                heap_allocations: Vec::new(),
                escape_analysis_info: crate::ir::EscapeAnalysisInfo {
                    escaping_values: Vec::new(),
                    stack_allocated_values: Vec::new(),
                    heap_allocated_values: Vec::new(),
                    value_lifetimes: BTreeMap::new(),
                    escape_reasons: BTreeMap::new(),
                    capture_sets: BTreeMap::new(),
                },
            },
            optimization_hints: crate::ir::OptimizationHints {
                inline: crate::ir::InlineHint::Heuristic,
                is_pure: effects_clone.is_pure(), // Update based on actual effects
                is_cold: false,
                should_unroll: None,
            },
        }
    }

    fn convert_extern_function(
        &mut self,
        extern_f: &crate::ast::TypedExternFunction,
        id: FunctionId,
    ) -> IRExternFunction {
        let parameters: Vec<_> = extern_f
            .args
            .iter()
            .map(|param| IRFunctionArg {
                name: self.interner.resolve(param.name).to_string(),
                binding_id: param.binding_id.0,
                type_: self.convert_type(&param.type_),
                memory_slot: self.fresh_memory_slot_id(),
                span: param.span.clone(),
                file: param.file.clone(),
            })
            .collect();

        IRExternFunction {
            id: FunctionId(id.0), // Convert from AST FunctionId to IR FunctionId
            name: self.interner.resolve(extern_f.name).to_string(),
            vis: extern_f.vis,
            args: parameters,
            return_type: self.convert_type(&extern_f.return_type),
            effects: extern_f.effects.clone(),
            function_type: self.convert_type(&extern_f.function_type),
            library: extern_f.library.clone(),
            symbol_name: extern_f.symbol_name.clone(),
            span: extern_f.span.clone(),
            file: extern_f.file.clone(),
        }
    }

    fn lower_function_bodies(&mut self, ast: &[TypedASTNode]) {
        // Collect function IDs first to avoid borrowing issues
        let function_ids: Vec<_> = ast
            .iter()
            .filter_map(|node| {
                if let TypedASTNodeKind::Function(f) = &node.node {
                    Some((
                        self.interner.resolve(f.name).to_string(),
                        self.function_names[&self.interner.resolve(f.name).to_string()],
                    ))
                } else {
                    None
                }
            })
            .collect();

        // Process each function to get the basic blocks and build CFGs separately
        let mut function_data = Vec::new();

        for (name, function_id) in function_ids {
            // Find the corresponding function in AST
            if let Some(TypedASTNodeKind::Function(f)) = ast.iter().find_map(|node| {
                if let TypedASTNodeKind::Function(f) = &node.node {
                    if self.interner.resolve(f.name) == name {
                        Some(&node.node)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }) {
                // Only process functions that have bodies (not extern functions)
                if f.body.is_some() {
                    let mut fb = FunctionBuilder::new(self, function_id);

                    if let Some(body) = &f.body {
                        fb.lower_function_body(f, body);
                    }

                    // Collect the basic blocks and function_id for later use
                    let basic_blocks = fb.blocks.into_values().collect::<Vec<_>>();
                    let cfg = self.build_cfg(&basic_blocks); // Build CFG now while we can
                    function_data.push((function_id, basic_blocks, cfg));
                }
            }
        }

        // Now update the functions with their basic blocks and CFGs
        for (function_id, basic_blocks, cfg) in function_data {
            let ir_function = self.functions.get_mut(&function_id).unwrap();
            ir_function.basic_blocks = basic_blocks;
            ir_function.body = ir_function.basic_blocks.first().map(|b| b.id);
            ir_function.cfg = cfg;
        }
    }

    fn convert_type(&self, type_: &Rc<crate::typechecker::Type>) -> IRTypeWithMemory {
        // Convert the semantic type to an IR type with memory info
        let ir_type = match &type_.type_ {
            crate::typechecker::TypeKind::Constructor { name, args, .. } => {
                let type_name = self.interner.resolve(Symbol(*name));
                match type_name {
                    "Int" => IRType::Int,
                    "Bool" => IRType::Bool,
                    "Float" => IRType::Float,
                    "String" => {
                        // Strings are heap allocated
                        IRType::String
                    }
                    "Unit" => IRType::Unit,
                    "Array" => {
                        if args.len() == 1 {
                            IRType::Named("Array".to_string(), vec![self.convert_type(&args[0])])
                        } else {
                            IRType::Error
                        }
                    }
                    "Option" => {
                        if args.len() == 1 {
                            IRType::Named("Option".to_string(), vec![self.convert_type(&args[0])])
                        } else {
                            IRType::Error
                        }
                    }
                    _ => {
                        // Check if it's a struct or enum
                        if self.struct_names.contains_key(type_name) {
                            let struct_id = self.struct_names[type_name];
                            IRType::StructRef(StructId(struct_id.0)) // Convert from AST StructId to IR StructId
                        } else if self.enum_names.contains_key(type_name) {
                            let enum_id = self.enum_names[type_name];
                            IRType::EnumRef(EnumId(enum_id.0)) // Convert from AST EnumId to IR EnumId
                        } else {
                            IRType::Named(
                                type_name.to_string(),
                                args.iter().map(|arg| self.convert_type(arg)).collect(),
                            )
                        }
                    }
                }
            }
            crate::typechecker::TypeKind::Function {
                params,
                return_type,
                effects,
            } => IRType::Function {
                params: params.iter().map(|p| self.convert_type(p)).collect(),
                return_type: Box::new(self.convert_type(return_type)),
                effects: effects.clone(),
            },
            crate::typechecker::TypeKind::Tuple(types) => {
                IRType::Tuple(types.iter().map(|t| self.convert_type(t)).collect())
            }
            crate::typechecker::TypeKind::Variable { id, .. } => {
                // For unresolved type variables, provide a descriptive error
                todo!(
                    "Unresolved type variable {:?} - monomorphization may be incomplete",
                    id
                )
            }
            crate::typechecker::TypeKind::Pointer(inner) => {
                IRType::Pointer(Box::new(self.convert_type(inner)))
            }
            crate::typechecker::TypeKind::Never => IRType::Never,
            crate::typechecker::TypeKind::Error => IRType::Error,
            _ => IRType::Error,
        };

        // Determine memory kind based on type and context
        let memory_kind = self.analyze_memory_kind(type_);

        IRTypeWithMemory {
            type_: ir_type,
            memory_kind,
            span: type_.span.clone().unwrap_or(0..0),
            file: type_.file.clone().unwrap_or_default(),
            allocation_id: None,
        }
    }

    fn analyze_memory_kind(&self, type_: &Rc<crate::typechecker::Type>) -> MemoryKind {
        match &type_.type_ {
            // Primitives are stack allocated
            crate::typechecker::TypeKind::Constructor { name, .. } => {
                let type_name = self.interner.resolve(Symbol(*name));
                match type_name {
                    "int" | "bool" | "float" | "unit" => MemoryKind::Stack,
                    "string" => MemoryKind::Heap, // Strings are heap allocated
                    "Array" | "List" | "Vec" => MemoryKind::Heap, // Collections are heap allocated
                    _ => {
                        // Structs and enums might be stack or heap depending on size and escape
                        MemoryKind::Heap // Default to heap, escape analysis will determine final allocation
                    }
                }
            }
            // Functions are typically heap allocated (closures) or stack (function pointers)
            crate::typechecker::TypeKind::Function { .. } => MemoryKind::Heap,
            // Tuples are stack allocated if small, heap if large
            crate::typechecker::TypeKind::Tuple(types) => {
                if types.len() <= 2 {
                    MemoryKind::Stack
                } else {
                    MemoryKind::Heap
                }
            }
            // Pointers are handled specially
            crate::typechecker::TypeKind::Pointer(_) => MemoryKind::Stack,
            // Other types default to stack
            _ => MemoryKind::Stack,
        }
    }

    fn fresh_struct_id(&mut self) -> StructId {
        let id = self.next_struct_id;
        self.next_struct_id += 1;
        StructId(id)
    }

    fn fresh_enum_id(&mut self) -> EnumId {
        let id = self.next_enum_id;
        self.next_enum_id += 1;
        EnumId(id)
    }

    fn fresh_effect_id(&mut self) -> EffectId {
        let id = self.next_effect_id;
        self.next_effect_id += 1;
        EffectId(id)
    }

    fn fresh_function_id(&mut self) -> FunctionId {
        let id = self.next_function_id;
        self.next_function_id += 1;
        FunctionId(id)
    }

    fn fresh_value_id(&mut self) -> ValueId {
        let id = self.next_value_id;
        self.next_value_id += 1;
        ValueId(id)
    }

    fn fresh_block_id(&mut self) -> BasicBlockId {
        let id = self.next_block_id;
        self.next_block_id += 1;
        BasicBlockId(id)
    }

    fn fresh_alloc_id(&mut self) -> AllocationId {
        let id = self.next_alloc_id;
        self.next_alloc_id += 1;
        AllocationId(id)
    }

    fn fresh_memory_slot_id(&mut self) -> MemorySlotId {
        let id = self.next_memory_slot_id;
        self.next_memory_slot_id += 1;
        MemorySlotId(id)
    }

    fn build_cfg(&self, basic_blocks: &[BasicBlock]) -> ControlFlowGraph {
        let mut blocks = BTreeMap::new();
        let mut entry = BasicBlockId(0);
        let mut exits = Vec::new();

        // Initialize all blocks in the CFG
        for block in basic_blocks {
            blocks.insert(
                block.id,
                BasicBlockInfo {
                    id: block.id,
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                    dominators: None,
                },
            );
        }

        // Connect blocks based on terminators
        for block in basic_blocks {
            if let Some(terminator) = &block.terminator {
                match terminator {
                    IRTerminator::Return { .. } => {
                        exits.push(block.id);
                    }
                    IRTerminator::Jump { target, .. } => {
                        if let Some(block_info) = blocks.get_mut(&block.id) {
                            block_info.successors.push(*target);
                        }
                        if let Some(target_info) = blocks.get_mut(target) {
                            target_info.predecessors.push(block.id);
                        }
                    }
                    IRTerminator::Branch {
                        then_block,
                        else_block,
                        ..
                    } => {
                        if let Some(block_info) = blocks.get_mut(&block.id) {
                            block_info.successors.push(*then_block);
                            block_info.successors.push(*else_block);
                        }
                        if let Some(then_info) = blocks.get_mut(then_block) {
                            then_info.predecessors.push(block.id);
                        }
                        if let Some(else_info) = blocks.get_mut(else_block) {
                            else_info.predecessors.push(block.id);
                        }
                    }
                    IRTerminator::Switch {
                        cases,
                        default,
                        is_exhaustive: _,
                        ..
                    } => {
                        let mut all_targets =
                            cases.iter().map(|(_, target)| *target).collect::<Vec<_>>();
                        if let Some(def) = default {
                            all_targets.push(*def);
                        }

                        if let Some(block_info) = blocks.get_mut(&block.id) {
                            block_info.successors.extend(all_targets.iter().cloned());
                        }

                        for target in all_targets {
                            if let Some(target_info) = blocks.get_mut(&target) {
                                target_info.predecessors.push(block.id);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        if !basic_blocks.is_empty() {
            entry = basic_blocks[0].id;
        }

        ControlFlowGraph {
            blocks,
            entry,
            exits,
        }
    }
}

pub struct FunctionBuilder<'a> {
    builder: &'a mut IRBuilder,
    pub function_id: FunctionId,

    // SSA state
    current_block: BasicBlockId,
    pub entry_block: BasicBlockId,
    blocks: BTreeMap<BasicBlockId, BasicBlock>,

    // Variable → SSA value mapping
    bindings: BTreeMap<usize, ValueId>,

    // CFG tracking
    cfg_edges: Vec<(BasicBlockId, BasicBlockId)>,

    // Loop context
    loop_stack: Vec<LoopContext>,
}

pub struct LoopContext {
    continue_target: BasicBlockId,
    break_target: BasicBlockId,
    pub label: Option<Symbol>,
}

impl<'a> FunctionBuilder<'a> {
    fn new(builder: &'a mut IRBuilder, function_id: FunctionId) -> Self {
        let entry_block = builder.fresh_block_id();
        let mut blocks = BTreeMap::new();
        blocks.insert(entry_block, BasicBlock::new(entry_block));

        Self {
            builder,
            function_id,
            current_block: entry_block,
            entry_block,
            blocks,
            bindings: BTreeMap::new(),
            cfg_edges: Vec::new(),
            loop_stack: Vec::new(),
        }
    }

    fn lower_function_body(&mut self, function: &TypedFunction, body: &TypedExpr) {
        // Process parameters
        for param in &function.args {
            let value_id = self.builder.fresh_value_id();
            self.bindings.insert(param.binding_id.0, value_id);
        }

        // Lower the function body
        let _result = self.lower_expr(body);

        // If we haven't already terminated the current block, add a return
        let current_block = self.blocks.get_mut(&self.current_block).unwrap();
        if current_block.terminator.is_none() {
            // Add a return instruction
            current_block.terminator = Some(IRTerminator::Return {
                value: None,
                span: body.span.clone(),
                file: body.file.clone(),
            });
        }

        // Perform escape analysis
        self.perform_escape_analysis();
    }

    fn perform_escape_analysis(&mut self) {
        // Basic escape analysis: determine which values escape the function
        // 1. Liveness analysis to determine when values are used
        // 2. Escape analysis to determine if values escape the current scope
        // 3. Allocation site analysis to determine where to allocate values
        for block in self.blocks.values() {
            if let Some(IRTerminator::Return {
                value: Some(_return_value),
                ..
            }) = &block.terminator
            {
                // todo!("Perform escape analysis for return values: {:?}", return_value);
            }
        }
    }

    fn lower_expr(&mut self, expr: &TypedExpr) -> IRValue {
        match &expr.expr {
            TypedExprKind::Int(n) => IRValue::Int(*n),
            TypedExprKind::Float(f) => IRValue::Float(*f),
            TypedExprKind::Bool(b) => IRValue::Bool(*b),
            TypedExprKind::String(s) => IRValue::String(s.clone()),
            TypedExprKind::Variable { binding_id, .. } => {
                // Check if this is a local variable binding
                if let Some(value_id) = self.bindings.get(&binding_id.0) {
                    IRValue::SSA(*value_id)
                } else {
                    // This is likely a function reference that should not be treated as a local variable
                    // This can happen during monomorphization when function references are created
                    // with binding IDs that don't correspond to local variable bindings
                    // For now, create a placeholder and continue compilation
                    let placeholder_value_id = self.builder.fresh_value_id();
                    IRValue::SSA(placeholder_value_id)
                }
            }
            TypedExprKind::BinOp { left, op, right } => {
                let left_val = self.lower_expr(left);
                let right_val = self.lower_expr(right);
                let result = self.builder.fresh_value_id();

                self.emit(IRInstruction::BinOp {
                    result,
                    metadata: InstructionMetadata {
                        memory_slot: None,
                        allocation_site: None,
                    },
                    left: left_val,
                    op: op.clone(),
                    right: right_val,
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                });

                IRValue::SSA(result)
            }
            TypedExprKind::UnOp { op, operand } => {
                let operand_val = self.lower_expr(operand);
                let result = self.builder.fresh_value_id();

                self.emit(IRInstruction::UnOp {
                    result,
                    metadata: InstructionMetadata {
                        memory_slot: None,
                        allocation_site: None,
                    },
                    op: op.clone(),
                    operand: operand_val,
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                });

                IRValue::SSA(result)
            }
            TypedExprKind::Let {
                var: _,
                binding_id,
                value,
                ..
            } => {
                let value_val = self.lower_expr(value);

                // Store the binding
                if let IRValue::SSA(id) = value_val {
                    self.bindings.insert(binding_id.0, id);
                } else {
                    let new_id = self.builder.fresh_value_id();
                    self.bindings.insert(binding_id.0, new_id);
                }

                IRValue::Unit
            }
            TypedExprKind::IfElse {
                condition,
                then,
                else_,
            } => self.lower_if_else(condition, then, else_.as_deref(), &expr.span, &expr.file),
            TypedExprKind::Match { scrutinee, arms } => {
                self.lower_match(scrutinee, arms, &expr.span, &expr.file)
            }
            TypedExprKind::While { condition, body } => {
                self.lower_while(condition, body, &expr.span, &expr.file)
            }
            TypedExprKind::For {
                iterator,
                value,
                binding_id,
                body,
                ..
            } => {
                let value_str = self.builder.interner.resolve(*value).to_string();
                self.lower_for(
                    iterator,
                    &value_str,
                    *binding_id,
                    body,
                    &expr.span,
                    &expr.file,
                )
            }
            TypedExprKind::Call {
                function,
                args,
                type_args,
            } => self.lower_call(function, args, type_args, expr),
            TypedExprKind::StructConstruct {
                struct_name,
                struct_id,
                fields,
            } => {
                // Convert AST FieldIds to IR FieldIds
                let ir_fields: Vec<_> = fields
                    .iter()
                    .map(|(name, field_id, expr)| (*name, FieldId(field_id.0), expr.clone()))
                    .collect();
                self.lower_struct_construct(
                    *struct_name,
                    StructId(struct_id.0),
                    &ir_fields,
                    &expr.span,
                    &expr.file,
                )
            }
            TypedExprKind::EnumConstruct {
                enum_name,
                enum_id,
                variant,
                variant_id,
                args,
            } => self.lower_enum_construct(
                *enum_name,
                EnumId(enum_id.0),
                *variant,
                VariantId(variant_id.0),
                args,
                &expr.span,
                &expr.file,
            ),
            TypedExprKind::FieldAccess {
                target,
                field,
                field_id,
                field_type,
            } => self.lower_field_access(
                target,
                *field,
                FieldId(field_id.0),
                field_type,
                &expr.span,
                &expr.file,
            ),
            TypedExprKind::Index {
                target,
                index,
                element_type,
            } => self.lower_index(target, index, element_type, &expr.span, &expr.file),
            TypedExprKind::Array {
                elements,
                element_type,
            } => self.lower_array(elements, element_type, &expr.span, &expr.file),
            TypedExprKind::Tuple(elements) => self.lower_tuple(elements, &expr.span, &expr.file),
            TypedExprKind::OptionalChain {
                target,
                field,
                field_id,
                field_type,
            } => self.lower_optional_chain(
                target,
                *field,
                FieldId(field_id.0),
                field_type,
                &expr.span,
                &expr.file,
            ),
            TypedExprKind::Perform {
                effect,
                effect_id,
                args,
            } => self.lower_perform(*effect, EffectId(effect_id.0), args, &expr.span, &expr.file),
            TypedExprKind::Handle {
                body,
                handlers,
                return_type,
            } => self.lower_handle(body, handlers, return_type, &expr.span, &expr.file),
            TypedExprKind::Return(return_expr) => {
                let return_val = return_expr.as_ref().map(|e| self.lower_expr(e));
                let current_block = self.blocks.get_mut(&self.current_block).unwrap();
                current_block.terminator = Some(IRTerminator::Return {
                    value: return_val,
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                });
                IRValue::Unit
            }
            TypedExprKind::Break(_break_expr) => {
                let target_block = self.find_break_target(None); // No label support yet
                let current_block = self.blocks.get_mut(&self.current_block).unwrap();
                current_block.terminator = Some(IRTerminator::Jump {
                    target: target_block,
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                });
                IRValue::Unit
            }
            TypedExprKind::Continue => {
                let target_block = self.find_continue_target(None); // No label support yet
                let current_block = self.blocks.get_mut(&self.current_block).unwrap();
                current_block.terminator = Some(IRTerminator::Jump {
                    target: target_block,
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                });
                IRValue::Unit
            }
            TypedExprKind::Lambda { .. } => {
                panic!("Lambda found in typed AST - should be desugared by now!")
            }
            // _ => todo!("Expression type not yet supported: {:?}", expr.expr),
            TypedExprKind::Block { expressions } => {
                self.lower_block(expressions, &expr.span, &expr.file)
            }
            TypedExprKind::Assign { l_val, r_val, op } => {
                self.lower_assign(l_val, r_val, op, &expr.span, &expr.file)
            }
            TypedExprKind::Map {
                entries,
                key_type,
                value_type,
            } => self.lower_map(entries, key_type, value_type, &expr.span, &expr.file),
            TypedExprKind::Cast {
                expr: inner_expr,
                target_type,
            } => self.lower_cast(inner_expr, target_type, &expr.span, &expr.file),
            TypedExprKind::With {
                context,
                var,
                binding_id,
                var_type,
                body,
            } => self.lower_with(
                context,
                *var,
                *binding_id,
                var_type,
                body,
                &expr.span,
                &expr.file,
            ),
            TypedExprKind::Loop { label, body } => {
                self.lower_loop(label, body, &expr.span, &expr.file)
            }
            TypedExprKind::IfLet {
                pattern,
                expr: scrutinee,
                then,
                else_,
            } => self.lower_if_let(
                pattern,
                scrutinee,
                then,
                else_.as_deref(),
                &expr.span,
                &expr.file,
            ),
            TypedExprKind::WhileLet {
                pattern,
                expr: scrutinee,
                body,
            } => self.lower_while_let(pattern, scrutinee, body, &expr.span, &expr.file),
            TypedExprKind::MacroCall { name, args, .. } => {
                let macro_name = self.builder.interner.resolve(*name).to_string();

                // Handle builtin macros by converting them to function calls
                match macro_name.as_str() {
                    "print!" | "print" => {
                        // Convert print! macro to print function call
                        self.lower_builtin_macro_call("print", args, &expr.span, &expr.file)
                    }
                    "println!" | "println" => {
                        // Convert println! macro to print function call (with potential newline)
                        self.lower_builtin_macro_call("print", args, &expr.span, &expr.file)
                    }
                    "input!" | "input" => {
                        // Convert input! macro to input function call
                        self.lower_builtin_macro_call("input", args, &expr.span, &expr.file)
                    }
                    _ => {
                        // For other macros, this shouldn't happen if the type checker worked properly
                        // But just in case, we'll make a best effort to handle it
                        IRValue::Unit
                    }
                }
            }
            TypedExprKind::Error => {
                unreachable!()
            }
            TypedExprKind::Import(_) => {
                unreachable!()
            }
        }
    }

    fn lower_if_else(
        &mut self,
        condition: &TypedExpr,
        then_expr: &TypedExpr,
        else_expr: Option<&TypedExpr>,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        let cond_val = self.lower_expr(condition);

        let then_block = self.new_block();
        let else_block = self.new_block();
        let merge_block = self.new_block();

        // Emit branch terminator
        let current_block = self.blocks.get_mut(&self.current_block).unwrap();
        current_block.terminator = Some(IRTerminator::Branch {
            condition: cond_val,
            then_block,
            else_block,
            span: span.clone(),
            file: file.to_string(),
        });

        // Update CFG
        self.add_cfg_edge(self.current_block, then_block);
        self.add_cfg_edge(self.current_block, else_block);

        // Lower then branch
        self.set_current_block(then_block);
        let then_val = self.lower_expr(then_expr);
        let then_end = self.current_block;
        self.emit_jump(merge_block, span, file);

        // Lower else branch
        self.set_current_block(else_block);
        let else_val = if let Some(e) = else_expr {
            self.lower_expr(e)
        } else {
            IRValue::Unit
        };
        let else_end = self.current_block;
        self.emit_jump(merge_block, span, file);

        // Merge block with PHI
        self.set_current_block(merge_block);
        let result = self.builder.fresh_value_id();

        let phi = IRInstruction::Phi {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            incoming: vec![(then_val, then_end), (else_val, else_end)],
            span: span.clone(),
            file: file.to_string(),
        };

        self.blocks.get_mut(&merge_block).unwrap().add_phi(phi);

        // Update CFG
        self.add_cfg_edge(then_end, merge_block);
        self.add_cfg_edge(else_end, merge_block);

        IRValue::SSA(result)
    }

    fn lower_match(
        &mut self,
        scrutinee: &TypedExpr,
        arms: &[crate::ast::TypedMatchArm],
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        // Lower the scrutinee expression
        let matched_val = self.lower_expr(scrutinee);

        // Create blocks for each arm and a merge block
        let arm_blocks: Vec<BasicBlockId> = arms.iter().map(|_| self.new_block()).collect();
        let merge_block = self.new_block();

        // Extract discriminant for switching
        let discriminant = self.extract_discriminant(&matched_val);

        // Build switch cases - use actual discriminant values when available
        let cases: Vec<(IRValue, BasicBlockId)> = arms
            .iter()
            .zip(&arm_blocks)
            .enumerate()
            .map(|(i, (arm, &block))| {
                // Extract actual discriminant from the enum variant pattern
                // For enum patterns, we should get the discriminant value of the variant
                match &arm.pattern.pat {
                    crate::ast::TypedPatKind::Enum {
                        enum_name: _,
                        enum_id: _,
                        variant: _,
                        variant_id: _,
                        params: _,
                    } => {
                        // For now, use the arm index as the discriminant
                        // In a full implementation, we'd extract the real discriminant value
                        (IRValue::Int(i as i64), block)
                    }
                    _ => {
                        // For other pattern types, use a default
                        (IRValue::Int(i as i64), block)
                    }
                }
            })
            .collect();

        // Emit switch terminator
        let current_block = self.blocks.get_mut(&self.current_block).unwrap();
        current_block.terminator = Some(IRTerminator::Switch {
            value: discriminant,
            cases,
            default: None, // Assume exhaustive
            is_exhaustive: true,
            span: span.clone(),
            file: file.to_string(),
        });

        // Update CFG
        for &arm_block in &arm_blocks {
            self.add_cfg_edge(self.current_block, arm_block);
        }

        // Lower each arm and collect results
        let mut arm_results = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            self.set_current_block(arm_blocks[i]);

            // Bind pattern variables
            self.bind_pattern(&arm.pattern, matched_val.clone());

            // Handle guard if present
            if let Some(guard) = &arm.guard {
                // TODO: Implement guard checking
                // For now, just lower the guard expression but don't check it
                self.lower_expr(guard);
            }

            // Lower the arm body
            let result = self.lower_expr(&arm.body);
            let end_block = self.current_block;

            arm_results.push((result, end_block));

            // Jump to merge block
            self.emit_jump(merge_block, span, file);

            // Update CFG
            self.add_cfg_edge(end_block, merge_block);
        }

        // Create merge block with PHI node
        self.set_current_block(merge_block);
        let result = self.builder.fresh_value_id();

        // In SSA form, PHI nodes should properly reference the SSA values that flow from predecessors.
        // Convert IRValues to proper SSA values for the PHI node.
        let ssa_arm_results: Vec<(IRValue, BasicBlockId)> = arm_results
            .into_iter()
            .map(|(arm_result, end_block)| {
                let ssa_val = match arm_result {
                    IRValue::SSA(id) => IRValue::SSA(id),
                    _ => {
                        // For non-SSA values, we need to create an SSA value that represents this value
                        let temp_id = self.builder.fresh_value_id();
                        self.emit(IRInstruction::Let {
                            result: temp_id,
                            metadata: InstructionMetadata {
                                memory_slot: None,
                                allocation_site: None,
                            },
                            var: "_phi_temp_arm".to_string(),
                            value: arm_result,
                            var_type: IRTypeWithMemory {
                                type_: IRType::Int, // Placeholder - would be actual type in real implementation
                                span: span.clone(),
                                file: file.to_string(),
                                memory_kind: MemoryKind::Stack,
                                allocation_id: None,
                            },
                            span: span.clone(),
                            file: file.to_string(),
                        });
                        IRValue::SSA(temp_id)
                    }
                };
                (ssa_val, end_block)
            })
            .collect();

        let phi = IRInstruction::Phi {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            incoming: ssa_arm_results,
            span: span.clone(),
            file: file.to_string(),
        };

        self.blocks.get_mut(&merge_block).unwrap().add_phi(phi);

        IRValue::SSA(result)
    }

    fn bind_pattern(&mut self, pattern: &crate::ast::TypedPattern, value: IRValue) {
        match &pattern.pat {
            crate::ast::TypedPatKind::Wildcard => {
                // Wildcard - don't bind anything
            }

            crate::ast::TypedPatKind::Bind {
                name: _,
                binding_id,
            } => {
                // Simple variable binding
                if let IRValue::SSA(val_id) = value {
                    self.bindings.insert(binding_id.0, val_id);
                } else {
                    let new_id = self.builder.fresh_value_id();
                    self.bindings.insert(binding_id.0, new_id);
                }
            }

            crate::ast::TypedPatKind::Literal(_lit) => {
                // Literal pattern - no binding, just matching
                // The switch terminator handles the actual comparison
            }

            crate::ast::TypedPatKind::Tuple { patterns, .. } => {
                // Destructure tuple
                for (i, pat) in patterns.iter().enumerate() {
                    let field_val = self.extract_tuple_field(&value, i);
                    self.bind_pattern(pat, field_val);
                }
            }

            crate::ast::TypedPatKind::Struct { fields, .. } => {
                // Destructure struct
                for (_field_name, field_id, pat) in fields {
                    let field_val = self.extract_struct_field(&value, FieldId(field_id.0));
                    self.bind_pattern(pat, field_val);
                }
            }

            crate::ast::TypedPatKind::Enum { params, .. } => {
                // Destructure enum variant
                for (i, param) in params.iter().enumerate() {
                    let field_val = self.extract_enum_field(&value, i);
                    self.bind_pattern(param, field_val);
                }
            }

            crate::ast::TypedPatKind::Or(patterns) => {
                // Or-pattern - bind in all branches (tricky, may need phi)
                for pat in patterns {
                    self.bind_pattern(pat, value.clone());
                }
            }

            crate::ast::TypedPatKind::As {
                name: _,
                binding_id,
                pattern,
            } => {
                // Bind the whole value
                if let IRValue::SSA(val_id) = &value {
                    self.bindings.insert(binding_id.0, *val_id);
                }
                // Then bind the inner pattern
                self.bind_pattern(pattern, value);
            }

            _ => {
                // Other pattern types not yet implemented
                todo!("Pattern kind: {:?}", pattern.pat);
            }
        }
    }

    fn extract_discriminant(&mut self, enum_val: &IRValue) -> IRValue {
        let result = self.builder.fresh_value_id();

        // Extract the ValueId from the IRValue if it's an SSA value
        let base_value_id = if let IRValue::SSA(id) = enum_val {
            *id
        } else {
            // If enum_val is not an SSA value, we need to create one
            // For now, just use a fresh ID - this might need more complex handling
            self.builder.fresh_value_id()
        };

        self.emit(IRInstruction::Load {
            result,
            address: enum_val.clone(),
            address_kind: AddressKind::StructField {
                base: base_value_id,
                field: FieldId(0), // Discriminant is field 0
            },
            type_info: IRTypeWithMemory {
                type_: IRType::Int,
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            span: 0..0,
            file: String::new(),
        });
        IRValue::SSA(result)
    }

    fn extract_tuple_field(&mut self, tuple_val: &IRValue, field_index: usize) -> IRValue {
        let result = self.builder.fresh_value_id();

        self.emit(IRInstruction::FieldAccess {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            target: tuple_val.clone(),
            field_id: FieldId(field_index),
            field_type: IRTypeWithMemory {
                type_: IRType::Error, // Will be refined by type checker
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            span: 0..0,
            file: String::new(),
        });

        IRValue::SSA(result)
    }

    fn extract_struct_field(&mut self, struct_val: &IRValue, field_id: FieldId) -> IRValue {
        let result = self.builder.fresh_value_id();

        self.emit(IRInstruction::FieldAccess {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            target: struct_val.clone(),
            field_id,
            field_type: IRTypeWithMemory {
                type_: IRType::Error, // Will be refined by type checker
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            span: 0..0,
            file: String::new(),
        });

        IRValue::SSA(result)
    }

    fn extract_enum_field(&mut self, enum_val: &IRValue, field_index: usize) -> IRValue {
        let result = self.builder.fresh_value_id();

        self.emit(IRInstruction::FieldAccess {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            target: enum_val.clone(),
            field_id: FieldId(field_index + 1), // +1 to skip discriminant at index 0
            field_type: IRTypeWithMemory {
                type_: IRType::Error, // Will be refined by type checker
                span: 0..0,
                file: String::new(),
                memory_kind: MemoryKind::Stack,
                allocation_id: None,
            },
            span: 0..0,
            file: String::new(),
        });

        IRValue::SSA(result)
    }

    fn lower_while(
        &mut self,
        condition: &TypedExpr,
        body: &TypedExpr,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        let loop_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        // Jump to loop condition
        self.emit_jump(loop_block, span, file);

        // Set up loop condition block
        self.set_current_block(loop_block);
        let cond_val = self.lower_expr(condition);

        // Branch based on condition
        let current_block = self.blocks.get_mut(&self.current_block).unwrap();
        current_block.terminator = Some(IRTerminator::Branch {
            condition: cond_val,
            then_block: body_block,
            else_block: exit_block,
            span: span.clone(),
            file: file.to_string(),
        });

        // Update CFG
        self.add_cfg_edge(loop_block, body_block);
        self.add_cfg_edge(loop_block, exit_block);

        // Lower loop body
        self.set_current_block(body_block);
        let _body_val = self.lower_expr(body);

        // Jump back to condition
        self.emit_jump(loop_block, span, file);

        // Set up exit block
        self.set_current_block(exit_block);

        IRValue::Unit
    }

    fn lower_for(
        &mut self,
        iterator: &TypedExpr,
        _value: &str,
        binding_id: BindingId,
        body: &TypedExpr,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        // Lower the iterator expression
        let _iter_expr = self.lower_expr(iterator);

        // Create a binding for the iterator
        let iter_id = self.builder.fresh_value_id();
        self.bindings.insert(binding_id.0, iter_id);

        // Create basic blocks
        let loop_condition_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        // Jump to loop condition
        self.emit_jump(loop_condition_block, span, file);

        // Loop condition: call .next() on iterator
        self.set_current_block(loop_condition_block);

        // Call iterator.next() - simplified approach
        // In reality, this should resolve the actual .next() method
        let next_result = self.builder.fresh_value_id();
        let func_id = self.builder.fresh_function_id(); // Get function ID before calling emit
        self.emit(IRInstruction::Call {
            result: next_result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            function: IRValue::FunctionRef(func_id), // Placeholder
            args: vec![IRValue::SSA(iter_id)],
            type_args: vec![],
            span: span.clone(),
            file: file.to_string(),
        });

        // Extract discriminant from Option result (Some = 0, None = 1)
        let next_val = IRValue::SSA(next_result);
        let discriminant = self.extract_discriminant(&next_val);

        // Switch on Some/None
        let current_block = self.blocks.get_mut(&self.current_block).unwrap();
        current_block.terminator = Some(IRTerminator::Switch {
            value: discriminant,
            cases: vec![
                (IRValue::Int(0), body_block), // Some case
                (IRValue::Int(1), exit_block), // None case
            ],
            default: None,
            is_exhaustive: true,
            span: span.clone(),
            file: file.to_string(),
        });

        // Update CFG
        self.add_cfg_edge(self.current_block, body_block);
        self.add_cfg_edge(self.current_block, exit_block);

        // Body block: extract value from Some, bind it, run body
        self.set_current_block(body_block);

        // Extract the inner value from Some
        let loop_value = self.extract_enum_field(&next_val, 0);

        // Bind the loop variable
        if let IRValue::SSA(val_id) = loop_value {
            self.bindings.insert(binding_id.0, val_id);
        }

        // Lower the body
        self.lower_expr(body);

        // Jump back to condition
        self.emit_jump(loop_condition_block, span, file);

        // Update CFG
        self.add_cfg_edge(self.current_block, loop_condition_block);

        // Exit block
        self.set_current_block(exit_block);

        IRValue::Unit
    }

    fn lower_call(
        &mut self,
        function: &TypedExpr,
        args: &[TypedExpr],
        type_args: &[Rc<crate::typechecker::Type>],
        expr: &TypedExpr,
    ) -> IRValue {
        let func_val = self.lower_expr(function);
        let arg_values: Vec<_> = args.iter().map(|arg| self.lower_expr(arg)).collect();
        let type_args: Vec<_> = type_args
            .iter()
            .map(|ty| self.builder.convert_type(ty))
            .collect();

        // Check if this is a call to a builtin function
        let result = self.builder.fresh_value_id();
        match &function.expr {
            TypedExprKind::Variable { name, .. } => {
                // Check if it's a builtin function
                let builtin_name = self.builder.interner.resolve(*name).to_string();
                if builtin_name == "print"
                    || builtin_name == "input"
                    || builtin_name == "int_to_string"
                    || builtin_name == "float_to_string"
                    || builtin_name == "bool_to_string"
                    || builtin_name == "typeof"
                {
                    // Generate CallBuiltin instruction for builtin functions
                    self.emit(IRInstruction::CallBuiltin {
                        result,
                        metadata: InstructionMetadata {
                            memory_slot: None,
                            allocation_site: None,
                        },
                        builtin_name,
                        args: arg_values,
                        span: expr.span.clone(),
                        file: expr.file.clone(),
                    });
                } else {
                    // Generate regular Call instruction for user-defined functions
                    self.emit(IRInstruction::Call {
                        result,
                        metadata: InstructionMetadata {
                            memory_slot: None,
                            allocation_site: None,
                        },
                        function: func_val,
                        args: arg_values,
                        type_args,
                        span: expr.span.clone(),
                        file: expr.file.clone(),
                    });
                }
            }
            _ => {
                // For function values (not direct variable references), assume it's a regular call
                self.emit(IRInstruction::Call {
                    result,
                    metadata: InstructionMetadata {
                        memory_slot: None,
                        allocation_site: None,
                    },
                    function: func_val,
                    args: arg_values,
                    type_args,
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                });
            }
        }

        IRValue::SSA(result)
    }

    fn lower_struct_construct(
        &mut self,
        _struct_name: Symbol,
        struct_id: StructId,
        fields: &[(Symbol, FieldId, TypedExpr)],
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        // Lower each field value
        let mut field_values = Vec::new();
        for (_, field_id, field_expr) in fields {
            let field_val = self.lower_expr(field_expr);
            field_values.push((FieldId(field_id.0), field_val)); // Convert from AST FieldId to IR FieldId
        }

        // Determine if this struct should be allocated on stack or heap based on escape analysis
        let memory_kind = self.analyze_struct_escape(struct_id, span);
        let allocation_id = if memory_kind == MemoryKind::Heap {
            Some(self.builder.fresh_alloc_id())
        } else {
            None
        };

        let result = self.builder.fresh_value_id();
        let slotid = self.builder.fresh_memory_slot_id();
        self.emit(IRInstruction::StructConstruct {
            result,
            metadata: InstructionMetadata {
                memory_slot: if memory_kind == MemoryKind::Stack {
                    Some(slotid)
                } else {
                    None
                },
                allocation_site: allocation_id,
            },
            struct_id,
            fields: field_values,
            span: span.clone(),
            file: file.to_string(),
        });

        IRValue::SSA(result)
    }

    fn analyze_struct_escape(&mut self, _struct_id: StructId, _span: &Range<usize>) -> MemoryKind {
        // Basic escape analysis for struct construction
        // For now, if the struct is large or we're in a return context, allocate on heap
        // In a real implementation, this would be more sophisticated

        // Check if we're in a return context or if the struct is large
        // For now, we'll assume all structs are stack allocated unless they're definitely escaping
        MemoryKind::Stack
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_enum_construct(
        &mut self,
        _enum_name: Symbol,
        enum_id: EnumId,
        _variant: Symbol,
        variant_id: VariantId,
        args: &[TypedExpr],
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        // Lower each argument value
        let arg_values: Vec<_> = args.iter().map(|arg| self.lower_expr(arg)).collect();

        let result = self.builder.fresh_value_id();
        self.emit(IRInstruction::EnumConstruct {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            enum_id: EnumId(enum_id.0), // Convert from AST EnumId to IR EnumId
            variant_id: VariantId(variant_id.0), // Convert from AST VariantId to IR VariantId
            args: arg_values,
            span: span.clone(),
            file: file.to_string(),
        });

        IRValue::SSA(result)
    }

    fn lower_field_access(
        &mut self,
        obj_expr: &TypedExpr,
        _field: Symbol,
        field_id: FieldId,
        field_type: &Rc<crate::typechecker::Type>,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        let obj_val = self.lower_expr(obj_expr);

        let result = self.builder.fresh_value_id();
        self.emit(IRInstruction::FieldAccess {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            target: obj_val,
            field_id: FieldId(field_id.0), // Convert from AST FieldId to IR FieldId
            field_type: self.builder.convert_type(field_type),
            span: span.clone(),
            file: file.to_string(),
        });

        IRValue::SSA(result)
    }

    fn lower_index(
        &mut self,
        collection: &TypedExpr,
        index: &TypedExpr,
        element_type: &Rc<crate::typechecker::Type>,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        let collection_val = self.lower_expr(collection);
        let index_val = self.lower_expr(index);

        let result = self.builder.fresh_value_id();
        self.emit(IRInstruction::Index {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            target: collection_val,
            index: index_val,
            element_type: self.builder.convert_type(element_type),
            span: span.clone(),
            file: file.to_string(),
        });

        IRValue::SSA(result)
    }

    fn lower_array(
        &mut self,
        elements: &[TypedExpr],
        element_type: &Rc<crate::typechecker::Type>,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        let element_values: Vec<_> = elements.iter().map(|elem| self.lower_expr(elem)).collect();

        // Arrays are always heap allocated
        let allocation_id = Some(self.builder.fresh_alloc_id());

        let result = self.builder.fresh_value_id();
        self.emit(IRInstruction::Array {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: allocation_id,
            },
            elements: element_values,
            element_type: self.builder.convert_type(element_type),
            span: span.clone(),
            file: file.to_string(),
        });

        IRValue::SSA(result)
    }

    fn lower_tuple(&mut self, elements: &[TypedExpr], span: &Range<usize>, file: &str) -> IRValue {
        let element_values: Vec<_> = elements.iter().map(|elem| self.lower_expr(elem)).collect();

        // Determine if tuple should be allocated on stack or heap based on size
        let memory_kind = if elements.len() > 2 {
            MemoryKind::Heap
        } else {
            MemoryKind::Stack
        };

        let result = self.builder.fresh_value_id();
        let slotid = self.builder.fresh_memory_slot_id();
        let allocid = self.builder.fresh_alloc_id();
        self.emit(IRInstruction::Tuple {
            result,
            metadata: InstructionMetadata {
                memory_slot: if memory_kind == MemoryKind::Stack {
                    Some(slotid)
                } else {
                    None
                },
                allocation_site: if memory_kind == MemoryKind::Heap {
                    Some(allocid)
                } else {
                    None
                },
            },
            elements: element_values,
            span: span.clone(),
            file: file.to_string(),
        });

        IRValue::SSA(result)
    }

    fn lower_optional_chain(
        &mut self,
        target: &TypedExpr,
        _field: Symbol,
        field_id: FieldId,
        field_type: &Rc<crate::typechecker::Type>,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        // Optional chain (obj?.field) is desugared to a match on Option
        let target_val = self.lower_expr(target);

        // Create blocks for Some case, None case, and merge
        let some_block = self.new_block();
        let none_block = self.new_block();
        let merge_block = self.new_block();

        let discriminant = self.extract_discriminant(&target_val);

        let current_block = self.blocks.get_mut(&self.current_block).unwrap();
        current_block.terminator = Some(IRTerminator::Switch {
            value: discriminant,
            cases: vec![
                (IRValue::Int(0), some_block), // Some variant
                (IRValue::Int(1), none_block), // None variant
            ],
            default: None,
            is_exhaustive: true,
            span: span.clone(),
            file: file.to_string(),
        });

        // Update CFG
        self.add_cfg_edge(self.current_block, some_block);
        self.add_cfg_edge(self.current_block, none_block);

        // Some case: extract inner value, access field, wrap in Some
        self.set_current_block(some_block);
        let inner_val = self.extract_enum_field(&target_val, 0); // Extract from Some
        let field_access_result = self.builder.fresh_value_id();
        self.emit(IRInstruction::FieldAccess {
            result: field_access_result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            target: inner_val,
            field_id,
            field_type: self.builder.convert_type(field_type),
            span: span.clone(),
            file: file.to_string(),
        });
        let some_result = IRValue::SSA(field_access_result);
        let some_end = self.current_block;
        self.emit_jump(merge_block, span, file);

        // None case: return None (represented as unit or null)
        self.set_current_block(none_block);
        let none_result = IRValue::Unit; // Represent None case
        let none_end = self.current_block;
        self.emit_jump(merge_block, span, file);

        // Merge block with PHI
        self.set_current_block(merge_block);
        let result = self.builder.fresh_value_id();

        // In SSA form, PHI nodes should properly reference the SSA values that flow from predecessors.
        // Convert IRValues to proper SSA values for the PHI node.
        let some_ssa_val = match some_result {
            IRValue::SSA(id) => IRValue::SSA(id),
            _ => {
                // For non-SSA values, we need to create an SSA value that represents this value
                let temp_id = self.builder.fresh_value_id();
                self.emit(IRInstruction::Let {
                    result: temp_id,
                    metadata: InstructionMetadata {
                        memory_slot: None,
                        allocation_site: None,
                    },
                    var: "_phi_temp_some".to_string(),
                    value: some_result,
                    var_type: IRTypeWithMemory {
                        type_: IRType::Int, // Placeholder - would be actual type in real implementation
                        span: span.clone(),
                        file: file.to_string(),
                        memory_kind: MemoryKind::Stack,
                        allocation_id: None,
                    },
                    span: span.clone(),
                    file: file.to_string(),
                });
                IRValue::SSA(temp_id)
            }
        };

        let none_ssa_val = match none_result {
            IRValue::SSA(id) => IRValue::SSA(id),
            _ => {
                // For non-SSA values, we need to create an SSA value that represents this value
                let temp_id = self.builder.fresh_value_id();
                self.emit(IRInstruction::Let {
                    result: temp_id,
                    metadata: InstructionMetadata {
                        memory_slot: None,
                        allocation_site: None,
                    },
                    var: "_phi_temp_none".to_string(),
                    value: none_result,
                    var_type: IRTypeWithMemory {
                        type_: IRType::Int, // Placeholder - would be actual type in real implementation
                        span: span.clone(),
                        file: file.to_string(),
                        memory_kind: MemoryKind::Stack,
                        allocation_id: None,
                    },
                    span: span.clone(),
                    file: file.to_string(),
                });
                IRValue::SSA(temp_id)
            }
        };

        let phi = IRInstruction::Phi {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            incoming: vec![(some_ssa_val, some_end), (none_ssa_val, none_end)],
            span: span.clone(),
            file: file.to_string(),
        };

        self.blocks.get_mut(&merge_block).unwrap().add_phi(phi);

        // Update CFG
        self.add_cfg_edge(some_end, merge_block);
        self.add_cfg_edge(none_end, merge_block);

        IRValue::SSA(result)
    }

    fn lower_perform(
        &mut self,
        _effect: Symbol,
        effect_id: EffectId,
        args: &[TypedExpr],
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        let arg_values: Vec<_> = args.iter().map(|arg| self.lower_expr(arg)).collect();

        let result = self.builder.fresh_value_id();
        self.emit(IRInstruction::Perform {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            effect_id: EffectId(effect_id.0), // Convert from AST EffectId to IR EffectId
            operation_id: 0, // This would be the operation ID in a real implementation
            args: arg_values,
            span: span.clone(),
            file: file.to_string(),
        });

        IRValue::SSA(result)
    }

    fn lower_handle(
        &mut self,
        body: &TypedExpr,
        handlers: &[crate::ast::TypedEffectHandler],
        return_type: &Rc<crate::typechecker::Type>,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        // Create blocks
        let body_block = self.new_block();
        let handler_blocks: Vec<_> = handlers.iter().map(|_| self.new_block()).collect();
        let merge_block = self.new_block();

        // Build handler block structures
        let mut ir_handlers = Vec::new();

        for (i, handler) in handlers.iter().enumerate() {
            let handler_block = handler_blocks[i];

            // Create parameters for handler
            let mut params = Vec::new();
            for param_tuple in &handler.params {
                let (_param_name, param_binding_id, param_type) = param_tuple;
                let param_id = self.builder.fresh_value_id();
                let slot = self.builder.fresh_memory_slot_id();
                let param_ir_type = self.builder.convert_type(param_type);

                params.push((param_id, slot, param_ir_type));

                // Bind parameter for use in handler body
                self.bindings.insert(param_binding_id.0, param_id);
            }

            // Create resume parameter (continuation)
            let resume_id = self.builder.fresh_value_id();
            let resume_slot = self.builder.fresh_memory_slot_id();
            let resume_type = self.builder.convert_type(&handler.resume_type);

            // Bind resume parameter
            self.bindings.insert(handler.resume_id.0, resume_id);

            // Create continuation type - using empty effect set as placeholder
            let continuation_type = crate::ir::ContinuationType {
                input_type: self.builder.convert_type(&handler.resume_type),
                output_type: self.builder.convert_type(return_type),
                captured_effects: crate::typechecker::EffectSet::pure(), // Using pure as placeholder
            };

            ir_handlers.push(crate::ir::EffectHandlerBlock {
                effect_id: EffectId(handler.effect_id.0),
                params,
                resume_param: resume_id,
                resume_memory: resume_slot,
                resume_type,
                continuation_type,
                body: handler_block,
                span: handler.span.clone(),
                file: handler.body.file.clone(), // Using file from body since handler doesn't have direct file field
            });
        }

        // Emit Handle terminator
        let current_block_id = self.current_block;
        let current_block = self.blocks.get_mut(&current_block_id).unwrap();
        current_block.terminator = Some(IRTerminator::Handle {
            body: body_block,
            handlers: ir_handlers.clone(),
            return_type: self.builder.convert_type(return_type),
            span: span.clone(),
            file: file.to_string(),
        });

        // Lower the body
        self.set_current_block(body_block);
        let body_result = self.lower_expr(body);
        let body_end = self.current_block;
        self.emit_jump(merge_block, span, file);

        // Lower each handler
        let mut handler_results = Vec::new();

        for (i, handler) in handlers.iter().enumerate() {
            self.set_current_block(handler_blocks[i]);

            // Lower handler body
            let handler_result = self.lower_expr(&handler.body);
            let handler_end = self.current_block;
            handler_results.push((handler_result, handler_end));

            self.emit_jump(merge_block, span, file);
        }

        // Create merge block with PHI
        self.set_current_block(merge_block);
        let result = self.builder.fresh_value_id();

        // Collect all incoming values (body + handlers)
        let mut incoming = vec![(body_result, body_end)];
        incoming.extend(handler_results);

        // In SSA form, PHI nodes should properly reference the SSA values that flow from predecessors.
        // Convert IRValues to proper SSA values for the PHI node.
        let ssa_incoming: Vec<(IRValue, BasicBlockId)> = incoming
            .into_iter()
            .map(|(incoming_result, end_block)| {
                let ssa_val = match incoming_result {
                    IRValue::SSA(id) => IRValue::SSA(id),
                    _ => {
                        // For non-SSA values, we need to create an SSA value that represents this value
                        let temp_id = self.builder.fresh_value_id();
                        self.emit(IRInstruction::Let {
                            result: temp_id,
                            metadata: InstructionMetadata {
                                memory_slot: None,
                                allocation_site: None,
                            },
                            var: "_phi_temp_handle".to_string(),
                            value: incoming_result,
                            var_type: IRTypeWithMemory {
                                type_: IRType::Int, // Placeholder - would be actual type in real implementation
                                span: span.clone(),
                                file: file.to_string(),
                                memory_kind: MemoryKind::Stack,
                                allocation_id: None,
                            },
                            span: span.clone(),
                            file: file.to_string(),
                        });
                        IRValue::SSA(temp_id)
                    }
                };
                (ssa_val, end_block)
            })
            .collect();

        let phi = IRInstruction::Phi {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            incoming: ssa_incoming,
            span: span.clone(),
            file: file.to_string(),
        };

        self.blocks.get_mut(&merge_block).unwrap().add_phi(phi);

        IRValue::SSA(result)
    }

    fn lower_block(
        &mut self,
        expressions: &[TypedExpr],
        _span: &Range<usize>,
        _file: &str,
    ) -> IRValue {
        let mut result = IRValue::Unit;

        for expr in expressions {
            result = self.lower_expr(expr);
        }

        result
    }

    fn lower_assign(
        &mut self,
        l_val: &TypedExpr,
        r_val: &TypedExpr,
        op: &crate::ast::AssignOp,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        let l_val_result = self.lower_expr(l_val);
        let r_val_result = self.lower_expr(r_val);

        let result = self.builder.fresh_value_id();
        self.emit(IRInstruction::Assign {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            l_val: l_val_result,
            r_val: r_val_result,
            op: op.clone(),
            span: span.clone(),
            file: file.to_string(),
        });

        IRValue::SSA(result)
    }

    fn lower_map(
        &mut self,
        entries: &[(TypedExpr, TypedExpr)],
        key_type: &Rc<crate::typechecker::Type>,
        value_type: &Rc<crate::typechecker::Type>,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        let entry_values: Vec<_> = entries
            .iter()
            .map(|(k, v)| (self.lower_expr(k), self.lower_expr(v)))
            .collect();

        let result = self.builder.fresh_value_id();
        self.emit(IRInstruction::Map {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            entries: entry_values,
            key_type: self.builder.convert_type(key_type),
            value_type: self.builder.convert_type(value_type),
            span: span.clone(),
            file: file.to_string(),
        });

        IRValue::SSA(result)
    }

    fn lower_cast(
        &mut self,
        expr: &TypedExpr,
        target_type: &Rc<crate::typechecker::Type>,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        let expr_result = self.lower_expr(expr);

        let result = self.builder.fresh_value_id();
        self.emit(IRInstruction::Cast {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            expr: expr_result,
            target_type: self.builder.convert_type(target_type),
            span: span.clone(),
            file: file.to_string(),
        });

        IRValue::SSA(result)
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_with(
        &mut self,
        context: &TypedExpr,
        _var: Symbol,
        binding_id: BindingId,
        _var_type: &Rc<crate::typechecker::Type>,
        body: &TypedExpr,
        _span: &Range<usize>,
        _file: &str,
    ) -> IRValue {
        // Lower the context expression
        let _context_val = self.lower_expr(context);

        // Create a binding for the variable
        let value_id = self.builder.fresh_value_id();
        self.bindings.insert(binding_id.0, value_id);

        // Lower the body with the binding in scope

        self.lower_expr(body)
    }

    fn lower_loop(
        &mut self,
        label: &Option<Symbol>,
        body: &TypedExpr,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        let body_block = self.new_block();
        let exit_block = self.new_block();

        // Create loop context
        self.loop_stack.push(LoopContext {
            continue_target: body_block,
            break_target: exit_block,
            label: *label,
        });

        // Jump to loop body
        self.emit_jump(body_block, span, file);

        // Set up loop body block
        self.set_current_block(body_block);
        let _body_val = self.lower_expr(body);

        // Jump back to loop body (infinite loop for now)
        self.emit_jump(body_block, span, file);

        // Set up exit block
        self.set_current_block(exit_block);

        // Pop loop context
        self.loop_stack.pop();

        IRValue::Unit
    }

    fn lower_if_let(
        &mut self,
        pattern: &crate::ast::TypedPattern,
        scrutinee: &TypedExpr,
        then_expr: &TypedExpr,
        else_expr: Option<&TypedExpr>,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        // Lower the scrutinee expression
        let scrutinee_val = self.lower_expr(scrutinee);

        // Create blocks for then, else, and merge
        let then_block = self.new_block();
        let else_block = self.new_block();
        let merge_block = self.new_block();

        // For if-let, we check if the scrutinee matches the pattern
        // In the case of Option<T>, we check if it's Some and bind the inner value
        let condition = scrutinee_val.clone(); // Placeholder

        let current_block = self.blocks.get_mut(&self.current_block).unwrap();
        current_block.terminator = Some(IRTerminator::Branch {
            condition,
            then_block,
            else_block,
            span: span.clone(),
            file: file.to_string(),
        });

        // Update CFG
        self.add_cfg_edge(self.current_block, then_block);
        self.add_cfg_edge(self.current_block, else_block);

        // Then block - handle pattern binding and then expression
        self.set_current_block(then_block);

        // Handle pattern binding
        if let crate::ast::TypedPatKind::Bind { binding_id, .. } = &pattern.pat {
            // Bind the scrutinee value to the pattern variable
            if let IRValue::SSA(id) = scrutinee_val {
                self.bindings.insert(binding_id.0, id);
            } else {
                let new_id = self.builder.fresh_value_id();
                self.bindings.insert(binding_id.0, new_id);
            }
        }

        let then_val = self.lower_expr(then_expr);
        let then_end = self.current_block;
        self.emit_jump(merge_block, span, file);

        // Else block - handle else expression
        self.set_current_block(else_block);
        let else_val = if let Some(else_expr) = else_expr {
            self.lower_expr(else_expr)
        } else {
            IRValue::Unit
        };
        let else_end = self.current_block;
        self.emit_jump(merge_block, span, file);

        // Merge block with PHI
        self.set_current_block(merge_block);
        let result = self.builder.fresh_value_id();

        // In SSA form, PHI nodes should properly reference the SSA values that flow from predecessors.
        // Convert IRValues to proper SSA values for the PHI node.
        let then_ssa_val = match then_val {
            IRValue::SSA(id) => IRValue::SSA(id),
            _ => {
                // For non-SSA values, we need to create an SSA value that represents this value
                let temp_id = self.builder.fresh_value_id();
                self.emit(IRInstruction::Let {
                    result: temp_id,
                    metadata: InstructionMetadata {
                        memory_slot: None,
                        allocation_site: None,
                    },
                    var: "_phi_temp_then".to_string(),
                    value: then_val,
                    var_type: IRTypeWithMemory {
                        type_: IRType::Int, // Placeholder - would be actual type in real implementation
                        span: span.clone(),
                        file: file.to_string(),
                        memory_kind: MemoryKind::Stack,
                        allocation_id: None,
                    },
                    span: span.clone(),
                    file: file.to_string(),
                });
                IRValue::SSA(temp_id)
            }
        };

        let else_ssa_val = match else_val {
            IRValue::SSA(id) => IRValue::SSA(id),
            _ => {
                // For non-SSA values, we need to create an SSA value that represents this value
                let temp_id = self.builder.fresh_value_id();
                self.emit(IRInstruction::Let {
                    result: temp_id,
                    metadata: InstructionMetadata {
                        memory_slot: None,
                        allocation_site: None,
                    },
                    var: "_phi_temp_else".to_string(),
                    value: else_val,
                    var_type: IRTypeWithMemory {
                        type_: IRType::Int, // Placeholder - would be actual type in real implementation
                        span: span.clone(),
                        file: file.to_string(),
                        memory_kind: MemoryKind::Stack,
                        allocation_id: None,
                    },
                    span: span.clone(),
                    file: file.to_string(),
                });
                IRValue::SSA(temp_id)
            }
        };

        let phi = IRInstruction::Phi {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            incoming: vec![(then_ssa_val, then_end), (else_ssa_val, else_end)],
            span: span.clone(),
            file: file.to_string(),
        };

        self.blocks.get_mut(&merge_block).unwrap().add_phi(phi);

        // Update CFG
        self.add_cfg_edge(then_end, merge_block);
        self.add_cfg_edge(else_end, merge_block);

        IRValue::SSA(result)
    }

    fn lower_while_let(
        &mut self,
        pattern: &crate::ast::TypedPattern,
        scrutinee: &TypedExpr,
        body: &TypedExpr,
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        // Create blocks for condition check, body, and exit
        let cond_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        // Jump to condition check
        self.emit_jump(cond_block, span, file);

        // Set up condition block
        self.set_current_block(cond_block);

        // Lower the scrutinee expression (this would typically be an iterator call in while-let)
        let scrutinee_val = self.lower_expr(scrutinee);

        // Create a conditional branch based on whether the pattern matches

        let current_block = self.blocks.get_mut(&self.current_block).unwrap();
        current_block.terminator = Some(IRTerminator::Branch {
            condition: scrutinee_val.clone(),
            then_block: body_block,
            else_block: exit_block,
            span: span.clone(),
            file: file.to_string(),
        });

        // Update CFG
        self.add_cfg_edge(cond_block, body_block);
        self.add_cfg_edge(cond_block, exit_block);

        // Body block
        self.set_current_block(body_block);

        // Handle pattern binding (simplified)
        if let crate::ast::TypedPatKind::Bind { binding_id, .. } = &pattern.pat {
            // Bind the scrutinee value to the pattern variable
            if let IRValue::SSA(id) = scrutinee_val {
                self.bindings.insert(binding_id.0, id);
            } else {
                let new_id = self.builder.fresh_value_id();
                self.bindings.insert(binding_id.0, new_id);
            }
        }

        // Lower the body
        let _body_val = self.lower_expr(body);

        // Jump back to condition to check again
        self.emit_jump(cond_block, span, file);

        // Exit block
        self.set_current_block(exit_block);

        IRValue::Unit
    }

    fn emit(&mut self, instruction: IRInstruction) {
        let block = self.blocks.get_mut(&self.current_block).unwrap();
        block.instructions.push(instruction);
    }

    fn emit_jump(&mut self, target: BasicBlockId, span: &Range<usize>, file: &str) {
        let current_block = self.blocks.get_mut(&self.current_block).unwrap();
        current_block.terminator = Some(IRTerminator::Jump {
            target,
            span: span.clone(),
            file: file.to_string(),
        });
        self.add_cfg_edge(self.current_block, target);
    }

    fn new_block(&mut self) -> BasicBlockId {
        let id = self.builder.fresh_block_id();
        self.blocks.insert(id, BasicBlock::new(id));
        id
    }

    fn set_current_block(&mut self, block: BasicBlockId) {
        self.current_block = block;
    }

    fn add_cfg_edge(&mut self, from: BasicBlockId, to: BasicBlockId) {
        self.cfg_edges.push((from, to));
    }

    fn find_break_target(&self, _label: Option<Symbol>) -> BasicBlockId {
        // Use the innermost loop
        self.loop_stack
            .last()
            .expect("Break without loop")
            .break_target
    }

    fn find_continue_target(&self, _label: Option<Symbol>) -> BasicBlockId {
        // Use the innermost loop
        self.loop_stack
            .last()
            .expect("Continue without loop")
            .continue_target
    }

    fn lower_builtin_macro_call(
        &mut self,
        func_name: &str,
        args: &[TypedExpr],
        span: &Range<usize>,
        file: &str,
    ) -> IRValue {
        // Convert the arguments
        let arg_values: Vec<_> = args.iter().map(|arg| self.lower_expr(arg)).collect();

        // Generate a call to the builtin function
        let result = self.builder.fresh_value_id();
        self.emit(IRInstruction::CallBuiltin {
            result,
            metadata: InstructionMetadata {
                memory_slot: None,
                allocation_site: None,
            },
            builtin_name: func_name.to_string(),
            args: arg_values,
            span: span.clone(),
            file: file.to_string(),
        });

        IRValue::SSA(result)
    }
}
