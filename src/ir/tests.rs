use crate::ast::Visibility;
use crate::ir::*;

#[test]
fn test_extern_function_creation() {
    let extern_func = IRExternFunction {
        id: FunctionId(1),
        name: "printf".to_string(),
        vis: Visibility::Public,
        args: vec![IRFunctionArg {
            name: "format".to_string(),
            binding_id: 1,
            type_: IRTypeWithMemory {
                type_: IRType::String,
                memory_kind: MemoryKind::Stack,
                span: 0..0,
                file: String::new(),
                allocation_id: None,
            },
            memory_slot: MemorySlotId(1),
            span: 0..0,
            file: String::new(),
        }],
        return_type: IRTypeWithMemory {
            type_: IRType::Int,
            memory_kind: MemoryKind::Stack,
            span: 0..0,
            file: String::new(),
            allocation_id: None,
        },
        effects: EffectSet::pure(),
        function_type: IRTypeWithMemory {
            type_: IRType::Function {
                params: vec![IRTypeWithMemory {
                    type_: IRType::String,
                    memory_kind: MemoryKind::Stack,
                    span: 0..0,
                    file: String::new(),
                    allocation_id: None,
                }],
                return_type: Box::new(IRTypeWithMemory {
                    type_: IRType::Int,
                    memory_kind: MemoryKind::Stack,
                    span: 0..0,
                    file: String::new(),
                    allocation_id: None,
                }),
                effects: EffectSet::pure(),
            },
            memory_kind: MemoryKind::Stack,
            span: 0..0,
            file: String::new(),
            allocation_id: None,
        },
        library: "libc".to_string(),
        symbol_name: None,
        span: 0..0,
        file: String::new(),
    };

    assert_eq!(extern_func.name, "printf");
    assert_eq!(extern_func.library, "libc");
    assert_eq!(extern_func.args.len(), 1);
    assert!(extern_func.effects.is_pure());
}

#[test]
fn test_extern_function_with_symbol_name() {
    let extern_func = IRExternFunction {
        id: FunctionId(2),
        name: "custom_sin".to_string(),
        vis: Visibility::Public,
        args: vec![IRFunctionArg {
            name: "x".to_string(),
            binding_id: 2,
            type_: IRTypeWithMemory {
                type_: IRType::Float,
                memory_kind: MemoryKind::Stack,
                span: 0..0,
                file: String::new(),
                allocation_id: None,
            },
            memory_slot: MemorySlotId(2),
            span: 0..0,
            file: String::new(),
        }],
        return_type: IRTypeWithMemory {
            type_: IRType::Float,
            memory_kind: MemoryKind::Stack,
            span: 0..0,
            file: String::new(),
            allocation_id: None,
        },
        effects: EffectSet::pure(),
        function_type: IRTypeWithMemory {
            type_: IRType::Function {
                params: vec![IRTypeWithMemory {
                    type_: IRType::Float,
                    memory_kind: MemoryKind::Stack,
                    span: 0..0,
                    file: String::new(),
                    allocation_id: None,
                }],
                return_type: Box::new(IRTypeWithMemory {
                    type_: IRType::Float,
                    memory_kind: MemoryKind::Stack,
                    span: 0..0,
                    file: String::new(),
                    allocation_id: None,
                }),
                effects: EffectSet::pure(),
            },
            memory_kind: MemoryKind::Stack,
            span: 0..0,
            file: String::new(),
            allocation_id: None,
        },
        library: "libm".to_string(),
        symbol_name: Some("sin".to_string()),
        span: 0..0,
        file: String::new(),
    };

    assert_eq!(extern_func.name, "custom_sin");
    assert_eq!(extern_func.library, "libm");
    assert_eq!(extern_func.symbol_name, Some("sin".to_string()));
}

#[test]
fn test_extern_function_with_effects() {
    let extern_func = IRExternFunction {
        id: FunctionId(3),
        name: "read_file".to_string(),
        vis: Visibility::Public,
        args: vec![IRFunctionArg {
            name: "path".to_string(),
            binding_id: 3,
            type_: IRTypeWithMemory {
                type_: IRType::String,
                memory_kind: MemoryKind::Stack,
                span: 0..0,
                file: String::new(),
                allocation_id: None,
            },
            memory_slot: MemorySlotId(3),
            span: 0..0,
            file: String::new(),
        }],
        return_type: IRTypeWithMemory {
            type_: IRType::String,
            memory_kind: MemoryKind::Heap,
            span: 0..0,
            file: String::new(),
            allocation_id: None,
        },
        effects: EffectSet {
            effects: vec![0], // Using dummy ID for "IO" effect
            rest: None,
        },
        function_type: IRTypeWithMemory {
            type_: IRType::Function {
                params: vec![IRTypeWithMemory {
                    type_: IRType::String,
                    memory_kind: MemoryKind::Stack,
                    span: 0..0,
                    file: String::new(),
                    allocation_id: None,
                }],
                return_type: Box::new(IRTypeWithMemory {
                    type_: IRType::String,
                    memory_kind: MemoryKind::Heap,
                    span: 0..0,
                    file: String::new(),
                    allocation_id: None,
                }),
                effects: EffectSet {
                    effects: vec![0], // Using dummy ID for "IO" effect
                    rest: None,
                },
            },
            memory_kind: MemoryKind::Stack,
            span: 0..0,
            file: String::new(),
            allocation_id: None,
        },
        library: "filelib".to_string(),
        symbol_name: None,
        span: 0..0,
        file: String::new(),
    };

    assert_eq!(extern_func.name, "read_file");
    assert_eq!(extern_func.library, "filelib");
    assert!(!extern_func.effects.is_pure());
    assert_eq!(extern_func.effects.effects.len(), 1);
    assert_eq!(extern_func.return_type.memory_kind, MemoryKind::Heap);
}

#[test]
fn test_extern_function_multiple_args() {
    let extern_func = IRExternFunction {
        id: FunctionId(4),
        name: "add_three_numbers".to_string(),
        vis: Visibility::Public,
        args: vec![
            IRFunctionArg {
                name: "a".to_string(),
                binding_id: 4,
                type_: IRTypeWithMemory {
                    type_: IRType::Int,
                    memory_kind: MemoryKind::Stack,
                    span: 0..0,
                    file: String::new(),
                    allocation_id: None,
                },
                memory_slot: MemorySlotId(4),
                span: 0..0,
                file: String::new(),
            },
            IRFunctionArg {
                name: "b".to_string(),
                binding_id: 5,
                type_: IRTypeWithMemory {
                    type_: IRType::Int,
                    memory_kind: MemoryKind::Stack,
                    span: 0..0,
                    file: String::new(),
                    allocation_id: None,
                },
                memory_slot: MemorySlotId(5),
                span: 0..0,
                file: String::new(),
            },
            IRFunctionArg {
                name: "c".to_string(),
                binding_id: 6,
                type_: IRTypeWithMemory {
                    type_: IRType::Int,
                    memory_kind: MemoryKind::Stack,
                    span: 0..0,
                    file: String::new(),
                    allocation_id: None,
                },
                memory_slot: MemorySlotId(6),
                span: 0..0,
                file: String::new(),
            },
        ],
        return_type: IRTypeWithMemory {
            type_: IRType::Int,
            memory_kind: MemoryKind::Stack,
            span: 0..0,
            file: String::new(),
            allocation_id: None,
        },
        effects: EffectSet::pure(),
        function_type: IRTypeWithMemory {
            type_: IRType::Function {
                params: vec![
                    IRTypeWithMemory {
                        type_: IRType::Int,
                        memory_kind: MemoryKind::Stack,
                        span: 0..0,
                        file: String::new(),
                        allocation_id: None,
                    },
                    IRTypeWithMemory {
                        type_: IRType::Int,
                        memory_kind: MemoryKind::Stack,
                        span: 0..0,
                        file: String::new(),
                        allocation_id: None,
                    },
                    IRTypeWithMemory {
                        type_: IRType::Int,
                        memory_kind: MemoryKind::Stack,
                        span: 0..0,
                        file: String::new(),
                        allocation_id: None,
                    },
                ],
                return_type: Box::new(IRTypeWithMemory {
                    type_: IRType::Int,
                    memory_kind: MemoryKind::Stack,
                    span: 0..0,
                    file: String::new(),
                    allocation_id: None,
                }),
                effects: EffectSet::pure(),
            },
            memory_kind: MemoryKind::Stack,
            span: 0..0,
            file: String::new(),
            allocation_id: None,
        },
        library: "mathlib".to_string(),
        symbol_name: None,
        span: 0..0,
        file: String::new(),
    };

    assert_eq!(extern_func.name, "add_three_numbers");
    assert_eq!(extern_func.library, "mathlib");
    assert_eq!(extern_func.args.len(), 3);
    assert!(extern_func.effects.is_pure());
}

#[test]
fn test_extern_function_private_visibility() {
    let extern_func = IRExternFunction {
        id: FunctionId(5),
        name: "internal_helper".to_string(),
        vis: Visibility::Private,
        args: vec![],
        return_type: IRTypeWithMemory {
            type_: IRType::Unit,
            memory_kind: MemoryKind::Stack,
            span: 0..0,
            file: String::new(),
            allocation_id: None,
        },
        effects: EffectSet::pure(),
        function_type: IRTypeWithMemory {
            type_: IRType::Function {
                params: vec![],
                return_type: Box::new(IRTypeWithMemory {
                    type_: IRType::Unit,
                    memory_kind: MemoryKind::Stack,
                    span: 0..0,
                    file: String::new(),
                    allocation_id: None,
                }),
                effects: EffectSet::pure(),
            },
            memory_kind: MemoryKind::Stack,
            span: 0..0,
            file: String::new(),
            allocation_id: None,
        },
        library: "helperlib".to_string(),
        symbol_name: None,
        span: 0..0,
        file: String::new(),
    };

    assert_eq!(extern_func.name, "internal_helper");
    assert_eq!(extern_func.library, "helperlib");
    assert_eq!(extern_func.vis, Visibility::Private);
    assert_eq!(extern_func.args.len(), 0);
}
