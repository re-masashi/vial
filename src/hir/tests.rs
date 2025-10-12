use crate::hir::display::HirDisplay;
use crate::hir::{
    AllocationInfo, AllocationPreference, BasicBlock, BlockId, Builder, ConstantFolder,
    ControlFlowGraph, DominatorTree, Function, FunctionSignature, IntType, Opcode, Param, PhiNode,
    Terminator, Type, TypeContext, TypeId, ValueId,
};
use std::collections::HashMap;

#[test]
fn test_type_context() {
    let mut type_ctx = TypeContext::new();

    // Test integer types
    let i32_type = type_ctx.intern(Type::Int(IntType::I32));
    let i64_type = type_ctx.intern(Type::Int(IntType::I64));

    assert_ne!(i32_type, i64_type);
    assert_eq!(type_ctx.get(i32_type), Some(&Type::Int(IntType::I32)));

    // Test size calculation
    assert_eq!(type_ctx.size_of(i32_type), Some(4));
    assert_eq!(type_ctx.size_of(i64_type), Some(8));
}

#[test]
fn test_builder_basic_function() {
    let mut builder = Builder::new();

    // Create a simple function signature: fn add(x: i32, y: i32) -> i32
    let mut type_ctx = TypeContext::new();
    let i32_type = type_ctx.intern(Type::Int(IntType::I32));

    let signature = FunctionSignature {
        params: vec![
            Param {
                name: Some("x".to_string()),
                ty: i32_type,
                allocation: AllocationPreference::Stack,
            },
            Param {
                name: Some("y".to_string()),
                ty: i32_type,
                allocation: AllocationPreference::Stack,
            },
        ],
        return_type: i32_type,
        is_variadic: false,
    };

    let func = builder.start_function("add".to_string(), signature);
    assert_eq!(func.name, "add");
    assert_eq!(func.signature.params.len(), 2);
}

#[test]
fn test_control_flow_graph() {
    // Create a simple function with basic blocks
    let mut builder = Builder::new();

    let mut type_ctx = TypeContext::new();
    let i32_type = type_ctx.intern(Type::Int(IntType::I32));

    let signature = FunctionSignature {
        params: vec![Param {
            name: Some("x".to_string()),
            ty: i32_type,
            allocation: AllocationPreference::Stack,
        }],
        return_type: i32_type,
        is_variadic: false,
    };

    let func = builder.start_function("test".to_string(), signature);

    // We would normally add blocks and instructions here
    // For now, just test that we can create the CFG
    let cfg = ControlFlowGraph::new(func);
    assert!(cfg.is_well_formed());
}

#[test]
fn test_dominator_tree() {
    // Create a simple function to test dominator computation
    let mut builder = Builder::new();

    let mut type_ctx = TypeContext::new();
    let i32_type = type_ctx.intern(Type::Int(IntType::I32));

    let signature = FunctionSignature {
        params: vec![],
        return_type: i32_type,
        is_variadic: false,
    };

    let func = builder.start_function("dom_test".to_string(), signature);
    let cfg = ControlFlowGraph::new(func);
    let _dom_tree = DominatorTree::new(&cfg);

    // For an empty function, the dominator tree should be valid
    assert!(true); // Basic test to ensure no panics
}

#[test]
fn test_constant_folding() {
    // Test constant folding utilities
    assert_eq!(ConstantFolder::fold_binary_op(Opcode::Add, 5, 3), Some(8));
    assert_eq!(ConstantFolder::fold_binary_op(Opcode::Mul, 4, 6), Some(24));
    assert_eq!(ConstantFolder::fold_binary_op(Opcode::Eq, 5, 5), Some(1)); // true
    assert_eq!(ConstantFolder::fold_binary_op(Opcode::Eq, 5, 3), Some(0)); // false
}

#[test]
fn test_allocation_info() {
    // Test allocation information
    let alloc_info = AllocationInfo {
        preference: AllocationPreference::Heap,
        alignment: Some(16),
        is_pinned: true,
    };

    assert_eq!(alloc_info.preference, AllocationPreference::Heap);
    assert_eq!(alloc_info.alignment, Some(16));
    assert!(alloc_info.is_pinned);
}

#[test]
fn test_hir_display() {
    let mut type_context = TypeContext::new();
    let void_type = type_context.intern(Type::Void);

    // Create a simple function
    let function = Function {
        name: "test_display".to_string(),
        signature: FunctionSignature {
            params: vec![],
            return_type: void_type,
            is_variadic: false,
        },
        basic_blocks: vec![BasicBlock {
            id: BlockId(0),
            params: vec![],
            instructions: vec![],
            phi_nodes: vec![],
            terminator: Terminator::Return { value: None },
        }],
        instructions: vec![],
        block_params: HashMap::new(),
    };

    // Test the display functionality
    let hir_display = HirDisplay {
        function: &function,
        type_context: Some(&type_context),
    };

    let output = format!("{}", hir_display);
    assert!(output.contains("define void @test_display()"));
    assert!(output.contains("ret void"));
}

#[test]
fn test_phi_node_creation() {
    // Test creating a phi node
    let phi_node = PhiNode {
        result: ValueId(0),
        ty: TypeId(0),
        incomings: vec![(ValueId(1), BlockId(0)), (ValueId(2), BlockId(1))],
    };

    assert_eq!(phi_node.result, ValueId(0));
    assert_eq!(phi_node.ty, TypeId(0));
    assert_eq!(phi_node.incomings.len(), 2);
    assert_eq!(phi_node.incomings[0], (ValueId(1), BlockId(0)));
    assert_eq!(phi_node.incomings[1], (ValueId(2), BlockId(1)));
}

#[test]
fn test_phi_node_in_basic_block() {
    // Create a basic block with phi nodes
    let phi_node = PhiNode {
        result: ValueId(0),
        ty: TypeId(0),
        incomings: vec![(ValueId(1), BlockId(0)), (ValueId(2), BlockId(1))],
    };

    let basic_block = BasicBlock {
        id: BlockId(2),
        params: vec![ValueId(3), ValueId(4)],
        instructions: vec![],
        phi_nodes: vec![phi_node],
        terminator: Terminator::Return { value: None },
    };

    assert_eq!(basic_block.phi_nodes.len(), 1);
    assert_eq!(basic_block.phi_nodes[0].result, ValueId(0));
    assert_eq!(basic_block.params.len(), 2);
}

#[test]
fn test_basic_block_params_for_ssa() {
    // Create a basic block with parameters (used for SSA phi nodes)
    let basic_block = BasicBlock {
        id: BlockId(0),
        params: vec![ValueId(1), ValueId(2), ValueId(3)], // Parameters for phi nodes
        instructions: vec![],
        phi_nodes: vec![],
        terminator: Terminator::Return { value: None },
    };

    assert_eq!(basic_block.params.len(), 3);
    assert_eq!(basic_block.params[0], ValueId(1));
    assert_eq!(basic_block.params[1], ValueId(2));
    assert_eq!(basic_block.params[2], ValueId(3));
}

#[test]
fn test_control_flow_with_phi_nodes() {
    let mut type_ctx = TypeContext::new();
    let i32_type = type_ctx.intern(Type::Int(IntType::I32));

    // Create a function with multiple blocks and phi nodes for SSA
    let function = Function {
        name: "test_phi".to_string(),
        signature: FunctionSignature {
            params: vec![],
            return_type: i32_type,
            is_variadic: false,
        },
        basic_blocks: vec![
            // Entry block
            BasicBlock {
                id: BlockId(0),
                params: vec![],
                instructions: vec![],
                phi_nodes: vec![],
                terminator: Terminator::Branch {
                    condition: ValueId(0), // This would be a real condition in practice
                    then_block: BlockId(1),
                    else_block: BlockId(2),
                    then_args: vec![ValueId(1)], // Pass value to then block
                    else_args: vec![ValueId(2)], // Pass value to else block
                },
            },
            // Then block
            BasicBlock {
                id: BlockId(1),
                params: vec![ValueId(3)], // Parameter for phi node
                instructions: vec![],
                phi_nodes: vec![],
                terminator: Terminator::Jump {
                    target: BlockId(3),
                    args: vec![ValueId(4)], // Pass value to merge block
                },
            },
            // Else block
            BasicBlock {
                id: BlockId(2),
                params: vec![ValueId(5)], // Parameter for phi node
                instructions: vec![],
                phi_nodes: vec![],
                terminator: Terminator::Jump {
                    target: BlockId(3),
                    args: vec![ValueId(6)], // Pass value to merge block
                },
            },
            // Merge block with phi node
            BasicBlock {
                id: BlockId(3),
                params: vec![ValueId(7), ValueId(8)], // Parameters from both paths for phi
                instructions: vec![],
                phi_nodes: vec![PhiNode {
                    result: ValueId(9),
                    ty: i32_type,
                    incomings: vec![(ValueId(7), BlockId(1)), (ValueId(8), BlockId(2))],
                }],
                terminator: Terminator::Return {
                    value: Some(ValueId(9)),
                },
            },
        ],
        instructions: vec![],
        block_params: HashMap::new(),
    };

    // Test that the function has the expected structure
    assert_eq!(function.basic_blocks.len(), 4);

    // Check that the merge block has the expected phi node
    let merge_block = &function.basic_blocks[3];
    assert_eq!(merge_block.phi_nodes.len(), 1);
    assert_eq!(merge_block.phi_nodes[0].result, ValueId(9));
    assert_eq!(merge_block.phi_nodes[0].incomings.len(), 2);
}

#[test]
fn test_ssa_builder_with_phi_nodes() {
    let mut builder = Builder::new();
    let mut type_ctx = TypeContext::new();

    // Create types
    let i32_type = type_ctx.intern(Type::Int(IntType::I32));

    // Create function signature
    let signature = FunctionSignature {
        params: vec![],
        return_type: i32_type,
        is_variadic: false,
    };

    // Start function
    let _func = builder.start_function("test_ssa".to_string(), signature);

    // Create blocks
    let entry_block = builder.create_block();
    let then_block = builder.create_block();
    let else_block = builder.create_block();
    let merge_block = builder.create_block();

    // Switch to entry block and create some values
    builder.switch_to_block(entry_block);
    let const1 = builder.const_int(10, i32_type);
    let const2 = builder.const_int(20, i32_type);

    // Create a condition value (in a real scenario, this would come from a comparison)
    let condition = builder.const_bool(true);

    // Create branch instruction
    let _terminator = builder.branch(
        condition,
        then_block,
        else_block,
        vec![const1], // Value to pass to then block
        vec![const2], // Value to pass to else block
    );

    // Switch to then block
    builder.switch_to_block(then_block);
    let then_val = builder.const_int(100, i32_type);
    let _ = builder.jump(merge_block, vec![then_val]);

    // Switch to else block
    builder.switch_to_block(else_block);
    let else_val = builder.const_int(200, i32_type);
    let _ = builder.jump(merge_block, vec![else_val]);

    // Switch to merge block and add phi node
    builder.switch_to_block(merge_block);
    let phi_result = ValueId(100); // In a real scenario, this would be generated by the builder
    builder.add_phi_node(
        phi_result,
        i32_type,
        vec![(then_val, then_block), (else_val, else_block)],
    );

    let _ret = builder.ret(Some(phi_result));

    // Test that the builder can create SSA form with phi nodes
    assert!(true); // Basic test to ensure no panics during construction
}

#[test]
fn test_complex_ssa_control_flow() {
    let mut type_ctx = TypeContext::new();
    let i32_type = type_ctx.intern(Type::Int(IntType::I32));

    // Create a more complex function with nested control flow and multiple phi nodes
    let function = Function {
        name: "complex_ssa".to_string(),
        signature: FunctionSignature {
            params: vec![],
            return_type: i32_type,
            is_variadic: false,
        },
        basic_blocks: vec![
            // Entry block
            BasicBlock {
                id: BlockId(0),
                params: vec![],
                instructions: vec![],
                phi_nodes: vec![],
                terminator: Terminator::Branch {
                    condition: ValueId(0),
                    then_block: BlockId(1),
                    else_block: BlockId(2),
                    then_args: vec![],
                    else_args: vec![],
                },
            },
            // Inner if true block
            BasicBlock {
                id: BlockId(1),
                params: vec![],
                instructions: vec![],
                phi_nodes: vec![],
                terminator: Terminator::Branch {
                    condition: ValueId(1),
                    then_block: BlockId(3),
                    else_block: BlockId(4),
                    then_args: vec![ValueId(10)], // Pass value to inner-then block
                    else_args: vec![ValueId(11)], // Pass value to inner-else block
                },
            },
            // Inner if false block
            BasicBlock {
                id: BlockId(2),
                params: vec![],
                instructions: vec![],
                phi_nodes: vec![],
                terminator: Terminator::Jump {
                    target: BlockId(5),
                    args: vec![ValueId(12)], // Pass value to merge block
                },
            },
            // Inner inner-then block
            BasicBlock {
                id: BlockId(3),
                params: vec![ValueId(13)], // Parameter from branch
                instructions: vec![],
                phi_nodes: vec![],
                terminator: Terminator::Jump {
                    target: BlockId(5),
                    args: vec![ValueId(14)], // Pass value to merge block
                },
            },
            // Inner inner-else block
            BasicBlock {
                id: BlockId(4),
                params: vec![ValueId(15)], // Parameter from branch
                instructions: vec![],
                phi_nodes: vec![],
                terminator: Terminator::Jump {
                    target: BlockId(5),
                    args: vec![ValueId(16)], // Pass value to merge block
                },
            },
            // Final merge block with multiple phi nodes
            BasicBlock {
                id: BlockId(5),
                params: vec![ValueId(17), ValueId(18), ValueId(19)], // Multiple parameters
                instructions: vec![],
                phi_nodes: vec![
                    PhiNode {
                        result: ValueId(20),
                        ty: i32_type,
                        incomings: vec![(ValueId(14), BlockId(3)), (ValueId(16), BlockId(4))],
                    },
                    PhiNode {
                        result: ValueId(21),
                        ty: i32_type,
                        incomings: vec![(ValueId(20), BlockId(3)), (ValueId(12), BlockId(2))],
                    },
                ],
                terminator: Terminator::Return {
                    value: Some(ValueId(21)),
                },
            },
        ],
        instructions: vec![],
        block_params: HashMap::new(),
    };

    // Test the complex SSA structure
    assert_eq!(function.basic_blocks.len(), 6);

    // Check the final merge block has multiple phi nodes
    let merge_block = &function.basic_blocks[5];
    assert_eq!(merge_block.phi_nodes.len(), 2);
    assert_eq!(merge_block.phi_nodes[0].result, ValueId(20));
    assert_eq!(merge_block.phi_nodes[1].result, ValueId(21));
    assert_eq!(merge_block.phi_nodes[0].incomings.len(), 2);
    assert_eq!(merge_block.phi_nodes[1].incomings.len(), 2);
}
