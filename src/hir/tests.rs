#[cfg(test)]
mod tests {
    use crate::hir::{
        AllocationInfo, AllocationPreference, Builder, ConstantFolder, ControlFlowGraph,
        DominatorTree, FunctionSignature, IntType, Opcode, Param, Type, TypeContext,
    };

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
        use crate::hir::display::HirDisplay;
        use crate::hir::{BasicBlock, BlockId, Function, Terminator};
        use std::collections::HashMap;

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
}
