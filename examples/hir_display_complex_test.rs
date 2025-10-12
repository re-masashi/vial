fn main() {
    use std::collections::HashMap;
    use vial::hir::display::HirDisplay;
    use vial::hir::{
        BasicBlock, BlockId, Function, FunctionSignature, Instruction, Opcode, Param, Terminator,
        Type, TypeContext, ValueId,
    };

    // Create a type context with some basic types
    let mut type_context = TypeContext::new();
    let i32_type = type_context.intern(Type::Int(vial::hir::IntType::I32));

    // Create a slightly more complex function
    let function = Function {
        name: "add_func".to_string(),
        signature: FunctionSignature {
            params: vec![
                Param {
                    name: Some("a".to_string()),
                    ty: i32_type,
                    allocation: Default::default(),
                },
                Param {
                    name: Some("b".to_string()),
                    ty: i32_type,
                    allocation: Default::default(),
                },
            ],
            return_type: i32_type,
            is_variadic: false,
        },
        basic_blocks: vec![BasicBlock {
            id: BlockId(0),
            params: vec![],
            instructions: vec![0], // Reference to first instruction
            phi_nodes: vec![],
            terminator: Terminator::Return {
                value: Some(ValueId(0)),
            }, // Return the result of the add
        }],
        instructions: vec![Instruction {
            opcode: Opcode::Add,
            args: vec![ValueId(1), ValueId(2)], // Would reference the parameters in a real scenario
            ty: i32_type,
            is_pure: true,
            allocation_info: None,
        }],
        block_params: HashMap::new(),
    };

    // Create an HIR display
    let hir_display = HirDisplay {
        function: &function,
        type_context: Some(&type_context),
    };

    println!("HIR Display Output for a more complex function:");
    println!("{}", hir_display);
}
