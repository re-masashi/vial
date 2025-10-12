fn main() {
    use std::collections::HashMap;
    use vial::hir::display::HirDisplay;
    use vial::hir::{
        BasicBlock, BlockId, Function, FunctionSignature, Terminator, Type, TypeContext,
    };

    // Create a simple type context with some basic types
    let mut type_context = TypeContext::new();
    let void_type = type_context.intern(Type::Void);

    // Create a simple function for testing
    let function = Function {
        name: "test_func".to_string(),
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

    // Create an HIR display
    let hir_display = HirDisplay {
        function: &function,
        type_context: Some(&type_context),
    };

    println!("HIR Display Output:");
    println!("{}", hir_display);
}
