// Test file to demonstrate the disassembler functionality
use vial::vm::{FunctionMetadata, disassembler::pretty_dump_bytecode};

fn main() {
    // Example bytecode for: R0 = R1 + R2; return
    let bytecode = vec![
        0x00, 0x00, 0x01, 0x02,  // IntAdd R0, R1, R2
        0xA4,                     // Return
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
    
    let disassembly = pretty_dump_bytecode(&bytecode, &functions, None, None, None);
    println!("{}", disassembly);
}