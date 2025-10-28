#[cfg(test)]
mod integration_tests {
    use vial::bytecode_validator::BytecodeValidator;
    use vial::vm::FunctionMetadata;

    #[test]
    fn test_validator_catches_bad_bytecode() {
        // Test that the validator catches truncated instructions
        let mut validator = BytecodeValidator::new();
        
        // Truncated IntAdd instruction (missing one operand byte)
        let bytecode = vec![0x00, 0x00, 0x00]; // Should be 4 bytes: [0x00, 0x00, 0x00, 0x00]
        let functions = vec![FunctionMetadata {
            bytecode_offset: 0,
            bytecode_length: 3,
            arg_count: 0,
            register_count: 0,
            local_stack_size: 0,
            stack_map_offset: 0,
            stack_map_count: 0,
            max_call_depth: 0,
            _padding: 0,
        }];
        
        assert!(!validator.validate_bytecode(&bytecode, &functions));
        assert!(!validator.errors.is_empty());
    }

    #[test]
    fn test_validator_accepts_valid_bytecode() {
        // Test that the validator accepts valid bytecode
        let mut validator = BytecodeValidator::new();
        
        // Valid MoveZero + Return sequence
        let bytecode = vec![0xC5, 0x00, 0xA4]; // MoveZero R0; Return
        let functions = vec![FunctionMetadata {
            bytecode_offset: 0,
            bytecode_length: 3,
            arg_count: 0,
            register_count: 0,
            local_stack_size: 0,
            stack_map_offset: 0,
            stack_map_count: 0,
            max_call_depth: 0,
            _padding: 0,
        }];
        
        assert!(validator.validate_bytecode(&bytecode, &functions));
        assert!(validator.errors.is_empty());
    }

    #[test]
    fn test_validator_catches_invalid_opcodes() {
        // Test that the validator catches invalid opcodes
        let mut validator = BytecodeValidator::new();
        
        // Invalid opcode 0xFE
        let bytecode = vec![0xFE, 0x00]; 
        let functions = vec![FunctionMetadata {
            bytecode_offset: 0,
            bytecode_length: 2,
            arg_count: 0,
            register_count: 0,
            local_stack_size: 0,
            stack_map_offset: 0,
            stack_map_count: 0,
            max_call_depth: 0,
            _padding: 0,
        }];
        
        assert!(!validator.validate_bytecode(&bytecode, &functions));
        assert!(!validator.errors.is_empty());
    }

    #[test]
    fn test_validator_catches_extended_opcode_issues() {
        // Test that the validator catches incomplete extended opcodes
        let mut validator = BytecodeValidator::new();
        
        // Extended opcode prefix (0xFF) without the extended opcode byte
        let bytecode = vec![0xFF]; 
        let functions = vec![FunctionMetadata {
            bytecode_offset: 0,
            bytecode_length: 1,
            arg_count: 0,
            register_count: 0,
            local_stack_size: 0,
            stack_map_offset: 0,
            stack_map_count: 0,
            max_call_depth: 0,
            _padding: 0,
        }];
        
        assert!(!validator.validate_bytecode(&bytecode, &functions));
        assert!(!validator.errors.is_empty());
    }
}