use crate::ast::*;
use crate::error::{ValidationErrorKind, VialError};
use crate::validation::UntypedValidator;

use std::fs;
use std::ops::Range;
use std::path::PathBuf;

fn create_test_validator() -> UntypedValidator {
    UntypedValidator::new(PathBuf::from("."))
}

fn dummy_span() -> Range<usize> {
    0..1
}

fn dummy_file() -> String {
    "test.vi".to_string()
}

fn cleanup_test_env(test_dir: &PathBuf) {
    let _ = fs::remove_dir_all(test_dir);
}

#[test]
fn test_duplicate_function_detection() {
    let mut validator = create_test_validator();

    let nodes = vec![
        ASTNode {
            span: dummy_span(),
            file: dummy_file(),
            node: ASTNodeKind::Function(Box::new(Function {
                span: 0..10,
                file: dummy_file(),
                vis: Visibility::Public,
                name: "foo".to_string(),
                type_params: vec![],
                args: vec![],
                return_type: None,
                where_constraints: vec![],
                effects: EffectAnnot::pure(),
                body: None,
            })),
            attributes: vec![],
        },
        ASTNode {
            span: dummy_span(),
            file: dummy_file(),
            node: ASTNodeKind::Function(Box::new(Function {
                span: 20..30,
                file: dummy_file(),
                vis: Visibility::Public,
                name: "foo".to_string(), // Duplicate
                type_params: vec![],
                args: vec![],
                return_type: None,
                where_constraints: vec![],
                effects: EffectAnnot::pure(),
                body: None,
            })),
            attributes: vec![],
        },
    ];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));

    assert!(validator.diagnostics.has_errors());
    assert_eq!(validator.diagnostics.errors.len(), 1);

    match &validator.diagnostics.errors[0] {
        VialError::ValidationError {
            kind: ValidationErrorKind::DuplicateDefinition { name, .. },
            ..
        } => {
            assert_eq!(name, "foo");
        }
        _ => panic!("Expected DuplicateDefinition error"),
    }
}

#[test]
fn test_duplicate_struct_fields() {
    let mut validator = create_test_validator();

    let nodes = vec![ASTNode {
        span: dummy_span(),
        file: dummy_file(),
        node: ASTNodeKind::Struct(Struct {
            span: dummy_span(),
            file: dummy_file(),
            name: "Point".to_string(),
            type_params: vec![],
            fields: vec![
                (
                    FnArg {
                        span: 0..5,
                        file: dummy_file(),
                        name: "x".to_string(),
                        type_: None,
                    },
                    Visibility::Public,
                ),
                (
                    FnArg {
                        span: 6..11,
                        file: dummy_file(),
                        name: "x".to_string(), // Duplicate
                        type_: None,
                    },
                    Visibility::Public,
                ),
            ],
            methods: vec![],
            vis: Visibility::Public,
        }),
        attributes: vec![],
    }];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));

    assert!(validator.diagnostics.has_errors());
    match &validator.diagnostics.errors[0] {
        VialError::ValidationError {
            kind: ValidationErrorKind::DuplicateField { field },
            ..
        } => {
            assert_eq!(field, "x");
        }
        _ => panic!("Expected DuplicateField error"),
    }
}

// Control Flow Tests

#[test]
fn test_break_outside_loop() {
    let mut validator = create_test_validator();

    let nodes = vec![ASTNode {
        span: dummy_span(),
        file: dummy_file(),
        node: ASTNodeKind::Expr(Expr {
            span: dummy_span(),
            file: dummy_file(),
            expr: ExprKind::Break(None),
        }),
        attributes: vec![],
    }];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));

    assert!(validator.diagnostics.has_errors());
    match &validator.diagnostics.errors[0] {
        VialError::ValidationError {
            kind: ValidationErrorKind::BreakOutsideLoop,
            ..
        } => {}
        _ => panic!("Expected BreakOutsideLoop error"),
    }
}

#[test]
fn test_continue_outside_loop() {
    let mut validator = create_test_validator();

    let nodes = vec![ASTNode {
        span: dummy_span(),
        file: dummy_file(),
        node: ASTNodeKind::Expr(Expr {
            span: dummy_span(),
            file: dummy_file(),
            expr: ExprKind::Continue,
        }),
        attributes: vec![],
    }];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));

    assert!(validator.diagnostics.has_errors());
    match &validator.diagnostics.errors[0] {
        VialError::ValidationError {
            kind: ValidationErrorKind::ContinueOutsideLoop,
            ..
        } => {}
        _ => panic!("Expected ContinueOutsideLoop error"),
    }
}

#[test]
fn test_return_outside_function() {
    let mut validator = create_test_validator();

    let nodes = vec![ASTNode {
        span: dummy_span(),
        file: dummy_file(),
        node: ASTNodeKind::Expr(Expr {
            span: dummy_span(),
            file: dummy_file(),
            expr: ExprKind::Return(None),
        }),
        attributes: vec![],
    }];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));

    assert!(
        !validator.diagnostics.has_errors(),
        "top level return should not error"
    );
}

#[test]
fn test_break_inside_loop_is_valid() {
    let mut validator = create_test_validator();

    let nodes = vec![ASTNode {
        span: dummy_span(),
        file: dummy_file(),
        node: ASTNodeKind::Expr(Expr {
            span: dummy_span(),
            file: dummy_file(),
            expr: ExprKind::Loop {
                label: None,
                body: Box::new(Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::Break(None),
                }),
            },
        }),
        attributes: vec![],
    }];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));

    assert!(!validator.diagnostics.has_errors());
}

// Desugaring Tests

#[test]
fn test_struct_methods_extracted_to_impl() {
    let mut validator = create_test_validator();

    let nodes = vec![ASTNode {
        span: dummy_span(),
        file: dummy_file(),
        node: ASTNodeKind::Struct(Struct {
            span: dummy_span(),
            file: dummy_file(),
            name: "Point".to_string(),
            type_params: vec![],
            fields: vec![],
            methods: vec![Function {
                span: dummy_span(),
                file: dummy_file(),
                vis: Visibility::Public,
                name: "new".to_string(),
                type_params: vec![],
                args: vec![],
                return_type: None,
                where_constraints: vec![],
                effects: EffectAnnot::pure(),
                body: None,
            }],
            vis: Visibility::Public,
        }),
        attributes: vec![],
    }];

    let result = validator.validate(nodes, &PathBuf::from("test.vi"));

    assert!(!validator.diagnostics.has_errors());

    // Should have struct + impl block
    assert_eq!(result.len(), 2);

    // First should be clean struct with no methods
    match &result[0].node {
        ASTNodeKind::Struct(s) => {
            assert_eq!(s.name, "Point");
            assert!(
                s.methods.is_empty(),
                "Struct should have no methods after desugaring"
            );
        }
        _ => panic!("Expected Struct"),
    }

    // Second should be impl block
    match &result[1].node {
        ASTNodeKind::Impl(impl_block) => {
            assert_eq!(impl_block.type_name, "Point");
            assert_eq!(impl_block.methods.len(), 1);
            assert_eq!(impl_block.methods[0].name, "new");
        }
        _ => panic!("Expected Impl"),
    }
}

#[test]
fn test_enum_methods_extracted_to_impl() {
    let mut validator = create_test_validator();

    let nodes = vec![ASTNode {
        span: dummy_span(),
        file: dummy_file(),
        node: ASTNodeKind::Enum(Enum {
            span: dummy_span(),
            file: dummy_file(),
            name: "Option".to_string(),
            type_params: vec![],
            variants: vec![],
            methods: vec![Function {
                span: dummy_span(),
                file: dummy_file(),
                vis: Visibility::Public,
                name: "is_some".to_string(),
                type_params: vec![],
                args: vec![],
                return_type: None,
                where_constraints: vec![],
                effects: EffectAnnot::pure(),
                body: None,
            }],
            vis: Visibility::Public,
        }),
        attributes: vec![],
    }];

    let result = validator.validate(nodes, &PathBuf::from("test.vi"));

    assert!(!validator.diagnostics.has_errors());
    assert_eq!(result.len(), 2);

    match &result[0].node {
        ASTNodeKind::Enum(e) => {
            assert!(e.methods.is_empty());
        }
        _ => panic!("Expected Enum"),
    }

    match &result[1].node {
        ASTNodeKind::Impl(impl_block) => {
            assert_eq!(impl_block.type_name, "Option");
            assert_eq!(impl_block.methods.len(), 1);
        }
        _ => panic!("Expected Impl"),
    }
}

#[test]
fn test_duplicate_methods_in_struct() {
    let mut validator = create_test_validator();

    let nodes = vec![ASTNode {
        span: dummy_span(),
        file: dummy_file(),
        node: ASTNodeKind::Struct(Struct {
            span: dummy_span(),
            file: dummy_file(),
            name: "Point".to_string(),
            type_params: vec![],
            fields: vec![],
            methods: vec![
                Function {
                    span: 0..10,
                    file: dummy_file(),
                    vis: Visibility::Public,
                    name: "foo".to_string(),
                    type_params: vec![],
                    args: vec![],
                    return_type: None,
                    where_constraints: vec![],
                    effects: EffectAnnot::pure(),
                    body: None,
                },
                Function {
                    span: 20..30,
                    file: dummy_file(),
                    vis: Visibility::Public,
                    name: "foo".to_string(), // Duplicate
                    type_params: vec![],
                    args: vec![],
                    return_type: None,
                    where_constraints: vec![],
                    effects: EffectAnnot::pure(),
                    body: None,
                },
            ],
            vis: Visibility::Public,
        }),
        attributes: vec![],
    }];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));

    assert!(validator.diagnostics.has_errors());
    match &validator.diagnostics.errors[0] {
        VialError::ValidationError {
            kind: ValidationErrorKind::DuplicateDefinition { name, .. },
            ..
        } => {
            assert_eq!(name, "foo");
        }
        _ => panic!("Expected DuplicateDefinition error"),
    }
}

// Complex Nested Control Flow

#[test]
fn test_nested_loops_with_valid_break_continue() {
    let mut validator = UntypedValidator::new(PathBuf::from("."));

    // while { for { if { break } } }
    let expr = Expr {
        span: 0..100,
        file: "test.vi".to_string(),
        expr: ExprKind::While(
            Box::new(Expr {
                span: 6..10,
                file: "test.vi".to_string(),
                expr: ExprKind::Bool(true),
            }),
            Box::new(Expr {
                span: 12..98,
                file: "test.vi".to_string(),
                expr: ExprKind::For {
                    iterator: Box::new(Expr {
                        span: 16..20,
                        file: "test.vi".to_string(),
                        expr: ExprKind::Array(vec![]),
                    }),
                    value: "x".to_string(),
                    expression: Box::new(Expr {
                        span: 24..95,
                        file: "test.vi".to_string(),
                        expr: ExprKind::IfElse {
                            condition: Box::new(Expr {
                                span: 27..31,
                                file: "test.vi".to_string(),
                                expr: ExprKind::Bool(true),
                            }),
                            then: Box::new(Expr {
                                span: 35..40,
                                file: "test.vi".to_string(),
                                expr: ExprKind::Break(None), // Valid - inside for loop
                            }),
                            else_: Some(Box::new(Expr {
                                span: 45..53,
                                file: "test.vi".to_string(),
                                expr: ExprKind::Continue, // Valid - inside for loop
                            })),
                        },
                    }),
                },
            }),
        ),
    };

    let nodes = vec![ASTNode {
        span: 0..100,
        file: "test.vi".to_string(),
        node: ASTNodeKind::Expr(expr),
        attributes: vec![],
    }];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));
    assert!(
        !validator.diagnostics.has_errors(),
        "Nested loops with break/continue should be valid"
    );
}

// Complex Desugaring Scenarios
#[test]
fn test_struct_with_multiple_impls_and_methods() {
    let mut validator = UntypedValidator::new(PathBuf::from("."));

    // Struct with 10 methods
    let methods: Vec<Function> = (0..10)
        .map(|i| Function {
            span: i * 10..(i + 1) * 10,
            file: "test.vi".to_string(),
            vis: Visibility::Public,
            name: format!("method_{}", i),
            type_params: vec![],
            args: vec![],
            return_type: None,
            where_constraints: vec![],
            effects: EffectAnnot::pure(),
            body: None,
        })
        .collect();

    let struct_def = Struct {
        span: 0..200,
        file: "test.vi".to_string(),
        name: "Complex".to_string(),
        type_params: vec![],
        fields: vec![],
        methods,
        vis: Visibility::Public,
    };

    let nodes = vec![ASTNode {
        span: 0..200,
        file: "test.vi".to_string(),
        node: ASTNodeKind::Struct(struct_def),
        attributes: vec![],
    }];

    let result = validator.validate(nodes, &PathBuf::from("test.vi"));

    assert!(!validator.diagnostics.has_errors());
    assert_eq!(result.len(), 2); // Struct + Impl

    match &result[1].node {
        ASTNodeKind::Impl(impl_block) => {
            assert_eq!(impl_block.methods.len(), 10);
        }
        _ => panic!("Expected Impl block"),
    }
}

#[test]
fn test_generic_struct_with_complex_bounds() {
    let mut validator = UntypedValidator::new(PathBuf::from("."));

    // struct Container<T: Clone + Debug, U: Default>
    let struct_def = Struct {
        span: 0..100,
        file: "test.vi".to_string(),
        name: "Container".to_string(),
        type_params: vec![
            TypeParam {
                name: "T".to_string(),
                kind: None,
                bounds: vec![
                    TypeAnnot {
                        span: 15..20,
                        file: "test.vi".to_string(),
                        type_: TypeAnnotKind::Named("Clone".to_string()),
                    },
                    TypeAnnot {
                        span: 23..28,
                        file: "test.vi".to_string(),
                        type_: TypeAnnotKind::Named("Debug".to_string()),
                    },
                ],
            },
            TypeParam {
                name: "U".to_string(),
                kind: None,
                bounds: vec![TypeAnnot {
                    span: 33..40,
                    file: "test.vi".to_string(),
                    type_: TypeAnnotKind::Named("Default".to_string()),
                }],
            },
        ],
        fields: vec![],
        methods: vec![],
        vis: Visibility::Public,
    };

    let nodes = vec![ASTNode {
        span: 0..100,
        file: "test.vi".to_string(),
        node: ASTNodeKind::Struct(struct_def),
        attributes: vec![],
    }];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));
    assert!(!validator.diagnostics.has_errors());
}

#[test]
fn test_conflicting_method_names_across_multiple_structs() {
    let mut validator = UntypedValidator::new(PathBuf::from("."));

    // Two different structs with same method name - should be OK
    let struct1 = Struct {
        span: 0..50,
        file: "test.vi".to_string(),
        name: "Foo".to_string(),
        type_params: vec![],
        fields: vec![],
        methods: vec![Function {
            span: 15..35,
            file: "test.vi".to_string(),
            vis: Visibility::Public,
            name: "process".to_string(),
            type_params: vec![],
            args: vec![],
            return_type: None,
            where_constraints: vec![],
            effects: EffectAnnot::pure(),
            body: None,
        }],
        vis: Visibility::Public,
    };

    let struct2 = Struct {
        span: 60..110,
        file: "test.vi".to_string(),
        name: "Bar".to_string(),
        type_params: vec![],
        fields: vec![],
        methods: vec![Function {
            span: 75..95,
            file: "test.vi".to_string(),
            vis: Visibility::Public,
            name: "process".to_string(), // Same name, different struct - OK
            type_params: vec![],
            args: vec![],
            return_type: None,
            where_constraints: vec![],
            effects: EffectAnnot::pure(),
            body: None,
        }],
        vis: Visibility::Public,
    };

    let nodes = vec![
        ASTNode {
            span: 0..50,
            file: "test.vi".to_string(),
            node: ASTNodeKind::Struct(struct1),
            attributes: vec![],
        },
        ASTNode {
            span: 60..110,
            file: "test.vi".to_string(),
            node: ASTNodeKind::Struct(struct2),
            attributes: vec![],
        },
    ];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));
    assert!(
        !validator.diagnostics.has_errors(),
        "Same method name in different structs should be valid"
    );
}

// Complex Duplicate Detection

#[test]
fn test_duplicate_across_different_scopes() {
    let mut validator = UntypedValidator::new(PathBuf::from("."));

    // Top-level function and enum with same name - ERROR
    let func = Function {
        span: 0..20,
        file: "test.vi".to_string(),
        vis: Visibility::Public,
        name: "Result".to_string(),
        type_params: vec![],
        args: vec![],
        return_type: None,
        where_constraints: vec![],
        effects: EffectAnnot::pure(),
        body: None,
    };

    let enum_def = Enum {
        span: 30..60,
        file: "test.vi".to_string(),
        name: "Result".to_string(), // Duplicate
        type_params: vec![],
        variants: vec![],
        methods: vec![],
        vis: Visibility::Public,
    };

    let nodes = vec![
        ASTNode {
            span: 0..20,
            file: "test.vi".to_string(),
            node: ASTNodeKind::Function(Box::new(func)),
            attributes: vec![],
        },
        ASTNode {
            span: 30..60,
            file: "test.vi".to_string(),
            node: ASTNodeKind::Enum(enum_def),
            attributes: vec![],
        },
    ];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));
    assert!(validator.diagnostics.has_errors());
}

#[test]
fn test_shadowing_in_nested_scopes_allowed() {
    let mut validator = UntypedValidator::new(PathBuf::from("."));

    // let x = 5; { let x = 10; } - should be OK (shadowing)
    let inner = Expr {
        span: 12..22,
        file: "test.vi".to_string(),
        expr: ExprKind::Let {
            var: "x".to_string(),
            type_annot: None,
            value: Box::new(Expr {
                span: 20..22,
                file: "test.vi".to_string(),
                expr: ExprKind::Int(10),
            }),
        },
    };

    let outer = Expr {
        span: 0..25,
        file: "test.vi".to_string(),
        expr: ExprKind::Block(vec![
            Expr {
                span: 0..10,
                file: "test.vi".to_string(),
                expr: ExprKind::Let {
                    var: "x".to_string(),
                    type_annot: None,
                    value: Box::new(Expr {
                        span: 8..9,
                        file: "test.vi".to_string(),
                        expr: ExprKind::Int(5),
                    }),
                },
            },
            Expr {
                span: 11..24,
                file: "test.vi".to_string(),
                expr: ExprKind::Block(vec![inner]),
            },
        ]),
    };

    let nodes = vec![ASTNode {
        span: 0..25,
        file: "test.vi".to_string(),
        node: ASTNodeKind::Expr(outer),
        attributes: vec![],
    }];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));
    assert!(
        !validator.diagnostics.has_errors(),
        "Variable shadowing in nested blocks should be allowed"
    );
}

// Import System Edge Cases

#[test]
fn test_circular_import_detection_three_modules() {
    // Create test files
    let test_dir = PathBuf::from("test_circular");
    let _ = fs::create_dir_all(test_dir.join("src"));

    // a.vi imports b.vi
    // b.vi imports c.vi
    // c.vi imports a.vi -> CIRCULAR!

    // This test requires actual file I/O to work properly
    // Skip for now, but structure is here

    cleanup_test_env(&test_dir);
}

#[test]
fn test_import_nonexistent_module() {
    let mut validator = UntypedValidator::new(PathBuf::from("."));

    let import = Import {
        span: 0..30,
        file: "test.vi".to_string(),
        path: vec!["nonexistent".to_string(), "module".to_string()],
        items: vec![],
        alias: None,
    };

    let nodes = vec![ASTNode {
        span: 0..30,
        file: "test.vi".to_string(),
        node: ASTNodeKind::Expr(Expr {
            span: 0..30,
            file: "test.vi".to_string(),
            expr: ExprKind::Import(import),
        }),
        attributes: vec![],
    }];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));
    assert!(validator.diagnostics.has_errors());

    match &validator.diagnostics.errors[0] {
        VialError::ValidationError {
            kind: ValidationErrorKind::ImportNotFound { path },
            ..
        } => {
            assert!(path.contains("nonexistent"));
        }
        _ => panic!("Expected ImportNotFound error"),
    }
}

#[test]
fn test_wildcard_import_with_specific_items_conflict() {
    let mut validator = UntypedValidator::new(PathBuf::from("."));

    // import "module" { * }  -- should import everything
    let import = Import {
        span: 0..20,
        file: "test.vi".to_string(),
        path: vec!["std".to_string(), "io".to_string()],
        items: vec![], // Empty = wildcard
        alias: None,
    };

    let nodes = vec![ASTNode {
        span: 0..20,
        file: "test.vi".to_string(),
        node: ASTNodeKind::Expr(Expr {
            span: 0..20,
            file: "test.vi".to_string(),
            expr: ExprKind::Import(import),
        }),
        attributes: vec![],
    }];

    let _result = validator.validate(nodes, &PathBuf::from("test.vi"));
    // Should not error - std lib imports are handled specially
}
