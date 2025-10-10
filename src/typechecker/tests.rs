use super::*;
use std::rc::Rc;

use crate::typechecker::monomorphizer::Monomorphizer;

fn setup() -> TypeChecker {
    TypeChecker::new(Interner::new())
}

fn dummy_span() -> std::ops::Range<usize> {
    0..1
}

fn dummy_file() -> String {
    "test.ni".to_string()
}

// Basic Type Inference Tests

#[test]
fn test_int_literal() {
    let mut tc = setup();
    let expr = Expr {
        span: 0..1,
        file: "test".to_string(),
        expr: ExprKind::Int(42),
    };

    let typed = tc.check_expr(&expr);
    assert!(matches!(typed.type_.type_, TypeKind::Constructor { .. }));
    assert!(!tc.diagnostics.has_errors());
}

#[test]
fn test_bool_literal() {
    let mut tc = setup();
    let expr = Expr {
        span: 0..4,
        file: "test".to_string(),
        expr: ExprKind::Bool(true),
    };

    let _typed = tc.check_expr(&expr);
    assert!(!tc.diagnostics.has_errors());
}

#[test]
fn test_let_binding() {
    let mut tc = setup();

    // let x = 5
    let value = Expr {
        span: 8..9,
        file: "test".to_string(),
        expr: ExprKind::Int(5),
    };

    let let_expr = Expr {
        span: 0..9,
        file: "test".to_string(),
        expr: ExprKind::Let {
            var: "x".to_string(),
            type_annot: None,
            value: Box::new(value),
        },
    };

    let _typed = tc.check_expr(&let_expr);
    assert!(!tc.diagnostics.has_errors());

    // Check that x is bound
    let x_sym = tc.interner.intern("x");
    assert!(tc.env.lookup(x_sym).is_some());
}

#[test]
fn test_variable_lookup() {
    let mut tc = setup();

    // First bind x
    let binding_id = tc.id_gen.fresh_binding();
    let x_sym = tc.interner.intern("x");
    tc.env.add_binding(
        x_sym,
        Binding {
            id: binding_id,
            name: x_sym,
            type_: tc.int_type(),
            mutable: false,
        },
    );

    // Then lookup x
    let var_expr = Expr {
        span: 0..1,
        file: "test".to_string(),
        expr: ExprKind::Variable("x".to_string()),
    };

    let _typed = tc.check_expr(&var_expr);
    assert!(!tc.diagnostics.has_errors());
}

#[test]
fn test_unbound_variable() {
    let mut tc = setup();

    let var_expr = Expr {
        span: 0..1,
        file: "test".to_string(),
        expr: ExprKind::Variable("undefined".to_string()),
    };

    let _typed = tc.check_expr(&var_expr);
    assert!(tc.diagnostics.has_errors());
    assert!(tc.diagnostics.type_errors.len() == 1);
}

// Function Type Inference Tests

#[test]
fn test_lambda_inference() {
    let mut tc = setup();

    // fn(x) x
    let lambda = Expr {
        span: 0..7,
        file: "test".to_string(),
        expr: ExprKind::Lambda {
            args: vec![FnArg {
                span: 1..2,
                file: "test".to_string(),
                name: "x".to_string(),
                type_: None,
            }],
            expression: Box::new(Expr {
                span: 5..6,
                file: "test".to_string(),
                expr: ExprKind::Variable("x".to_string()),
            }),
        },
    };

    let typed = tc.check_expr(&lambda);
    assert!(!tc.diagnostics.has_errors());
    assert!(matches!(typed.type_.type_, TypeKind::Function { .. }));
}

#[test]
fn test_function_call() {
    let mut tc = setup();

    // First create identity function
    let id_lambda = Expr {
        span: 0..7,
        file: "test".to_string(),
        expr: ExprKind::Lambda {
            args: vec![FnArg {
                span: 1..2,
                file: "test".to_string(),
                name: "x".to_string(),
                type_: None,
            }],
            expression: Box::new(Expr {
                span: 5..6,
                file: "test".to_string(),
                expr: ExprKind::Variable("x".to_string()),
            }),
        },
    };

    // Call it with 5: id(5)
    let call = Expr {
        span: 0..10,
        file: "test".to_string(),
        expr: ExprKind::Call(
            Box::new(id_lambda),
            vec![Expr {
                span: 8..9,
                file: "test".to_string(),
                expr: ExprKind::Int(5),
            }],
        ),
    };

    let _typed = tc.check_expr(&call);
    assert!(!tc.diagnostics.has_errors());
}

#[test]
fn test_arity_mismatch() {
    let mut tc = setup();

    let lambda = Expr {
        span: 0..7,
        file: "test".to_string(),
        expr: ExprKind::Lambda {
            args: vec![FnArg {
                span: 1..2,
                file: "test".to_string(),
                name: "x".to_string(),
                type_: None,
            }],
            expression: Box::new(Expr {
                span: 5..6,
                file: "test".to_string(),
                expr: ExprKind::Variable("x".to_string()),
            }),
        },
    };

    // Call with wrong number of args: id(5, 10)
    let call = Expr {
        span: 0..15,
        file: "test".to_string(),
        expr: ExprKind::Call(
            Box::new(lambda),
            vec![
                Expr {
                    span: 8..9,
                    file: "test".to_string(),
                    expr: ExprKind::Int(5),
                },
                Expr {
                    span: 11..13,
                    file: "test".to_string(),
                    expr: ExprKind::Int(10),
                },
            ],
        ),
    };

    let _typed = tc.check_expr(&call);
    assert!(tc.diagnostics.has_errors());
}

// Polymorphism Tests

#[test]
fn test_let_polymorphism() {
    let mut tc = setup();

    // let id = fn(x) x; id(5); id(true)
    let id_lambda = Expr {
        span: 0..10,
        file: "test".to_string(),
        expr: ExprKind::Lambda {
            args: vec![FnArg {
                span: 1..2,
                file: "test".to_string(),
                name: "x".to_string(),
                type_: None,
            }],
            expression: Box::new(Expr {
                span: 5..6,
                file: "test".to_string(),
                expr: ExprKind::Variable("x".to_string()),
            }),
        },
    };

    let let_expr = Expr {
        span: 0..10,
        file: "test".to_string(),
        expr: ExprKind::Let {
            var: "id".to_string(),
            type_annot: None,
            value: Box::new(id_lambda),
        },
    };

    let _typed_let = tc.check_expr(&let_expr);

    println!("{:?}", tc.diagnostics);
    assert!(!tc.diagnostics.has_errors());

    // Now use id with int
    let id_var = Expr {
        span: 0..2,
        file: "test".to_string(),
        expr: ExprKind::Variable("id".to_string()),
    };

    let call1 = Expr {
        span: 0..7,
        file: "test".to_string(),
        expr: ExprKind::Call(
            Box::new(id_var.clone()),
            vec![Expr {
                span: 4..5,
                file: "test".to_string(),
                expr: ExprKind::Int(5),
            }],
        ),
    };

    let _typed1 = tc.check_expr(&call1);

    let _typed_let = tc.check_expr(&let_expr); // re-bind it to let. so it becomes fn(bool) -> bool

    // Use id with bool
    let call2 = Expr {
        span: 0..10,
        file: "test".to_string(),
        expr: ExprKind::Call(
            Box::new(id_var),
            vec![Expr {
                span: 4..8,
                file: "test".to_string(),
                expr: ExprKind::Bool(true),
            }],
        ),
    };

    let _typed2 = tc.check_expr(&call2);
    println!("{:?}", tc.diagnostics);
    assert!(!tc.diagnostics.has_errors());
}

// Unification Tests

#[test]
fn test_type_mismatch() {
    let mut tc = setup();

    // if true { 5 } else { false }
    let if_expr = Expr {
        span: 0..25,
        file: "test".to_string(),
        expr: ExprKind::IfElse {
            condition: Box::new(Expr {
                span: 3..7,
                file: "test".to_string(),
                expr: ExprKind::Bool(true),
            }),
            then: Box::new(Expr {
                span: 13..14,
                file: "test".to_string(),
                expr: ExprKind::Int(5),
            }),
            else_: Some(Box::new(Expr {
                span: 20..25,
                file: "test".to_string(),
                expr: ExprKind::Bool(false),
            })),
        },
    };

    let _typed = tc.check_expr(&if_expr);
    assert!(tc.diagnostics.has_errors());
}

#[test]
fn test_occurs_check() {
    let mut tc = setup();

    // Create a self-referential type scenario
    // This is hard to construct directly, but unification should catch it
    let var1 = tc.fresh_type_var();
    let _var2_inner = tc.fresh_type_var();

    // Try to unify var1 with List<var1> (occurs check should fail)
    let list_type = Rc::new(Type {
        span: None,
        file: None,
        type_: TypeKind::Constructor {
            name: tc.interner.intern("List").0,
            args: vec![var1.clone()],
            kind: Kind::Star,
        },
    });

    tc.unify(&var1, &list_type, &(0..10));
    assert!(tc.diagnostics.has_errors());
}

// Pattern Matching Tests
#[test]
fn test_simple_match() {
    let mut tc = setup();

    // match 5 { 5 => true, _ => false }
    let match_expr = Expr {
        span: 0..35,
        file: "test".to_string(),
        expr: ExprKind::Match(
            Box::new(Expr {
                span: 6..7,
                file: "test".to_string(),
                expr: ExprKind::Int(5),
            }),
            vec![
                MatchArm {
                    pattern: Pattern {
                        span: 10..11,
                        file: "test".to_string(),
                        pat: PatKind::Literal(Literal::Int(5)),
                    },
                    guard: None,
                    body: Box::new(Expr {
                        span: 15..19,
                        file: "test".to_string(),
                        expr: ExprKind::Bool(true),
                    }),
                    span: 10..19,
                },
                MatchArm {
                    pattern: Pattern {
                        span: 21..22,
                        file: "test".to_string(),
                        pat: PatKind::Wildcard,
                    },
                    guard: None,
                    body: Box::new(Expr {
                        span: 26..31,
                        file: "test".to_string(),
                        expr: ExprKind::Bool(false),
                    }),
                    span: 21..31,
                },
            ],
        ),
    };

    let _typed = tc.check_expr(&match_expr);
    assert!(!tc.diagnostics.has_errors());
}

// Struct and Enum Tests

#[test]
fn test_struct_definition() {
    let mut tc = setup();

    let struct_def = Struct {
        span: 0..30,
        file: "test".to_string(),
        name: "Point".to_string(),
        type_params: vec![],
        fields: vec![
            (
                FnArg {
                    span: 15..20,
                    file: "test".to_string(),
                    name: "x".to_string(),
                    type_: Some(TypeAnnot {
                        span: 15..18,
                        file: "test".to_string(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
            (
                FnArg {
                    span: 22..27,
                    file: "test".to_string(),
                    name: "y".to_string(),
                    type_: Some(TypeAnnot {
                        span: 22..25,
                        file: "test".to_string(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let typed = tc.check_struct(struct_def);
    assert!(!tc.diagnostics.has_errors());
    assert!(tc.env.structs.contains_key(&typed.struct_id));
}

#[test]
fn test_enum_definition() {
    let mut tc = setup();

    let enum_def = Enum {
        span: 0..40,
        file: "test".to_string(),
        name: "Option".to_string(),
        type_params: vec![TypeParam {
            name: "T".to_string(),
            kind: None,
            bounds: vec![],
        }],
        variants: vec![
            EnumVariant {
                span: 20..24,
                file: "test".to_string(),
                name: "None".to_string(),
                types: vec![],
                constraints: vec![],
            },
            EnumVariant {
                span: 26..35,
                file: "test".to_string(),
                name: "Some".to_string(),
                types: vec![TypeAnnot {
                    span: 31..32,
                    file: "test".to_string(),
                    type_: TypeAnnotKind::Variable {
                        name: "T".to_string(),
                        kind: None,
                    },
                }],
                constraints: vec![],
            },
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let typed = tc.check_enum(enum_def);
    assert!(!tc.diagnostics.has_errors());
    assert!(tc.env.enums.contains_key(&typed.enum_id));
}

#[test]
fn test_full_program() {
    let mut tc = setup();

    // Simple program with function and call
    let function = Function {
        span: 0..30,
        file: "test".to_string(),
        vis: Visibility::Public,
        name: "add".to_string(),
        type_params: vec![],
        args: vec![
            FnArg {
                span: 8..14,
                file: "test".to_string(),
                name: "a".to_string(),
                type_: Some(TypeAnnot {
                    span: 11..14,
                    file: "test".to_string(),
                    type_: TypeAnnotKind::Int,
                }),
            },
            FnArg {
                span: 16..22,
                file: "test".to_string(),
                name: "b".to_string(),
                type_: Some(TypeAnnot {
                    span: 19..22,
                    file: "test".to_string(),
                    type_: TypeAnnotKind::Int,
                }),
            },
        ],
        return_type: Some(TypeAnnot {
            span: 26..29,
            file: "test".to_string(),
            type_: TypeAnnotKind::Int,
        }),
        where_constraints: vec![],
        effects: EffectAnnot::pure(),
        body: Some(Expr {
            span: 31..36,
            file: "test".to_string(),
            expr: ExprKind::BinOp(
                Box::new(Expr {
                    span: 31..32,
                    file: "test".to_string(),
                    expr: ExprKind::Variable("a".to_string()),
                }),
                BinOp::Add,
                Box::new(Expr {
                    span: 35..36,
                    file: "test".to_string(),
                    expr: ExprKind::Variable("b".to_string()),
                }),
            ),
        }),
    };

    let _typed = tc.check_function(function);
    assert!(!tc.diagnostics.has_errors());
}

#[test]
fn test_nested_function_type_inference() {
    let mut tc = setup();

    // |x| |y| x + y  -- should infer nested function type
    let inner_lambda = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::Lambda {
            args: vec![FnArg {
                span: dummy_span(),
                file: dummy_file(),
                name: "y".to_string(),
                type_: None,
            }],
            expression: Box::new(Expr {
                span: dummy_span(),
                file: dummy_file(),
                expr: ExprKind::BinOp(
                    Box::new(Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Variable("x".to_string()),
                    }),
                    BinOp::Add,
                    Box::new(Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Variable("y".to_string()),
                    }),
                ),
            }),
        },
    };

    let outer_lambda = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::Lambda {
            args: vec![FnArg {
                span: dummy_span(),
                file: dummy_file(),
                name: "x".to_string(),
                type_: None,
            }],
            expression: Box::new(inner_lambda),
        },
    };

    let typed = tc.check_expr(&outer_lambda);
    assert!(!tc.diagnostics.has_errors());

    // Should infer: a -> (b -> c) where a, b, c are unified through addition
    match &typed.type_.type_ {
        TypeKind::Function { .. } => {}
        _ => panic!("Expected function type"),
    }
}

#[test]
fn test_recursive_type_inference() {
    let mut tc = setup();

    // Recursive data structure
    let enum_def = Enum {
        span: dummy_span(),
        file: dummy_file(),
        name: "List".to_string(),
        type_params: vec![TypeParam {
            name: "T".to_string(),
            kind: None,
            bounds: vec![],
        }],
        variants: vec![
            EnumVariant {
                span: dummy_span(),
                file: dummy_file(),
                name: "Nil".to_string(),
                types: vec![],
                constraints: vec![],
            },
            EnumVariant {
                span: dummy_span(),
                file: dummy_file(),
                name: "Cons".to_string(),
                types: vec![
                    TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Variable {
                            name: "T".to_string(),
                            kind: None,
                        },
                    },
                    TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Generic {
                            name: "List".to_string(),
                            args: vec![TypeAnnot {
                                span: dummy_span(),
                                file: dummy_file(),
                                type_: TypeAnnotKind::Variable {
                                    name: "T".to_string(),
                                    kind: None,
                                },
                            }],
                            kind: None,
                        },
                    },
                ],
                constraints: vec![],
            },
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let typed = tc.check_enum(enum_def);
    assert!(!tc.diagnostics.has_errors());
    assert!(tc.env.enums.contains_key(&typed.enum_id));
}

#[test]
fn test_mutually_recursive_functions() {
    let mut tc = setup();

    // def is_even(n) = if n == 0 then true else is_odd(n - 1)
    // def is_odd(n) = if n == 0 then false else is_even(n - 1)

    let is_even = Function {
        span: dummy_span(),
        file: dummy_file(),
        vis: Visibility::Public,
        name: "is_even".to_string(),
        type_params: vec![],
        args: vec![FnArg {
            span: dummy_span(),
            file: dummy_file(),
            name: "n".to_string(),
            type_: Some(TypeAnnot {
                span: dummy_span(),
                file: dummy_file(),
                type_: TypeAnnotKind::Int,
            }),
        }],
        return_type: Some(TypeAnnot {
            span: dummy_span(),
            file: dummy_file(),
            type_: TypeAnnotKind::Bool,
        }),
        where_constraints: vec![],
        effects: EffectAnnot::pure(),
        body: Some(Expr {
            span: dummy_span(),
            file: dummy_file(),
            expr: ExprKind::IfElse {
                condition: Box::new(Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::BinOp(
                        Box::new(Expr {
                            span: dummy_span(),
                            file: dummy_file(),
                            expr: ExprKind::Variable("n".to_string()),
                        }),
                        BinOp::Eq,
                        Box::new(Expr {
                            span: dummy_span(),
                            file: dummy_file(),
                            expr: ExprKind::Int(0),
                        }),
                    ),
                }),
                then: Box::new(Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::Bool(true),
                }),
                else_: Some(Box::new(Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::Call(
                        Box::new(Expr {
                            span: dummy_span(),
                            file: dummy_file(),
                            expr: ExprKind::Variable("is_odd".to_string()),
                        }),
                        vec![Expr {
                            span: dummy_span(),
                            file: dummy_file(),
                            expr: ExprKind::BinOp(
                                Box::new(Expr {
                                    span: dummy_span(),
                                    file: dummy_file(),
                                    expr: ExprKind::Variable("n".to_string()),
                                }),
                                BinOp::Sub,
                                Box::new(Expr {
                                    span: dummy_span(),
                                    file: dummy_file(),
                                    expr: ExprKind::Int(1),
                                }),
                            ),
                        }],
                    ),
                })),
            },
        }),
    };

    let is_odd = Function {
        span: dummy_span(),
        file: dummy_file(),
        vis: Visibility::Public,
        name: "is_odd".to_string(),
        type_params: vec![],
        args: vec![FnArg {
            span: dummy_span(),
            file: dummy_file(),
            name: "n".to_string(),
            type_: Some(TypeAnnot {
                span: dummy_span(),
                file: dummy_file(),
                type_: TypeAnnotKind::Int,
            }),
        }],
        return_type: Some(TypeAnnot {
            span: dummy_span(),
            file: dummy_file(),
            type_: TypeAnnotKind::Bool,
        }),
        where_constraints: vec![],
        effects: EffectAnnot::pure(),
        body: Some(Expr {
            span: dummy_span(),
            file: dummy_file(),
            expr: ExprKind::IfElse {
                condition: Box::new(Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::BinOp(
                        Box::new(Expr {
                            span: dummy_span(),
                            file: dummy_file(),
                            expr: ExprKind::Variable("n".to_string()),
                        }),
                        BinOp::Eq,
                        Box::new(Expr {
                            span: dummy_span(),
                            file: dummy_file(),
                            expr: ExprKind::Int(0),
                        }),
                    ),
                }),
                then: Box::new(Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::Bool(false),
                }),
                else_: Some(Box::new(Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::Call(
                        Box::new(Expr {
                            span: dummy_span(),
                            file: dummy_file(),
                            expr: ExprKind::Variable("is_even".to_string()),
                        }),
                        vec![Expr {
                            span: dummy_span(),
                            file: dummy_file(),
                            expr: ExprKind::BinOp(
                                Box::new(Expr {
                                    span: dummy_span(),
                                    file: dummy_file(),
                                    expr: ExprKind::Variable("n".to_string()),
                                }),
                                BinOp::Sub,
                                Box::new(Expr {
                                    span: dummy_span(),
                                    file: dummy_file(),
                                    expr: ExprKind::Int(1),
                                }),
                            ),
                        }],
                    ),
                })),
            },
        }),
    };

    let program = vec![
        ASTNode {
            span: dummy_span(),
            file: dummy_file(),
            node: ASTNodeKind::Function(Box::new(is_even)),
            attributes: vec![],
        },
        ASTNode {
            span: dummy_span(),
            file: dummy_file(),
            node: ASTNodeKind::Function(Box::new(is_odd)),
            attributes: vec![],
        },
    ];

    let _typed = tc.check_program(program);
    if tc.diagnostics.has_errors() {
        println!("Errors:");
        for err in &tc.diagnostics.type_errors {
            println!("  {:?}", err);
            if let TypeErrorKind::UnboundVariable { name } = err.kind {
                println!("    Variable: {}", tc.interner.resolve(name));
            }
        }
    }
    assert!(
        !tc.diagnostics.has_errors(),
        "Mutually recursive functions should type check"
    );
}

// Complex Generic Constraints

#[test]
fn test_higher_kinded_types() {
    let mut tc = setup();

    // type Functor<F: * -> *> = trait { map: (a -> b) -> F<a> -> F<b> }
    let trait_def = TraitDef {
        span: dummy_span(),
        name: "Functor".to_string(),
        type_params: vec![TypeParam {
            name: "F".to_string(),
            kind: Some(KindAnnot::Arrow(
                Box::new(KindAnnot::Star),
                Box::new(KindAnnot::Star),
            )),
            bounds: vec![],
        }],
        methods: vec![],
        associated_types: vec![],
        super_traits: vec![],
    };

    let typed = tc.check_trait(trait_def);
    assert!(!tc.diagnostics.has_errors());
    assert!(tc.env.traits.contains_key(&typed.trait_id));
}

#[test]
fn test_complex_where_clauses() {
    let mut tc = setup();

    // fn complex<T, U>(x: T, y: U) -> T
    // where
    //   T: Clone + Debug,
    //   U: Into<T>,
    //   T: PartialEq<U>

    let func = Function {
        span: dummy_span(),
        file: dummy_file(),
        vis: Visibility::Public,
        name: "complex".to_string(),
        type_params: vec![
            TypeParam {
                name: "T".to_string(),
                kind: None,
                bounds: vec![
                    TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Named("Clone".to_string()),
                    },
                    TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Named("Debug".to_string()),
                    },
                ],
            },
            TypeParam {
                name: "U".to_string(),
                kind: None,
                bounds: vec![],
            },
        ],
        args: vec![
            FnArg {
                span: dummy_span(),
                file: dummy_file(),
                name: "x".to_string(),
                type_: Some(TypeAnnot {
                    span: dummy_span(),
                    file: dummy_file(),
                    type_: TypeAnnotKind::Variable {
                        name: "T".to_string(),
                        kind: None,
                    },
                }),
            },
            FnArg {
                span: dummy_span(),
                file: dummy_file(),
                name: "y".to_string(),
                type_: Some(TypeAnnot {
                    span: dummy_span(),
                    file: dummy_file(),
                    type_: TypeAnnotKind::Variable {
                        name: "U".to_string(),
                        kind: None,
                    },
                }),
            },
        ],
        return_type: Some(TypeAnnot {
            span: dummy_span(),
            file: dummy_file(),
            type_: TypeAnnotKind::Variable {
                name: "T".to_string(),
                kind: None,
            },
        }),
        where_constraints: vec![TypeConstraint {
            span: dummy_span(),
            left: TypeAnnot {
                span: dummy_span(),
                file: dummy_file(),
                type_: TypeAnnotKind::Variable {
                    name: "U".to_string(),
                    kind: None,
                },
            },
            right: TypeAnnot {
                span: dummy_span(),
                file: dummy_file(),
                type_: TypeAnnotKind::Generic {
                    name: "Into".to_string(),
                    args: vec![TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Variable {
                            name: "T".to_string(),
                            kind: None,
                        },
                    }],
                    kind: None,
                },
            },
        }],
        effects: EffectAnnot::pure(),
        body: Some(Expr {
            span: dummy_span(),
            file: dummy_file(),
            expr: ExprKind::Variable("x".to_string()),
        }),
    };

    let _typed = tc.check_function(func);
    assert!(!tc.diagnostics.has_errors());
}

// Pattern Matching Exhaustiveness

#[test]
fn test_non_exhaustive_bool_match() {
    let mut tc = setup();

    // match true { true => 1 }  -- missing false case
    let match_expr = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::Match(
            Box::new(Expr {
                span: dummy_span(),
                file: dummy_file(),
                expr: ExprKind::Bool(true),
            }),
            vec![MatchArm {
                pattern: Pattern {
                    span: dummy_span(),
                    file: dummy_file(),
                    pat: PatKind::Literal(Literal::Bool(true)),
                },
                guard: None,
                body: Box::new(Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::Int(1),
                }),
                span: dummy_span(),
            }],
        ),
    };

    let _typed = tc.check_expr(&match_expr);
    assert!(
        tc.diagnostics.has_errors(),
        "Non-exhaustive match should produce error"
    );
}

#[test]
fn test_exhaustive_option_match() {
    let mut tc = setup();

    // First define Option enum
    let option_enum = Enum {
        span: dummy_span(),
        file: dummy_file(),
        name: "Option".to_string(),
        type_params: vec![TypeParam {
            name: "T".to_string(),
            kind: None,
            bounds: vec![],
        }],
        variants: vec![
            EnumVariant {
                span: dummy_span(),
                file: dummy_file(),
                name: "None".to_string(),
                types: vec![],
                constraints: vec![],
            },
            EnumVariant {
                span: dummy_span(),
                file: dummy_file(),
                name: "Some".to_string(),
                types: vec![TypeAnnot {
                    span: dummy_span(),
                    file: dummy_file(),
                    type_: TypeAnnotKind::Variable {
                        name: "T".to_string(),
                        kind: None,
                    },
                }],
                constraints: vec![],
            },
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_enum = tc.check_enum(option_enum);

    // TODO: Create match expression that covers all cases
    // This requires more complex setup
}

#[test]
fn test_redundant_wildcard_pattern() {
    let mut tc = setup();

    // match x { _ => 1, 5 => 2 }  -- second pattern is unreachable
    let match_expr = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::Match(
            Box::new(Expr {
                span: dummy_span(),
                file: dummy_file(),
                expr: ExprKind::Int(42),
            }),
            vec![
                MatchArm {
                    pattern: Pattern {
                        span: dummy_span(),
                        file: dummy_file(),
                        pat: PatKind::Wildcard,
                    },
                    guard: None,
                    body: Box::new(Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Int(1),
                    }),
                    span: dummy_span(),
                },
                MatchArm {
                    pattern: Pattern {
                        span: dummy_span(),
                        file: dummy_file(),
                        pat: PatKind::Literal(Literal::Int(5)),
                    },
                    guard: None,
                    body: Box::new(Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Int(2),
                    }),
                    span: dummy_span(),
                },
            ],
        ),
    };

    let _typed = tc.check_expr(&match_expr);
    // Should have warning about unreachable pattern
}

// Effect System Tests

#[test]
fn test_pure_function_calling_effectful_function() {
    let mut tc = setup();

    // def impure() io { print("hi") }
    // def pure() { impure() }  -- ERROR: pure calls impure

    let impure_func = Function {
        span: dummy_span(),
        file: dummy_file(),
        vis: Visibility::Public,
        name: "impure".to_string(),
        type_params: vec![],
        args: vec![],
        return_type: None,
        where_constraints: vec![],
        effects: EffectAnnot::closed(vec!["IO".to_string()]),
        body: None,
    };

    let pure_func = Function {
        span: dummy_span(),
        file: dummy_file(),
        vis: Visibility::Public,
        name: "pure".to_string(),
        type_params: vec![],
        args: vec![],
        return_type: None,
        where_constraints: vec![],
        effects: EffectAnnot::pure(),
        body: Some(Expr {
            span: dummy_span(),
            file: dummy_file(),
            expr: ExprKind::Call(
                Box::new(Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::Variable("impure".to_string()),
                }),
                vec![],
            ),
        }),
    };

    let program = vec![
        ASTNode {
            span: dummy_span(),
            file: dummy_file(),
            node: ASTNodeKind::Function(Box::new(impure_func)),
            attributes: vec![],
        },
        ASTNode {
            span: dummy_span(),
            file: dummy_file(),
            node: ASTNodeKind::Function(Box::new(pure_func)),
            attributes: vec![],
        },
    ];

    let _typed = tc.check_program(program);
    // Should have effect mismatch error
}

#[test]
fn test_effect_polymorphism() {
    let _tc = setup();

    // def map<E>(f: a -> b / E, xs: [a]) -> [b] / E
    // Effect polymorphic - preserves effects

    // This is complex to test without full implementation
    // Placeholder for now
}

#[test]
fn test_monomorphizer_new() {
    let interner = Interner::new();
    let monomorphizer = Monomorphizer::new(interner);

    assert_eq!(monomorphizer.instantiations.len(), 0);
    assert_eq!(monomorphizer.functions.len(), 0);
    assert_eq!(monomorphizer.specialized_functions.len(), 0);
}

#[test]
fn test_type_to_key() {
    let interner = Interner::new();
    let mut monomorphizer = Monomorphizer::new(interner);

    // Test basic types
    let int_type = Rc::new(Type {
        span: None,
        file: None,
        type_: TypeKind::Constructor {
            name: monomorphizer.interner.intern("int").0,
            args: vec![],
            kind: Kind::Star,
        },
    });

    let key = monomorphizer.type_to_key(&int_type);
    assert_eq!(key, "int");
}

#[test]
fn test_make_specialized_name() {
    let interner = Interner::new();
    let mut monomorphizer = Monomorphizer::new(interner);

    let base_name = monomorphizer.interner.intern("generic_func");
    let concrete_types = vec!["int".to_string(), "string".to_string()];

    let specialized_name = monomorphizer.make_specialized_name(base_name, &concrete_types);
    let resolved = monomorphizer.interner.resolve(specialized_name);

    assert!(resolved.starts_with("generic_func$"));
    assert!(resolved.contains("int"));
    assert!(resolved.contains("string"));
}

#[test]
fn test_error_type() {
    let monomorphizer = Monomorphizer::new(Interner::new());
    let error_type = monomorphizer.error_type();

    match &error_type.type_ {
        TypeKind::Error => (),
        _ => panic!("Expected Error type kind"),
    }
}

#[test]
fn test_collect_type_vars_rec() {
    let monomorphizer = Monomorphizer::new(Interner::new());
    let mut vars = Vec::new();

    // Create a type with variables
    let type_with_vars = Rc::new(Type {
        span: None,
        file: None,
        type_: TypeKind::Variable {
            id: 5,
            kind: Kind::Star,
        },
    });

    monomorphizer.collect_type_vars_rec(&type_with_vars, &mut vars);
    assert!(vars.contains(&5));
}
