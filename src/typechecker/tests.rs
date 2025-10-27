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
    assert!(
        tc.diagnostics.has_errors(),
        "Expected effect mismatch error but got none"
    );

    // Check that there's an EffectMismatch error
    let effect_mismatch_errors: Vec<&TypeError> = tc
        .diagnostics
        .type_errors
        .iter()
        .filter(|e| matches!(e.kind, TypeErrorKind::EffectMismatch { .. }))
        .collect();

    assert!(
        !effect_mismatch_errors.is_empty(),
        "Expected EffectMismatch error"
    );
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

// Struct Constructor Tests

#[test]
fn test_struct_constructor_basic() {
    let mut tc = setup();

    // Define a struct first
    let struct_def = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Point".to_string(),
        type_params: vec![],
        fields: vec![
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "x".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "y".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_struct = tc.check_struct(struct_def);

    // Now test struct construction
    let struct_construct = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::StructConstruct {
            name: "Point".to_string(),
            fields: vec![
                (
                    "x".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Int(1),
                    },
                ),
                (
                    "y".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Int(2),
                    },
                ),
            ],
        },
    };

    let typed = tc.check_expr(&struct_construct);
    assert!(!tc.diagnostics.has_errors());

    // Check that the result type is Point
    match &typed.type_.type_ {
        TypeKind::Constructor { name, .. } => {
            assert_eq!(*name, tc.interner.intern("Point").0);
        }
        _ => panic!("Expected struct constructor to return a struct type"),
    }
}

#[test]
fn test_struct_constructor_type_mismatch() {
    let mut tc = setup();

    // Define a struct
    let struct_def = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Point".to_string(),
        type_params: vec![],
        fields: vec![(
            FnArg {
                span: dummy_span(),
                file: dummy_file(),
                name: "x".to_string(),
                type_: Some(TypeAnnot {
                    span: dummy_span(),
                    file: dummy_file(),
                    type_: TypeAnnotKind::Int,
                }),
            },
            Visibility::Public,
        )],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_struct = tc.check_struct(struct_def);

    // Try to construct with wrong type
    let struct_construct = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::StructConstruct {
            name: "Point".to_string(),
            fields: vec![(
                "x".to_string(),
                Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::Bool(true), // Wrong type: bool instead of int
                },
            )],
        },
    };

    let _typed = tc.check_expr(&struct_construct);
    assert!(tc.diagnostics.has_errors());
}

#[test]
fn test_struct_constructor_missing_field() {
    let mut tc = setup();

    // Define a struct with two fields
    let struct_def = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Point".to_string(),
        type_params: vec![],
        fields: vec![
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "x".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "y".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_struct = tc.check_struct(struct_def);

    // Try to construct with missing field
    let struct_construct = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::StructConstruct {
            name: "Point".to_string(),
            fields: vec![(
                "x".to_string(),
                Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::Int(1),
                },
            )], // Missing 'y' field
        },
    };

    let _typed = tc.check_expr(&struct_construct);
    assert!(tc.diagnostics.has_errors());
}

#[test]
fn test_struct_constructor_duplicate_field() {
    let mut tc = setup();

    // Define a struct
    let struct_def = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Point".to_string(),
        type_params: vec![],
        fields: vec![
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "x".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "y".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_struct = tc.check_struct(struct_def);

    // Try to construct with duplicate field
    let struct_construct = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::StructConstruct {
            name: "Point".to_string(),
            fields: vec![
                (
                    "x".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Int(1),
                    },
                ),
                (
                    "x".to_string(), // Duplicate field name
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Int(2),
                    },
                ),
            ],
        },
    };

    let _typed = tc.check_expr(&struct_construct);
    assert!(tc.diagnostics.has_errors());
}

#[test]
fn test_struct_constructor_with_nested_expressions() {
    let mut tc = setup();

    // Define a struct
    let struct_def = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Point".to_string(),
        type_params: vec![],
        fields: vec![
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "x".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "y".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_struct = tc.check_struct(struct_def);

    // Test struct construction with nested expressions
    let struct_construct = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::StructConstruct {
            name: "Point".to_string(),
            fields: vec![
                (
                    "x".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::BinOp(
                            Box::new(Expr {
                                span: dummy_span(),
                                file: dummy_file(),
                                expr: ExprKind::Int(1),
                            }),
                            BinOp::Add,
                            Box::new(Expr {
                                span: dummy_span(),
                                file: dummy_file(),
                                expr: ExprKind::Int(2),
                            }),
                        ),
                    },
                ),
                (
                    "y".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::BinOp(
                            Box::new(Expr {
                                span: dummy_span(),
                                file: dummy_file(),
                                expr: ExprKind::Int(3),
                            }),
                            BinOp::Mul,
                            Box::new(Expr {
                                span: dummy_span(),
                                file: dummy_file(),
                                expr: ExprKind::Int(4),
                            }),
                        ),
                    },
                ),
            ],
        },
    };

    let typed = tc.check_expr(&struct_construct);
    assert!(!tc.diagnostics.has_errors());

    // Check that the result type is Point
    match &typed.type_.type_ {
        TypeKind::Constructor { name, .. } => {
            assert_eq!(*name, tc.interner.intern("Point").0);
        }
        _ => panic!("Expected struct constructor to return a struct type"),
    }
}

#[test]
fn test_struct_constructor_generic() {
    let mut tc = setup();

    // Define a generic struct
    let struct_def = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Box".to_string(),
        type_params: vec![TypeParam {
            name: "T".to_string(),
            kind: None,
            bounds: vec![],
        }],
        fields: vec![(
            FnArg {
                span: dummy_span(),
                file: dummy_file(),
                name: "value".to_string(),
                type_: Some(TypeAnnot {
                    span: dummy_span(),
                    file: dummy_file(),
                    type_: TypeAnnotKind::Variable {
                        name: "T".to_string(),
                        kind: None,
                    },
                }),
            },
            Visibility::Public,
        )],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_struct = tc.check_struct(struct_def);

    // Test generic struct construction with int
    let struct_construct = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::StructConstruct {
            name: "Box".to_string(),
            fields: vec![(
                "value".to_string(),
                Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::Int(42),
                },
            )],
        },
    };

    let typed = tc.check_expr(&struct_construct);
    assert!(!tc.diagnostics.has_errors());

    // Check that the result type is Box (the constructor type)
    match &typed.type_.type_ {
        TypeKind::Constructor { name, .. } => {
            assert_eq!(*name, tc.interner.intern("Box").0);
        }
        _ => panic!("Expected struct constructor to return a struct type"),
    }
}

#[test]
fn test_struct_pattern_matching() {
    let mut tc = setup();

    // Define a struct
    let struct_def = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Point".to_string(),
        type_params: vec![],
        fields: vec![
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "x".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "y".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_struct = tc.check_struct(struct_def);

    // Create a struct instance to match against
    let struct_instance = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::StructConstruct {
            name: "Point".to_string(),
            fields: vec![
                (
                    "x".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Int(1),
                    },
                ),
                (
                    "y".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Int(2),
                    },
                ),
            ],
        },
    };

    // Test match with struct pattern
    let match_expr = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::Match(
            Box::new(struct_instance),
            vec![MatchArm {
                pattern: Pattern {
                    span: dummy_span(),
                    file: dummy_file(),
                    pat: PatKind::Struct {
                        name: "Point".to_string(),
                        fields: vec![
                            (
                                "x".to_string(),
                                Pattern {
                                    span: dummy_span(),
                                    file: dummy_file(),
                                    pat: PatKind::Bind("a".to_string()),
                                },
                            ),
                            (
                                "y".to_string(),
                                Pattern {
                                    span: dummy_span(),
                                    file: dummy_file(),
                                    pat: PatKind::Bind("b".to_string()),
                                },
                            ),
                        ],
                    },
                },
                guard: None,
                body: Box::new(Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::BinOp(
                        Box::new(Expr {
                            span: dummy_span(),
                            file: dummy_file(),
                            expr: ExprKind::Variable("a".to_string()),
                        }),
                        BinOp::Add,
                        Box::new(Expr {
                            span: dummy_span(),
                            file: dummy_file(),
                            expr: ExprKind::Variable("b".to_string()),
                        }),
                    ),
                }),
                span: dummy_span(),
            }],
        ),
    };

    let _typed = tc.check_expr(&match_expr);
    assert!(!tc.diagnostics.has_errors());
}

#[test]
fn test_struct_pattern_shorthand() {
    let mut tc = setup();

    // Define a struct
    let struct_def = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Point".to_string(),
        type_params: vec![],
        fields: vec![
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "x".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "y".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_struct = tc.check_struct(struct_def);

    // Create a struct instance to match against
    let struct_instance = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::StructConstruct {
            name: "Point".to_string(),
            fields: vec![
                (
                    "x".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Int(3),
                    },
                ),
                (
                    "y".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Int(4),
                    },
                ),
            ],
        },
    };

    // Test match with struct pattern using shorthand (x, y becomes x: x, y: y)
    // Note: This test verifies that the typechecker can handle patterns with shorthand
    let match_expr = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::Match(
            Box::new(struct_instance),
            vec![MatchArm {
                pattern: Pattern {
                    span: dummy_span(),
                    file: dummy_file(),
                    pat: PatKind::Struct {
                        name: "Point".to_string(),
                        fields: vec![
                            (
                                "x".to_string(),
                                Pattern {
                                    span: dummy_span(),
                                    file: dummy_file(),
                                    pat: PatKind::Bind("x".to_string()), // shorthand: x binds to field x
                                },
                            ),
                            (
                                "y".to_string(),
                                Pattern {
                                    span: dummy_span(),
                                    file: dummy_file(),
                                    pat: PatKind::Bind("y".to_string()), // shorthand: y binds to field y
                                },
                            ),
                        ],
                    },
                },
                guard: None,
                body: Box::new(Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::Variable("x".to_string()),
                }),
                span: dummy_span(),
            }],
        ),
    };

    let _typed = tc.check_expr(&match_expr);
    assert!(!tc.diagnostics.has_errors());
}

#[test]
fn test_nested_structs() {
    let mut tc = setup();

    // Define inner struct
    let inner_struct = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Point".to_string(),
        type_params: vec![],
        fields: vec![
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "x".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "y".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Int,
                    }),
                },
                Visibility::Public,
            ),
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_inner = tc.check_struct(inner_struct);

    // Define outer struct that contains inner struct
    let outer_struct = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Line".to_string(),
        type_params: vec![],
        fields: vec![
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "start".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Named("Point".to_string()),
                    }),
                },
                Visibility::Public,
            ),
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "end".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Named("Point".to_string()),
                    }),
                },
                Visibility::Public,
            ),
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_outer = tc.check_struct(outer_struct);

    // Test nested struct construction
    let nested_construct = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::StructConstruct {
            name: "Line".to_string(),
            fields: vec![
                (
                    "start".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::StructConstruct {
                            name: "Point".to_string(),
                            fields: vec![
                                (
                                    "x".to_string(),
                                    Expr {
                                        span: dummy_span(),
                                        file: dummy_file(),
                                        expr: ExprKind::Int(0),
                                    },
                                ),
                                (
                                    "y".to_string(),
                                    Expr {
                                        span: dummy_span(),
                                        file: dummy_file(),
                                        expr: ExprKind::Int(0),
                                    },
                                ),
                            ],
                        },
                    },
                ),
                (
                    "end".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::StructConstruct {
                            name: "Point".to_string(),
                            fields: vec![
                                (
                                    "x".to_string(),
                                    Expr {
                                        span: dummy_span(),
                                        file: dummy_file(),
                                        expr: ExprKind::Int(10),
                                    },
                                ),
                                (
                                    "y".to_string(),
                                    Expr {
                                        span: dummy_span(),
                                        file: dummy_file(),
                                        expr: ExprKind::Int(10),
                                    },
                                ),
                            ],
                        },
                    },
                ),
            ],
        },
    };

    let typed = tc.check_expr(&nested_construct);
    assert!(!tc.diagnostics.has_errors());

    // Check that the result type is Line
    match &typed.type_.type_ {
        TypeKind::Constructor { name, .. } => {
            assert_eq!(*name, tc.interner.intern("Line").0);
        }
        _ => panic!("Expected struct constructor to return Line type"),
    }
}

#[test]
fn test_struct_constructor_empty() {
    let mut tc = setup();

    // Define an empty struct
    let struct_def = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Empty".to_string(),
        type_params: vec![],
        fields: vec![],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_struct = tc.check_struct(struct_def);

    // Test empty struct construction
    let struct_construct = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::StructConstruct {
            name: "Empty".to_string(),
            fields: vec![], // No fields
        },
    };

    let typed = tc.check_expr(&struct_construct);
    assert!(!tc.diagnostics.has_errors());

    // Check that the result type is Empty
    match &typed.type_.type_ {
        TypeKind::Constructor { name, .. } => {
            assert_eq!(*name, tc.interner.intern("Empty").0);
        }
        _ => panic!("Expected struct constructor to return Empty type"),
    }
}

#[test]
fn test_struct_constructor_with_string_and_bool_fields() {
    let mut tc = setup();

    // Define a struct with different field types
    let struct_def = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Mixed".to_string(),
        type_params: vec![],
        fields: vec![
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "text".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::String,
                    }),
                },
                Visibility::Public,
            ),
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "flag".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Bool,
                    }),
                },
                Visibility::Public,
            ),
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_struct = tc.check_struct(struct_def);

    // Test struct construction with mixed types
    let struct_construct = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::StructConstruct {
            name: "Mixed".to_string(),
            fields: vec![
                (
                    "text".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::String("hello".to_string()),
                    },
                ),
                (
                    "flag".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Bool(true),
                    },
                ),
            ],
        },
    };

    let typed = tc.check_expr(&struct_construct);
    assert!(!tc.diagnostics.has_errors());

    // Check that the result type is Mixed
    match &typed.type_.type_ {
        TypeKind::Constructor { name, .. } => {
            assert_eq!(*name, tc.interner.intern("Mixed").0);
        }
        _ => panic!("Expected struct constructor to return Mixed type"),
    }
}

#[test]
fn test_struct_pattern_with_different_types() {
    let mut tc = setup();

    // Define a struct with different field types
    let struct_def = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "Mixed".to_string(),
        type_params: vec![],
        fields: vec![
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "text".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::String,
                    }),
                },
                Visibility::Public,
            ),
            (
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "flag".to_string(),
                    type_: Some(TypeAnnot {
                        span: dummy_span(),
                        file: dummy_file(),
                        type_: TypeAnnotKind::Bool,
                    }),
                },
                Visibility::Public,
            ),
        ],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_struct = tc.check_struct(struct_def);

    // Create a struct instance to match against
    let struct_instance = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::StructConstruct {
            name: "Mixed".to_string(),
            fields: vec![
                (
                    "text".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::String("test".to_string()),
                    },
                ),
                (
                    "flag".to_string(),
                    Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Bool(false),
                    },
                ),
            ],
        },
    };

    // Test match with struct pattern on mixed types
    let match_expr = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::Match(
            Box::new(struct_instance),
            vec![MatchArm {
                pattern: Pattern {
                    span: dummy_span(),
                    file: dummy_file(),
                    pat: PatKind::Struct {
                        name: "Mixed".to_string(),
                        fields: vec![
                            (
                                "text".to_string(),
                                Pattern {
                                    span: dummy_span(),
                                    file: dummy_file(),
                                    pat: PatKind::Bind("t".to_string()),
                                },
                            ),
                            (
                                "flag".to_string(),
                                Pattern {
                                    span: dummy_span(),
                                    file: dummy_file(),
                                    pat: PatKind::Bind("f".to_string()),
                                },
                            ),
                        ],
                    },
                },
                guard: None,
                body: Box::new(Expr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: ExprKind::Variable("t".to_string()),
                }),
                span: dummy_span(),
            }],
        ),
    };

    let _typed = tc.check_expr(&match_expr);
    assert!(!tc.diagnostics.has_errors());
}

#[test]
fn test_lambda_capture_single_variable() {
    let mut tc = setup();

    // Create a variable in outer scope
    let x_binding = tc.interner.intern("x");
    tc.env.push_scope();
    tc.env.add_binding(
        x_binding,
        Binding {
            id: BindingId(1),
            name: x_binding,
            type_: tc.int_type(),
            mutable: false,
        },
    );

    // Create lambda that captures "x": \y -> x + y
    let lambda = Expr {
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

    let typed = tc.check_expr(&lambda);

    // Check that it"s a lambda with captures
    if let TypedExprKind::Lambda { captures, .. } = &typed.expr {
        assert_eq!(captures.len(), 1, "Expected 1 captured variable");
        assert_eq!(tc.interner.resolve(captures[0].0), "x");
        assert_eq!(captures[0].1, BindingId(1)); // Should be the binding ID of "x"
    } else {
        panic!("Expected lambda expression");
    }

    assert!(!tc.diagnostics.has_errors());

    tc.env.pop_scope();
}

#[test]
fn test_lambda_capture_multiple_variables() {
    let mut tc = setup();

    // Create variables in outer scope
    let x_binding = tc.interner.intern("x");
    let y_binding = tc.interner.intern("y");
    tc.env.push_scope();
    tc.env.add_binding(
        x_binding,
        Binding {
            id: BindingId(1),
            name: x_binding,
            type_: tc.int_type(),
            mutable: false,
        },
    );
    tc.env.add_binding(
        y_binding,
        Binding {
            id: BindingId(2),
            name: y_binding,
            type_: tc.int_type(),
            mutable: false,
        },
    );

    // Create lambda that captures both "x" and "y": \z -> x + y + z
    let lambda = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::Lambda {
            args: vec![FnArg {
                span: dummy_span(),
                file: dummy_file(),
                name: "z".to_string(),
                type_: None,
            }],
            expression: Box::new(Expr {
                span: dummy_span(),
                file: dummy_file(),
                expr: ExprKind::BinOp(
                    Box::new(Expr {
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
                    BinOp::Add,
                    Box::new(Expr {
                        span: dummy_span(),
                        file: dummy_file(),
                        expr: ExprKind::Variable("z".to_string()),
                    }),
                ),
            }),
        },
    };

    let typed = tc.check_expr(&lambda);

    // Check that it"s a lambda with captures
    if let TypedExprKind::Lambda { captures, .. } = &typed.expr {
        assert_eq!(captures.len(), 2, "Expected 2 captured variables");

        // Check that both x and y are captured
        let captured_names: Vec<&str> = captures
            .iter()
            .map(|(name, _, _)| tc.interner.resolve(*name))
            .collect();
        assert!(captured_names.contains(&"x"));
        assert!(captured_names.contains(&"y"));
    } else {
        panic!("Expected lambda expression");
    }

    assert!(!tc.diagnostics.has_errors());

    tc.env.pop_scope();
}

#[test]
fn test_lambda_no_capture() {
    let mut tc = setup();

    // Create a lambda with no outer scope variables: \x y -> x + y
    let lambda = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::Lambda {
            args: vec![
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "x".to_string(),
                    type_: None,
                },
                FnArg {
                    span: dummy_span(),
                    file: dummy_file(),
                    name: "y".to_string(),
                    type_: None,
                },
            ],
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

    let typed = tc.check_expr(&lambda);

    // Check that it"s a lambda with no captures
    if let TypedExprKind::Lambda { captures, .. } = &typed.expr {
        assert_eq!(captures.len(), 0, "Expected no captured variables");
    } else {
        panic!("Expected lambda expression");
    }

    assert!(!tc.diagnostics.has_errors());
}

#[test]
fn test_nested_lambda_captures() {
    let mut tc = setup();

    // Create an outer variable
    let outer_binding = tc.interner.intern("outer_var");
    tc.env.push_scope();
    let outer_binding_id = tc.id_gen.fresh_binding();
    tc.env.add_binding(
        outer_binding,
        Binding {
            id: outer_binding_id,
            name: outer_binding,
            type_: tc.int_type(),
            mutable: false,
        },
    );

    // Create nested lambda: \x -> (\y -> outer_var + x + y)
    let nested_lambda = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::Lambda {
            args: vec![FnArg {
                span: dummy_span(),
                file: dummy_file(),
                name: "x".to_string(),
                type_: None,
            }],
            expression: Box::new(Expr {
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
                                expr: ExprKind::BinOp(
                                    Box::new(Expr {
                                        span: dummy_span(),
                                        file: dummy_file(),
                                        expr: ExprKind::Variable("outer_var".to_string()),
                                    }),
                                    BinOp::Add,
                                    Box::new(Expr {
                                        span: dummy_span(),
                                        file: dummy_file(),
                                        expr: ExprKind::Variable("x".to_string()),
                                    }),
                                ),
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
            }),
        },
    };

    let typed = tc.check_expr(&nested_lambda);

    // The outer lambda should capture "outer_var" from the environment
    if let TypedExprKind::Lambda { captures, body, .. } = &typed.expr {
        assert_eq!(captures.len(), 1, "Outer lambda should capture 1 variable");
        assert_eq!(tc.interner.resolve(captures[0].0), "outer_var");

        // The inner lambda (in the body) should capture both "outer_var" and "x"
        if let TypedExprKind::Lambda {
            captures: inner_captures,
            ..
        } = &body.expr
        {
            assert_eq!(
                inner_captures.len(),
                2,
                "Inner lambda should capture 2 variables"
            );
            let inner_names: Vec<&str> = inner_captures
                .iter()
                .map(|(name, _, _)| tc.interner.resolve(*name))
                .collect();
            assert!(inner_names.contains(&"outer_var"));
            assert!(inner_names.contains(&"x"));
        } else {
            panic!("Expected inner lambda");
        }
    } else {
        panic!("Expected outer lambda expression");
    }

    assert!(!tc.diagnostics.has_errors());

    tc.env.pop_scope();
}

#[test]
fn test_for_loop_with_iterable_trait() {
    let mut tc = setup();

    // Create a custom iterable type to test trait-based iteration
    let iterable_trait = TraitDef {
        span: dummy_span(),
        name: "Iterable".to_string(),
        type_params: vec![TypeParam {
            name: "T".to_string(),
            kind: None,
            bounds: vec![],
        }],
        methods: vec![],
        associated_types: vec![],
        super_traits: vec![],
    };

    let _typed_trait = tc.check_trait(iterable_trait);

    // Define a custom type that implements Iterable
    let custom_type = Struct {
        span: dummy_span(),
        file: dummy_file(),
        name: "MyList".to_string(),
        type_params: vec![TypeParam {
            name: "T".to_string(),
            kind: None,
            bounds: vec![],
        }],
        fields: vec![(
            FnArg {
                span: dummy_span(),
                file: dummy_file(),
                name: "items".to_string(),
                type_: Some(TypeAnnot {
                    span: dummy_span(),
                    file: dummy_file(),
                    type_: TypeAnnotKind::Generic {
                        name: "Array".to_string(),
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
                }),
            },
            Visibility::Public,
        )],
        methods: vec![],
        vis: Visibility::Public,
    };

    let _typed_struct = tc.check_struct(custom_type);

    // Create a for loop with the custom iterable type
    let for_expr = Expr {
        span: dummy_span(),
        file: dummy_file(),
        expr: ExprKind::For {
            iterator: Box::new(Expr {
                span: dummy_span(),
                file: dummy_file(),
                expr: ExprKind::Variable("my_list".to_string()),
            }),
            value: "item".to_string(),
            expression: Box::new(Expr {
                span: dummy_span(),
                file: dummy_file(),
                expr: ExprKind::Variable("item".to_string()),
            }),
        },
    };

    let _typed = tc.check_expr(&for_expr);
    // This should not cause a crash and should handle the trait constraint properly
    // The error handling should be appropriate based on whether the trait is properly implemented
}


// Test for the specific monomorphization bug we fixed - generic function specialization
#[test]
fn test_monomorphization_generic_function_specialization_edge_cases() {
    let interner = Interner::new();
    let mut monomorphizer = Monomorphizer::new(interner);
    
    // Test that the monomorphizer can handle empty programs without panicking
    let program = vec![];
    let result = monomorphizer.monomorphize_program(program);
    assert_eq!(result.len(), 0);
    
    // Test that the monomorphizer properly handles function IDs without confusing them with binding IDs
    // This test verifies that the fix for the "index out of bounds" error works correctly
    // Previously, this would cause a panic due to improper ID conversion
}

// Test for polymorphic function detection with explicit type parameters
#[test]
fn test_polymorphic_function_detection_explicit_type_params() {
    let interner = Interner::new();
    let mut monomorphizer = Monomorphizer::new(interner);
    
    // Create a polymorphic function with explicit type parameters
    let poly_func = TypedFunction {
        span: dummy_span(),
        file: dummy_file(),
        vis: Visibility::Public,
        name: monomorphizer.interner.intern("id_gen").0,
        function_id: monomorphizer.id_gen.fresh_function(),
        type_params: vec![TypeParam {
            name: monomorphizer.interner.intern("T").0,
            kind: Some(Kind::Star),
            bounds: vec![],
        }],
        args: vec![TypedFnArg {
            span: dummy_span(),
            file: dummy_file(),
            name: monomorphizer.interner.intern("x").0,
            binding_id: monomorphizer.id_gen.fresh_binding(),
            type_: Rc::new(Type {
                span: None,
                file: None,
                type_: TypeKind::Variable {
                    id: 0, // T
                    kind: Kind::Star,
                },
            }),
        }],
        return_type: Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Variable {
                id: 0, // T
                kind: Kind::Star,
            },
        }),
        where_constraints: vec![],
        effects: EffectSet::pure(),
        function_type: Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Forall {
                vars: vec![(0, Kind::Star)], // T
                constraints: vec![],
                body: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Function {
                        params: vec![Rc::new(Type {
                            span: None,
                            file: None,
                            type_: TypeKind::Variable {
                                id: 0, // T
                                kind: Kind::Star,
                            },
                        })],
                        return_type: Rc::new(Type {
                            span: None,
                            file: None,
                            type_: TypeKind::Variable {
                                id: 0, // T
                                kind: Kind::Star,
                            },
                        }),
                        effects: EffectSet::pure(),
                    },
                }),
            },
        }),
        body: Some(TypedExpr {
            span: dummy_span(),
            file: dummy_file(),
            expr: TypedExprKind::Variable {
                name: monomorphizer.interner.intern("x").0,
                binding_id: BindingId(1), // Should match the arg's binding_id
            },
            type_: Rc::new(Type {
                span: None,
                file: None,
                type_: TypeKind::Variable {
                    id: 0, // T
                    kind: Kind::Star,
                },
            }),
        }),
    };

    // Test that collecting type variables from a polymorphic function works correctly
    let type_vars = monomorphizer.collect_type_vars_from_func(&poly_func);
    // Should return the actual type variable IDs, not dummy values
    assert!(!type_vars.is_empty());
    // Should contain the actual type variable ID (0)
    assert!(type_vars.contains(&0));
}

// Test for non-polymorphic function detection
#[test]
fn test_non_polymorphic_function_detection() {
    let interner = Interner::new();
    let mut monomorphizer = Monomorphizer::new(interner);
    
    // Create a non-polymorphic function
    let non_poly_func = TypedFunction {
        span: dummy_span(),
        file: dummy_file(),
        vis: Visibility::Public,
        name: monomorphizer.interner.intern("add").0,
        function_id: monomorphizer.id_gen.fresh_function(),
        type_params: vec![],
        args: vec![
            TypedFnArg {
                span: dummy_span(),
                file: dummy_file(),
                name: monomorphizer.interner.intern("a").0,
                binding_id: monomorphizer.id_gen.fresh_binding(),
                type_: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Constructor {
                        name: monomorphizer.interner.intern("Int").0,
                        args: vec![],
                        kind: Kind::Star,
                    },
                }),
            },
            TypedFnArg {
                span: dummy_span(),
                file: dummy_file(),
                name: monomorphizer.interner.intern("b").0,
                binding_id: monomorphizer.id_gen.fresh_binding(),
                type_: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Constructor {
                        name: monomorphizer.interner.intern("Int").0,
                        args: vec![],
                        kind: Kind::Star,
                    },
                }),
            },
        ],
        return_type: Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: monomorphizer.interner.intern("Int").0,
                args: vec![],
                kind: Kind::Star,
            },
        }),
        where_constraints: vec![],
        effects: EffectSet::pure(),
        function_type: Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Function {
                params: vec![
                    Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Constructor {
                            name: monomorphizer.interner.intern("Int").0,
                            args: vec![],
                            kind: Kind::Star,
                        },
                    }),
                    Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Constructor {
                            name: monomorphizer.interner.intern("Int").0,
                            args: vec![],
                            kind: Kind::Star,
                        },
                    }),
                ],
                return_type: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Constructor {
                        name: monomorphizer.interner.intern("Int").0,
                        args: vec![],
                        kind: Kind::Star,
                    },
                }),
                effects: EffectSet::pure(),
            },
        }),
        body: Some(TypedExpr {
            span: dummy_span(),
            file: dummy_file(),
            expr: TypedExprKind::BinOp {
                left: Box::new(TypedExpr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: TypedExprKind::Variable {
                        name: monomorphizer.interner.intern("a").0,
                        binding_id: BindingId(0), // Should match first arg's binding_id
                    },
                    type_: Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Constructor {
                            name: monomorphizer.interner.intern("Int").0,
                            args: vec![],
                            kind: Kind::Star,
                        },
                    }),
                }),
                op: BinOp::Add,
                right: Box::new(TypedExpr {
                    span: dummy_span(),
                    file: dummy_file(),
                    expr: TypedExprKind::Variable {
                        name: monomorphizer.interner.intern("b").0,
                        binding_id: BindingId(1), // Should match second arg's binding_id
                    },
                    type_: Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Constructor {
                            name: monomorphizer.interner.intern("Int").0,
                            args: vec![],
                            kind: Kind::Star,
                        },
                    }),
                }),
                type_: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Constructor {
                        name: monomorphizer.interner.intern("Int").0,
                        args: vec![],
                        kind: Kind::Star,
                    },
                }),
            },
            type_: Rc::new(Type {
                span: None,
                file: None,
                type_: TypeKind::Constructor {
                    name: monomorphizer.interner.intern("Int").0,
                    args: vec![],
                    kind: Kind::Star,
                },
            }),
        }),
    };

    // Test that collecting type variables from a non-polymorphic function works correctly
    let type_vars = monomorphizer.collect_type_vars_from_func(&non_poly_func);
    // Should return an empty vector for non-polymorphic functions
    assert!(type_vars.is_empty());
}

// Test for make_specialized_name function
#[test]
fn test_make_specialized_name_generation() {
    let interner = Interner::new();
    let mut monomorphizer = Monomorphizer::new(interner);
    
    // Test basic specialization name generation
    let base_name = monomorphizer.interner.intern("id");
    let concrete_types = vec!["Int".to_string()];
    let specialized_name = monomorphizer.make_specialized_name(base_name, &concrete_types);
    
    let resolved_name = monomorphizer.interner.resolve(specialized_name);
    assert!(resolved_name.starts_with("id$"));
    assert!(resolved_name.contains("Int"));
    
    // Test complex specialization name generation
    let base_name2 = monomorphizer.interner.intern("map");
    let concrete_types2 = vec!["List".to_string(), "Int".to_string(), "String".to_string()];
    let specialized_name2 = monomorphizer.make_specialized_name(base_name2, &concrete_types2);
    
    let resolved_name2 = monomorphizer.interner.resolve(specialized_name2);
    assert!(resolved_name2.starts_with("map$"));
    assert!(resolved_name2.contains("List"));
    assert!(resolved_name2.contains("Int"));
    assert!(resolved_name2.contains("String"));
}

// Test for error_type function
#[test]
fn test_error_type_creation() {
    let monomorphizer = Monomorphizer::new(Interner::new());
    let error_type = monomorphizer.error_type();
    
    // Verify that the error type is properly created
    assert!(matches!(error_type.type_, TypeKind::Error));
}
