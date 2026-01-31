use crate::ast::*;

pub fn desugar_ast(program: Program) -> Program {
    program.into_iter().map(desugar_item).collect()
}

fn desugar_item(item: Node<Item>) -> Node<Item> {
    let Node { data, meta } = item;
    let desugared_data = match data {
        Item::Function(func) => Item::Function(Box::new(desugar_function(*func))),
        Item::Impl(impl_def) => Item::Impl(desugar_impl(impl_def)),
        _ => data,
    };

    Node {
        data: desugared_data,
        meta,
    }
}

fn desugar_function(mut func: Function) -> Function {
    func.body = desugar_expr(func.body);
    func
}

fn desugar_impl(mut impl_def: Impl) -> Impl {
    impl_def.items = impl_def
        .items
        .into_iter()
        .map(|item| {
            let Node { data, meta } = item;
            let desugared_data = match data {
                ImplItem::TypeAlias(alias) => ImplItem::TypeAlias(alias),
                ImplItem::Function(func) => ImplItem::Function(Box::new(desugar_function(*func))),
            };
            Node {
                data: desugared_data,
                meta,
            }
        })
        .collect();
    impl_def
}

fn desugar_expr(expr: Node<Expr>) -> Node<Expr> {
    let Node { data, meta } = expr;
    let desugared_data = match data {
        Expr::Binary { left, op, right } => Expr::Binary {
            left: Box::new(desugar_expr(*left)),
            op,
            right: Box::new(desugar_expr(*right)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op,
            expr: Box::new(desugar_expr(*expr)),
        },
        Expr::Literal(lit) => Expr::Literal(lit),
        Expr::Variable(name) => Expr::Variable(name),
        Expr::Call { fun, args } => {
            let desugared_args = args.into_iter().map(desugar_expr).collect();
            Expr::Call {
                fun: Box::new(desugar_expr(*fun)),
                args: desugared_args,
            }
        }
        Expr::Lambda {
            params,
            return_type,
            body,
        } => Expr::Lambda {
            params,
            return_type,
            body: Box::new(desugar_expr(*body)),
        },
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => Expr::If {
            cond: Box::new(desugar_expr(*cond)),
            then_branch: Box::new(desugar_expr(*then_branch)),
            else_branch: else_branch.map(|e| Box::new(desugar_expr(*e))),
        },
        Expr::While { cond, body } => Expr::While {
            cond: Box::new(desugar_expr(*cond)),
            body: Box::new(desugar_expr(*body)),
        },
        Expr::For { var, iter, body } => Expr::For {
            var,
            iter: Box::new(desugar_expr(*iter)),
            body: Box::new(desugar_expr(*body)),
        },
        Expr::Match { expr, arms } => Expr::Match {
            expr: Box::new(desugar_expr(*expr)),
            arms: arms
                .into_iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern,
                    body: desugar_expr(arm.body),
                })
                .collect(),
        },
        Expr::Block(exprs) => Expr::Block(exprs.into_iter().map(desugar_expr).collect()),
        Expr::Tuple(exprs) => Expr::Tuple(exprs.into_iter().map(desugar_expr).collect()),
        Expr::Array(exprs) => Expr::Array(exprs.into_iter().map(desugar_expr).collect()),

        // The main desugaring - convert Pipe expressions to Call expressions
        Expr::Pipe { left, right } => {
            let left_desugared = desugar_expr(*left);
            let right_desugared = desugar_expr(*right);

            // Convert left |> right to right(left) or right(left, args...) depending on right's form
            convert_pipe_to_call(left_desugared, right_desugared)
        }

        Expr::FieldAccess { expr, field } => Expr::FieldAccess {
            expr: Box::new(desugar_expr(*expr)),
            field,
        },
        Expr::Index { expr, index } => Expr::Index {
            expr: Box::new(desugar_expr(*expr)),
            index: Box::new(desugar_expr(*index)),
        },
        Expr::StructLiteral { name, fields, base } => Expr::StructLiteral {
            name,
            fields: fields
                .into_iter()
                .map(|field| StructLitField {
                    name: field.name,
                    value: field.value.map(desugar_expr),
                })
                .collect(),
            base: base.map(|b| Box::new(desugar_expr(*b))),
        },
        Expr::EnumVariant {
            enum_name,
            variant_name,
            data,
        } => Expr::EnumVariant {
            enum_name,
            variant_name,
            data: data.map(|inner_data| match inner_data {
                EnumVariantData::Tuple(exprs) => {
                    EnumVariantData::Tuple(exprs.into_iter().map(desugar_expr).collect())
                }
                EnumVariantData::Struct(field_exprs) => EnumVariantData::Struct(
                    field_exprs
                        .into_iter()
                        .map(|field| StructLitField {
                            name: field.name,
                            value: field.value.map(desugar_expr),
                        })
                        .collect(),
                ),
            }),
        },
        Expr::Assign { left, right } => Expr::Assign {
            left: Box::new(desugar_expr(*left)),
            right: Box::new(desugar_expr(*right)),
        },
        Expr::Let {
            name,
            type_ann,
            value,
        } => Expr::Let {
            name,
            type_ann,
            value: Box::new(desugar_expr(*value)),
        },
        Expr::StaticCall {
            type_name,
            method,
            args,
        } => {
            let desugared_args = args.into_iter().map(desugar_expr).collect();
            Expr::StaticCall {
                type_name,
                method,
                args: desugared_args,
            }
        }
        Expr::Break => Expr::Break,
        Expr::Continue => Expr::Continue,
        Expr::Return(value) => Expr::Return(value.map(|v| Box::new(desugar_expr(*v)))),
    };

    Node {
        data: desugared_data,
        meta,
    }
}

fn convert_pipe_to_call(left: Node<Expr>, right: Node<Expr>) -> Expr {
    match right.data {
        // If right is a call expression, prepend left to the arguments
        Expr::Call { fun, mut args } => {
            args.insert(0, left);
            Expr::Call { fun, args }
        }
        // If right is a field access, treat it as a method call and prepend left as argument
        Expr::FieldAccess { expr, field } => {
            let function_access = Node {
                data: Expr::FieldAccess { expr, field },
                meta: right.meta,
            };
            Expr::Call {
                fun: Box::new(function_access),
                args: vec![left],
            }
        }
        // If right is a variable or simple expression, call it with left as the argument
        _ => Expr::Call {
            fun: Box::new(right),
            args: vec![left],
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::meta::{Location, Meta};

    fn dummy_meta() -> Meta {
        Meta {
            location: Location {
                span: 0..0,
                file: "test".to_string(),
            },
            attributes: vec![],
        }
    }

    #[test]
    fn test_desugar_simple_pipe() {
        // Test: x |> f  =>  f(x)
        let left_expr = Node {
            data: Expr::Variable("x".to_string()),
            meta: dummy_meta(),
        };
        let right_expr = Node {
            data: Expr::Variable("f".to_string()),
            meta: dummy_meta(),
        };

        let result = convert_pipe_to_call(left_expr, right_expr);

        match result {
            Expr::Call { fun, args } => {
                assert_eq!(args.len(), 1);
                match &*fun {
                    Node {
                        data: Expr::Variable(name),
                        ..
                    } => {
                        assert_eq!(name, "f");
                    }
                    _ => panic!("Expected Variable"),
                }

                match &args[0] {
                    Node {
                        data: Expr::Variable(name),
                        ..
                    } => {
                        assert_eq!(name, "x");
                    }
                    _ => panic!("Expected Variable"),
                }
            }
            _ => panic!("Expected Call expression"),
        }
    }

    #[test]
    fn test_desugar_pipe_with_call() {
        // Test: x |> f(y)  =>  f(x, y)
        let left_expr = Node {
            data: Expr::Variable("x".to_string()),
            meta: dummy_meta(),
        };

        let right_fun = Node {
            data: Expr::Variable("f".to_string()),
            meta: dummy_meta(),
        };

        let right_arg = Node {
            data: Expr::Variable("y".to_string()),
            meta: dummy_meta(),
        };

        let right_expr = Node {
            data: Expr::Call {
                fun: Box::new(right_fun),
                args: vec![right_arg],
            },
            meta: dummy_meta(),
        };

        let result = convert_pipe_to_call(left_expr, right_expr);

        match result {
            Expr::Call { fun, args } => {
                assert_eq!(args.len(), 2); // x and y
                match &*fun {
                    Node {
                        data: Expr::Variable(name),
                        ..
                    } => {
                        assert_eq!(name, "f");
                    }
                    _ => panic!("Expected Variable"),
                }

                match &args[0] {
                    Node {
                        data: Expr::Variable(name),
                        ..
                    } => {
                        assert_eq!(name, "x"); // Left operand comes first
                    }
                    _ => panic!("Expected Variable"),
                }

                match &args[1] {
                    Node {
                        data: Expr::Variable(name),
                        ..
                    } => {
                        assert_eq!(name, "y"); // Original argument follows
                    }
                    _ => panic!("Expected Variable"),
                }
            }
            _ => panic!("Expected Call expression"),
        }
    }

    #[test]
    fn test_desugar_pipe_with_field_access() {
        // expr1 |> expr2.a should become expr2.a(expr1)
        let left_expr = Node {
            data: Expr::Variable("obj".to_string()),
            meta: dummy_meta(),
        };

        let expr = Node {
            data: Expr::Variable("obj2".to_string()),
            meta: dummy_meta(),
        };

        let right_expr = Node {
            data: Expr::FieldAccess {
                expr: Box::new(expr),
                field: "method".to_string(),
            },
            meta: dummy_meta(),
        };

        let result = convert_pipe_to_call(left_expr, right_expr);

        match result {
            Expr::Call { fun, args } => {
                assert_eq!(args.len(), 1);
                match &*fun {
                    Node {
                        data: Expr::FieldAccess { field, .. },
                        ..
                    } => {
                        assert_eq!(field, "method");
                    }
                    _ => panic!("Expected FieldAccess"),
                }

                match &args[0] {
                    Node {
                        data: Expr::Variable(name),
                        ..
                    } => {
                        assert_eq!(name, "obj");
                    }
                    _ => panic!("Expected Variable"),
                }
            }
            _ => panic!("Expected Call expression"),
        }
    }
}
