use std::rc::Rc;

use crate::ast::*;
use crate::typechecker::*;

pub struct LambdaDesugarer {
    pub interner: Interner,
    id_gen: IdGen,
    lambda_counter: usize,
}

impl LambdaDesugarer {
    pub fn new(interner: Interner) -> Self {
        Self {
            interner,
            id_gen: IdGen::new(),
            lambda_counter: 0,
        }
    }

    pub fn desugar_program(&mut self, program: Vec<TypedASTNode>) -> Vec<TypedASTNode> {
        let mut result = Vec::new();
        let mut top_level_functions = Vec::new();

        for node in program {
            if let Some(new_node) = self.desugar_node(node, &mut top_level_functions) {
                result.push(new_node)
            }
        }

        // Add all the new top-level functions we created from lambdas
        result.extend(top_level_functions);

        result
    }

    fn desugar_node(
        &mut self,
        node: TypedASTNode,
        top_level_functions: &mut Vec<TypedASTNode>,
    ) -> Option<TypedASTNode> {
        match node.node {
            TypedASTNodeKind::Expr(expr) => {
                let (new_expr, new_functions) = self.desugar_expr(expr);
                top_level_functions.extend(new_functions);

                Some(TypedASTNode {
                    span: node.span,
                    file: node.file,
                    node: TypedASTNodeKind::Expr(new_expr),
                    attributes: node.attributes,
                })
            }
            TypedASTNodeKind::Function(func) => {
                let (new_func, new_functions) = self.desugar_function(*func);
                top_level_functions.extend(new_functions);

                Some(TypedASTNode {
                    span: node.span,
                    file: node.file,
                    node: TypedASTNodeKind::Function(Box::new(new_func)),
                    attributes: node.attributes,
                })
            }
            _ => Some(node), // Other node types remain unchanged for now
        }
    }

    fn desugar_function(&mut self, func: TypedFunction) -> (TypedFunction, Vec<TypedASTNode>) {
        let mut all_new_functions = Vec::new();

        let new_body = if let Some(body) = func.body {
            let (desugared_body, new_funcs) = self.desugar_expr(body);
            all_new_functions.extend(new_funcs);
            Some(desugared_body)
        } else {
            None
        };

        let new_func = TypedFunction {
            span: func.span,
            file: func.file,
            vis: func.vis,
            name: func.name,
            function_id: func.function_id,
            type_params: func.type_params,
            args: func.args,
            return_type: func.return_type,
            where_constraints: func.where_constraints,
            effects: func.effects,
            function_type: func.function_type,
            body: new_body,
        };

        (new_func, all_new_functions)
    }

    fn desugar_expr(&mut self, expr: TypedExpr) -> (TypedExpr, Vec<TypedASTNode>) {
        let mut new_functions = Vec::new();
        let new_expr = match expr.expr {
            TypedExprKind::Spread {
                value,
                element_type,
            } => {
                // For spread, we'll return the expression as-is with the same structure
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Spread {
                        value,
                        element_type,
                    },
                    type_: expr.type_,
                }
            }
            TypedExprKind::Lambda {
                args,
                body,
                captures,
                function_type,
            } => {
                // Convert the lambda to a top-level function
                let new_function =
                    self.create_function_from_lambda(*body, args, &captures, function_type.clone());

                let func_name = new_function.name;
                let func_id = new_function.function_id;

                // Add the new function to our collection of new functions
                new_functions.push(TypedASTNode {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    node: TypedASTNodeKind::Function(Box::new(new_function)),
                    attributes: vec![],
                });

                // Return a variable expression that references the new top-level function
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Variable {
                        name: func_name,
                        binding_id: BindingId(func_id.0),
                    },
                    type_: function_type,
                }
            }

            // Handle other expressions that might contain lambdas
            TypedExprKind::Block { expressions } => {
                let mut new_expressions = Vec::new();
                for expr in expressions {
                    let (desugared_expr, mut funcs) = self.desugar_expr(expr);
                    new_functions.append(&mut funcs);
                    new_expressions.push(desugared_expr);
                }
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Block {
                        expressions: new_expressions,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::Let {
                var,
                binding_id,
                var_type,
                value,
            } => {
                let (desugared_value, mut funcs) = self.desugar_expr(*value);
                new_functions.append(&mut funcs);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Let {
                        var,
                        binding_id,
                        var_type,
                        value: Box::new(desugared_value),
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::IfElse {
                condition,
                then,
                else_,
            } => {
                let (desugared_condition, mut funcs1) = self.desugar_expr(*condition);
                new_functions.append(&mut funcs1);

                let (desugared_then, mut funcs2) = self.desugar_expr(*then);
                new_functions.append(&mut funcs2);

                let (desugared_else, _funcs3): (Option<Box<TypedExpr>>, Vec<TypedASTNode>) =
                    if let Some(else_expr) = else_ {
                        let (desugared, mut funcs) = self.desugar_expr(*else_expr);
                        new_functions.append(&mut funcs);
                        (Some(Box::new(desugared)), vec![])
                    } else {
                        (None, vec![])
                    };

                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::IfElse {
                        condition: Box::new(desugared_condition),
                        then: Box::new(desugared_then),
                        else_: desugared_else,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::Match { scrutinee, arms } => {
                let (desugared_scrutinee, mut funcs1) = self.desugar_expr(*scrutinee);
                new_functions.append(&mut funcs1);

                let mut new_arms = Vec::new();
                for arm in arms {
                    let (desugared_arm_body, mut funcs) = self.desugar_expr(*arm.body);
                    new_functions.append(&mut funcs);

                    new_arms.push(TypedMatchArm {
                        pattern: arm.pattern,
                        guard: arm.guard,
                        body: Box::new(desugared_arm_body),
                        span: arm.span,
                    });
                }

                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Match {
                        scrutinee: Box::new(desugared_scrutinee),
                        arms: new_arms,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::Call {
                function,
                args,
                type_args,
            } => {
                let (desugared_function, mut funcs1) = self.desugar_expr(*function);
                new_functions.append(&mut funcs1);

                let mut new_args = Vec::new();
                for arg in args {
                    let (desugared_arg, mut funcs) = self.desugar_expr(arg);
                    new_functions.append(&mut funcs);
                    new_args.push(desugared_arg);
                }

                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Call {
                        function: Box::new(desugared_function),
                        args: new_args,
                        type_args,
                    },
                    type_: expr.type_,
                }
            }

            // Handle remaining expressions that don't have special logic above
            TypedExprKind::BinOp { left, op, right } => {
                let (desugared_left, mut funcs1) = self.desugar_expr(*left);
                new_functions.append(&mut funcs1);
                let (desugared_right, mut funcs2) = self.desugar_expr(*right);
                new_functions.append(&mut funcs2);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::BinOp {
                        left: Box::new(desugared_left),
                        op,
                        right: Box::new(desugared_right),
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::UnOp { op, operand } => {
                let (desugared_operand, mut funcs) = self.desugar_expr(*operand);
                new_functions.append(&mut funcs);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::UnOp {
                        op,
                        operand: Box::new(desugared_operand),
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::Array {
                elements,
                element_type,
            } => {
                let mut new_elements = Vec::new();
                for element in elements {
                    let (desugared_element, mut funcs) = self.desugar_expr(element);
                    new_functions.append(&mut funcs);
                    new_elements.push(desugared_element);
                }
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Array {
                        elements: new_elements,
                        element_type,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::Tuple(elements) => {
                let mut new_elements = Vec::new();
                for element in elements {
                    let (desugared_element, mut funcs) = self.desugar_expr(element);
                    new_functions.append(&mut funcs);
                    new_elements.push(desugared_element);
                }
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Tuple(new_elements),
                    type_: expr.type_,
                }
            }

            TypedExprKind::FieldAccess {
                target,
                field,
                field_id,
                field_type,
            } => {
                let (desugared_target, mut funcs) = self.desugar_expr(*target);
                new_functions.append(&mut funcs);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::FieldAccess {
                        target: Box::new(desugared_target),
                        field,
                        field_id,
                        field_type,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::Index {
                target,
                index,
                element_type,
            } => {
                let (desugared_target, mut funcs1) = self.desugar_expr(*target);
                new_functions.append(&mut funcs1);
                let (desugared_index, mut funcs2) = self.desugar_expr(*index);
                new_functions.append(&mut funcs2);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Index {
                        target: Box::new(desugared_target),
                        index: Box::new(desugared_index),
                        element_type,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::Return(value) => {
                if let Some(val) = value {
                    let (desugared_val, mut funcs) = self.desugar_expr(*val);
                    new_functions.append(&mut funcs);
                    TypedExpr {
                        span: expr.span,
                        file: expr.file,
                        expr: TypedExprKind::Return(Some(Box::new(desugared_val))),
                        type_: expr.type_,
                    }
                } else {
                    TypedExpr {
                        span: expr.span,
                        file: expr.file,
                        expr: TypedExprKind::Return(None),
                        type_: expr.type_,
                    }
                }
            }

            TypedExprKind::Assign { l_val, r_val, op } => {
                let (desugared_l_val, mut funcs1) = self.desugar_expr(*l_val);
                new_functions.append(&mut funcs1);
                let (desugared_r_val, mut funcs2) = self.desugar_expr(*r_val);
                new_functions.append(&mut funcs2);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Assign {
                        l_val: Box::new(desugared_l_val),
                        r_val: Box::new(desugared_r_val),
                        op,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::Map {
                entries,
                key_type,
                value_type,
            } => {
                let mut new_entries = Vec::new();
                for (key, value) in entries {
                    let (desugared_key, mut funcs1) = self.desugar_expr(key);
                    new_functions.append(&mut funcs1);
                    let (desugared_value, mut funcs2) = self.desugar_expr(value);
                    new_functions.append(&mut funcs2);
                    new_entries.push((desugared_key, desugared_value));
                }
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Map {
                        entries: new_entries,
                        key_type,
                        value_type,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::EnumConstruct {
                enum_name,
                enum_id,
                variant,
                variant_id,
                args,
            } => {
                let mut new_args = Vec::new();
                for arg in args {
                    let (desugared_arg, mut funcs) = self.desugar_expr(arg);
                    new_functions.append(&mut funcs);
                    new_args.push(desugared_arg);
                }
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::EnumConstruct {
                        enum_name,
                        enum_id,
                        variant,
                        variant_id,
                        args: new_args,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::OptionalChain {
                target,
                field,
                field_id,
                field_type,
            } => {
                let (desugared_target, mut funcs) = self.desugar_expr(*target);
                new_functions.append(&mut funcs);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::OptionalChain {
                        target: Box::new(desugared_target),
                        field,
                        field_id,
                        field_type,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::Cast { expr, target_type } => {
                let (desugared_expr, mut funcs) = self.desugar_expr(*(expr.clone()));
                new_functions.append(&mut funcs);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Cast {
                        expr: Box::new(desugared_expr),
                        target_type,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::With {
                context,
                var,
                binding_id,
                var_type,
                body,
            } => {
                let (desugared_context, mut funcs1) = self.desugar_expr(*context);
                new_functions.append(&mut funcs1);
                let (desugared_body, mut funcs2) = self.desugar_expr(*body);
                new_functions.append(&mut funcs2);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::With {
                        context: Box::new(desugared_context),
                        var,
                        binding_id,
                        var_type,
                        body: Box::new(desugared_body),
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::Loop { label, body } => {
                let (desugared_body, mut funcs) = self.desugar_expr(*body);
                new_functions.append(&mut funcs);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Loop {
                        label,
                        body: Box::new(desugared_body),
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::For {
                iterator,
                iterator_type,
                value,
                binding_id,
                value_type,
                body,
            } => {
                let (desugared_iterator, mut funcs1) = self.desugar_expr(*iterator);
                new_functions.append(&mut funcs1);
                let (desugared_body, mut funcs2) = self.desugar_expr(*body);
                new_functions.append(&mut funcs2);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::For {
                        iterator: Box::new(desugared_iterator),
                        iterator_type,
                        value,
                        binding_id,
                        value_type,
                        body: Box::new(desugared_body),
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::While { condition, body } => {
                let (desugared_condition, mut funcs1) = self.desugar_expr(*condition);
                new_functions.append(&mut funcs1);
                let (desugared_body, mut funcs2) = self.desugar_expr(*body);
                new_functions.append(&mut funcs2);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::While {
                        condition: Box::new(desugared_condition),
                        body: Box::new(desugared_body),
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::IfLet {
                pattern,
                expr,
                then,
                else_,
            } => {
                let (desugared_expr, mut funcs1) = self.desugar_expr(*(expr.clone()));
                new_functions.append(&mut funcs1);
                let (desugared_then, mut funcs2) = self.desugar_expr(*then);
                new_functions.append(&mut funcs2);

                let desugared_else = if let Some(else_expr) = else_ {
                    let (desugared, mut funcs) = self.desugar_expr(*else_expr);
                    new_functions.append(&mut funcs);
                    Some(Box::new(desugared))
                } else {
                    None
                };

                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::IfLet {
                        pattern,
                        expr: Box::new(desugared_expr),
                        then: Box::new(desugared_then),
                        else_: desugared_else,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::WhileLet {
                pattern,
                expr,
                body,
            } => {
                let (desugared_expr, mut funcs1) = self.desugar_expr(*(expr.clone()));
                new_functions.append(&mut funcs1);
                let (desugared_body, mut funcs2) = self.desugar_expr(*body);
                new_functions.append(&mut funcs2);
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::WhileLet {
                        pattern,
                        expr: Box::new(desugared_expr),
                        body: Box::new(desugared_body),
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::Break(value) => {
                if let Some(val) = value {
                    let (desugared_val, mut funcs) = self.desugar_expr(*val);
                    new_functions.append(&mut funcs);
                    TypedExpr {
                        span: expr.span,
                        file: expr.file,
                        expr: TypedExprKind::Break(Some(Box::new(desugared_val))),
                        type_: expr.type_,
                    }
                } else {
                    TypedExpr {
                        span: expr.span,
                        file: expr.file,
                        expr: TypedExprKind::Break(None),
                        type_: expr.type_,
                    }
                }
            }

            TypedExprKind::Perform {
                effect,
                effect_id,
                args,
            } => {
                let mut new_args = Vec::new();
                for arg in args {
                    let (desugared_arg, mut funcs) = self.desugar_expr(arg);
                    new_functions.append(&mut funcs);
                    new_args.push(desugared_arg);
                }
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Perform {
                        effect,
                        effect_id,
                        args: new_args,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::Handle {
                body,
                handlers,
                return_type,
            } => {
                let (desugared_body, mut funcs1) = self.desugar_expr(*body);
                new_functions.append(&mut funcs1);

                let mut new_handlers = Vec::new();
                for handler in handlers {
                    let (desugared_handler_body, mut funcs) = self.desugar_expr(handler.body);
                    new_functions.append(&mut funcs);
                    new_handlers.push(TypedEffectHandler {
                        span: handler.span,
                        effect: handler.effect,
                        effect_id: handler.effect_id,
                        params: handler.params,
                        resume_param: handler.resume_param,
                        resume_id: handler.resume_id,
                        resume_type: handler.resume_type,
                        body: desugared_handler_body,
                    });
                }

                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::Handle {
                        body: Box::new(desugared_body),
                        handlers: new_handlers,
                        return_type,
                    },
                    type_: expr.type_,
                }
            }

            TypedExprKind::MacroCall {
                name,
                macro_id,
                args,
                delimiter,
            } => {
                let mut new_args = Vec::new();
                for arg in args {
                    let (desugared_arg, mut funcs) = self.desugar_expr(arg);
                    new_functions.append(&mut funcs);
                    new_args.push(desugared_arg);
                }
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::MacroCall {
                        name,
                        macro_id,
                        args: new_args,
                        delimiter,
                    },
                    type_: expr.type_,
                }
            }

            // Leave primitive values as they are
            TypedExprKind::Int(n) => TypedExpr {
                span: expr.span,
                file: expr.file,
                expr: TypedExprKind::Int(n),
                type_: expr.type_,
            },
            TypedExprKind::Float(f) => TypedExpr {
                span: expr.span,
                file: expr.file,
                expr: TypedExprKind::Float(f),
                type_: expr.type_,
            },
            TypedExprKind::Bool(b) => TypedExpr {
                span: expr.span,
                file: expr.file,
                expr: TypedExprKind::Bool(b),
                type_: expr.type_,
            },
            TypedExprKind::String(s) => TypedExpr {
                span: expr.span,
                file: expr.file,
                expr: TypedExprKind::String(s),
                type_: expr.type_,
            },
            TypedExprKind::Variable { name, binding_id } => TypedExpr {
                span: expr.span,
                file: expr.file,
                expr: TypedExprKind::Variable { name, binding_id },
                type_: expr.type_,
            },
            TypedExprKind::Continue => TypedExpr {
                span: expr.span,
                file: expr.file,
                expr: TypedExprKind::Continue,
                type_: expr.type_,
            },
            TypedExprKind::Error => TypedExpr {
                span: expr.span,
                file: expr.file,
                expr: TypedExprKind::Error,
                type_: expr.type_,
            },
            TypedExprKind::Import(import) => TypedExpr {
                span: expr.span,
                file: expr.file,
                expr: TypedExprKind::Import(import),
                type_: expr.type_,
            },
            TypedExprKind::StructConstruct {
                struct_name,
                struct_id,
                fields,
            } => {
                let mut new_fields = Vec::new();
                for (field_name, field_id, field_expr) in fields {
                    let (desugared_field_expr, mut funcs) = self.desugar_expr(field_expr);
                    new_functions.append(&mut funcs);
                    new_fields.push((field_name, field_id, desugared_field_expr));
                }
                TypedExpr {
                    span: expr.span,
                    file: expr.file,
                    expr: TypedExprKind::StructConstruct {
                        struct_name,
                        struct_id,
                        fields: new_fields,
                    },
                    type_: expr.type_,
                }
            }
        };

        (new_expr, new_functions)
    }

    fn create_function_from_lambda(
        &mut self,
        body: TypedExpr,
        args: Vec<TypedFnArg>,
        _captures: &[(Symbol, BindingId, Rc<Type>)],
        function_type: Rc<Type>,
    ) -> TypedFunction {
        // Generate a unique name for the lambda function
        let lambda_name = format!("lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;
        let name_sym = self.interner.intern(&lambda_name);

        // Extract return type from function type
        let return_type = match &function_type.type_ {
            TypeKind::Function { return_type, .. } => return_type.clone(),
            _ => {
                // Fallback to a fresh type variable if function type is unexpected
                Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Variable {
                        id: self.id_gen.fresh_type().0,
                        kind: Kind::Star,
                    },
                })
            }
        };

        // Create a new function ID for this lambda
        let function_id = self.id_gen.fresh_function();

        TypedFunction {
            span: body.span.clone(),
            file: body.file.clone(),
            vis: Visibility::Private,
            name: name_sym,
            function_id,
            type_params: vec![], // Lambdas don't have explicit type parameters
            args,
            return_type,
            where_constraints: vec![],
            effects: EffectSet::pure(), // Assume pure for now
            function_type,
            body: Some(body),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lambda_desugarer_new() {
        let interner = Interner::new();
        let desugarer = LambdaDesugarer::new(interner);

        assert_eq!(desugarer.lambda_counter, 0);
    }

    #[test]
    fn test_desugar_empty_program() {
        let interner = Interner::new();
        let mut desugarer = LambdaDesugarer::new(interner);

        let empty_program = vec![];
        let result = desugarer.desugar_program(empty_program);

        assert!(result.is_empty());
    }

    #[test]
    fn test_lambda_counter_increment() {
        let interner = Interner::new();
        let mut desugarer = LambdaDesugarer::new(interner);

        let initial_counter = desugarer.lambda_counter;
        desugarer.lambda_counter += 1;

        assert_eq!(desugarer.lambda_counter, initial_counter + 1);
    }
}
