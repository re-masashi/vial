use crate::ast::*;

pub struct MethodCallDesugarer;

impl Default for MethodCallDesugarer {
    fn default() -> Self {
        Self::new()
    }
}

impl MethodCallDesugarer {
    pub fn new() -> Self {
        Self
    }

    pub fn desugar_program(&mut self, program: Vec<ASTNode>) -> Vec<ASTNode> {
        let mut result = Vec::new();

        for node in program {
            match node.node {
                ASTNodeKind::Impl(ref impl_block) => {
                    // Transform impl blocks into standalone functions
                    let standalone_functions =
                        self.desugar_impl_to_functions(impl_block.clone(), &node);
                    result.extend(standalone_functions);
                }
                _ => {
                    result.push(self.desugar_node(node));
                }
            }
        }

        result
    }

    fn desugar_impl_to_functions(&mut self, impl_block: Impl, node: &ASTNode) -> Vec<ASTNode> {
        let type_name = impl_block.type_name.clone();

        impl_block
            .methods
            .into_iter()
            .map(|method| {
                // Transform method into standalone function
                // impl MyStruct { def foo(self, x) {} }
                // becomes
                // def MyStruct.foo(self, x) {}

                let new_name = format!("{}.{}", type_name, method.name);

                let desugared_body = method.body.map(desugar_expr);

                let new_function = Function {
                    span: method.span.clone(),
                    file: method.file.clone(),
                    vis: method.vis,
                    name: new_name,
                    type_params: method.type_params,
                    args: method.args,
                    return_type: method.return_type,
                    where_constraints: method.where_constraints,
                    effects: method.effects,
                    body: desugared_body,
                };

                ASTNode {
                    span: node.span.clone(),
                    file: node.file.clone(),
                    node: ASTNodeKind::Function(Box::new(new_function)),
                    attributes: node.attributes.clone(),
                }
            })
            .collect()
    }

    fn desugar_node(&mut self, node: ASTNode) -> ASTNode {
        match node.node {
            ASTNodeKind::Expr(expr) => {
                let new_expr = desugar_expr(expr);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Expr(new_expr),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::Function(func) => {
                let new_func = self.desugar_function(*func);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Function(Box::new(new_func)),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::Struct(s) => {
                let new_struct = self.desugar_struct(s);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Struct(new_struct),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::Enum(e) => {
                let new_enum = self.desugar_enum(e);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Enum(new_enum),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::Trait(t) => {
                let new_trait = self.desugar_trait(t);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Trait(new_trait),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::Impl(_) => {
                // Impl blocks are handled in desugar_program
                panic!("Impl block found in desugar_node - should be handled in desugar_program")
            }
            // Other node types remain unchanged
            _ => node,
        }
    }

    fn desugar_function(&mut self, func: Function) -> Function {
        let new_body = func.body.map(desugar_expr);

        Function {
            span: func.span,
            file: func.file,
            vis: func.vis,
            name: func.name,
            type_params: func.type_params,
            args: func.args,
            return_type: func.return_type,
            where_constraints: func.where_constraints,
            effects: func.effects,
            body: new_body,
        }
    }

    fn desugar_struct(&mut self, s: Struct) -> Struct {
        let new_methods = s
            .methods
            .into_iter()
            .map(|method| self.desugar_function(method))
            .collect();

        Struct {
            span: s.span,
            file: s.file,
            name: s.name,
            type_params: s.type_params,
            fields: s.fields,
            methods: new_methods,
            vis: s.vis,
        }
    }

    fn desugar_enum(&mut self, e: Enum) -> Enum {
        let new_methods = e
            .methods
            .into_iter()
            .map(|method| self.desugar_function(method))
            .collect();

        Enum {
            span: e.span,
            file: e.file,
            name: e.name,
            type_params: e.type_params,
            variants: e.variants,
            methods: new_methods,
            vis: e.vis,
        }
    }

    fn desugar_trait(&mut self, t: TraitDef) -> TraitDef {
        let new_methods = t
            .methods
            .into_iter()
            .map(|method| self.desugar_function(method))
            .collect();

        TraitDef {
            span: t.span,
            name: t.name,
            type_params: t.type_params,
            super_traits: t.super_traits,
            methods: new_methods,
            associated_types: t.associated_types,
        }
    }
}

fn desugar_expr(expr: Expr) -> Expr {
    let new_expr_kind = match expr.expr {
        ExprKind::Call(function, args) => {
            // Check if function is a FieldAccess (method call pattern)
            if let ExprKind::FieldAccess(target, field) = function.expr {
                // This is a method call: target.field(args)

                let desugared_receiver = desugar_expr(*target);
                let desugared_args: Vec<Expr> = args.into_iter().map(desugar_expr).collect();

                // Transform to: field(receiver, ...args)
                let mut all_args = vec![desugared_receiver];
                all_args.extend(desugared_args);

                // The method is just the field name as a variable
                ExprKind::Call(
                    Box::new(Expr {
                        span: function.span.clone(),
                        file: function.file.clone(),
                        expr: ExprKind::Variable(field),
                    }),
                    all_args,
                )
            } else {
                // Regular function call (not a method call)
                let desugared_function = desugar_expr(*function);
                let desugared_args = args.into_iter().map(desugar_expr).collect();

                ExprKind::Call(Box::new(desugared_function), desugared_args)
            }
        }

        ExprKind::Block(expressions) => {
            let new_expressions = expressions.into_iter().map(desugar_expr).collect();
            ExprKind::Block(new_expressions)
        }

        ExprKind::Let {
            var,
            type_annot,
            value,
        } => {
            let desugared_value = desugar_expr(*value);
            ExprKind::Let {
                var,
                type_annot,
                value: Box::new(desugared_value),
            }
        }

        ExprKind::IfElse {
            condition,
            then,
            else_,
        } => {
            let desugared_condition = desugar_expr(*condition);
            let desugared_then = desugar_expr(*then);
            let desugared_else = else_.map(|e| Box::new(desugar_expr(*e)));

            ExprKind::IfElse {
                condition: Box::new(desugared_condition),
                then: Box::new(desugared_then),
                else_: desugared_else,
            }
        }

        ExprKind::Match(scrutinee, arms) => {
            let desugared_scrutinee = desugar_expr(*scrutinee);
            let new_arms = arms
                .into_iter()
                .map(|arm| MatchArm {
                    pattern: desugar_pattern(arm.pattern),
                    guard: arm.guard.map(desugar_expr),
                    body: Box::new(desugar_expr(*arm.body)),
                    span: arm.span,
                })
                .collect();

            ExprKind::Match(Box::new(desugared_scrutinee), new_arms)
        }

        ExprKind::BinOp(left, op, right) => {
            let desugared_left = desugar_expr(*left);
            let desugared_right = desugar_expr(*right);
            ExprKind::BinOp(Box::new(desugared_left), op, Box::new(desugared_right))
        }

        ExprKind::UnOp(op, operand) => {
            let desugared_operand = desugar_expr(*operand);
            ExprKind::UnOp(op, Box::new(desugared_operand))
        }

        ExprKind::Array(elements) => {
            let new_elements = elements.into_iter().map(desugar_expr).collect();
            ExprKind::Array(new_elements)
        }

        ExprKind::Tuple(elements) => {
            let new_elements = elements.into_iter().map(desugar_expr).collect();
            ExprKind::Tuple(new_elements)
        }

        ExprKind::FieldAccess(target, field) => {
            let desugared_target = desugar_expr(*target);
            ExprKind::FieldAccess(Box::new(desugared_target), field)
        }

        ExprKind::OptionalChain(target, field) => {
            let desugared_target = desugar_expr(*target);
            ExprKind::OptionalChain(Box::new(desugared_target), field)
        }

        ExprKind::Index(target, index) => {
            let desugared_target = desugar_expr(*target);
            let desugared_index = desugar_expr(*index);
            ExprKind::Index(Box::new(desugared_target), Box::new(desugared_index))
        }

        ExprKind::Return(value) => {
            let desugared_value = value.map(|v| Box::new(desugar_expr(*v)));
            ExprKind::Return(desugared_value)
        }

        ExprKind::Break(value) => {
            let desugared_value = value.map(|v| Box::new(desugar_expr(*v)));
            ExprKind::Break(desugared_value)
        }

        ExprKind::Assign { l_val, r_val, op } => {
            let desugared_l_val = desugar_expr(*l_val);
            let desugared_r_val = desugar_expr(*r_val);
            ExprKind::Assign {
                l_val: Box::new(desugared_l_val),
                r_val: Box::new(desugared_r_val),
                op,
            }
        }

        ExprKind::Map(entries) => {
            let new_entries = entries
                .into_iter()
                .map(|(k, v)| (desugar_expr(k), desugar_expr(v)))
                .collect();
            ExprKind::Map(new_entries)
        }

        ExprKind::EnumConstruct {
            name,
            variant,
            args,
        } => {
            let new_args = args.into_iter().map(desugar_expr).collect();
            ExprKind::EnumConstruct {
                name,
                variant,
                args: new_args,
            }
        }

        ExprKind::StructConstruct { name, fields } => {
            let new_fields = fields
                .into_iter()
                .map(|(field_name, expr)| (field_name, desugar_expr(expr)))
                .collect();
            ExprKind::StructConstruct {
                name,
                fields: new_fields,
            }
        }

        ExprKind::Cast { expr, target_type } => {
            let desugared_expr = desugar_expr(*expr);
            ExprKind::Cast {
                expr: Box::new(desugared_expr),
                target_type,
            }
        }

        ExprKind::With { context, var, body } => {
            let desugared_context = desugar_expr(*context);
            let desugared_body = desugar_expr(*body);
            ExprKind::With {
                context: Box::new(desugared_context),
                var,
                body: Box::new(desugared_body),
            }
        }

        ExprKind::Loop { label, body } => {
            let desugared_body = desugar_expr(*body);
            ExprKind::Loop {
                label,
                body: Box::new(desugared_body),
            }
        }

        ExprKind::For {
            iterator,
            value,
            expression,
        } => {
            let desugared_iterator = desugar_expr(*iterator);
            let desugared_body = desugar_expr(*expression);
            ExprKind::For {
                iterator: Box::new(desugared_iterator),
                value,
                expression: Box::new(desugared_body),
            }
        }

        ExprKind::While(condition, body) => {
            let desugared_condition = desugar_expr(*condition);
            let desugared_body = desugar_expr(*body);
            ExprKind::While(Box::new(desugared_condition), Box::new(desugared_body))
        }

        ExprKind::IfLet {
            pattern,
            expr,
            then,
            else_,
        } => {
            let desugared_pattern = desugar_pattern(pattern);
            let desugared_expr = desugar_expr(*expr);
            let desugared_then = desugar_expr(*then);
            let desugared_else = else_.map(|e| Box::new(desugar_expr(*e)));

            ExprKind::IfLet {
                pattern: desugared_pattern,
                expr: Box::new(desugared_expr),
                then: Box::new(desugared_then),
                else_: desugared_else,
            }
        }

        ExprKind::WhileLet {
            pattern,
            expr,
            body,
        } => {
            let desugared_pattern = desugar_pattern(pattern);
            let desugared_expr = desugar_expr(*expr);
            let desugared_body = desugar_expr(*body);

            ExprKind::WhileLet {
                pattern: desugared_pattern,
                expr: Box::new(desugared_expr),
                body: Box::new(desugared_body),
            }
        }

        ExprKind::Perform { effect, args } => {
            let new_args = args.into_iter().map(desugar_expr).collect();
            ExprKind::Perform {
                effect,
                args: new_args,
            }
        }

        ExprKind::Handle { body, handlers } => {
            let desugared_body = desugar_expr(*body);
            let new_handlers = handlers
                .into_iter()
                .map(|handler| EffectHandler {
                    span: handler.span,
                    effect: handler.effect,
                    params: handler.params,
                    resume_param: handler.resume_param,
                    body: desugar_expr(handler.body),
                })
                .collect();

            ExprKind::Handle {
                body: Box::new(desugared_body),
                handlers: new_handlers,
            }
        }

        ExprKind::MacroCall(name, args, delimiter) => {
            // Macro desugaring now happens in untyped validation to ensure
            // top-level macro calls are properly desugared before main function creation
            let new_args = args.iter().cloned().map(desugar_expr).collect();
            ExprKind::MacroCall(name, new_args, delimiter)
        }

        ExprKind::Lambda { args, expression } => {
            let desugared_body = desugar_expr(*expression);
            ExprKind::Lambda {
                args,
                expression: Box::new(desugared_body),
            }
        }

        // Primitive values - no desugaring needed
        ExprKind::Int(n) => ExprKind::Int(n),
        ExprKind::Float(f) => ExprKind::Float(f),
        ExprKind::Bool(b) => ExprKind::Bool(b),
        ExprKind::String(s) => ExprKind::String(s),
        ExprKind::Variable(name) => ExprKind::Variable(name),
        ExprKind::Continue => ExprKind::Continue,
        ExprKind::Error => ExprKind::Error,
        ExprKind::Import(import) => ExprKind::Import(import),
    };

    Expr {
        span: expr.span,
        file: expr.file,
        expr: new_expr_kind,
    }
}

fn desugar_pattern(pattern: Pattern) -> Pattern {
    let new_pat = match pattern.pat {
        PatKind::Array(patterns) => {
            let new_patterns = patterns.into_iter().map(desugar_pattern).collect();
            PatKind::Array(new_patterns)
        }

        PatKind::Tuple(patterns) => {
            let new_patterns = patterns.into_iter().map(desugar_pattern).collect();
            PatKind::Tuple(new_patterns)
        }

        PatKind::Or(patterns) => {
            let new_patterns = patterns.into_iter().map(desugar_pattern).collect();
            PatKind::Or(new_patterns)
        }

        PatKind::As { name, pattern } => {
            let desugared_pattern = desugar_pattern(*pattern);
            PatKind::As {
                name,
                pattern: Box::new(desugared_pattern),
            }
        }

        PatKind::Struct { name, fields } => {
            let new_fields = fields
                .into_iter()
                .map(|(field_name, field_pattern)| (field_name, desugar_pattern(field_pattern)))
                .collect();
            PatKind::Struct {
                name,
                fields: new_fields,
            }
        }

        PatKind::Enum {
            name,
            variant,
            params,
        } => {
            let new_params = params.into_iter().map(desugar_pattern).collect();
            PatKind::Enum {
                name,
                variant,
                params: new_params,
            }
        }

        // Patterns that don't need desugaring
        PatKind::Wildcard => PatKind::Wildcard,
        PatKind::Bind(name) => PatKind::Bind(name),
        PatKind::Literal(lit) => PatKind::Literal(lit),
        PatKind::Range(start, end) => PatKind::Range(start, end),
        PatKind::Rest(name) => PatKind::Rest(name),
        PatKind::Error => PatKind::Error,
    };

    Pattern {
        span: pattern.span,
        file: pattern.file,
        pat: new_pat,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_method_call_desugarer_new() {
        let mut desugarer = MethodCallDesugarer::new();
        // This test verifies the constructor works by trying to use the instance
        let empty_program = vec![];
        let result = desugarer.desugar_program(empty_program);
        assert!(result.is_empty());
    }

    #[test]
    fn test_method_call_desugarer_default() {
        let mut desugarer = MethodCallDesugarer::default();
        // This test verifies the default constructor works by trying to use the instance
        let empty_program = vec![];
        let result = desugarer.desugar_program(empty_program);
        assert!(result.is_empty());
    }

    #[test]
    fn test_desugar_empty_program() {
        let mut desugarer = MethodCallDesugarer::new();
        let empty_program = vec![];
        let result = desugarer.desugar_program(empty_program);

        assert!(result.is_empty());
    }
}
