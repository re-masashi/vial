// USELESS FILE, BUT MIGHT BE HELPFUL IN THE FUTURE

use crate::ast::*;

pub struct ArrayPatternDesugarer;

impl Default for ArrayPatternDesugarer {
    fn default() -> Self {
        Self::new()
    }
}

impl ArrayPatternDesugarer {
    pub fn new() -> Self {
        Self
    }

    pub fn desugar_program(self, program: Vec<ASTNode>) -> Vec<ASTNode> {
        program.into_iter().map(Self::desugar_node).collect()
    }

    fn desugar_node(node: ASTNode) -> ASTNode {
        match node.node {
            ASTNodeKind::Expr(expr) => {
                let new_expr = Self::desugar_expr(expr);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Expr(new_expr),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::Function(func) => {
                let new_func = Self::desugar_function(*func);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Function(Box::new(new_func)),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::Struct(s) => {
                let new_struct = Self::desugar_struct(s);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Struct(new_struct),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::Enum(e) => {
                let new_enum = Self::desugar_enum(e);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Enum(new_enum),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::Trait(t) => {
                let new_trait = Self::desugar_trait(t);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Trait(new_trait),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::EffectDef(effect) => {
                let new_effect = Self::desugar_effect_def(effect);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::EffectDef(new_effect),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::ExternFunction(extern_func) => {
                let new_extern_func = Self::desugar_extern_function(*extern_func);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::ExternFunction(Box::new(new_extern_func)),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::MacroDef(macro_def) => {
                let new_macro = Self::desugar_macro_def(macro_def);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::MacroDef(new_macro),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::Impl(impl_block) => {
                let new_impl = Self::desugar_impl(impl_block);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Impl(new_impl),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::TypeAlias(alias) => {
                let new_alias = Self::desugar_type_alias(alias);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::TypeAlias(new_alias),
                    attributes: node.attributes,
                }
            }
            _ => node,
        }
    }

    fn desugar_function(func: Function) -> Function {
        let new_body = func.body.map(Self::desugar_expr);

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

    fn desugar_struct(s: Struct) -> Struct {
        let new_methods = s.methods.into_iter().map(Self::desugar_function).collect();

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

    fn desugar_enum(e: Enum) -> Enum {
        let new_methods = e.methods.into_iter().map(Self::desugar_function).collect();

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

    fn desugar_trait(t: TraitDef) -> TraitDef {
        let new_methods = t.methods.into_iter().map(Self::desugar_function).collect();

        TraitDef {
            span: t.span,
            name: t.name,
            type_params: t.type_params,
            super_traits: t.super_traits,
            methods: new_methods,
            associated_types: t.associated_types,
        }
    }

    fn desugar_effect_def(effect: EffectDef) -> EffectDef {
        EffectDef {
            span: effect.span,
            file: effect.file,
            vis: effect.vis,
            name: effect.name,
            type_params: effect.type_params,
            operations: effect.operations,
            where_constraints: effect.where_constraints,
        }
    }

    fn desugar_extern_function(extern_func: ExternFunction) -> ExternFunction {
        let new_args = extern_func.args;
        ExternFunction {
            span: extern_func.span,
            file: extern_func.file,
            vis: extern_func.vis,
            name: extern_func.name,
            type_params: extern_func.type_params,
            args: new_args,
            return_type: extern_func.return_type,
            where_constraints: extern_func.where_constraints,
            effects: extern_func.effects,
            library: extern_func.library,
            symbol_name: extern_func.symbol_name,
        }
    }

    fn desugar_macro_def(macro_def: MacroDef) -> MacroDef {
        MacroDef {
            span: macro_def.span,
            file: macro_def.file,
            name: macro_def.name,
            rules: macro_def.rules,
            hygiene: macro_def.hygiene,
        }
    }

    fn desugar_impl(impl_block: Impl) -> Impl {
        let new_methods = impl_block
            .methods
            .into_iter()
            .map(Self::desugar_function)
            .collect();

        Impl {
            span: impl_block.span,
            file: impl_block.file,
            type_name: impl_block.type_name,
            type_params: impl_block.type_params,
            methods: new_methods,
            trait_: impl_block.trait_,
            where_constraints: impl_block.where_constraints,
        }
    }

    fn desugar_type_alias(alias: TypeAlias) -> TypeAlias {
        TypeAlias {
            span: alias.span,
            file: alias.file,
            name: alias.name,
            type_params: alias.type_params,
            target_type: alias.target_type,
            where_constraints: alias.where_constraints,
        }
    }

    fn desugar_expr(expr: Expr) -> Expr {
        let new_expr_kind = match expr.expr {
            ExprKind::Match(scrutinee, arms) => {
                let desugared_scrutinee = Self::desugar_expr(*scrutinee);
                let new_arms = arms
                    .into_iter()
                    .map(|arm| MatchArm {
                        pattern: desugar_pattern(arm.pattern),
                        guard: arm.guard.map(Self::desugar_expr),
                        body: Box::new(Self::desugar_expr(*arm.body)),
                        span: arm.span,
                    })
                    .collect();

                ExprKind::Match(Box::new(desugared_scrutinee), new_arms)
            }

            ExprKind::IfLet {
                pattern,
                expr: scrutinee,
                then,
                else_,
            } => {
                let desugared_pattern = desugar_pattern(pattern);
                let desugared_scrutinee = Self::desugar_expr(*scrutinee);
                let desugared_then = Self::desugar_expr(*then);
                let desugared_else = else_.map(|e| Box::new(Self::desugar_expr(*e)));

                ExprKind::IfLet {
                    pattern: desugared_pattern,
                    expr: Box::new(desugared_scrutinee),
                    then: Box::new(desugared_then),
                    else_: desugared_else,
                }
            }

            ExprKind::WhileLet {
                pattern,
                expr: scrutinee,
                body,
            } => {
                let desugared_pattern = desugar_pattern(pattern);
                let desugared_scrutinee = Self::desugar_expr(*scrutinee);
                let desugared_body = Self::desugar_expr(*body);

                ExprKind::WhileLet {
                    pattern: desugared_pattern,
                    expr: Box::new(desugared_scrutinee),
                    body: Box::new(desugared_body),
                }
            }

            // Handle other expressions recursively
            ExprKind::Block(expressions) => {
                let new_expressions = expressions.into_iter().map(Self::desugar_expr).collect();
                ExprKind::Block(new_expressions)
            }

            ExprKind::Let {
                var,
                type_annot,
                value,
            } => {
                let desugared_value = Self::desugar_expr(*value);
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
                let desugared_condition = Self::desugar_expr(*condition);
                let desugared_then = Self::desugar_expr(*then);
                let desugared_else = else_.map(|e| Box::new(Self::desugar_expr(*e)));

                ExprKind::IfElse {
                    condition: Box::new(desugared_condition),
                    then: Box::new(desugared_then),
                    else_: desugared_else,
                }
            }

            ExprKind::BinOp(left, op, right) => {
                let desugared_left = Self::desugar_expr(*left);
                let desugared_right = Self::desugar_expr(*right);
                ExprKind::BinOp(Box::new(desugared_left), op, Box::new(desugared_right))
            }

            ExprKind::UnOp(op, operand) => {
                let desugared_operand = Self::desugar_expr(*operand);
                ExprKind::UnOp(op, Box::new(desugared_operand))
            }

            ExprKind::Array(elements) => {
                let new_elements = elements.into_iter().map(Self::desugar_expr).collect();
                ExprKind::Array(new_elements)
            }

            ExprKind::Tuple(elements) => {
                let new_elements = elements.into_iter().map(Self::desugar_expr).collect();
                ExprKind::Tuple(new_elements)
            }

            ExprKind::FieldAccess(target, field) => {
                let desugared_target = Self::desugar_expr(*target);
                ExprKind::FieldAccess(Box::new(desugared_target), field)
            }

            ExprKind::OptionalChain(target, field) => {
                let desugared_target = Self::desugar_expr(*target);
                ExprKind::OptionalChain(Box::new(desugared_target), field)
            }

            ExprKind::Index(target, index) => {
                let desugared_target = Self::desugar_expr(*target);
                let desugared_index = Self::desugar_expr(*index);
                ExprKind::Index(Box::new(desugared_target), Box::new(desugared_index))
            }

            ExprKind::Return(value) => {
                let desugared_value = value.map(|v| Box::new(Self::desugar_expr(*v)));
                ExprKind::Return(desugared_value)
            }

            ExprKind::Break(value) => {
                let desugared_value = value.map(|v| Box::new(Self::desugar_expr(*v)));
                ExprKind::Break(desugared_value)
            }

            ExprKind::Assign { l_val, r_val, op } => {
                let desugared_l_val = Self::desugar_expr(*l_val);
                let desugared_r_val = Self::desugar_expr(*r_val);
                ExprKind::Assign {
                    l_val: Box::new(desugared_l_val),
                    r_val: Box::new(desugared_r_val),
                    op,
                }
            }

            ExprKind::Map(entries) => {
                let new_entries = entries
                    .into_iter()
                    .map(|(k, v)| (Self::desugar_expr(k), Self::desugar_expr(v)))
                    .collect();
                ExprKind::Map(new_entries)
            }

            ExprKind::EnumConstruct {
                name,
                variant,
                args,
            } => {
                let new_args = args.into_iter().map(Self::desugar_expr).collect();
                ExprKind::EnumConstruct {
                    name,
                    variant,
                    args: new_args,
                }
            }

            ExprKind::StructConstruct { name, fields } => {
                let new_fields = fields
                    .into_iter()
                    .map(|(field_name, expr)| (field_name, Self::desugar_expr(expr)))
                    .collect();
                ExprKind::StructConstruct {
                    name,
                    fields: new_fields,
                }
            }

            ExprKind::Call(function, args) => {
                let new_function = Self::desugar_expr(*function);
                let new_args = args.into_iter().map(Self::desugar_expr).collect();
                ExprKind::Call(Box::new(new_function), new_args)
            }

            ExprKind::Cast { expr, target_type } => {
                let desugared_expr = Self::desugar_expr(*expr);
                ExprKind::Cast {
                    expr: Box::new(desugared_expr),
                    target_type,
                }
            }

            ExprKind::With { context, var, body } => {
                let desugared_context = Self::desugar_expr(*context);
                let desugared_body = Self::desugar_expr(*body);
                ExprKind::With {
                    context: Box::new(desugared_context),
                    var,
                    body: Box::new(desugared_body),
                }
            }

            ExprKind::Loop { label, body } => {
                let desugared_body = Self::desugar_expr(*body);
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
                let desugared_iterator = Self::desugar_expr(*iterator);
                let desugared_body = Self::desugar_expr(*expression);
                ExprKind::For {
                    iterator: Box::new(desugared_iterator),
                    value,
                    expression: Box::new(desugared_body),
                }
            }

            ExprKind::While(condition, body) => {
                let desugared_condition = Self::desugar_expr(*condition);
                let desugared_body = Self::desugar_expr(*body);
                ExprKind::While(Box::new(desugared_condition), Box::new(desugared_body))
            }

            ExprKind::Perform { effect, args } => {
                let new_args = args.into_iter().map(Self::desugar_expr).collect();
                ExprKind::Perform {
                    effect,
                    args: new_args,
                }
            }

            ExprKind::Handle { body, handlers } => {
                let desugared_body = Self::desugar_expr(*body);
                let new_handlers = handlers
                    .into_iter()
                    .map(|handler| EffectHandler {
                        span: handler.span,
                        effect: handler.effect,
                        params: handler.params,
                        resume_param: handler.resume_param,
                        body: Self::desugar_expr(handler.body),
                    })
                    .collect();

                ExprKind::Handle {
                    body: Box::new(desugared_body),
                    handlers: new_handlers,
                }
            }

            ExprKind::MacroCall(name, args, delimiter) => {
                let new_args = args.iter().cloned().map(Self::desugar_expr).collect();
                ExprKind::MacroCall(name, new_args, delimiter)
            }

            ExprKind::Lambda { args, expression } => {
                let desugared_body = Self::desugar_expr(*expression);
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
            ExprKind::Spread(spread_expr) => {
                ExprKind::Spread(Box::new(Self::desugar_expr(*spread_expr)))
            }
        };

        Expr {
            span: expr.span,
            file: expr.file,
            expr: new_expr_kind,
        }
    }
}

fn desugar_pattern(pattern: Pattern) -> Pattern {
    let new_pat = match pattern.pat {
        PatKind::Array(elements) => {
            // For now, we just recursively desugar the patterns
            let new_elements = elements
                .into_iter()
                .map(|element| match element {
                    ArrayPatElement::Pattern(pattern) => {
                        ArrayPatElement::Pattern(desugar_pattern(pattern))
                    }
                    ArrayPatElement::Spread(pattern) => {
                        ArrayPatElement::Spread(desugar_pattern(pattern))
                    }
                })
                .collect();
            PatKind::Array(new_elements)
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
