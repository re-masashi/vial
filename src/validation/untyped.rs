use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

use super::imports::*;
use crate::ast::*;
use crate::error::{ErrorReporter, ValidationErrorKind, VialError};

#[derive(Debug)]
pub struct UntypedValidator {
    defined_items: HashMap<String, Range<usize>>,
    module_resolver: ModuleResolver,
    macros: HashMap<String, MacroDef>,
    in_loop: bool,
    in_function: bool,
    in_user_function: bool, // either in user-defined function (true) vs auto-generated main (fals)
    current_file: String,

    pub diagnostics: ErrorReporter,
}

impl UntypedValidator {
    pub fn new(project_root: PathBuf) -> Self {
        Self {
            defined_items: HashMap::new(),
            module_resolver: ModuleResolver::new(&project_root),
            macros: HashMap::new(),
            in_loop: false,
            in_function: false,
            in_user_function: false,
            current_file: String::new(),
            diagnostics: ErrorReporter::new(),
        }
    }

    pub fn validate(&mut self, nodes: Vec<ASTNode>, current_file: &Path) -> Vec<ASTNode> {
        self.current_file = current_file.display().to_string();

        self.collect_macros(&nodes);

        let expanded = self.expand_macros(nodes);

        let desugared = self.desugar(expanded);

        let with_imports = self
            .module_resolver
            .resolve_imports(desugared, current_file);

        let validated = self.validate_nodes(with_imports);

        self.diagnostics
            .errors
            .append(&mut self.module_resolver.diagnostics.errors);
        self.diagnostics
            .warnings
            .append(&mut self.module_resolver.diagnostics.warnings);

        validated
    }

    fn desugar(&mut self, nodes: Vec<ASTNode>) -> Vec<ASTNode> {
        let mut exprs = Vec::new();
        let mut other_nodes = Vec::new();

        for node in nodes {
            match node.node {
                ASTNodeKind::Expr(expr) => {
                    match expr.expr {
                        ExprKind::Import(_) => {
                            // `import` expressions should NEVER be treated as "executable" (idk a better term for this) expressions
                            // they should remain as separate nodes to be processed during import resolution
                            other_nodes.push(ASTNode {
                                span: node.span.clone(),
                                file: node.file.clone(),
                                node: ASTNodeKind::Expr(expr),
                                attributes: node.attributes.clone(),
                            });
                        }
                        _ => {
                            exprs.push(expr);
                        }
                    }
                }
                ASTNodeKind::Function(ref func_box) => {
                    if func_box.as_ref().name == "main" {
                        // mangle the explicit main function name to avoid conflicts
                        let mangled_func = Function {
                            name: "$mangled_main$".to_string(),
                            ..func_box.as_ref().clone()
                        };
                        other_nodes.push(ASTNode {
                            span: node.span.clone(),
                            file: node.file.clone(),
                            node: ASTNodeKind::Function(Box::new(mangled_func)),
                            attributes: node.attributes.clone(),
                        });
                    } else {
                        other_nodes.push(node);
                    }
                }
                ASTNodeKind::Struct(ref s) => {
                    let (clean_struct, impl_block) = self.desugar_struct(s.clone(), &node);

                    other_nodes.push(ASTNode {
                        span: node.span.clone(),
                        file: node.file.clone(),
                        node: ASTNodeKind::Struct(clean_struct),
                        attributes: node.attributes.clone(),
                    });

                    if let Some(impl_node) = impl_block {
                        other_nodes.push(impl_node);
                    }
                }

                ASTNodeKind::Enum(ref e) => {
                    let (clean_enum, impl_block) = self.desugar_enum(e.clone(), &node);

                    other_nodes.push(ASTNode {
                        span: node.span.clone(),
                        file: node.file.clone(),
                        node: ASTNodeKind::Enum(clean_enum),
                        attributes: node.attributes.clone(),
                    });

                    if let Some(impl_node) = impl_block {
                        other_nodes.push(impl_node);
                    }
                }

                _ => other_nodes.push(node),
            }
        }

        // CReate a main function only if there are top-level expressions
        // or maybe i can create a main with just `return 0`?
        if !exprs.is_empty() {
            let main_function = self.create_main_function(exprs);
            other_nodes.push(main_function);
        }

        other_nodes
    }

    fn create_main_function(&self, exprs: Vec<Expr>) -> ASTNode {
        let block_expr = Expr {
            span: if exprs.is_empty() {
                0..0
            } else {
                exprs[0].span.clone()
            },
            file: if exprs.is_empty() {
                String::new()
            } else {
                exprs[0].file.clone()
            },
            expr: ExprKind::Block(exprs.clone()),
        };

        let function = Function {
            span: if exprs.is_empty() {
                0..0
            } else {
                exprs[0].span.clone()
            },
            file: if exprs.is_empty() {
                String::new()
            } else {
                exprs[0].file.clone()
            },
            vis: Visibility::Public, // main should always be public ig
            name: "main".to_string(),
            type_params: vec![],
            args: vec![],
            return_type: None,
            where_constraints: vec![],
            effects: EffectAnnot::pure(),
            body: Some(block_expr),
        };

        ASTNode {
            span: if exprs.is_empty() {
                0..0
            } else {
                exprs[0].span.clone()
            },
            file: if exprs.is_empty() {
                String::new()
            } else {
                exprs[0].file.clone()
            },
            node: ASTNodeKind::Function(Box::new(function)),
            attributes: vec![],
        }
    }

    fn desugar_struct(&mut self, struct_def: Struct, node: &ASTNode) -> (Struct, Option<ASTNode>) {
        if struct_def.methods.is_empty() {
            return (struct_def, None);
        }

        // check for duplicate methodnames
        let mut seen_methods: HashMap<String, Range<usize>> = HashMap::new();
        for method in &struct_def.methods {
            if let Some(existing) = seen_methods.get(&method.name) {
                self.diagnostics.add_error(VialError::ValidationError {
                    span: method.span.clone(),
                    file: method.file.clone(),
                    kind: ValidationErrorKind::DuplicateDefinition {
                        name: method.name.clone(),
                        first_defined: existing.clone(),
                    },
                });
            } else {
                seen_methods.insert(method.name.clone(), method.span.clone());
            }
        }

        // extract methods into impl block
        let impl_block = Impl {
            span: struct_def.span.clone(),
            file: struct_def.file.clone(),
            type_name: struct_def.name.clone(),
            type_params: struct_def.type_params.clone(),
            methods: struct_def.methods.clone(),
            trait_: None,
            where_constraints: vec![],
        };

        let clean_struct = Struct {
            span: struct_def.span,
            file: struct_def.file,
            name: struct_def.name,
            type_params: struct_def.type_params,
            fields: struct_def.fields,
            methods: vec![], // all extracted into sep impls
            vis: struct_def.vis,
        };

        let impl_node = ASTNode {
            span: node.span.clone(),
            file: node.file.clone(),
            node: ASTNodeKind::Impl(impl_block),
            attributes: vec![],
        };

        (clean_struct, Some(impl_node))
    }

    fn desugar_enum(&mut self, enum_def: Enum, node: &ASTNode) -> (Enum, Option<ASTNode>) {
        if enum_def.methods.is_empty() {
            return (enum_def, None);
        }

        // check for duplicate method names
        let mut seen_methods: HashMap<String, Range<usize>> = HashMap::new();
        for method in &enum_def.methods {
            if let Some(existing) = seen_methods.get(&method.name) {
                self.diagnostics.add_error(VialError::ValidationError {
                    span: method.span.clone(),
                    file: method.file.clone(),
                    kind: ValidationErrorKind::DuplicateDefinition {
                        name: method.name.clone(),
                        first_defined: existing.clone(),
                    },
                });
            } else {
                seen_methods.insert(method.name.clone(), method.span.clone());
            }
        }

        // extract methods into impl block
        let impl_block = Impl {
            span: enum_def.span.clone(),
            file: enum_def.file.clone(),
            type_name: enum_def.name.clone(),
            type_params: enum_def.type_params.clone(),
            methods: enum_def.methods.clone(),
            trait_: None,
            where_constraints: vec![],
        };

        let clean_enum = Enum {
            span: enum_def.span,
            file: enum_def.file,
            name: enum_def.name,
            type_params: enum_def.type_params,
            variants: enum_def.variants,
            methods: vec![],
            vis: enum_def.vis,
        };

        let impl_node = ASTNode {
            span: node.span.clone(),
            file: node.file.clone(),
            node: ASTNodeKind::Impl(impl_block),
            attributes: vec![],
        };

        (clean_enum, Some(impl_node))
    }

    fn collect_macros(&mut self, nodes: &[ASTNode]) {
        for node in nodes {
            if let ASTNodeKind::MacroDef(macro_def) = &node.node {
                if let Some(existing) = self.macros.get(&macro_def.name) {
                    self.diagnostics.add_error(VialError::ValidationError {
                        span: macro_def.span.clone(),
                        file: macro_def.file.clone(),
                        kind: ValidationErrorKind::DuplicateDefinition {
                            name: macro_def.name.clone(),
                            first_defined: existing.span.clone(),
                        },
                    });
                } else {
                    self.macros
                        .insert(macro_def.name.clone(), macro_def.clone());
                }
            }
        }
    }

    fn expand_macros(&mut self, nodes: Vec<ASTNode>) -> Vec<ASTNode> {
        nodes
            .into_iter()
            .flat_map(|node| self.expand_node(node))
            .collect()
    }

    fn expand_node(&mut self, node: ASTNode) -> Vec<ASTNode> {
        match node.node {
            ASTNodeKind::Expr(expr) => {
                let expanded = self.expand_expr(expr);
                vec![ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Expr(expanded),
                    attributes: node.attributes,
                }]
            }
            ASTNodeKind::MacroDef(_) => {
                // idk what to do with it
                vec![]
            }
            _ => vec![node],
        }
    }

    fn expand_expr(&mut self, expr: Expr) -> Expr {
        match expr.expr {
            ExprKind::MacroCall(name, _args, _delimiter) => {
                if let Some(_macro_def) = self.macros.get(&name) {
                    // TODO: Actual macro expansion
                    self.diagnostics.add_error(VialError::ValidationError {
                        span: expr.span.clone(),
                        file: expr.file.clone(),
                        kind: ValidationErrorKind::MacroExpansionFailed {
                            name: name.clone(),
                            reason: "Macro expansion not yet implemented".to_string(),
                        },
                    });
                    Expr {
                        span: expr.span,
                        file: expr.file,
                        expr: ExprKind::Error,
                    }
                } else {
                    self.diagnostics.add_error(VialError::ValidationError {
                        span: expr.span.clone(),
                        file: expr.file.clone(),
                        kind: ValidationErrorKind::MacroNotFound { name },
                    });
                    Expr {
                        span: expr.span,
                        file: expr.file,
                        expr: ExprKind::Error,
                    }
                }
            }

            ExprKind::Block(exprs) => {
                let expanded: Vec<_> = exprs.into_iter().map(|e| self.expand_expr(e)).collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Block(expanded),
                }
            }

            ExprKind::IfElse {
                condition,
                then,
                else_,
            } => Expr {
                span: expr.span,
                file: expr.file,
                expr: ExprKind::IfElse {
                    condition: Box::new(self.expand_expr(*condition)),
                    then: Box::new(self.expand_expr(*then)),
                    else_: else_.map(|e| Box::new(self.expand_expr(*e))),
                },
            },

            // Expand other expression types
            ExprKind::BinOp(left, op, right) => {
                let expanded_left = self.expand_expr(*left);
                let expanded_right = self.expand_expr(*right);
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::BinOp(
                        Box::new(expanded_left),
                        op.clone(),
                        Box::new(expanded_right),
                    ),
                }
            }
            ExprKind::UnOp(op, operand) => {
                let expanded_operand = self.expand_expr(*operand);
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::UnOp(op.clone(), Box::new(expanded_operand)),
                }
            }
            ExprKind::Call(func, args) => {
                let expanded_func = self.expand_expr(*func);
                let expanded_args: Vec<_> =
                    args.into_iter().map(|arg| self.expand_expr(arg)).collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Call(Box::new(expanded_func), expanded_args),
                }
            }
            ExprKind::FieldAccess(target, field) => {
                let expanded_target = self.expand_expr(*target);
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::FieldAccess(Box::new(expanded_target), field),
                }
            }
            ExprKind::Index(target, index) => {
                let expanded_target = self.expand_expr(*target);
                let expanded_index = self.expand_expr(*index);
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Index(Box::new(expanded_target), Box::new(expanded_index)),
                }
            }
            ExprKind::Array(elements) => {
                let expanded_elements: Vec<_> = elements
                    .into_iter()
                    .map(|elem| self.expand_expr(elem))
                    .collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Array(expanded_elements),
                }
            }
            ExprKind::Tuple(elements) => {
                let expanded_elements: Vec<_> = elements
                    .into_iter()
                    .map(|elem| self.expand_expr(elem))
                    .collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Tuple(expanded_elements),
                }
            }
            ExprKind::Map(entries) => {
                let expanded_entries: Vec<_> = entries
                    .into_iter()
                    .map(|(k, v)| (self.expand_expr(k), self.expand_expr(v)))
                    .collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Map(expanded_entries),
                }
            }
            ExprKind::EnumConstruct {
                name,
                variant,
                args,
            } => {
                let expanded_args: Vec<_> =
                    args.into_iter().map(|arg| self.expand_expr(arg)).collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::EnumConstruct {
                        name,
                        variant,
                        args: expanded_args,
                    },
                }
            }
            ExprKind::StructConstruct { name, fields } => {
                let expanded_fields: Vec<_> = fields
                    .into_iter()
                    .map(|(field_name, field_expr)| (field_name, self.expand_expr(field_expr)))
                    .collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::StructConstruct {
                        name,
                        fields: expanded_fields,
                    },
                }
            }
            _ => expr,
        }
    }

    fn validate_nodes(&mut self, nodes: Vec<ASTNode>) -> Vec<ASTNode> {
        // First pass: collect all top-level names
        self.collect_top_level_names(&nodes);

        // Second pass: validate each node
        nodes
            .into_iter()
            .map(|node| self.validate_node(node))
            .collect()
    }

    fn validate_node(&mut self, node: ASTNode) -> ASTNode {
        self.current_file = node.file.clone();

        match node.node {
            ASTNodeKind::Expr(expr) => {
                let validated = self.validate_expr(expr);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Expr(validated),
                    attributes: node.attributes,
                }
            }

            ASTNodeKind::Function(func) => {
                let validated = self.validate_function(*func);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Function(Box::new(validated)),
                    attributes: node.attributes,
                }
            }

            ASTNodeKind::Struct(s) => {
                let validated = self.validate_struct(s);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Struct(validated),
                    attributes: node.attributes,
                }
            }

            ASTNodeKind::Enum(e) => {
                let validated = self.validate_enum(e);
                ASTNode {
                    span: node.span,
                    file: node.file,
                    node: ASTNodeKind::Enum(validated),
                    attributes: node.attributes,
                }
            }

            _ => node,
        }
    }

    fn validate_expr(&mut self, expr: Expr) -> Expr {
        match expr.expr {
            ExprKind::Break(val) => {
                if !self.in_loop {
                    self.diagnostics.add_error(VialError::ValidationError {
                        span: expr.span.clone(),
                        file: expr.file.clone(),
                        kind: ValidationErrorKind::BreakOutsideLoop,
                    });
                }
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Break(val.map(|v| Box::new(self.validate_expr(*v)))),
                }
            }

            ExprKind::Continue => {
                if !self.in_loop {
                    self.diagnostics.add_error(VialError::ValidationError {
                        span: expr.span.clone(),
                        file: expr.file.clone(),
                        kind: ValidationErrorKind::ContinueOutsideLoop,
                    });
                }
                expr
            }

            ExprKind::Return(val) => {
                // ~return is only valid inside user-defined functions, not in auto-generated main~
                // if !self.in_user_function {
                //     self.diagnostics.add_error(ValidationError {
                //         span: expr.span.clone(),
                //         file: expr.file.clone(),
                //         kind: ValidationErrorKind::ReturnOutsideFunction,
                //     });
                // }
                // top level return acts like exit() function with the val as the exit code.
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Return(val.map(|v| Box::new(self.validate_expr(*v)))),
                }
            }

            ExprKind::Loop { label, body } => {
                let was_in_loop = self.in_loop;
                self.in_loop = true;
                let validated_body = self.validate_expr(*body);
                self.in_loop = was_in_loop;

                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Loop {
                        label,
                        body: Box::new(validated_body),
                    },
                }
            }

            ExprKind::While(cond, body) => {
                let was_in_loop = self.in_loop;
                self.in_loop = true;
                let validated_cond = self.validate_expr(*cond);
                let validated_body = self.validate_expr(*body);
                self.in_loop = was_in_loop;

                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::While(Box::new(validated_cond), Box::new(validated_body)),
                }
            }

            ExprKind::For {
                iterator,
                value,
                expression,
            } => {
                let was_in_loop = self.in_loop;
                self.in_loop = true;
                let validated_iter = self.validate_expr(*iterator);
                let validated_body = self.validate_expr(*expression);
                self.in_loop = was_in_loop;

                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::For {
                        iterator: Box::new(validated_iter),
                        value,
                        expression: Box::new(validated_body),
                    },
                }
            }

            ExprKind::Import(import) => {
                // Import validation is handled by ModuleResolver during import resolution
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Import(import),
                }
            }

            ExprKind::Block(exprs) => {
                let validated_exprs: Vec<_> =
                    exprs.into_iter().map(|e| self.validate_expr(e)).collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Block(validated_exprs),
                }
            }

            ExprKind::Assign { l_val, r_val, op } => {
                let validated_l_val = self.validate_expr(*l_val);
                let validated_r_val = self.validate_expr(*r_val);
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Assign {
                        l_val: Box::new(validated_l_val),
                        r_val: Box::new(validated_r_val),
                        op: op.clone(),
                    },
                }
            }
            ExprKind::Array(elements) => {
                let validated_elements: Vec<_> = elements
                    .into_iter()
                    .map(|e| self.validate_expr(e))
                    .collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Array(validated_elements),
                }
            }
            ExprKind::Tuple(elements) => {
                let validated_elements: Vec<_> = elements
                    .into_iter()
                    .map(|e| self.validate_expr(e))
                    .collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Tuple(validated_elements),
                }
            }
            ExprKind::Index(target, index) => {
                let validated_target = self.validate_expr(*target);
                let validated_index = self.validate_expr(*index);
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Index(Box::new(validated_target), Box::new(validated_index)),
                }
            }
            ExprKind::FieldAccess(target, field) => {
                let validated_target = self.validate_expr(*target);
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::FieldAccess(Box::new(validated_target), field.clone()),
                }
            }
            ExprKind::EnumConstruct {
                name,
                variant,
                args,
            } => {
                let validated_args: Vec<_> =
                    args.into_iter().map(|a| self.validate_expr(a)).collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::EnumConstruct {
                        name: name.clone(),
                        variant: variant.clone(),
                        args: validated_args,
                    },
                }
            }
            ExprKind::StructConstruct { name, fields } => {
                let validated_fields: Vec<_> = fields
                    .into_iter()
                    .map(|(field_name, field_expr)| {
                        (field_name.clone(), self.validate_expr(field_expr))
                    })
                    .collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::StructConstruct {
                        name: name.clone(),
                        fields: validated_fields,
                    },
                }
            }
            ExprKind::Cast {
                expr: inner,
                target_type,
            } => {
                let validated_inner = self.validate_expr(*inner);
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Cast {
                        expr: Box::new(validated_inner),
                        target_type: target_type.clone(),
                    },
                }
            }
            ExprKind::With { context, var, body } => {
                let validated_context = self.validate_expr(*context);
                let validated_body = self.validate_expr(*body);
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::With {
                        context: Box::new(validated_context),
                        var: var.clone(),
                        body: Box::new(validated_body),
                    },
                }
            }
            ExprKind::Match(scrutinee, arms) => {
                let validated_scrutinee = self.validate_expr(*scrutinee);
                let validated_arms: Vec<_> = arms
                    .into_iter()
                    .map(|arm| {
                        let pattern = arm.pattern; // Patterns are validated separately
                        let guard = arm.guard.map(|g| self.validate_expr(g));
                        let body = self.validate_expr(*arm.body);
                        MatchArm {
                            pattern,
                            guard,
                            body: Box::new(body),
                            span: arm.span,
                        }
                    })
                    .collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Match(Box::new(validated_scrutinee), validated_arms),
                }
            }
            ExprKind::Perform { effect, args } => {
                let validated_args: Vec<_> =
                    args.into_iter().map(|a| self.validate_expr(a)).collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Perform {
                        effect: effect.clone(),
                        args: validated_args,
                    },
                }
            }
            ExprKind::Handle { body, handlers } => {
                let validated_body = self.validate_expr(*body);
                let validated_handlers: Vec<_> = handlers
                    .into_iter()
                    .map(|handler| {
                        let body = self.validate_expr(handler.body);
                        EffectHandler {
                            effect: handler.effect,
                            params: handler.params,
                            resume_param: handler.resume_param,
                            body,
                            span: handler.span,
                        }
                    })
                    .collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::Handle {
                        body: Box::new(validated_body),
                        handlers: validated_handlers,
                    },
                }
            }
            ExprKind::OptionalChain(target, field) => {
                let validated_target = self.validate_expr(*target);
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::OptionalChain(Box::new(validated_target), field.clone()),
                }
            }
            ExprKind::MacroCall(name, args, delimiter) => {
                let validated_args: Vec<_> =
                    args.into_iter().map(|a| self.validate_expr(a)).collect();
                Expr {
                    span: expr.span,
                    file: expr.file,
                    expr: ExprKind::MacroCall(name.clone(), validated_args, delimiter.clone()),
                }
            }
            _ => expr,
        }
    }

    fn validate_function(&mut self, func: Function) -> Function {
        // uniqueness check already done in collect_top_level_names

        let was_in_function = self.in_function;
        let was_in_user_function = self.in_user_function;

        self.in_function = true;
        self.in_user_function = func.name != "main";

        let validated_body = func.body.map(|b| self.validate_expr(b));

        self.in_function = was_in_function;
        self.in_user_function = was_in_user_function;

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
            body: validated_body,
        }
    }

    fn validate_struct(&mut self, struct_def: Struct) -> Struct {
        let mut seen_fields = HashMap::new();

        for (field, _vis) in &struct_def.fields {
            if let Some(_existing) = seen_fields.get(&field.name) {
                self.diagnostics.add_error(VialError::ValidationError {
                    span: field.span.clone(),
                    file: field.file.clone(),
                    kind: ValidationErrorKind::DuplicateField {
                        field: field.name.clone(),
                    },
                });
            } else {
                seen_fields.insert(field.name.clone(), field.span.clone());
            }
        }

        struct_def
    }

    fn validate_enum(&mut self, enum_def: Enum) -> Enum {
        let mut seen_variants = HashMap::new();

        for variant in &enum_def.variants {
            if let Some(_existing) = seen_variants.get(&variant.name) {
                self.diagnostics.add_error(VialError::ValidationError {
                    span: variant.span.clone(),
                    file: variant.file.clone(),
                    kind: ValidationErrorKind::DuplicateVariant {
                        variant: variant.name.clone(),
                    },
                });
            } else {
                seen_variants.insert(variant.name.clone(), variant.span.clone());
            }
        }

        enum_def
    }

    // fn validate_impl(&mut self, impl_block: Impl) -> Impl {
    //     // Check for duplicate methods in impl
    //     let mut seen_methods: HashMap<String, Range<usize>> = HashMap::new();

    //     for method in &impl_block.methods {
    //         let key = format!("{}::{}", impl_block.type_name, method.name);

    //         if let Some(existing) = seen_methods.get(&key) {
    //             self.diagnostics.add_error(ValidationError {
    //                 span: method.span.clone(),
    //                 file: method.file.clone(),
    //                 kind: ValidationErrorKind::DuplicateDefinition {
    //                     name: method.name.clone(),
    //                     first_defined: existing.clone(),
    //                 },
    //             });
    //         } else {
    //             seen_methods.insert(key, method.span.clone());
    //         }
    //     }

    //     impl_block
    // }

    fn collect_top_level_names(&mut self, nodes: &[ASTNode]) {
        for node in nodes {
            match &node.node {
                ASTNodeKind::Function(func) => {
                    if let Some(existing) = self.defined_items.get(&func.name) {
                        self.diagnostics.add_error(VialError::ValidationError {
                            span: func.span.clone(),
                            file: func.file.clone(),
                            kind: ValidationErrorKind::DuplicateDefinition {
                                name: func.name.clone(),
                                first_defined: existing.clone(),
                            },
                        });
                    } else {
                        self.defined_items
                            .insert(func.name.clone(), func.span.clone());
                    }
                }

                ASTNodeKind::Struct(s) => {
                    if let Some(existing) = self.defined_items.get(&s.name) {
                        self.diagnostics.add_error(VialError::ValidationError {
                            span: s.span.clone(),
                            file: s.file.clone(),
                            kind: ValidationErrorKind::DuplicateDefinition {
                                name: s.name.clone(),
                                first_defined: existing.clone(),
                            },
                        });
                    } else {
                        self.defined_items.insert(s.name.clone(), s.span.clone());
                    }
                }

                ASTNodeKind::Enum(e) => {
                    if let Some(existing) = self.defined_items.get(&e.name) {
                        self.diagnostics.add_error(VialError::ValidationError {
                            span: e.span.clone(),
                            file: e.file.clone(),
                            kind: ValidationErrorKind::DuplicateDefinition {
                                name: e.name.clone(),
                                first_defined: existing.clone(),
                            },
                        });
                    } else {
                        self.defined_items.insert(e.name.clone(), e.span.clone());
                    }
                }

                ASTNodeKind::TypeAlias(ta) => {
                    if let Some(existing) = self.defined_items.get(&ta.name) {
                        self.diagnostics.add_error(VialError::ValidationError {
                            span: ta.span.clone(),
                            file: ta.file.clone(),
                            kind: ValidationErrorKind::DuplicateDefinition {
                                name: ta.name.clone(),
                                first_defined: existing.clone(),
                            },
                        });
                    } else {
                        self.defined_items.insert(ta.name.clone(), ta.span.clone());
                    }
                }

                ASTNodeKind::Trait(t) => {
                    if let Some(existing) = self.defined_items.get(&t.name) {
                        self.diagnostics.add_error(VialError::ValidationError {
                            span: t.span.clone(),
                            file: "".to_string(),
                            kind: ValidationErrorKind::DuplicateDefinition {
                                name: t.name.clone(),
                                first_defined: existing.clone(),
                            },
                        });
                    } else {
                        self.defined_items.insert(t.name.clone(), t.span.clone());
                    }
                }

                ASTNodeKind::EffectDef(eff) => {
                    if let Some(existing) = self.defined_items.get(&eff.name) {
                        self.diagnostics.add_error(VialError::ValidationError {
                            span: eff.span.clone(),
                            file: eff.file.clone(),
                            kind: ValidationErrorKind::DuplicateDefinition {
                                name: eff.name.clone(),
                                first_defined: existing.clone(),
                            },
                        });
                    } else {
                        self.defined_items
                            .insert(eff.name.clone(), eff.span.clone());
                    }
                }

                ASTNodeKind::Impl(_) => {}

                _ => {}
            }
        }
    }
}
