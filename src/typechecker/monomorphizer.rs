use crate::typechecker::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Monomorphizer {
    pub interner: Interner,
    id_gen: IdGen,
    pub instantiations: HashMap<(FunctionId, Vec<String>), FunctionId>,
    pub functions: HashMap<FunctionId, TypedFunction>,
    pub structs: HashMap<StructId, TypedStruct>,
    pub enums: HashMap<EnumId, TypedEnum>,
    pub effects: HashMap<EffectId, TypedEffectDef>,
    pub type_aliases: HashMap<TypeAliasId, TypedTypeAlias>,
    pub specialized_functions: Vec<TypedFunction>,
    pub specialized_structs: Vec<TypedStruct>,
    pub specialized_enums: Vec<TypedEnum>,
    pub specialized_effects: Vec<TypedEffectDef>,
    pub specialized_type_aliases: Vec<TypedTypeAlias>,
}

impl Monomorphizer {
    pub fn new(interner: Interner) -> Self {
        Self {
            interner,
            id_gen: IdGen::new(),
            instantiations: HashMap::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            effects: HashMap::new(),
            type_aliases: HashMap::new(),
            specialized_functions: Vec::new(),
            specialized_structs: Vec::new(),
            specialized_enums: Vec::new(),
            specialized_effects: Vec::new(),
            specialized_type_aliases: Vec::new(),
        }
    }

    pub fn monomorphize_program(&mut self, program: Vec<TypedASTNode>) -> Vec<TypedASTNode> {
        // Collect all generic definitions
        for node in &program {
            match &node.node {
                TypedASTNodeKind::Function(func) => {
                    self.functions.insert(func.function_id, (**func).clone());
                }
                TypedASTNodeKind::Struct(s) => {
                    self.structs.insert(s.struct_id, s.clone());
                }
                TypedASTNodeKind::Enum(e) => {
                    self.enums.insert(e.enum_id, e.clone());
                }
                TypedASTNodeKind::EffectDef(e) => {
                    self.effects.insert(e.effect_id, e.clone());
                }
                TypedASTNodeKind::TypeAlias(a) => {
                    self.type_aliases.insert(a.alias_id, a.clone());
                }
                _ => {}
            }
        }

        // Process nodes
        let mut result = Vec::new();
        for node in program {
            match node.node {
                TypedASTNodeKind::Function(func) => {
                    // Check if function is generic (has explicit type parameters)
                    // Don't consider functions with only inferred type variables as generic
                    // because those should be processed normally (e.g., main function)
                    let has_explicit_type_params = !func.type_params.is_empty();

                    if has_explicit_type_params {
                        // This is a truly generic function - don't include it in output directly
                        continue; // Don't add to result yet
                    } else {
                        // Non-generic function - keep it and monomorphize body
                        let mono_func = self.monomorphize_function_body(*func);
                        result.push(TypedASTNode {
                            span: node.span,
                            file: node.file,
                            node: TypedASTNodeKind::Function(Box::new(mono_func)),
                            attributes: node.attributes,
                        });
                    }
                }
                TypedASTNodeKind::Struct(s) => {
                    // Check if struct is generic
                    if !s.type_params.is_empty() {
                        // Generic struct - will be specialized when used
                        continue; // Don't add to result yet
                    } else {
                        // Non-generic struct - keep it and monomorphize if needed
                        let mono_struct = self.monomorphize_struct(s);
                        result.push(TypedASTNode {
                            span: node.span,
                            file: node.file,
                            node: TypedASTNodeKind::Struct(mono_struct),
                            attributes: node.attributes,
                        });
                    }
                }
                TypedASTNodeKind::Enum(e) => {
                    // Check if enum is generic
                    if !e.type_params.is_empty() {
                        // Generic enum - will be specialized when used
                        continue; // Don't add to result yet
                    } else {
                        // Non-generic enum - keep it and monomorphize if needed
                        let mono_enum = self.monomorphize_enum(e);
                        result.push(TypedASTNode {
                            span: node.span,
                            file: node.file,
                            node: TypedASTNodeKind::Enum(mono_enum),
                            attributes: node.attributes,
                        });
                    }
                }
                TypedASTNodeKind::EffectDef(e) => {
                    // Check if effect is generic
                    if !e.type_params.is_empty() {
                        // Generic effect - will be specialized when used
                        continue; // Don't add to result yet
                    } else {
                        // Non-generic effect - keep it and monomorphize if needed
                        let mono_effect = self.monomorphize_effect_def(e);
                        result.push(TypedASTNode {
                            span: node.span,
                            file: node.file,
                            node: TypedASTNodeKind::EffectDef(mono_effect),
                            attributes: node.attributes,
                        });
                    }
                }
                TypedASTNodeKind::TypeAlias(a) => {
                    // Check if type alias is generic
                    if !a.type_params.is_empty() {
                        // Generic type alias - will be specialized when used
                        continue; // Don't add to result yet
                    } else {
                        // Non-generic type alias - keep it and monomorphize if needed
                        let mono_alias = self.monomorphize_type_alias(a);
                        result.push(TypedASTNode {
                            span: node.span,
                            file: node.file,
                            node: TypedASTNodeKind::TypeAlias(mono_alias),
                            attributes: node.attributes,
                        });
                    }
                }
                TypedASTNodeKind::Expr(expr) => {
                    let mono_expr = self.monomorphize_expr(expr);
                    result.push(TypedASTNode {
                        span: node.span,
                        file: node.file,
                        node: TypedASTNodeKind::Expr(mono_expr),
                        attributes: node.attributes,
                    });
                }
                _ => result.push(node),
            }
        }

        // Add specialized functions
        for func in self.specialized_functions.drain(..) {
            result.push(TypedASTNode {
                span: func.span.clone(),
                file: func.file.clone(),
                node: TypedASTNodeKind::Function(Box::new(func)),
                attributes: vec![],
            });
        }

        // Add specialized structs
        for s in self.specialized_structs.drain(..) {
            result.push(TypedASTNode {
                span: s.span.clone(),
                file: s.file.clone(),
                node: TypedASTNodeKind::Struct(s),
                attributes: vec![],
            });
        }

        // Add specialized enums
        for e in self.specialized_enums.drain(..) {
            result.push(TypedASTNode {
                span: e.span.clone(),
                file: e.file.clone(),
                node: TypedASTNodeKind::Enum(e),
                attributes: vec![],
            });
        }

        // Add specialized effects
        for e in self.specialized_effects.drain(..) {
            result.push(TypedASTNode {
                span: e.span.clone(),
                file: e.file.clone(),
                node: TypedASTNodeKind::EffectDef(e),
                attributes: vec![],
            });
        }

        // Add specialized type aliases
        for a in self.specialized_type_aliases.drain(..) {
            result.push(TypedASTNode {
                span: a.span.clone(),
                file: a.file.clone(),
                node: TypedASTNodeKind::TypeAlias(a),
                attributes: vec![],
            });
        }

        result
    }

    fn monomorphize_function_body(&mut self, mut func: TypedFunction) -> TypedFunction {
        if let Some(body) = func.body {
            let mono_body = self.monomorphize_expr(body);
            // If the function's return type is a variable and the body has a concrete type,
            // update the return type to match the body's type
            if let TypeKind::Variable { .. } = func.return_type.type_
                && !matches!(mono_body.type_.type_, TypeKind::Variable { .. })
            {
                func.return_type = mono_body.type_.clone();
            }
            func.body = Some(mono_body);
        }

        // Also update function type to remove Forall and update return type in function type
        if let TypeKind::Forall { body, .. } = &func.function_type.type_ {
            let mut updated_func_type = body.clone();

            // If the return type in function type is a variable and we have resolved it,
            // update the function type as well
            if let TypeKind::Function {
                params,
                return_type: func_return_type,
                effects,
            } = &updated_func_type.type_
            {
                let final_return_type =
                    if matches!(func_return_type.type_, TypeKind::Variable { .. })
                        && !matches!(func.return_type.type_, TypeKind::Variable { .. })
                    {
                        func.return_type.clone()
                    } else {
                        func_return_type.clone()
                    };

                updated_func_type = Rc::new(Type {
                    span: updated_func_type.span.clone(),
                    file: updated_func_type.file.clone(),
                    type_: TypeKind::Function {
                        params: params.clone(),
                        return_type: final_return_type,
                        effects: effects.clone(),
                    },
                });
            }

            func.function_type = updated_func_type;
        }
        func
    }

    fn monomorphize_struct(&mut self, mut s: TypedStruct) -> TypedStruct {
        // Monomorphize field types
        s.fields = s
            .fields
            .into_iter()
            .map(|(arg, vis, field_id)| {
                let arg = TypedFnArg {
                    span: arg.span,
                    file: arg.file,
                    name: arg.name,
                    binding_id: arg.binding_id,
                    type_: self.monomorphize_type(&arg.type_),
                };
                (arg, vis, field_id)
            })
            .collect();

        // Monomorphize method bodies
        s.methods = s
            .methods
            .into_iter()
            .map(|m| self.monomorphize_function_body(m))
            .collect();
        s
    }

    fn monomorphize_enum(&mut self, mut e: TypedEnum) -> TypedEnum {
        // Monomorphize variant types
        e.variants = e
            .variants
            .into_iter()
            .map(|mut v| {
                v.types = v
                    .types
                    .into_iter()
                    .map(|t| self.monomorphize_type(&t))
                    .collect();
                v.constructor_type = self.monomorphize_type(&v.constructor_type);
                v
            })
            .collect();

        // Monomorphize method bodies
        e.methods = e
            .methods
            .into_iter()
            .map(|m| self.monomorphize_function_body(m))
            .collect();
        e
    }

    fn monomorphize_effect_def(&mut self, mut e: TypedEffectDef) -> TypedEffectDef {
        // Monomorphize operation types
        e.operations = e
            .operations
            .into_iter()
            .map(|mut op| {
                op.params = op
                    .params
                    .into_iter()
                    .map(|t| self.monomorphize_type(&t))
                    .collect();
                op.return_type = self.monomorphize_type(&op.return_type);
                op.operation_type = self.monomorphize_type(&op.operation_type);
                op
            })
            .collect();
        e
    }

    fn monomorphize_type_alias(&mut self, mut a: TypedTypeAlias) -> TypedTypeAlias {
        a.target_type = self.monomorphize_type(&a.target_type);
        a.expanded_type = self.monomorphize_type(&a.expanded_type);
        a
    }

    fn monomorphize_expr(&mut self, expr: TypedExpr) -> TypedExpr {
        let (new_expr, new_type) = match expr.expr {
            TypedExprKind::Call {
                function,
                args,
                type_args,
            } => {
                let mono_args: Vec<_> = args
                    .into_iter()
                    .map(|a| self.monomorphize_expr(a))
                    .collect();

                if let TypedExprKind::Variable { binding_id, .. } = &function.expr {
                    let func_id = FunctionId(binding_id.0);

                    if let Some(original_func) = self.functions.get(&func_id).cloned() {
                        let type_vars = self.collect_type_vars_from_func(&original_func);

                        // Check if this function has type parameters or type variables that need monomorphization
                        let needs_monomorphization =
                            !type_vars.is_empty() || !original_func.type_params.is_empty();

                        if needs_monomorphization {
                            // Get concrete types from arguments
                            let concrete_types: Vec<_> = mono_args
                                .iter()
                                .map(|arg| self.type_to_key(&arg.type_))
                                .collect();

                            let (specialized_id, specialized_name) = self
                                .get_or_create_specialization(
                                    func_id,
                                    &original_func,
                                    &concrete_types,
                                    &mono_args,
                                );

                            let specialized_func = self.functions.get(&specialized_id).unwrap();
                            let specialized_func_type = specialized_func.function_type.clone();

                            let return_type = match &specialized_func_type.type_ {
                                TypeKind::Function { return_type, .. } => return_type.clone(),
                                _ => self.error_type(),
                            };

                            let call_expr = TypedExprKind::Call {
                                function: Box::new(TypedExpr {
                                    span: function.span.clone(),
                                    file: function.file.clone(),
                                    expr: TypedExprKind::Variable {
                                        name: specialized_name,
                                        binding_id: BindingId(specialized_id.0),
                                    },
                                    type_: specialized_func_type,
                                }),
                                args: mono_args,
                                type_args: vec![],
                            };

                            return TypedExpr {
                                span: expr.span,
                                file: expr.file,
                                expr: call_expr,
                                type_: return_type,
                            };
                        }
                    }
                }

                let mono_func = self.monomorphize_expr(*function);
                let ret_type = match &mono_func.type_.type_ {
                    TypeKind::Function { return_type, .. } => return_type.clone(),
                    _ => expr.type_.clone(),
                };

                (
                    TypedExprKind::Call {
                        function: Box::new(mono_func),
                        args: mono_args,
                        type_args,
                    },
                    ret_type,
                )
            }

            TypedExprKind::StructConstruct {
                struct_name,
                struct_id,
                fields,
            } => {
                // Check if this struct is generic and needs monomorphization
                let needs_specialization = {
                    if let Some(original_struct) = self.structs.get(&struct_id) {
                        !original_struct.type_params.is_empty()
                    } else {
                        false
                    }
                };

                if needs_specialization {
                    // Get original struct again after we're done borrowing
                    let original_struct = self.structs.get(&struct_id).unwrap().clone();
                    let concrete_types: Vec<_> = self
                        .extract_concrete_types_from_struct_construct(&original_struct, &fields);

                    let (specialized_id, specialized_name) = self.get_or_create_specialized_struct(
                        struct_id,
                        &original_struct,
                        &concrete_types,
                    );

                    // Monomorphize the fields with the substitution
                    let mono_fields: Vec<_> = fields
                        .into_iter()
                        .map(|(name, field_id, field_expr)| {
                            (name, field_id, self.monomorphize_expr(field_expr))
                        })
                        .collect();

                    return TypedExpr {
                        span: expr.span,
                        file: expr.file,
                        expr: TypedExprKind::StructConstruct {
                            struct_name: specialized_name,
                            struct_id: specialized_id,
                            fields: mono_fields,
                        },
                        type_: self
                            .create_specialized_struct_type(specialized_name, &concrete_types),
                    };
                }

                // Non-generic struct
                let mono_fields: Vec<_> = fields
                    .into_iter()
                    .map(|(name, field_id, field_expr)| {
                        (name, field_id, self.monomorphize_expr(field_expr))
                    })
                    .collect();

                (
                    TypedExprKind::StructConstruct {
                        struct_name,
                        struct_id,
                        fields: mono_fields,
                    },
                    expr.type_.clone(),
                )
            }

            TypedExprKind::EnumConstruct {
                enum_name,
                enum_id,
                variant,
                variant_id,
                args,
            } => {
                // Check if this enum is generic and needs monomorphization
                let needs_specialization = {
                    if let Some(original_enum) = self.enums.get(&enum_id) {
                        !original_enum.type_params.is_empty()
                    } else {
                        false
                    }
                };

                if needs_specialization {
                    let original_enum = self.enums.get(&enum_id).unwrap().clone();
                    // Extract concrete types from the constructor arguments
                    let concrete_types: Vec<_> = args
                        .iter()
                        .map(|arg| self.type_to_key(&arg.type_))
                        .collect();

                    let (specialized_id, specialized_name) = self.get_or_create_specialized_enum(
                        enum_id,
                        &original_enum,
                        &concrete_types,
                    );

                    // Monomorphize the constructor arguments
                    let mono_args: Vec<_> = args
                        .into_iter()
                        .map(|arg| self.monomorphize_expr(arg))
                        .collect();

                    return TypedExpr {
                        span: expr.span,
                        file: expr.file,
                        expr: TypedExprKind::EnumConstruct {
                            enum_name: specialized_name,
                            enum_id: specialized_id,
                            variant,
                            variant_id,
                            args: mono_args,
                        },
                        type_: self.create_specialized_enum_type(specialized_name, &concrete_types),
                    };
                }

                // Non-generic enum
                let mono_args: Vec<_> = args
                    .into_iter()
                    .map(|arg| self.monomorphize_expr(arg))
                    .collect();

                (
                    TypedExprKind::EnumConstruct {
                        enum_name,
                        enum_id,
                        variant,
                        variant_id,
                        args: mono_args,
                    },
                    expr.type_.clone(),
                )
            }

            TypedExprKind::Variable { name, binding_id } => {
                // Keep as-is, type will be preserved
                (
                    TypedExprKind::Variable { name, binding_id },
                    expr.type_.clone(),
                )
            }

            TypedExprKind::BinOp { left, op, right } => (
                TypedExprKind::BinOp {
                    left: Box::new(self.monomorphize_expr(*left)),
                    op,
                    right: Box::new(self.monomorphize_expr(*right)),
                },
                expr.type_.clone(),
            ),

            TypedExprKind::UnOp { op, operand } => (
                TypedExprKind::UnOp {
                    op,
                    operand: Box::new(self.monomorphize_expr(*operand)),
                },
                expr.type_.clone(),
            ),

            TypedExprKind::IfElse {
                condition,
                then,
                else_,
            } => (
                TypedExprKind::IfElse {
                    condition: Box::new(self.monomorphize_expr(*condition)),
                    then: Box::new(self.monomorphize_expr(*then)),
                    else_: else_.map(|e| Box::new(self.monomorphize_expr(*e))),
                },
                expr.type_.clone(),
            ),

            TypedExprKind::Block { expressions } => {
                let mono_exprs: Vec<_> = expressions
                    .into_iter()
                    .map(|e| self.monomorphize_expr(e))
                    .collect();
                let last_type = mono_exprs
                    .last()
                    .map(|e| e.type_.clone())
                    .unwrap_or_else(|| expr.type_.clone());

                (
                    TypedExprKind::Block {
                        expressions: mono_exprs,
                    },
                    last_type,
                )
            }

            TypedExprKind::Let {
                var,
                binding_id,
                var_type,
                value,
            } => (
                TypedExprKind::Let {
                    var,
                    binding_id,
                    var_type: self.monomorphize_type(&var_type),
                    value: Box::new(self.monomorphize_expr(*value)),
                },
                expr.type_.clone(),
            ),

            TypedExprKind::Array {
                elements,
                element_type,
            } => {
                let mono_elements: Vec<_> = elements
                    .into_iter()
                    .map(|e| self.monomorphize_expr(e))
                    .collect();

                (
                    TypedExprKind::Array {
                        elements: mono_elements,
                        element_type: self.monomorphize_type(&element_type),
                    },
                    expr.type_.clone(),
                )
            }

            TypedExprKind::Tuple(elems) => {
                let mono_elems: Vec<_> = elems
                    .into_iter()
                    .map(|e| self.monomorphize_expr(e))
                    .collect();

                (TypedExprKind::Tuple(mono_elems), expr.type_.clone())
            }

            TypedExprKind::FieldAccess {
                target,
                field,
                field_id,
                field_type,
            } => {
                let mono_target = self.monomorphize_expr(*target);

                (
                    TypedExprKind::FieldAccess {
                        target: Box::new(mono_target),
                        field,
                        field_id,
                        field_type: self.monomorphize_type(&field_type),
                    },
                    self.monomorphize_type(&field_type),
                )
            }

            TypedExprKind::Index {
                target,
                index,
                element_type,
            } => {
                let mono_target = self.monomorphize_expr(*target);
                let mono_index = self.monomorphize_expr(*index);

                (
                    TypedExprKind::Index {
                        target: Box::new(mono_target),
                        index: Box::new(mono_index),
                        element_type: self.monomorphize_type(&element_type),
                    },
                    self.monomorphize_type(&element_type),
                )
            }

            TypedExprKind::Cast {
                expr: inner,
                target_type,
            } => {
                let mono_inner = self.monomorphize_expr(*inner);

                (
                    TypedExprKind::Cast {
                        expr: Box::new(mono_inner),
                        target_type: self.monomorphize_type(&target_type),
                    },
                    self.monomorphize_type(&target_type),
                )
            }

            TypedExprKind::Lambda {
                args,
                body,
                captures,
                function_type,
            } => {
                let mono_args: Vec<_> = args
                    .into_iter()
                    .map(|arg| TypedFnArg {
                        span: arg.span,
                        file: arg.file,
                        name: arg.name,
                        binding_id: arg.binding_id,
                        type_: self.monomorphize_type(&arg.type_),
                    })
                    .collect();

                let mono_body = self.monomorphize_expr(*body);
                let mono_captures: Vec<_> = captures
                    .into_iter()
                    .map(|(name, id, ty)| (name, id, self.monomorphize_type(&ty)))
                    .collect();

                (
                    TypedExprKind::Lambda {
                        args: mono_args,
                        body: Box::new(mono_body),
                        captures: mono_captures,
                        function_type: self.monomorphize_type(&function_type),
                    },
                    self.monomorphize_type(&function_type),
                )
            }

            TypedExprKind::Match { scrutinee, arms } => {
                let mono_scrutinee = self.monomorphize_expr(*scrutinee);
                let mono_arms: Vec<_> = arms
                    .into_iter()
                    .map(|arm| TypedMatchArm {
                        pattern: self.monomorphize_pattern(arm.pattern),
                        guard: arm.guard.map(|g| self.monomorphize_expr(g)),
                        body: Box::new(self.monomorphize_expr(*arm.body)),
                        span: arm.span,
                    })
                    .collect();

                (
                    TypedExprKind::Match {
                        scrutinee: Box::new(mono_scrutinee),
                        arms: mono_arms,
                    },
                    expr.type_.clone(),
                )
            }

            TypedExprKind::Loop { label, body } => {
                let mono_body = self.monomorphize_expr(*body);

                (
                    TypedExprKind::Loop {
                        label,
                        body: Box::new(mono_body),
                    },
                    expr.type_.clone(),
                )
            }

            TypedExprKind::While { condition, body } => {
                let mono_condition = self.monomorphize_expr(*condition);
                let mono_body = self.monomorphize_expr(*body);

                (
                    TypedExprKind::While {
                        condition: Box::new(mono_condition),
                        body: Box::new(mono_body),
                    },
                    expr.type_.clone(),
                )
            }

            TypedExprKind::For {
                iterator,
                iterator_type,
                value,
                binding_id,
                value_type,
                body,
            } => {
                let mono_iterator = self.monomorphize_expr(*iterator);
                let mono_body = self.monomorphize_expr(*body);

                (
                    TypedExprKind::For {
                        iterator: Box::new(mono_iterator),
                        iterator_type: self.monomorphize_type(&iterator_type),
                        value,
                        binding_id,
                        value_type: self.monomorphize_type(&value_type),
                        body: Box::new(mono_body),
                    },
                    expr.type_.clone(),
                )
            }

            TypedExprKind::IfLet {
                pattern,
                expr: scrutinee,
                then,
                else_,
            } => {
                let mono_scrutinee = self.monomorphize_expr(*scrutinee);
                let mono_pattern = self.monomorphize_pattern(pattern);
                let mono_then = self.monomorphize_expr(*then);
                let mono_else = else_.map(|e| Box::new(self.monomorphize_expr(*e)));

                (
                    TypedExprKind::IfLet {
                        pattern: mono_pattern,
                        expr: Box::new(mono_scrutinee),
                        then: Box::new(mono_then),
                        else_: mono_else,
                    },
                    expr.type_.clone(),
                )
            }

            TypedExprKind::WhileLet {
                pattern,
                expr: scrutinee,
                body,
            } => {
                let mono_scrutinee = self.monomorphize_expr(*scrutinee);
                let mono_pattern = self.monomorphize_pattern(pattern);
                let mono_body = self.monomorphize_expr(*body);

                (
                    TypedExprKind::WhileLet {
                        pattern: mono_pattern,
                        expr: Box::new(mono_scrutinee),
                        body: Box::new(mono_body),
                    },
                    expr.type_.clone(),
                )
            }

            TypedExprKind::Return(val) => (
                TypedExprKind::Return(val.map(|v| Box::new(self.monomorphize_expr(*v)))),
                expr.type_.clone(),
            ),

            TypedExprKind::Break(val) => (
                TypedExprKind::Break(val.map(|v| Box::new(self.monomorphize_expr(*v)))),
                expr.type_.clone(),
            ),

            TypedExprKind::Continue => (TypedExprKind::Continue, expr.type_.clone()),

            TypedExprKind::Perform {
                effect,
                effect_id,
                args,
            } => {
                let mono_args: Vec<_> = args
                    .into_iter()
                    .map(|arg| self.monomorphize_expr(arg))
                    .collect();

                // Check if this effect is generic and needs monomorphization
                let needs_specialization = {
                    if let Some(original_effect) = self.effects.get(&effect_id) {
                        !original_effect.type_params.is_empty()
                    } else {
                        false
                    }
                };

                if needs_specialization {
                    let original_effect = self.effects.get(&effect_id).unwrap().clone();
                    // Extract concrete types from the operation arguments
                    let concrete_types: Vec<_> = mono_args
                        .iter()
                        .map(|arg| self.type_to_key(&arg.type_))
                        .collect();

                    let (specialized_id, specialized_name) = self.get_or_create_specialized_effect(
                        effect_id,
                        &original_effect,
                        &concrete_types,
                    );

                    return TypedExpr {
                        span: expr.span,
                        file: expr.file,
                        expr: TypedExprKind::Perform {
                            effect: specialized_name,
                            effect_id: specialized_id,
                            args: mono_args,
                        },
                        type_: self.monomorphize_type(&expr.type_), // Use the original type that should be updated
                    };
                }

                (
                    TypedExprKind::Perform {
                        effect,
                        effect_id,
                        args: mono_args,
                    },
                    expr.type_.clone(),
                )
            }

            TypedExprKind::Handle {
                body,
                handlers,
                return_type,
            } => {
                let mono_body = self.monomorphize_expr(*body);
                let mono_handlers: Vec<_> = handlers
                    .into_iter()
                    .map(|handler| {
                        TypedEffectHandler {
                            span: handler.span,
                            effect: handler.effect,
                            effect_id: handler.effect_id,
                            params: handler
                                .params // Need to handle type substitution for the handler params too
                                .into_iter()
                                .map(|(name, id, ty)| (name, id, self.monomorphize_type(&ty)))
                                .collect(),
                            resume_param: handler.resume_param,
                            resume_id: handler.resume_id,
                            resume_type: self.monomorphize_type(&handler.resume_type),
                            body: self.monomorphize_expr(handler.body),
                        }
                    })
                    .collect();

                (
                    TypedExprKind::Handle {
                        body: Box::new(mono_body),
                        handlers: mono_handlers,
                        return_type: self.monomorphize_type(&return_type),
                    },
                    self.monomorphize_type(&return_type),
                )
            }

            TypedExprKind::With {
                context,
                var,
                binding_id,
                var_type,
                body,
            } => {
                let mono_context = self.monomorphize_expr(*context);
                let mono_body = self.monomorphize_expr(*body);

                (
                    TypedExprKind::With {
                        context: Box::new(mono_context),
                        var,
                        binding_id,
                        var_type: self.monomorphize_type(&var_type),
                        body: Box::new(mono_body),
                    },
                    expr.type_.clone(),
                )
            }

            TypedExprKind::OptionalChain {
                target,
                field,
                field_id,
                field_type,
            } => {
                let mono_target = self.monomorphize_expr(*target);

                (
                    TypedExprKind::OptionalChain {
                        target: Box::new(mono_target),
                        field,
                        field_id,
                        field_type: self.monomorphize_type(&field_type),
                    },
                    self.monomorphize_type(&field_type),
                )
            }

            TypedExprKind::MacroCall {
                name,
                macro_id,
                args,
                delimiter,
            } => {
                let mono_args: Vec<_> = args
                    .into_iter()
                    .map(|arg| self.monomorphize_expr(arg))
                    .collect();

                // For builtin macros, resolve the return type to concrete types
                let macro_name_str = self.interner.resolve(name);
                let resolved_type = match macro_name_str {
                    "println" | "print" => {
                        // These macros return unit
                        Rc::new(Type {
                            span: expr.type_.span.clone(),
                            file: expr.type_.file.clone(),
                            type_: TypeKind::Constructor {
                                name: self.interner.intern("unit").0, // Symbol ID for "unit"
                                args: vec![],
                                kind: Kind::Star,
                            },
                        })
                    }
                    "input" => {
                        // input! returns string
                        Rc::new(Type {
                            span: expr.type_.span.clone(),
                            file: expr.type_.file.clone(),
                            type_: TypeKind::Constructor {
                                name: self.interner.intern("string").0, // Symbol ID for "string"
                                args: vec![],
                                kind: Kind::Star,
                            },
                        })
                    }
                    "typeof" => {
                        // typeof! returns string
                        Rc::new(Type {
                            span: expr.type_.span.clone(),
                            file: expr.type_.file.clone(),
                            type_: TypeKind::Constructor {
                                name: self.interner.intern("string").0, // Symbol ID for "string"
                                args: vec![],
                                kind: Kind::Star,
                            },
                        })
                    }
                    _ => {
                        // For other macros, keep the original type
                        expr.type_.clone()
                    }
                };

                (
                    TypedExprKind::MacroCall {
                        name,
                        macro_id,
                        args: mono_args,
                        delimiter,
                    },
                    resolved_type,
                )
            }

            TypedExprKind::Import(typed_import) => {
                (TypedExprKind::Import(typed_import), expr.type_.clone())
            }

            TypedExprKind::Error => (TypedExprKind::Error, expr.type_.clone()),

            other => (other, expr.type_.clone()),
        };

        TypedExpr {
            span: expr.span,
            file: expr.file,
            expr: new_expr,
            type_: new_type,
        }
    }

    fn monomorphize_type(&mut self, ty: &Rc<Type>) -> Rc<Type> {
        // Apply substitution to the type to resolve any type variables that have been bound
        // For now, we'll return the type as-is, but eventually we'll want to handle instantiation of generic types
        ty.clone()
    }

    fn get_or_create_specialization(
        &mut self,
        original_id: FunctionId,
        original_func: &TypedFunction,
        concrete_types: &[String],
        arg_exprs: &[TypedExpr],
    ) -> (FunctionId, Symbol) {
        let key = (original_id, concrete_types.to_vec());
        if let Some(&existing_id) = self.instantiations.get(&key) {
            let func = self.functions.get(&existing_id).unwrap();
            return (existing_id, func.name);
        }

        let new_id = self.id_gen.fresh_function();
        self.instantiations.insert(key, new_id);

        // Build substitution from parameters
        let mut subst = Substitution {
            map: HashMap::new(),
        };
        for (arg_expr, param) in arg_exprs.iter().zip(&original_func.args) {
            unify_for_monomorphization(&param.type_, &arg_expr.type_, &mut subst);
        }

        // CRITICAL FIX: If function body exists, use its type to infer return type
        if let Some(body) = &original_func.body {
            // Apply current substitution to body to get concrete type
            let subst_body = self.substitute_expr_with_subst(body, &subst);
            // Unify return type with body type
            unify_for_monomorphization(&original_func.return_type, &subst_body.type_, &mut subst);
        }

        let specialized_name = self.make_specialized_name(original_func.name, concrete_types);

        // Now apply complete substitution
        let specialized_args: Vec<_> = original_func
            .args
            .iter()
            .map(|arg| TypedFnArg {
                span: arg.span.clone(),
                file: arg.file.clone(),
                name: arg.name,
                binding_id: arg.binding_id,
                type_: subst.apply(&arg.type_),
            })
            .collect();

        let specialized_return = subst.apply(&original_func.return_type);

        // Build specialized function type
        let specialized_func_type = Rc::new(Type {
            span: original_func.function_type.span.clone(),
            file: original_func.function_type.file.clone(),
            type_: TypeKind::Function {
                params: specialized_args.iter().map(|a| a.type_.clone()).collect(),
                return_type: specialized_return.clone(),
                effects: original_func.effects.clone(),
            },
        });

        // Apply substitution to body
        let specialized_body = original_func.body.as_ref().map(|b| {
            let subst_body = self.substitute_expr_with_subst(b, &subst);
            self.monomorphize_expr(subst_body)
        });

        let specialized = TypedFunction {
            span: original_func.span.clone(),
            file: original_func.file.clone(),
            vis: original_func.vis,
            name: specialized_name,
            function_id: new_id,
            type_params: vec![],
            args: specialized_args,
            return_type: specialized_return,
            where_constraints: vec![],
            effects: original_func.effects.clone(),
            function_type: specialized_func_type,
            body: specialized_body,
        };

        self.specialized_functions.push(specialized.clone());
        self.functions.insert(new_id, specialized);

        (new_id, specialized_name)
    }

    #[allow(clippy::only_used_in_recursion)]
    fn get_or_create_specialized_struct(
        &mut self,
        _original_id: StructId,
        original_struct: &TypedStruct,
        concrete_types: &[String],
    ) -> (StructId, Symbol) {
        // Check if we already have this specialization
        // We'll use a simple approach for now - store the original for reference
        // In a more complex implementation, we'd create a new StructId and store the specialized struct
        let specialized_struct = self.specialize_struct(original_struct, concrete_types);
        let new_id = specialized_struct.struct_id; // This should be a fresh ID

        // For now, just add it to specialized structs and return it
        self.specialized_structs.push(specialized_struct);

        // Return the new ID and name (the specialize_struct function should create a new name)
        // For now, we'll return the original ID and a modified name
        let specialized_name = self.make_specialized_name(original_struct.name, concrete_types);
        (new_id, specialized_name)
    }

    fn get_or_create_specialized_enum(
        &mut self,
        _original_id: EnumId,
        original_enum: &TypedEnum,
        concrete_types: &[String],
    ) -> (EnumId, Symbol) {
        // Create a specialized enum instance
        let specialized_enum = self.specialize_enum(original_enum, concrete_types);
        let new_id = specialized_enum.enum_id;

        self.specialized_enums.push(specialized_enum);

        let specialized_name = self.make_specialized_name(original_enum.name, concrete_types);
        (new_id, specialized_name)
    }

    fn get_or_create_specialized_effect(
        &mut self,
        _original_id: EffectId,
        original_effect: &TypedEffectDef,
        concrete_types: &[String],
    ) -> (EffectId, Symbol) {
        // Create a specialized effect instance
        let specialized_effect = self.specialize_effect_def(original_effect, concrete_types);
        let new_id = specialized_effect.effect_id;

        self.specialized_effects.push(specialized_effect);

        let specialized_name = self.make_specialized_name(original_effect.name, concrete_types);
        (new_id, specialized_name)
    }

    fn specialize_struct(
        &mut self,
        original: &TypedStruct,
        concrete_types: &[String],
    ) -> TypedStruct {
        // Create a new struct ID for the specialized version
        let new_id = self.id_gen.fresh_struct();

        // Create a substitution map from type parameters to concrete types
        let mut subst = Substitution {
            map: HashMap::new(),
        };
        for (i, type_param) in original.type_params.iter().enumerate() {
            if i < concrete_types.len() {
                let concrete_type = self.create_simple_type_from_key(&concrete_types[i]);
                subst.bind(type_param.var_id, concrete_type);
            }
        }

        // Apply substitution to the struct fields
        let specialized_fields: Vec<_> = original
            .fields
            .iter()
            .map(|(arg, vis, field_id)| {
                let specialized_arg = TypedFnArg {
                    span: arg.span.clone(),
                    file: arg.file.clone(),
                    name: arg.name,
                    binding_id: arg.binding_id,
                    type_: subst.apply(&arg.type_),
                };
                (specialized_arg, *vis, *field_id)
            })
            .collect();

        // Specialize methods
        let specialized_methods: Vec<_> = original
            .methods
            .iter()
            .map(|m| self.specialize_function(m, &subst, concrete_types))
            .collect();

        // Create a new name for the specialized struct
        let specialized_name = self.make_specialized_name(original.name, concrete_types);

        // Create the specialized struct type
        let specialized_type =
            self.create_specialized_struct_type(specialized_name, concrete_types);

        TypedStruct {
            span: original.span.clone(),
            file: original.file.clone(),
            name: specialized_name,
            struct_id: new_id,
            vis: original.vis,
            type_params: vec![], // No type parameters in specialized version
            fields: specialized_fields,
            methods: specialized_methods,
            struct_type: specialized_type,
        }
    }

    fn specialize_enum(&mut self, original: &TypedEnum, concrete_types: &[String]) -> TypedEnum {
        // Create a new enum ID for the specialized version
        let new_id = self.id_gen.fresh_enum();

        // Create a substitution map from type parameters to concrete types
        let mut subst = Substitution {
            map: HashMap::new(),
        };
        for (i, type_param) in original.type_params.iter().enumerate() {
            if i < concrete_types.len() {
                let concrete_type = self.create_simple_type_from_key(&concrete_types[i]);
                subst.bind(type_param.var_id, concrete_type);
            }
        }

        // Apply substitution to the enum variants
        let specialized_variants: Vec<_> = original
            .variants
            .iter()
            .map(|v| {
                TypedEnumVariant {
                    span: v.span.clone(),
                    file: v.file.clone(),
                    name: v.name,
                    variant_id: v.variant_id,
                    types: v.types.iter().map(|t| subst.apply(t)).collect(),
                    constraints: v.constraints.clone(), // constraints may need substitution too
                    constructor_type: subst.apply(&v.constructor_type),
                }
            })
            .collect();

        // Specialize methods
        let specialized_methods: Vec<_> = original
            .methods
            .iter()
            .map(|m| self.specialize_function(m, &subst, concrete_types))
            .collect();

        // Create a new name for the specialized enum
        let specialized_name = self.make_specialized_name(original.name, concrete_types);

        // Create the specialized enum type
        let specialized_type = self.create_specialized_enum_type(specialized_name, concrete_types);

        TypedEnum {
            span: original.span.clone(),
            file: original.file.clone(),
            name: specialized_name,
            enum_id: new_id,
            vis: original.vis,
            type_params: vec![], // No type parameters in specialized version
            variants: specialized_variants,
            methods: specialized_methods,
            enum_type: specialized_type,
        }
    }

    fn specialize_effect_def(
        &mut self,
        original: &TypedEffectDef,
        concrete_types: &[String],
    ) -> TypedEffectDef {
        // Create a new effect ID for the specialized version
        let new_id = self.id_gen.fresh_effect();

        // Create a substitution map from type parameters to concrete types
        let mut subst = Substitution {
            map: HashMap::new(),
        };
        for (i, type_param) in original.type_params.iter().enumerate() {
            if i < concrete_types.len() {
                let concrete_type = self.create_simple_type_from_key(&concrete_types[i]);
                subst.bind(type_param.var_id, concrete_type);
            }
        }

        // Apply substitution to operations
        let specialized_operations: Vec<_> = original
            .operations
            .iter()
            .map(|op| TypedEffectOperation {
                span: op.span.clone(),
                name: op.name,
                operation_id: op.operation_id,
                params: op.params.iter().map(|p| subst.apply(p)).collect(),
                return_type: subst.apply(&op.return_type),
                operation_type: subst.apply(&op.operation_type),
            })
            .collect();

        // Create a new name for the specialized effect
        let specialized_name = self.make_specialized_name(original.name, concrete_types);

        TypedEffectDef {
            span: original.span.clone(),
            file: original.file.clone(),
            vis: original.vis,
            name: specialized_name,
            effect_id: new_id,
            type_params: vec![], // No type parameters in specialized version
            operations: specialized_operations,
            where_constraints: original.where_constraints.clone(), // May need substitution too
        }
    }

    // Helper to create specialized struct type
    fn create_specialized_struct_type_from_keys(
        &mut self,
        name: Symbol,
        concrete_type_keys: &[String],
    ) -> Rc<Type> {
        // Convert type keys back to types. For now, we'll use a simple approach.
        // Would need to look up the concrete types from their keys.
        let args: Vec<Rc<Type>> = concrete_type_keys
            .iter()
            .map(|key| {
                // We'll need to resolve the type from the key
                // This is a simplified approach - in practice you'd have a mapping from string keys to types
                match key.as_str() {
                    "int" => Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Constructor {
                            name: self.interner.intern("int").0,
                            args: vec![],
                            kind: Kind::Star,
                        },
                    }),
                    "bool" => Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Constructor {
                            name: self.interner.intern("bool").0,
                            args: vec![],
                            kind: Kind::Star,
                        },
                    }),
                    "string" => Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Constructor {
                            name: self.interner.intern("string").0,
                            args: vec![],
                            kind: Kind::Star,
                        },
                    }),
                    t if t.contains("<") => {
                        // Handle nested types like List<int>
                        let parts: Vec<&str> = t.split('<').collect();
                        if parts.len() >= 2 {
                            let base_name = parts[0];
                            let arg_part = parts[1].trim_end_matches('>');
                            Rc::new(Type {
                                span: None,
                                file: None,
                                type_: TypeKind::Constructor {
                                    name: self.interner.intern(base_name).0,
                                    args: vec![self.create_simple_type_from_key(arg_part)],
                                    kind: Kind::Star,
                                },
                            })
                        } else {
                            self.create_simple_type_from_key(t)
                        }
                    }
                    _ => self.create_simple_type_from_key(key),
                }
            })
            .collect();

        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: name.0,
                args,
                kind: Kind::Star,
            },
        })
    }

    // Helper to create simple type from a key (for types that don't have nested generics)
    fn create_simple_type_from_key(&mut self, key: &str) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: self.interner.intern(key).0,
                args: vec![],
                kind: Kind::Star,
            },
        })
    }

    // Helper to create specialized struct type
    fn create_specialized_struct_type(
        &mut self,
        name: Symbol,
        concrete_types: &[String],
    ) -> Rc<Type> {
        self.create_specialized_struct_type_from_keys(name, concrete_types)
    }

    // Helper to create specialized enum type
    fn create_specialized_enum_type(
        &mut self,
        name: Symbol,
        concrete_types: &[String],
    ) -> Rc<Type> {
        self.create_specialized_struct_type_from_keys(name, concrete_types) // Same implementation
    }

    // Helper to extract concrete types from struct construction
    fn extract_concrete_types_from_struct_construct(
        &self,
        struct_def: &TypedStruct,
        fields: &[(Symbol, FieldId, TypedExpr)],
    ) -> Vec<String> {
        // This is a more complex method. In a real scenario, we'd need to match fields to their expected types
        // and extract the concrete type information.
        // For now, we'll return an empty vector and implement better logic later.
        let mut concrete_types = Vec::new();

        // Look through the field types to extract concrete types corresponding to the type parameters
        for (field_name, _, field_expr) in fields {
            // Find which struct field this corresponds to and get its type
            for (arg, _, _field_id) in &struct_def.fields {
                if &arg.name == field_name {
                    concrete_types.push(self.type_to_key(&field_expr.type_));
                    break;
                }
            }
        }

        concrete_types
    }

    fn specialize_function(
        &mut self,
        original: &TypedFunction,
        subst: &Substitution,
        concrete_types: &[String],
    ) -> TypedFunction {
        // Create a new function ID for the specialized version
        let new_id = self.id_gen.fresh_function();

        // Apply substitution to function arguments
        let specialized_args: Vec<_> = original
            .args
            .iter()
            .map(|arg| TypedFnArg {
                span: arg.span.clone(),
                file: arg.file.clone(),
                name: arg.name,
                binding_id: arg.binding_id,
                type_: subst.apply(&arg.type_),
            })
            .collect();

        // Apply substitution to return type
        let specialized_return = subst.apply(&original.return_type);

        // Create specialized function type
        let specialized_func_type = Rc::new(Type {
            span: original.function_type.span.clone(),
            file: original.function_type.file.clone(),
            type_: TypeKind::Function {
                params: specialized_args.iter().map(|a| a.type_.clone()).collect(),
                return_type: specialized_return.clone(),
                effects: original.effects.clone(), // effects may need substitution too
            },
        });

        // Apply substitution to the body if it exists
        let specialized_body = original
            .body
            .as_ref()
            .map(|b| self.substitute_expr_with_subst(b, subst));

        // Create a new specialized name
        let specialized_name = self.make_specialized_name(original.name, concrete_types);

        TypedFunction {
            span: original.span.clone(),
            file: original.file.clone(),
            vis: original.vis,
            name: specialized_name,
            function_id: new_id,
            type_params: vec![], // No type parameters in specialized version
            args: specialized_args,
            return_type: specialized_return,
            where_constraints: original.where_constraints.clone(), // May need substitution too
            effects: original.effects.clone(),
            function_type: specialized_func_type,
            body: specialized_body,
        }
    }

    fn substitute_expr_with_subst(&mut self, expr: &TypedExpr, subst: &Substitution) -> TypedExpr {
        // CRITICAL: Apply substitution to the expression's type itself
        let new_type = subst.apply(&expr.type_);

        let new_expr = match &expr.expr {
            TypedExprKind::Int(n) => TypedExprKind::Int(*n),
            TypedExprKind::Float(f) => TypedExprKind::Float(*f),
            TypedExprKind::Bool(b) => TypedExprKind::Bool(*b),
            TypedExprKind::String(s) => TypedExprKind::String(s.clone()),

            TypedExprKind::Variable { name, binding_id } => TypedExprKind::Variable {
                name: *name,
                binding_id: *binding_id,
            },

            TypedExprKind::BinOp { left, op, right } => TypedExprKind::BinOp {
                left: Box::new(self.substitute_expr_with_subst(left, subst)),
                op: op.clone(),
                right: Box::new(self.substitute_expr_with_subst(right, subst)),
            },

            TypedExprKind::UnOp { op, operand } => TypedExprKind::UnOp {
                op: op.clone(),
                operand: Box::new(self.substitute_expr_with_subst(operand, subst)),
            },

            TypedExprKind::Call {
                function,
                args,
                type_args,
            } => TypedExprKind::Call {
                function: Box::new(self.substitute_expr_with_subst(function, subst)),
                args: args
                    .iter()
                    .map(|a| self.substitute_expr_with_subst(a, subst))
                    .collect(),
                type_args: type_args.iter().map(|t| subst.apply(t)).collect(),
            },

            TypedExprKind::IfElse {
                condition,
                then,
                else_,
            } => TypedExprKind::IfElse {
                condition: Box::new(self.substitute_expr_with_subst(condition, subst)),
                then: Box::new(self.substitute_expr_with_subst(then, subst)),
                else_: else_
                    .as_ref()
                    .map(|e| Box::new(self.substitute_expr_with_subst(e, subst))),
            },

            TypedExprKind::Block { expressions } => TypedExprKind::Block {
                expressions: expressions
                    .iter()
                    .map(|e| self.substitute_expr_with_subst(e, subst))
                    .collect(),
            },

            TypedExprKind::Let {
                var,
                binding_id,
                var_type,
                value,
            } => TypedExprKind::Let {
                var: *var,
                binding_id: *binding_id,
                var_type: subst.apply(var_type),
                value: Box::new(self.substitute_expr_with_subst(value, subst)),
            },

            TypedExprKind::Loop { label, body } => TypedExprKind::Loop {
                label: *label,
                body: Box::new(self.substitute_expr_with_subst(body, subst)),
            },

            TypedExprKind::While { condition, body } => TypedExprKind::While {
                condition: Box::new(self.substitute_expr_with_subst(condition, subst)),
                body: Box::new(self.substitute_expr_with_subst(body, subst)),
            },

            TypedExprKind::Match { scrutinee, arms } => TypedExprKind::Match {
                scrutinee: Box::new(self.substitute_expr_with_subst(scrutinee, subst)),
                arms: arms
                    .iter()
                    .map(|arm| TypedMatchArm {
                        pattern: self.substitute_pattern(&arm.pattern, subst),
                        guard: arm
                            .guard
                            .as_ref()
                            .map(|g| self.substitute_expr_with_subst(g, subst)),
                        body: Box::new(self.substitute_expr_with_subst(&arm.body, subst)),
                        span: arm.span.clone(),
                    })
                    .collect(),
            },

            TypedExprKind::Return(val) => TypedExprKind::Return(
                val.as_ref()
                    .map(|v| Box::new(self.substitute_expr_with_subst(v, subst))),
            ),

            TypedExprKind::Break(val) => TypedExprKind::Break(
                val.as_ref()
                    .map(|v| Box::new(self.substitute_expr_with_subst(v, subst))),
            ),

            TypedExprKind::Continue => TypedExprKind::Continue,

            TypedExprKind::Cast {
                expr: inner,
                target_type,
            } => TypedExprKind::Cast {
                expr: Box::new(self.substitute_expr_with_subst(inner, subst)),
                target_type: subst.apply(target_type),
            },

            TypedExprKind::Array {
                elements,
                element_type,
            } => TypedExprKind::Array {
                elements: elements
                    .iter()
                    .map(|e| self.substitute_expr_with_subst(e, subst))
                    .collect(),
                element_type: subst.apply(element_type),
            },

            TypedExprKind::Tuple(elems) => TypedExprKind::Tuple(
                elems
                    .iter()
                    .map(|e| self.substitute_expr_with_subst(e, subst))
                    .collect(),
            ),

            TypedExprKind::FieldAccess {
                target,
                field,
                field_id,
                field_type,
            } => TypedExprKind::FieldAccess {
                target: Box::new(self.substitute_expr_with_subst(target, subst)),
                field: *field,
                field_id: *field_id,
                field_type: subst.apply(field_type),
            },

            TypedExprKind::Index {
                target,
                index,
                element_type,
            } => TypedExprKind::Index {
                target: Box::new(self.substitute_expr_with_subst(target, subst)),
                index: Box::new(self.substitute_expr_with_subst(index, subst)),
                element_type: subst.apply(element_type),
            },

            TypedExprKind::Lambda {
                args,
                body,
                captures,
                function_type,
            } => TypedExprKind::Lambda {
                args: args
                    .iter()
                    .map(|arg| TypedFnArg {
                        span: arg.span.clone(),
                        file: arg.file.clone(),
                        name: arg.name,
                        binding_id: arg.binding_id,
                        type_: subst.apply(&arg.type_),
                    })
                    .collect(),
                body: Box::new(self.substitute_expr_with_subst(body, subst)),
                captures: captures
                    .iter()
                    .map(|(n, id, t)| (*n, *id, subst.apply(t)))
                    .collect(),
                function_type: subst.apply(function_type),
            },

            TypedExprKind::Perform {
                effect,
                effect_id,
                args,
            } => TypedExprKind::Perform {
                effect: *effect,
                effect_id: *effect_id,
                args: args
                    .iter()
                    .map(|a| self.substitute_expr_with_subst(a, subst))
                    .collect(),
            },

            other => other.clone(),
        };

        TypedExpr {
            span: expr.span.clone(),
            file: expr.file.clone(),
            expr: new_expr,
            type_: new_type, // CRITICAL: Use substituted type
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn substitute_pattern(&self, pat: &TypedPattern, subst: &Substitution) -> TypedPattern {
        let new_type = subst.apply(&pat.type_);

        let new_pat = match &pat.pat {
            TypedPatKind::Wildcard => TypedPatKind::Wildcard,

            TypedPatKind::Bind { name, binding_id } => TypedPatKind::Bind {
                name: *name,
                binding_id: *binding_id,
            },

            TypedPatKind::Literal(lit) => TypedPatKind::Literal(lit.clone()),

            TypedPatKind::Array {
                elements,
                element_type,
            } => TypedPatKind::Array {
                elements: elements
                    .iter()
                    .map(|e| self.substitute_array_pattern_element(e, subst))
                    .collect(),
                element_type: subst.apply(element_type),
            },

            TypedPatKind::Or(patterns) => TypedPatKind::Or(
                patterns
                    .iter()
                    .map(|p| self.substitute_pattern(p, subst))
                    .collect(),
            ),

            TypedPatKind::As {
                name,
                binding_id,
                pattern,
            } => TypedPatKind::As {
                name: *name,
                binding_id: *binding_id,
                pattern: Box::new(self.substitute_pattern(pattern, subst)),
            },

            TypedPatKind::Struct {
                name,
                struct_id,
                fields,
            } => TypedPatKind::Struct {
                name: *name,
                struct_id: *struct_id,
                fields: fields
                    .iter()
                    .map(|(f, fid, p)| (*f, *fid, self.substitute_pattern(p, subst)))
                    .collect(),
            },

            TypedPatKind::Enum {
                enum_name,
                enum_id,
                variant,
                variant_id,
                params,
            } => TypedPatKind::Enum {
                enum_name: *enum_name,
                enum_id: *enum_id,
                variant: *variant,
                variant_id: *variant_id,
                params: params
                    .iter()
                    .map(|p| self.substitute_pattern(p, subst))
                    .collect(),
            },

            TypedPatKind::Range { start, end } => TypedPatKind::Range {
                start: start.clone(),
                end: end.clone(),
            },

            TypedPatKind::Rest { name, binding_id } => TypedPatKind::Rest {
                name: *name,
                binding_id: *binding_id,
            },

            TypedPatKind::Tuple {
                patterns,
                element_types: _,
            } => TypedPatKind::Tuple {
                patterns: patterns
                    .iter()
                    .map(|p| self.substitute_pattern(p, subst))
                    .collect(),
                element_types: patterns.iter().map(|p| p.type_.clone()).collect(), // Keep the original element types
            },

            TypedPatKind::Error => TypedPatKind::Error,
        };

        TypedPattern {
            span: pat.span.clone(),
            file: pat.file.clone(),
            pat: new_pat,
            type_: new_type,
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn substitute_array_pattern_element(
        &self,
        element: &TypedArrayPatElement,
        subst: &Substitution,
    ) -> TypedArrayPatElement {
        match element {
            TypedArrayPatElement::Pattern(pattern) => {
                TypedArrayPatElement::Pattern(self.substitute_pattern(pattern, subst))
            }
            TypedArrayPatElement::Spread(pattern) => {
                TypedArrayPatElement::Spread(self.substitute_pattern(pattern, subst))
            }
        }
    }

    fn monomorphize_pattern(&mut self, pat: TypedPattern) -> TypedPattern {
        // Apply substitution to the pattern's type
        let new_type = self.monomorphize_type(&pat.type_);

        let new_pat = match pat.pat {
            TypedPatKind::Wildcard => TypedPatKind::Wildcard,
            TypedPatKind::Bind { name, binding_id } => TypedPatKind::Bind { name, binding_id },
            TypedPatKind::Literal(lit) => TypedPatKind::Literal(lit),
            TypedPatKind::Array {
                elements,
                element_type,
            } => TypedPatKind::Array {
                elements: elements
                    .into_iter()
                    .map(|e| self.monomorphize_array_pattern_element(e))
                    .collect(),
                element_type: self.monomorphize_type(&element_type),
            },
            TypedPatKind::Or(patterns) => TypedPatKind::Or(
                patterns
                    .into_iter()
                    .map(|p| self.monomorphize_pattern(p))
                    .collect(),
            ),
            TypedPatKind::As {
                name,
                binding_id,
                pattern,
            } => TypedPatKind::As {
                name,
                binding_id,
                pattern: Box::new(self.monomorphize_pattern(*pattern)),
            },
            TypedPatKind::Struct {
                name,
                struct_id,
                fields,
            } => TypedPatKind::Struct {
                name,
                struct_id,
                fields: fields
                    .into_iter()
                    .map(|(f, fid, p)| (f, fid, self.monomorphize_pattern(p)))
                    .collect(),
            },
            TypedPatKind::Enum {
                enum_name,
                enum_id,
                variant,
                variant_id,
                params,
            } => TypedPatKind::Enum {
                enum_name,
                enum_id,
                variant,
                variant_id,
                params: params
                    .into_iter()
                    .map(|p| self.monomorphize_pattern(p))
                    .collect(),
            },
            TypedPatKind::Range { start, end } => TypedPatKind::Range { start, end },
            TypedPatKind::Rest { name, binding_id } => TypedPatKind::Rest { name, binding_id },
            TypedPatKind::Tuple {
                patterns,
                element_types,
            } => TypedPatKind::Tuple {
                patterns: patterns
                    .into_iter()
                    .map(|p| self.monomorphize_pattern(p))
                    .collect(),
                element_types: element_types
                    .into_iter()
                    .map(|t| self.monomorphize_type(&t))
                    .collect(),
            },
            TypedPatKind::Error => TypedPatKind::Error,
        };

        TypedPattern {
            span: pat.span,
            file: pat.file,
            pat: new_pat,
            type_: new_type,
        }
    }

    fn monomorphize_array_pattern_element(
        &mut self,
        element: TypedArrayPatElement,
    ) -> TypedArrayPatElement {
        match element {
            TypedArrayPatElement::Pattern(pattern) => {
                TypedArrayPatElement::Pattern(self.monomorphize_pattern(pattern))
            }
            TypedArrayPatElement::Spread(pattern) => {
                TypedArrayPatElement::Spread(self.monomorphize_pattern(pattern))
            }
        }
    }

    pub fn collect_type_vars_from_func(&self, func: &TypedFunction) -> Vec<usize> {
        // Check if the function type is a Forall type, which indicates polymorphism
        match &func.function_type.type_ {
            TypeKind::Forall { vars, .. } => {
                // This is a polymorphic function, extract the actual type variable IDs
                vars.iter().map(|(id, _)| *id).collect()
            }
            _ => {
                // For regular functions, collect actual type variables
                let mut vars = Vec::new();
                for arg in &func.args {
                    self.collect_type_vars_rec(&arg.type_, &mut vars);
                }
                self.collect_type_vars_rec(&func.return_type, &mut vars);
                vars.sort();
                vars.dedup();
                vars
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    pub fn collect_type_vars_rec(&self, ty: &Rc<Type>, vars: &mut Vec<usize>) {
        match &ty.type_ {
            TypeKind::Variable { id, .. } => vars.push(*id),
            TypeKind::Constructor { args, .. } => {
                for arg in args {
                    self.collect_type_vars_rec(arg, vars);
                }
            }
            TypeKind::Function {
                params,
                return_type,
                ..
            } => {
                for param in params {
                    self.collect_type_vars_rec(param, vars);
                }
                self.collect_type_vars_rec(return_type, vars);
            }
            _ => {}
        }
    }

    pub fn type_to_key(&self, ty: &Rc<Type>) -> String {
        match &ty.type_ {
            TypeKind::Constructor { name, args, .. } => {
                let name_str = self.interner.resolve(Symbol(*name));
                if args.is_empty() {
                    name_str.to_string()
                } else {
                    let arg_strs: Vec<_> = args.iter().map(|a| self.type_to_key(a)).collect();
                    format!("{}<{}>", name_str, arg_strs.join(","))
                }
            }
            TypeKind::Variable { id, .. } => format!("?{}", id),
            _ => "unknown".to_string(),
        }
    }

    pub fn make_specialized_name(&mut self, base: Symbol, concrete_types: &[String]) -> Symbol {
        let base_str = self.interner.resolve(base);
        let type_str = concrete_types
            .join("_")
            .replace("<", "")
            .replace(">", "")
            .replace(",", "_");
        let specialized = format!("{}${}", base_str, type_str);
        self.interner.intern(&specialized)
    }

    pub fn fresh_function(&mut self) -> FunctionId {
        self.id_gen.fresh_function()
    }

    pub fn fresh_binding(&mut self) -> BindingId {
        self.id_gen.fresh_binding()
    }

    pub fn error_type(&self) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Error,
        })
    }
}

fn unify_for_monomorphization(pattern: &Rc<Type>, concrete: &Rc<Type>, subst: &mut Substitution) {
    match (&pattern.type_, &concrete.type_) {
        (TypeKind::Variable { id, .. }, _) => {
            subst.bind(TypeId(*id), concrete.clone());
        }
        (TypeKind::Constructor { args: args1, .. }, TypeKind::Constructor { args: args2, .. })
            if args1.len() == args2.len() =>
        {
            for (a1, a2) in args1.iter().zip(args2.iter()) {
                unify_for_monomorphization(a1, a2, subst);
            }
        }
        (
            TypeKind::Function {
                params: p1,
                return_type: r1,
                ..
            },
            TypeKind::Function {
                params: p2,
                return_type: r2,
                ..
            },
        ) if p1.len() == p2.len() => {
            for (param1, param2) in p1.iter().zip(p2.iter()) {
                unify_for_monomorphization(param1, param2, subst);
            }
            unify_for_monomorphization(r1, r2, subst);
        }
        _ => {}
    }
}
