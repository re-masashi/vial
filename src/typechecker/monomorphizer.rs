use crate::typechecker::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Monomorphizer {
    pub interner: Interner,
    id_gen: IdGen,
    pub instantiations: HashMap<(FunctionId, Vec<String>), FunctionId>,
    pub functions: HashMap<FunctionId, TypedFunction>,
    pub specialized_functions: Vec<TypedFunction>,
}

impl Monomorphizer {
    pub fn new(interner: Interner) -> Self {
        Self {
            interner,
            id_gen: IdGen::new(),
            instantiations: HashMap::new(),
            functions: HashMap::new(),
            specialized_functions: Vec::new(),
        }
    }

    pub fn monomorphize_program(&mut self, program: Vec<TypedASTNode>) -> Vec<TypedASTNode> {
        // Collect all functions
        for node in &program {
            if let TypedASTNodeKind::Function(func) = &node.node {
                self.functions.insert(func.function_id, (**func).clone());
            }
        }

        // Process nodes
        let mut result = Vec::new();
        for node in program {
            match node.node {
                TypedASTNodeKind::Function(func) => {
                    if func.type_params.is_empty() {
                        let type_vars = self.collect_type_vars_from_func(&func);
                        if type_vars.is_empty() {
                            // No type variables - keep it and monomorphize body
                            let mono_func = self.monomorphize_function_body(*func);
                            result.push(TypedASTNode {
                                span: node.span,
                                file: node.file,
                                node: TypedASTNodeKind::Function(Box::new(mono_func)),
                                attributes: node.attributes,
                            });
                        } else {
                            // Has type variables but no type params - this is a generic function
                            // Don't include it in output, it will be specialized
                        }
                    }
                    // Functions with explicit type parameters are also generic and shouldn't be included
                    // until they are specialized
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

        result
    }

    fn monomorphize_function_body(&mut self, mut func: TypedFunction) -> TypedFunction {
        if let Some(body) = func.body {
            func.body = Some(self.monomorphize_expr(body));
        }
        func
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

                        if !type_vars.is_empty() {
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
                    var_type,
                    value: Box::new(self.monomorphize_expr(*value)),
                },
                expr.type_.clone(),
            ),

            other => (other, expr.type_.clone()),
        };

        TypedExpr {
            span: expr.span,
            file: expr.file,
            expr: new_expr,
            type_: new_type,
        }
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
            self.unify_for_monomorphization(&param.type_, &arg_expr.type_, &mut subst);
        }

        // CRITICAL FIX: If function body exists, use its type to infer return type
        if let Some(body) = &original_func.body {
            // Apply current substitution to body to get concrete type
            let subst_body = self.substitute_expr_with_subst(body, &subst);
            // Unify return type with body type
            self.unify_for_monomorphization(
                &original_func.return_type,
                &subst_body.type_,
                &mut subst,
            );
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
    fn unify_for_monomorphization(
        &self,
        pattern: &Rc<Type>,
        concrete: &Rc<Type>,
        subst: &mut Substitution,
    ) {
        match (&pattern.type_, &concrete.type_) {
            (TypeKind::Variable { id, .. }, _) => {
                subst.bind(TypeId(*id), concrete.clone());
            }
            (
                TypeKind::Constructor { args: args1, .. },
                TypeKind::Constructor { args: args2, .. },
            ) if args1.len() == args2.len() => {
                for (a1, a2) in args1.iter().zip(args2.iter()) {
                    self.unify_for_monomorphization(a1, a2, subst);
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
                    self.unify_for_monomorphization(param1, param2, subst);
                }
                self.unify_for_monomorphization(r1, r2, subst);
            }
            _ => {}
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
                patterns,
                element_type,
            } => TypedPatKind::Array {
                patterns: patterns
                    .iter()
                    .map(|p| self.substitute_pattern(p, subst))
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

    pub fn error_type(&self) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Error,
        })
    }
}
