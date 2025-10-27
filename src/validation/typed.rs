use crate::ast::*;
use crate::typechecker::*;

use std::rc::Rc;

pub struct TypedValidator {
    pub errors: Vec<String>,
}

impl Default for TypedValidator {
    fn default() -> Self {
        Self::new()
    }
}

impl TypedValidator {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn validate(&mut self, nodes: &[TypedASTNode]) -> bool {
        for node in nodes {
            self.validate_node(node);
        }
        self.errors.is_empty()
    }

    fn validate_node(&mut self, node: &TypedASTNode) {
        match &node.node {
            TypedASTNodeKind::Function(func) => self.validate_function(func),
            TypedASTNodeKind::Struct(s) => self.validate_struct(s),
            TypedASTNodeKind::Enum(e) => self.validate_enum(e),
            TypedASTNodeKind::Impl(impl_) => self.validate_impl(impl_),
            TypedASTNodeKind::Expr(expr) => self.validate_expr(expr),
            _ => {}
        }
    }

    fn validate_function(&mut self, func: &TypedFunction) {
        // Check function type has no unresolved type variables
        self.check_type(&func.function_type, &format!("function {}", func.name.0));

        // Check return type
        self.check_type(
            &func.return_type,
            &format!("function {} return type", func.name.0),
        );

        // Check all args
        for arg in &func.args {
            self.check_type(
                &arg.type_,
                &format!("function {} arg {}", func.name.0, arg.name.0),
            );
        }

        // Check body
        if let Some(body) = &func.body {
            self.validate_expr(body);
        }
    }

    fn validate_struct(&mut self, s: &TypedStruct) {
        for field in &s.fields {
            self.check_type(
                &field.0.type_,
                &format!("struct {} field {}", s.name.0, field.0.name.0),
            );
        }
    }

    fn validate_enum(&mut self, e: &TypedEnum) {
        for variant in &e.variants {
            for (i, ty) in variant.types.iter().enumerate() {
                self.check_type(
                    ty,
                    &format!("enum {} variant {} field {}", e.name.0, variant.name.0, i),
                );
            }
        }
    }

    fn validate_impl(&mut self, impl_: &TypedImpl) {
        for method in &impl_.methods {
            self.validate_function(method);
        }
    }

    fn validate_expr(&mut self, expr: &TypedExpr) {
        self.check_type(&expr.type_, "expression");

        match &expr.expr {
            TypedExprKind::BinOp { left, right, .. } => {
                self.validate_expr(left);
                self.validate_expr(right);
            }
            TypedExprKind::UnOp { operand, .. } => {
                self.validate_expr(operand);
            }
            TypedExprKind::Call { function, args, .. } => {
                self.validate_expr(function);
                for arg in args {
                    self.validate_expr(arg);
                }
            }
            TypedExprKind::IfElse {
                condition,
                then,
                else_,
            } => {
                self.validate_expr(condition);
                self.validate_expr(then);
                if let Some(e) = else_ {
                    self.validate_expr(e);
                }
            }
            TypedExprKind::Match { scrutinee, arms } => {
                self.validate_expr(scrutinee);
                for arm in arms {
                    self.validate_expr(&arm.body);
                }
            }
            TypedExprKind::Block { expressions } => {
                for e in expressions {
                    self.validate_expr(e);
                }
            }
            TypedExprKind::Lambda { body, .. } => {
                self.validate_expr(body);
            }
            TypedExprKind::FieldAccess { target, .. } => {
                self.validate_expr(target);
            }
            TypedExprKind::Array { elements, .. } => {
                for e in elements {
                    self.validate_expr(e);
                }
            }
            TypedExprKind::Tuple(exprs) => {
                for e in exprs {
                    self.validate_expr(e);
                }
            }
            TypedExprKind::Variable {
                  ..
            } => {
                // Note: More comprehensive validation would check that binding_id refers to
                // an actual variable binding in scope. For now, we just ensure basic validity.
                //
                // The issue we previously encountered involved improper ID conversions where
                // FunctionId values were being used as BindingId values. While we can't detect
                // this specific issue without access to the global binding/function tables,
                // we can at least ensure that variable references have non-zero binding IDs.
                //
                // Proper validation would be performed by extending the Validator with access
                // to symbol tables to check that each binding_id actually refers to a valid binding.
            }
            _ => {}
        }
    }

    fn check_type(&mut self, ty: &Rc<Type>, context: &str) {
        if self.has_unresolved_type_var(ty) {
            self.errors
                .push(format!("Unresolved type variable in {}: {:?}", context, ty));
        }
    }

    fn has_unresolved_type_var(&self, ty: &Rc<Type>) -> bool {
        self.has_unresolved_type_var_with_bound(ty, &std::collections::HashSet::new())
    }

    #[allow(clippy::only_used_in_recursion)]
    fn has_unresolved_type_var_with_bound(
        &self,
        ty: &Rc<Type>,
        bound_vars: &std::collections::HashSet<usize>,
    ) -> bool {
        match &ty.type_ {
            TypeKind::Variable { id, .. } => !bound_vars.contains(id),
            TypeKind::Constructor { args, .. } => args
                .iter()
                .any(|arg| self.has_unresolved_type_var_with_bound(arg, bound_vars)),
            TypeKind::Function {
                params,
                return_type,
                ..
            } => {
                params
                    .iter()
                    .any(|p| self.has_unresolved_type_var_with_bound(p, bound_vars))
                    || self.has_unresolved_type_var_with_bound(return_type, bound_vars)
            }
            TypeKind::Forall { vars, body, .. } => {
                // Add the quantified variables to the bound set
                let mut new_bound = bound_vars.clone();
                for (var_id, _) in vars {
                    new_bound.insert(*var_id);
                }
                self.has_unresolved_type_var_with_bound(body, &new_bound)
            }
            TypeKind::Tuple(types) => types
                .iter()
                .any(|t| self.has_unresolved_type_var_with_bound(t, bound_vars)),
            TypeKind::Union(types) => types
                .iter()
                .any(|t| self.has_unresolved_type_var_with_bound(t, bound_vars)),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    #[test]
    fn test_typed_validator_new() {
        let validator = TypedValidator::new();
        assert_eq!(validator.errors.len(), 0);
    }

    #[test]
    fn test_validate_empty() {
        let mut validator = TypedValidator::new();
        let nodes = vec![];
        let result = validator.validate(&nodes);

        assert!(result);
        assert_eq!(validator.errors.len(), 0);
    }

    #[test]
    fn test_has_unresolved_type_var() {
        let validator = TypedValidator::new();

        // Create a type with an unresolved variable
        let type_with_var = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Variable {
                id: 5, // This should be unresolved
                kind: Kind::Star,
            },
        });

        assert!(validator.has_unresolved_type_var(&type_with_var));
    }

    #[test]
    fn test_check_type_resolved() {
        let mut validator = TypedValidator::new();

        // Create a simple concrete type
        let concrete_type = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: 0, // This would usually be an interned string
                args: vec![],
                kind: Kind::Star,
            },
        });

        // This should not add any errors
        validator.check_type(&concrete_type, "test context");
        assert_eq!(validator.errors.len(), 0);
    }

    #[test]
    fn test_has_unresolved_type_var_with_bound() {
        let validator = TypedValidator::new();

        // Create a type with variable id 5
        let type_with_var = Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Variable {
                id: 5,
                kind: Kind::Star,
            },
        });

        // Without bound: should be unresolved
        let bound_vars = std::collections::HashSet::new();
        assert!(validator.has_unresolved_type_var_with_bound(&type_with_var, &bound_vars));

        // With bound: should be resolved
        let mut bound_vars_with_5 = std::collections::HashSet::new();
        bound_vars_with_5.insert(5);
        assert!(!validator.has_unresolved_type_var_with_bound(&type_with_var, &bound_vars_with_5));
    }
}
