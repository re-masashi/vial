use crate::typechecker::*;
use std::collections::HashMap;

// Trait Resolver
#[derive(Debug, Clone)]
pub struct TraitResolver {
    // Map from (TraitId, TypeId) to impl block
    impls: HashMap<(TraitId, TypeId), TypedImpl>,
    // Blanket impls (generic impls like impl<T> Trait for T)
    blanket_impls: Vec<TypedImpl>,
    // Trait method cache
    // method_cache: HashMap<(TraitId, Symbol), FunctionId>,
}

impl Default for TraitResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl TraitResolver {
    pub fn new() -> Self {
        Self {
            impls: HashMap::new(),
            blanket_impls: Vec::new(),
            // method_cache: HashMap::new(),
        }
    }

    pub fn register_impl(&mut self, impl_block: TypedImpl) {
        if let Some((_, trait_id)) = impl_block.trait_ {
            // Check if it's a blanket impl (has type parameters)
            if !impl_block.type_params.is_empty() {
                self.blanket_impls.push(impl_block);
            } else {
                self.impls
                    .insert((trait_id, impl_block.type_id), impl_block);
            }
        }
    }

    // Find impl for a concrete type
    pub fn find_impl(&self, trait_id: TraitId, ty: &Rc<Type>) -> Option<&TypedImpl> {
        match &ty.type_ {
            TypeKind::Constructor { name, .. } => {
                // Try concrete impl first
                let type_id = TypeId(*name);
                if let Some(impl_block) = self.impls.get(&(trait_id, type_id)) {
                    return Some(impl_block);
                }

                // Try blanket impls
                for blanket in &self.blanket_impls {
                    if let Some((_, blanket_trait)) = blanket.trait_
                        && blanket_trait == trait_id
                    {
                        // TODO: Check if type matches blanket impl pattern
                        return Some(blanket);
                    }
                }

                None
            }
            _ => None,
        }
    }

    // Resolve method call on a trait
    pub fn resolve_trait_method(
        &self,
        trait_id: TraitId,
        method_name: Symbol,
        receiver_ty: &Rc<Type>,
    ) -> Option<FunctionId> {
        if let Some(impl_block) = self.find_impl(trait_id, receiver_ty) {
            for method in &impl_block.methods {
                if method.name == method_name {
                    return Some(method.function_id);
                }
            }
        }
        None
    }
}

// Trait Constraint Solver
#[derive(Debug, Clone)]
pub struct TraitConstraint {
    pub type_: Rc<Type>,
    pub trait_id: TraitId,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub struct ConstraintSolver {
    constraints: Vec<TraitConstraint>,
    trait_resolver: TraitResolver,
}

impl ConstraintSolver {
    pub fn new(trait_resolver: TraitResolver) -> Self {
        Self {
            constraints: Vec::new(),
            trait_resolver,
        }
    }

    pub fn add_constraint(&mut self, constraint: TraitConstraint) {
        self.constraints.push(constraint);
    }

    pub fn solve(&self, diagnostics: &mut Diagnostics) -> bool {
        let mut success = true;

        for constraint in &self.constraints {
            if !self.check_constraint(constraint, diagnostics) {
                success = false;
            }
        }

        success
    }

    fn check_constraint(
        &self,
        constraint: &TraitConstraint,
        diagnostics: &mut Diagnostics,
    ) -> bool {
        // Check if type implements trait
        match self
            .trait_resolver
            .find_impl(constraint.trait_id, &constraint.type_)
        {
            Some(_) => true,
            None => {
                diagnostics.add_type_error(TypeError {
                    span: constraint.span.clone(),
                    file: String::new(),
                    kind: TypeErrorKind::TraitNotImplemented {
                        trait_: Symbol(constraint.trait_id.0),
                        type_: constraint.type_.clone(),
                    },
                });
                false
            }
        }
    }
}

// Coherence Checking (no overlapping impls)
pub fn check_coherence(impls: &[TypedImpl], _diagnostics: &mut Diagnostics) {
    let mut seen = HashMap::new();

    for impl_block in impls {
        if let Some((_, trait_id)) = impl_block.trait_ {
            let key = (trait_id, impl_block.type_id);

            if let Some(_existing) = seen.get(&key) {
                todo!("just tel the user that bad things happen when you redeclare an impl")
                // diagnostics.add_validation_error(ValidationError {
                //     span: impl_block.span.clone(),
                //     file: impl_block.file.clone(),
                //     kind: ValidationErrorKind::DuplicateDefinition {
                //         name: impl_block.type_name,
                //         first_defined: *existing,
                //     },
                // });
            } else {
                seen.insert(key, impl_block.span.clone());
            }
        }
    }
}
