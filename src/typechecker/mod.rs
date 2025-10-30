// src/typechecker/mod.rs
use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::rc::Rc;

use crate::ast::*;

use ariadne::{Color, Label, Report, ReportKind, Source};

pub mod exhaustiveness_checker;
pub mod monomorphizer;
pub mod resolver;

#[cfg(test)]
pub mod tests;

use exhaustiveness_checker::*;
use resolver::*;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Type {
    pub span: Option<Range<usize>>,
    pub file: Option<String>,
    pub type_: TypeKind,
}

impl Type {
    pub fn display(&self, interner: &Interner) -> String {
        match &self.type_ {
            TypeKind::Constructor { name, args, .. } => {
                if args.is_empty() {
                    interner.resolve(Symbol(*name)).to_string()
                } else {
                    format!(
                        "{}<{}>",
                        interner.resolve(Symbol(*name)),
                        args.iter()
                            .map(|a| a.display(interner))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            TypeKind::Variable { id, .. } => format!("?{}", id),
            TypeKind::Function {
                params,
                return_type,
                ..
            } => {
                format!(
                    "({}) -> {}",
                    params
                        .iter()
                        .map(|p| p.display(interner))
                        .collect::<Vec<_>>()
                        .join(", "),
                    return_type.display(interner)
                )
            }
            TypeKind::Never => "!".to_string(),
            TypeKind::Error => "<error>".to_string(),
            TypeKind::Pointer(inner) => format!("*{}", inner.display(interner)),
            _ => format!("{:?}", self.type_),
        }
    }

    pub fn new_error() -> Type {
        Type {
            span: None,
            file: None,
            type_: TypeKind::Error,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TypeKind {
    Constructor {
        name: usize,
        args: Vec<Rc<Type>>,
        kind: Kind,
    },

    Variable {
        id: usize,
        kind: Kind,
    },

    // Row polymorphism: { x: Int, y: String | rest }
    Row {
        fields: Vec<(usize, Rc<Type>)>, // <- Vec instead of HashMap
        rest: Option<usize>,
    },

    Function {
        params: Vec<Rc<Type>>,
        return_type: Rc<Type>,
        effects: EffectSet,
    },

    Trait(Vec<usize>),

    Tuple(Vec<Rc<Type>>),

    Union(Vec<Rc<Type>>),

    // GADT: Universal quantification
    Forall {
        vars: Vec<(usize, Kind)>,
        constraints: Vec<Constraint>,
        body: Rc<Type>,
    },

    // GADT: Existential types
    Exists {
        vars: Vec<(usize, Kind)>,
        constraints: Vec<Constraint>,
        body: Rc<Type>,
    },

    Never,
    // Unit, // unit is a constructor
    Error,
    Pointer(Rc<Type>), // For FFI and low-level operations
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct EffectSet {
    pub effects: Vec<usize>,
    pub rest: Option<usize>,
}

impl EffectSet {
    pub fn pure() -> Self {
        Self {
            effects: vec![],
            rest: None,
        }
    }

    // Union two effect sets
    pub fn union(mut self, other: &EffectSet) -> Self {
        // Add effects from other that aren't already in self
        for effect in &other.effects {
            if !self.effects.contains(effect) {
                self.effects.push(*effect);
            }
        }

        // For open effect sets, we need to handle the rest variable appropriately
        // For now, if either set has a rest, we return the more general one
        if self.rest.is_none() {
            self.rest = other.rest;
        }

        self
    }

    // Check if this effect set is a subset of another (i.e., can be handled by other)
    pub fn is_subset_of(&self, other: &EffectSet) -> bool {
        // Every effect in self must be in other
        for effect in &self.effects {
            if !other.effects.contains(effect) {
                // If other has a rest variable, it can handle unknown effects
                if other.rest.is_none() {
                    return false;
                }
            }
        }
        true
    }

    // Check if this effect set is empty (pure)
    pub fn is_pure(&self) -> bool {
        self.effects.is_empty()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Constraint {
    Equal(Rc<Type>, Rc<Type>),
    Trait(usize, usize),
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub span: Range<usize>,
    pub file: String,
    pub kind: TypeErrorKind,
}

#[derive(Debug, Clone)]
pub enum TypeErrorKind {
    TypeMismatch {
        expected: Rc<Type>,
        found: Rc<Type>,
        context: String,
    },
    UnboundVariable {
        name: Symbol,
    },
    UnboundType {
        name: Symbol,
    },
    ArityMismatch {
        expected: usize,
        found: usize,
        function: Symbol,
    },
    EffectMismatch {
        required: EffectSet,
        found: EffectSet,
    },
    OccursCheck {
        var: TypeId,
        in_type: Rc<Type>,
    },
    InvalidFieldAccess {
        type_: Rc<Type>,
        field: Symbol,
    },
    MissingField {
        struct_name: Symbol,
        field: Symbol,
    },
    DuplicateField {
        field: Symbol,
    },
    NotAFunction {
        type_: Rc<Type>,
    },
    PrivacyViolation {
        item: Symbol,
    },
    TraitNotImplemented {
        trait_: Symbol,
        type_: Rc<Type>,
    },
    AmbiguousType {
        candidates: Vec<Rc<Type>>,
    },
    NonExhaustiveMatch {
        missing_patterns: Vec<String>,
    },
    InvalidOperator {
        op: String,
    },
}

impl TypeError {
    pub fn report(&self, interner: &Interner, source: &str) {
        match &self.kind {
            TypeErrorKind::TypeMismatch {
                expected,
                found,
                context,
            } => {
                Report::build(ReportKind::Error, (self.file.clone(), self.span.clone()))
                    .with_code("E001")
                    .with_message("Type mismatch")
                    .with_label(
                        Label::new((self.file.clone(), self.span.clone()))
                            .with_message(format!(
                                "Expected type {}, but found {}",
                                expected.display(interner),
                                found.display(interner)
                            ))
                            .with_color(Color::Red),
                    )
                    .with_note(format!("In context: {}", context))
                    .finish()
                    .eprint((self.file.clone(), Source::from(source)))
                    .unwrap();
            }

            TypeErrorKind::UnboundVariable { name } => {
                Report::build(ReportKind::Error, (self.file.clone(), self.span.clone()))
                    .with_code("E002")
                    .with_message(format!(
                        "Cannot find variable `{}`",
                        interner.resolve(*name)
                    ))
                    .with_label(
                        Label::new((self.file.clone(), self.span.clone()))
                            .with_message("Not found in this scope")
                            .with_color(Color::Red),
                    )
                    .with_help("Did you mean to import it?")
                    .finish()
                    .eprint((self.file.clone(), Source::from(source)))
                    .unwrap();
            }

            TypeErrorKind::ArityMismatch {
                expected,
                found,
                function,
            } => {
                Report::build(ReportKind::Error, (self.file.clone(), self.span.clone()))
                    .with_code("E003")
                    .with_message(format!(
                        "Function `{}` takes {} arguments but {} were supplied",
                        interner.resolve(*function),
                        expected,
                        found
                    ))
                    .with_label(
                        Label::new((self.file.clone(), self.span.clone()))
                            .with_message(format!("Expected {} arguments", expected))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint((self.file.clone(), Source::from(source)))
                    .unwrap();
            }

            TypeErrorKind::NonExhaustiveMatch { missing_patterns } => {
                Report::build(ReportKind::Error, (self.file.clone(), self.span.clone()))
                    .with_code("E004")
                    .with_message("Non-exhaustive patterns")
                    .with_label(
                        Label::new((self.file.clone(), self.span.clone()))
                            .with_message(format!(
                                "Pattern(s) {} not covered",
                                missing_patterns.join(", ")
                            ))
                            .with_color(Color::Red),
                    )
                    .with_help(format!("Add patterns for: {}", missing_patterns.join(", ")))
                    .finish()
                    .eprint((self.file.clone(), Source::from(source)))
                    .unwrap();
            }

            _ => {
                Report::build(ReportKind::Error, (self.file.clone(), self.span.clone()))
                    .with_message(format!("Type error: {:?}", self.kind))
                    .with_label(
                        Label::new((self.file.clone(), self.span.clone())).with_color(Color::Red),
                    )
                    .finish()
                    .eprint((self.file.clone(), Source::from(source)))
                    .unwrap();
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct Diagnostics {
    pub type_errors: Vec<TypeError>,
    // pub validation_errors: Vec<ValidationError>,
    // pub codegen_errors: Vec<CodeGenError>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_type_error(&mut self, error: TypeError) {
        self.type_errors.push(error);
    }

    // pub fn add_validation_error(&mut self, error: ValidationError) {
    //     self.validation_errors.push(error);
    // }

    // pub fn add_codegen_error(&mut self, error: CodeGenError) {
    //     self.codegen_errors.push(error);
    // }

    pub fn has_errors(&self) -> bool {
        !self.type_errors.is_empty()
        // || !self.validation_errors.is_empty()
        // || !self.codegen_errors.is_empty()
    }

    pub fn report_all(
        &self,
        interner: &Interner,
        sources: &std::collections::HashMap<String, String>,
    ) {
        // Debug print statement removed to avoid invalid interner access
        // println!("{:?}", self.type_errors);

        // TODO: Sort errors by location for better UX
        for error in &self.type_errors {
            if let Some(source) = sources.get(&error.file) {
                error.report(interner, source);
            }
        }

        // for error in &self.validation_errors {
        //     if let Some(source) = sources.get(&error.file) {
        //         error.report(interner, source);
        //     }
        // }

        // for error in &self.codegen_errors {
        //     // Report codegen errors...
        // }
    }
}

// Type Environment
#[derive(Debug, Clone)]
pub struct TypeEnv {
    pub scopes: Vec<Scope>,
    pub bindings: HashMap<BindingId, Binding>,
    pub types: HashMap<TypeId, TypeInfo>,
    pub functions: HashMap<FunctionId, FunctionInfo>,
    pub structs: HashMap<StructId, StructInfo>,
    pub enums: HashMap<EnumId, EnumInfo>,
    pub traits: HashMap<TraitId, TraitInfo>,
    pub effects: HashMap<EffectId, EffectInfo>,
    pub type_aliases: HashMap<TypeAliasId, TypeAliasInfo>,
}

#[derive(Debug, Clone)]
pub struct TypeAliasInfo {
    pub id: TypeAliasId,
    pub name: Symbol,
    pub expanded_type: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    names: HashMap<Symbol, BindingId>,
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub id: BindingId,
    pub name: Symbol,
    pub type_: Rc<Type>,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub id: TypeId,
    pub name: Symbol,
    pub kind: Kind,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub id: FunctionId,
    pub name: Symbol,
    pub type_: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct StructInfo {
    pub id: StructId,
    pub name: Symbol,
    pub type_params: Vec<TypeId>,
    pub fields: HashMap<Symbol, (FieldId, Rc<Type>, Visibility)>,
}

#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub id: EnumId,
    pub name: Symbol,
    pub type_params: Vec<TypeId>,
    pub variants: HashMap<Symbol, (VariantId, Vec<Rc<Type>>)>,
}

#[derive(Debug, Clone)]
pub struct TraitInfo {
    pub id: TraitId,
    pub name: Symbol,
    pub methods: HashMap<Symbol, FunctionId>,
}

#[derive(Debug, Clone)]
pub struct EffectInfo {
    pub id: EffectId,
    pub name: Symbol,
    pub operations: HashMap<Symbol, Rc<Type>>,
    pub type_params: Vec<(Symbol, TypeId)>, // Store type parameter names and their type variable IDs
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope {
                names: HashMap::new(),
            }],
            bindings: HashMap::new(),
            types: HashMap::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            traits: HashMap::new(),
            effects: HashMap::new(),
            type_aliases: HashMap::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope {
            names: HashMap::new(),
        });
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn add_binding(&mut self, name: Symbol, binding: Binding) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.names.insert(name, binding.id);
        }
        self.bindings.insert(binding.id, binding);
    }

    pub fn lookup(&self, name: Symbol) -> Option<&Binding> {
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.names.get(&name) {
                return self.bindings.get(&id);
            }
        }
        None
    }

    pub fn add_struct(&mut self, info: StructInfo) {
        self.structs.insert(info.id, info);
    }

    pub fn add_trait(&mut self, info: TraitInfo) {
        self.traits.insert(info.id, info);
    }

    pub fn add_effect(&mut self, info: EffectInfo) {
        self.effects.insert(info.id, info);
    }

    pub fn get_struct(&self, id: StructId) -> Option<&StructInfo> {
        self.structs.get(&id)
    }

    pub fn add_enum(&mut self, info: EnumInfo) {
        self.enums.insert(info.id, info);
    }

    pub fn get_enum(&self, id: EnumId) -> Option<&EnumInfo> {
        self.enums.get(&id)
    }
}

// Type Checker
pub struct TypeChecker {
    pub env: TypeEnv,
    pub interner: Interner,
    pub id_gen: IdGen,
    pub diagnostics: Diagnostics,
    substitution: Substitution,
    pub constraints: Vec<Constraint>,
    pub trait_resolver: TraitResolver,
    pub constraint_solver: ConstraintSolver,
    current_function_effects: EffectSet, // Track effects allowed in current context
}

#[derive(Debug, Default, Clone)]
struct Substitution {
    map: HashMap<TypeId, Rc<Type>>,
}

impl Substitution {
    fn apply(&self, ty: &Rc<Type>) -> Rc<Type> {
        match &ty.type_ {
            TypeKind::Variable { id, kind: _ } => {
                if let Some(t) = self.map.get(&TypeId(*id)) {
                    // Recursively apply to handle chains of substitutions (e.g., a -> b -> Int)
                    self.apply(t)
                } else {
                    ty.clone()
                }
            }
            TypeKind::Function {
                params,
                return_type,
                effects,
            } => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Function {
                    params: params.iter().map(|p| self.apply(p)).collect(),
                    return_type: self.apply(return_type),
                    effects: effects.clone(),
                },
            }),
            TypeKind::Constructor { name, args, kind } => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Constructor {
                    name: *name,
                    args: args.iter().map(|a| self.apply(a)).collect(),
                    kind: kind.clone(),
                },
            }),
            TypeKind::Tuple(types) => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Tuple(types.iter().map(|t| self.apply(t)).collect()),
            }),
            TypeKind::Union(types) => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Union(types.iter().map(|t| self.apply(t)).collect()),
            }),
            TypeKind::Row { fields, rest } => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Row {
                    fields: fields
                        .iter()
                        .map(|(name, t)| (*name, self.apply(t)))
                        .collect(),
                    rest: *rest,
                },
            }),
            TypeKind::Forall {
                vars,
                body,
                constraints,
            } => {
                // For Forall types, we should not substitute the quantified variables
                // We need to exclude the bound variables from substitution
                Rc::new(Type {
                    span: ty.span.clone(),
                    file: ty.file.clone(),
                    type_: TypeKind::Forall {
                        vars: vars.clone(),
                        body: self.apply_with_bound_vars(body, vars),
                        constraints: constraints.clone(), // constraints need special handling too
                    },
                })
            }
            TypeKind::Exists {
                vars,
                body,
                constraints,
            } => {
                // Similar to Forall
                Rc::new(Type {
                    span: ty.span.clone(),
                    file: ty.file.clone(),
                    type_: TypeKind::Exists {
                        vars: vars.clone(),
                        body: self.apply_with_bound_vars(body, vars),
                        constraints: constraints.clone(),
                    },
                })
            }
            TypeKind::Trait(types) => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Trait(types.to_vec()),
            }),
            TypeKind::Pointer(inner) => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Pointer(self.apply(inner)),
            }),
            TypeKind::Never | TypeKind::Error => ty.clone(),
        }
    }

    // Apply substitution but excluding bound variables (for Forall/Exists)
    fn apply_with_bound_vars(&self, ty: &Rc<Type>, bound_vars: &[(usize, Kind)]) -> Rc<Type> {
        // Create a temporary substitution without the bound variables
        let mut filtered_subst = Substitution {
            map: self.map.clone(),
        };

        // Remove bound variables from the substitution
        for (var_id, _) in bound_vars {
            filtered_subst.map.remove(&TypeId(*var_id));
        }

        filtered_subst.apply(ty)
    }

    fn bind(&mut self, id: TypeId, ty: Rc<Type>) {
        self.map.insert(id, ty);
    }
}

impl TypeChecker {
    pub fn new(mut interner: Interner) -> Self {
        let trait_resolver = TraitResolver::new();

        let _int_sym = interner.intern("Int");
        let _bool_sym = interner.intern("Bool");
        let _float_sym = interner.intern("Float");
        let _string_sym = interner.intern("String");
        let _unit_sym = interner.intern("Unit");

        Self {
            env: TypeEnv::new(),
            interner,
            id_gen: IdGen::new(),
            diagnostics: Diagnostics::new(),
            substitution: Substitution::default(),
            constraints: Vec::new(),
            trait_resolver: trait_resolver.clone(),
            constraint_solver: ConstraintSolver::new(trait_resolver),
            current_function_effects: EffectSet::pure(),
        }
    }

    fn convert_effect_annot(&mut self, annot: &EffectAnnot) -> EffectSet {
        // Process each effect in the annotation and create proper substitutions
        for (effect_name, type_args) in &annot.effects {
            if !type_args.is_empty() {
                let effect_sym = self.interner.intern(effect_name);

                // Look up the effect template to get its type parameters
                // Collect effect info to avoid borrowing issues
                let effect_info_opt = self
                    .env
                    .effects
                    .values()
                    .find(|e| e.name == effect_sym)
                    .cloned();

                if let Some(effect_info) = effect_info_opt {
                    // Create substitution mapping type parameters to concrete types
                    for (i, type_arg) in type_args.iter().enumerate() {
                        if i < effect_info.type_params.len() {
                            let (_param_name, type_var_id) = effect_info.type_params[i];

                            // Resolve the concrete type argument
                            let concrete_type = self.resolve_type_annot(type_arg);

                            // Add mapping to current substitution
                            self.substitution
                                .map
                                .insert(TypeId(type_var_id.0), concrete_type);
                        }
                    }
                }
            }
        }

        EffectSet {
            effects: annot
                .effects
                .iter()
                .map(|(effect_name, _type_args)| self.interner.intern(effect_name).0)
                .collect(),
            rest: annot.rest.as_ref().map(|r| self.interner.intern(r).0),
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn convert_kind_annot(&self, annot: &KindAnnot) -> Kind {
        match annot {
            KindAnnot::Star => Kind::Star,
            KindAnnot::Arrow(a, b) => Kind::Arrow(
                Box::new(self.convert_kind_annot(a)),
                Box::new(self.convert_kind_annot(b)),
            ),
        }
    }

    pub fn fresh_type_var(&mut self) -> Rc<Type> {
        let id = self.id_gen.fresh_type();
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Variable {
                id: id.0,
                kind: Kind::Star,
            },
        })
    }

    pub fn check_program(&mut self, nodes: Vec<ASTNode>) -> Vec<TypedASTNode> {
        let mut typed_nodes = Vec::new();
        let mut typed_impls = Vec::new();

        // FIRST PASS: Register all top-level definitions
        for node in &nodes {
            match &node.node {
                ASTNodeKind::Function(f) => {
                    self.pre_register_function(f);
                }
                ASTNodeKind::Struct(s) => {
                    let struct_id = self.id_gen.fresh_struct();
                    let name = self.interner.intern(&s.name);

                    // Pre-register fields with their types
                    let mut fields = HashMap::new();
                    for (field_arg, vis) in &s.fields {
                        let field_name = self.interner.intern(&field_arg.name);
                        let field_id = self.id_gen.fresh_field();
                        let field_type = if let Some(type_annot) = &field_arg.type_ {
                            self.resolve_type_annot(type_annot)
                        } else {
                            self.fresh_type_var()
                        };
                        fields.insert(field_name, (field_id, field_type, *vis));
                    }

                    let struct_info = StructInfo {
                        id: struct_id,
                        name,
                        type_params: vec![],
                        fields,
                    };
                    self.env.structs.insert(struct_id, struct_info);
                }
                ASTNodeKind::Enum(e) => {
                    let enum_id = self.id_gen.fresh_enum();
                    let name = self.interner.intern(&e.name);
                    let enum_info = EnumInfo {
                        id: enum_id,
                        name,
                        type_params: vec![],
                        variants: HashMap::new(),
                    };
                    self.env.enums.insert(enum_id, enum_info);
                }
                ASTNodeKind::Trait(t) => {
                    let trait_id = self.id_gen.fresh_trait();
                    let name = self.interner.intern(&t.name);
                    let trait_info = TraitInfo {
                        id: trait_id,
                        name,
                        methods: HashMap::new(),
                    };
                    self.env.traits.insert(trait_id, trait_info);
                }
                _ => {}
            }
        }

        // SECOND PASS: Type check everything
        for node in nodes {
            let typed = self.check_node(node);

            if let TypedASTNodeKind::Impl(ref impl_block) = typed.node {
                typed_impls.push(impl_block.clone());
            }

            typed_nodes.push(typed);
        }

        check_coherence(&typed_impls, &mut self.diagnostics);
        self.constraint_solver.solve(&mut self.diagnostics);

        // Apply substitution to all typed nodes to resolve type variables
        let mut resolved_nodes = Vec::new();
        for node in typed_nodes {
            resolved_nodes.push(self.apply_substitution_to_node(node));
        }

        resolved_nodes
    }

    // Pre-register function in environment so it can be referenced
    fn pre_register_function(&mut self, func: &Function) {
        let func_id = self.id_gen.fresh_function();
        let name_sym = self.interner.intern(&func.name);

        // Create type parameter mapping first
        let mut type_param_map = std::collections::HashMap::new();
        let mut typed_type_params = vec![];

        for tp in &func.type_params {
            let var_id = self.id_gen.fresh_type();
            let name = self.interner.intern(&tp.name);
            type_param_map.insert(
                name,
                Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Variable {
                        id: var_id.0,
                        kind: tp
                            .kind
                            .as_ref()
                            .map(|k| self.convert_kind_annot(k))
                            .unwrap_or(Kind::Star),
                    },
                }),
            );
            typed_type_params.push(TypedTypeParam {
                name,
                var_id,
                kind: tp
                    .kind
                    .as_ref()
                    .map(|k| self.convert_kind_annot(k))
                    .unwrap_or(Kind::Star),
                bounds: tp
                    .bounds
                    .iter()
                    .map(|b| self.resolve_type_annot(b))
                    .collect(),
            });
        }

        // Build proper function type from signature, using type parameters where appropriate
        let param_types: Vec<Rc<Type>> = func
            .args
            .iter()
            .map(|arg| {
                if let Some(type_annot) = &arg.type_ {
                    // Use the type parameter mapping when resolving type annotations
                    self.resolve_type_annot_with_params(type_annot, &type_param_map)
                } else {
                    self.fresh_type_var()
                }
            })
            .collect();

        let return_type = if let Some(ret) = &func.return_type {
            self.resolve_type_annot_with_params(ret, &type_param_map)
        } else {
            self.fresh_type_var()
        };

        let effects = self.convert_effect_annot(&func.effects);

        let base_func_type = Rc::new(Type {
            span: Some(func.span.clone()),
            file: Some(func.file.clone()),
            type_: TypeKind::Function {
                params: param_types,
                return_type,
                effects,
            },
        });

        // For generic functions, wrap in Forall type to preserve polymorphism
        let func_type = if !func.type_params.is_empty() {
            // Convert type parameter bounds to trait constraints
            let mut constraints = Vec::new();
            for tp in typed_type_params.iter() {
                for bound in &tp.bounds {
                    // Find the trait that this bound refers to
                    // The bound should be a trait type, so we need to extract the trait id
                    if let TypeKind::Constructor { name, .. } = &bound.type_ {
                        // Create a trait constraint: this type parameter implements this trait
                        constraints.push(Constraint::Trait(tp.var_id.0, *name));
                    }
                }
            }

            Rc::new(Type {
                span: Some(func.span.clone()),
                file: Some(func.file.clone()),
                type_: TypeKind::Forall {
                    vars: typed_type_params
                        .iter()
                        .map(|tp| (tp.var_id.0, tp.kind.clone()))
                        .collect(),
                    constraints, // Now with proper trait constraints
                    body: base_func_type,
                },
            })
        } else {
            base_func_type
        };

        let func_info = FunctionInfo {
            id: func_id,
            name: name_sym,
            type_: func_type.clone(),
        };

        self.env.functions.insert(func_id, func_info);

        self.env.add_binding(
            name_sym,
            Binding {
                id: BindingId(func_id.0),
                name: name_sym,
                type_: func_type,
                mutable: false,
            },
        );
    }

    pub fn instantiate_generic_call(
        &mut self,
        func_expr: &TypedExpr,
        type_args: Vec<Rc<Type>>,
    ) -> TypedExpr {
        // Extract function being called
        if let TypedExprKind::Variable { name, binding_id } = &func_expr.expr {
            // Lookup function definition
            if let Some(func_info) = self.env.functions.get(&FunctionId(binding_id.0)) {
                // For a generic function, we need to instantiate it with the provided type arguments
                // First, check if the function is polymorphic (has type variables)
                let instantiated_type = match &func_info.type_.type_ {
                    TypeKind::Forall { vars, body, .. } => {
                        // Create substitution mapping each quantified variable to the corresponding type arg
                        if vars.len() != type_args.len() {
                            // Error: wrong number of type arguments
                            self.diagnostics.add_type_error(TypeError {
                                span: func_expr.span.clone(),
                                file: func_expr.file.clone(),
                                kind: TypeErrorKind::ArityMismatch {
                                    expected: vars.len(),
                                    found: type_args.len(),
                                    function: *name,
                                },
                            });
                            return self.error_expr(&Expr {
                                span: func_expr.span.clone(),
                                file: func_expr.file.clone(),
                                expr: ExprKind::Error,
                            });
                        }

                        // Create substitution from type parameters to type arguments
                        let mut subst = Substitution::default();
                        for ((var_id, _), arg_type) in vars.iter().zip(type_args.iter()) {
                            subst.bind(TypeId(*var_id), arg_type.clone());
                        }

                        // Apply substitution to the function body
                        subst.apply(body)
                    }
                    _ => {
                        // Not a polymorphic function, so type args should not be provided
                        if !type_args.is_empty() {
                            self.diagnostics.add_type_error(TypeError {
                                span: func_expr.span.clone(),
                                file: func_expr.file.clone(),
                                kind: TypeErrorKind::ArityMismatch {
                                    expected: 0,
                                    found: type_args.len(),
                                    function: *name,
                                },
                            });
                        }
                        func_info.type_.clone()
                    }
                };

                // Return a copy of the function expression with the instantiated type
                TypedExpr {
                    span: func_expr.span.clone(),
                    file: func_expr.file.clone(),
                    expr: func_expr.expr.clone(),
                    type_: instantiated_type,
                }
            } else {
                // Function not found in environment
                self.diagnostics.add_type_error(TypeError {
                    span: func_expr.span.clone(),
                    file: func_expr.file.clone(),
                    kind: TypeErrorKind::UnboundVariable { name: *name },
                });
                self.error_expr(&Expr {
                    span: func_expr.span.clone(),
                    file: func_expr.file.clone(),
                    expr: ExprKind::Error,
                })
            }
        } else {
            // Not a variable reference, so can't be a generic function call
            func_expr.clone()
        }
    }

    fn check_node(&mut self, node: ASTNode) -> TypedASTNode {
        match node.node {
            ASTNodeKind::Expr(expr) => {
                let typed = self.check_expr(&expr);
                TypedASTNode {
                    span: node.span,
                    file: node.file,
                    node: TypedASTNodeKind::Expr(typed),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::Function(func) => {
                let typed = self.check_function(*func);
                TypedASTNode {
                    span: node.span,
                    file: node.file,
                    node: TypedASTNodeKind::Function(Box::new(typed)),
                    attributes: node.attributes,
                }
            }
            ASTNodeKind::Struct(struct_def) => {
                let typed = self.check_struct(struct_def);
                TypedASTNode {
                    span: node.span,
                    file: node.file,
                    node: TypedASTNodeKind::Struct(typed),
                    attributes: node.attributes,
                }
            }

            ASTNodeKind::Enum(enum_def) => {
                let typed = self.check_enum(enum_def);
                TypedASTNode {
                    span: node.span,
                    file: node.file,
                    node: TypedASTNodeKind::Enum(typed),
                    attributes: node.attributes,
                }
            }

            ASTNodeKind::TypeAlias(alias) => {
                let typed = self.check_type_alias(alias);
                TypedASTNode {
                    span: node.span,
                    file: node.file,
                    node: TypedASTNodeKind::TypeAlias(typed),
                    attributes: node.attributes,
                }
            }

            ASTNodeKind::Impl(impl_block) => {
                let typed = self.check_impl(impl_block);
                TypedASTNode {
                    span: node.span,
                    file: node.file,
                    node: TypedASTNodeKind::Impl(typed),
                    attributes: node.attributes,
                }
            }

            ASTNodeKind::Trait(trait_def) => {
                let typed = self.check_trait(trait_def);
                TypedASTNode {
                    span: node.span,
                    file: node.file,
                    node: TypedASTNodeKind::Trait(typed),
                    attributes: node.attributes,
                }
            }

            ASTNodeKind::EffectDef(effect) => {
                let typed = self.check_effect_def(effect);
                TypedASTNode {
                    span: node.span,
                    file: node.file,
                    node: TypedASTNodeKind::EffectDef(typed),
                    attributes: node.attributes,
                }
            }

            ASTNodeKind::ExternFunction(extern_func) => {
                let typed = self.check_extern_function(*extern_func);
                TypedASTNode {
                    span: node.span,
                    file: node.file,
                    node: TypedASTNodeKind::ExternFunction(Box::new(typed)),
                    attributes: node.attributes,
                }
            }

            ASTNodeKind::MacroDef(macro_def) => {
                let typed = self.check_macro_def(macro_def);
                TypedASTNode {
                    span: node.span,
                    file: node.file,
                    node: TypedASTNodeKind::MacroDef(typed),
                    attributes: node.attributes,
                }
            }

            ASTNodeKind::Error => TypedASTNode {
                span: node.span,
                file: node.file,
                node: TypedASTNodeKind::Error,
                attributes: node.attributes,
            },
        }
    }

    // Generalize: Convert free type variables to polymorphic type scheme
    fn generalize(&self, ty: &Rc<Type>, env_types: &HashSet<TypeId>) -> Rc<Type> {
        // Apply substitution to get the actual type first
        let substituted_ty = self.substitution.apply(ty);

        // Find free type variables in the substituted type
        let free_vars = self.free_type_vars(&substituted_ty);
        let quantified: Vec<_> = free_vars
            .into_iter()
            .filter(|var_id| !env_types.contains(var_id))
            .collect();

        if quantified.is_empty() {
            // If no variables to quantify, return the substituted type
            substituted_ty
        } else {
            // Create polymorphic type with Forall
            Rc::new(Type {
                span: substituted_ty.span.clone(),
                file: substituted_ty.file.clone(),
                type_: TypeKind::Forall {
                    vars: quantified
                        .into_iter()
                        .map(|id| (id.0, Kind::Star))
                        .collect(),
                    constraints: vec![],
                    body: substituted_ty, // Use the substituted type
                },
            })
        }
    }

    // Instantiate: Replace bound type variables with fresh ones
    fn instantiate(&mut self, ty: &Rc<Type>) -> Rc<Type> {
        match &ty.type_ {
            TypeKind::Forall {
                vars,
                body,
                constraints,
            } => {
                // Create completely fresh variables
                let mut local_subst: HashMap<usize, Rc<Type>> = HashMap::new();

                for (var_id, kind) in vars {
                    let fresh = Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Variable {
                            id: self.id_gen.fresh_type().0,
                            kind: kind.clone(),
                        },
                    });
                    local_subst.insert(*var_id, fresh);
                }

                // Process constraints - for each constraint, we need to ensure it's satisfied
                // For trait constraints, we need to verify that the instantiated type implements the trait
                for constraint in constraints {
                    match constraint {
                        Constraint::Trait(var_id, _trait_id) => {
                            // Get the instantiated type for this variable
                            if let Some(_instantiated_type) = local_subst.get(var_id) {
                                // Add a trait implementation requirement to the current context
                                // This would typically be handled by a constraint solver
                                // For now, we'll record this requirement
                            }
                        }
                        Constraint::Equal(t1, t2) => {
                            // Apply substitution to both types
                            let _subst_t1 = self.apply_local_subst_only(t1, &local_subst);
                            let _subst_t2 = self.apply_local_subst_only(t2, &local_subst);
                            // For now, just continue - in a full implementation, we'd enforce equality
                        }
                    }
                }

                // Apply ONLY local substitution to the body, ignoring global substitution
                // This ensures that each instantiation creates fresh type variables that won't
                // be shared across different uses of the polymorphic function
                self.apply_local_subst_only(body, &local_subst)
            }
            _ => {
                // For non-Forall types, apply the current substitution
                self.substitution.apply(ty)
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn apply_local_subst_only(
        &self,
        ty: &Rc<Type>,
        local_subst: &HashMap<usize, Rc<Type>>,
    ) -> Rc<Type> {
        match &ty.type_ {
            TypeKind::Variable { id, .. } => {
                local_subst.get(id).cloned().unwrap_or_else(|| ty.clone())
            }
            TypeKind::Function {
                params,
                return_type,
                effects,
            } => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Function {
                    params: params
                        .iter()
                        .map(|p| self.apply_local_subst_only(p, local_subst))
                        .collect(),
                    return_type: self.apply_local_subst_only(return_type, local_subst),
                    effects: effects.clone(),
                },
            }),
            TypeKind::Constructor { name, args, kind } => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Constructor {
                    name: *name,
                    args: args
                        .iter()
                        .map(|a| self.apply_local_subst_only(a, local_subst))
                        .collect(),
                    kind: kind.clone(),
                },
            }),
            TypeKind::Tuple(types) => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Tuple(
                    types
                        .iter()
                        .map(|t| self.apply_local_subst_only(t, local_subst))
                        .collect(),
                ),
            }),
            TypeKind::Union(types) => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Union(
                    types
                        .iter()
                        .map(|t| self.apply_local_subst_only(t, local_subst))
                        .collect(),
                ),
            }),
            TypeKind::Row { fields, rest } => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Row {
                    fields: fields
                        .iter()
                        .map(|(name, t)| (*name, self.apply_local_subst_only(t, local_subst)))
                        .collect(),
                    rest: *rest,
                },
            }),
            TypeKind::Trait(types) => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Trait(types.to_vec()),
            }),
            TypeKind::Pointer(inner) => Rc::new(Type {
                span: ty.span.clone(),
                file: ty.file.clone(),
                type_: TypeKind::Pointer(self.apply_local_subst_only(inner, local_subst)),
            }),
            // Forall and Exists shouldn't appear in instantiation context as they should be handled at the instantiate level
            _ => ty.clone(),
        }
    }

    // Collect free type variables
    fn free_type_vars(&self, ty: &Rc<Type>) -> HashSet<TypeId> {
        let mut vars = HashSet::new();
        self.collect_free_vars(ty, &mut vars);
        vars
    }

    #[allow(clippy::only_used_in_recursion)]
    fn collect_free_vars(&self, ty: &Rc<Type>, vars: &mut HashSet<TypeId>) {
        match &ty.type_ {
            TypeKind::Variable { id, .. } => {
                vars.insert(TypeId(*id));
            }
            TypeKind::Constructor { args, .. } => {
                for arg in args {
                    self.collect_free_vars(arg, vars);
                }
            }
            TypeKind::Function {
                params,
                return_type,
                ..
            } => {
                for param in params {
                    self.collect_free_vars(param, vars);
                }
                self.collect_free_vars(return_type, vars);
            }
            TypeKind::Tuple(types) | TypeKind::Union(types) => {
                for t in types {
                    self.collect_free_vars(t, vars);
                }
            }
            TypeKind::Forall { body, .. } | TypeKind::Exists { body, .. } => {
                self.collect_free_vars(body, vars);
            }
            TypeKind::Pointer(inner) => {
                self.collect_free_vars(inner, vars);
            }
            _ => {}
        }
    }

    // Get free type vars in environment
    fn env_free_vars(&self) -> HashSet<TypeId> {
        let mut vars = HashSet::new();
        for binding in self.env.bindings.values() {
            self.collect_free_vars(&binding.type_, &mut vars);
        }
        vars
    }

    // Find captured variables in an expression - variables that are referenced
    // but not defined in the lambda's own scope (i.e., they come from outer scopes)
    fn find_captures_in_lambda_body(
        &self,
        body: &TypedExpr,
        lambda_arg_binding_ids: &std::collections::HashSet<BindingId>,
    ) -> Vec<(Symbol, BindingId, Rc<Type>)> {
        let mut captures = Vec::new();
        let mut visited = std::collections::HashSet::new();

        self.collect_captures_from_expr(body, lambda_arg_binding_ids, &mut captures, &mut visited);

        captures
    }

    fn collect_captures_from_expr(
        &self,
        expr: &TypedExpr,
        lambda_arg_binding_ids: &std::collections::HashSet<BindingId>,
        captures: &mut Vec<(Symbol, BindingId, Rc<Type>)>,
        visited: &mut std::collections::HashSet<BindingId>,
    ) {
        match &expr.expr {
            TypedExprKind::Variable { name, binding_id } => {
                // Check if this binding ID is NOT one of the lambda's own arguments
                // If it's not a lambda argument, and we haven't captured it yet, it's a capture
                if !lambda_arg_binding_ids.contains(binding_id) && !visited.contains(binding_id) {
                    // DEBUG: Print what variable we're processing
                    println!(
                        "DEBUG: Processing variable {} with binding ID {:?}",
                        self.interner.resolve(*name),
                        binding_id
                    );

                    // Look up the binding in the environment to get its type
                    if let Some(binding) = self.env.bindings.get(binding_id) {
                        println!("DEBUG: Found binding in environment");
                        captures.push((*name, *binding_id, binding.type_.clone()));
                        visited.insert(*binding_id);
                    } else {
                        println!("DEBUG: Binding NOT found in environment");
                    }
                } else {
                    println!(
                        "DEBUG: Skipping variable {} - is lambda arg: {}, already visited: {}",
                        self.interner.resolve(*name),
                        lambda_arg_binding_ids.contains(binding_id),
                        visited.contains(binding_id)
                    );
                }
            }
            TypedExprKind::Lambda { body, args, .. } => {
                // For nested lambdas, we need to find all variables that this nested lambda
                // references from outside its own scope. All such variables must also be
                // captured by the current lambda, because when the nested lambda is executed,
                // it will need access to these values.

                // Create binding IDs that are local to the nested lambda (not captures)
                let mut nested_local_bindings = std::collections::HashSet::new();
                for arg in args {
                    nested_local_bindings.insert(arg.binding_id);
                }

                // DEBUG: Print what's in nested_local_bindings
                println!("DEBUG: nested_local_bindings contains:");
                for binding_id in &nested_local_bindings {
                    println!("  - {:?}", binding_id);
                }

                // Find all captures that the nested lambda needs
                let mut nested_captures = Vec::new();
                let mut nested_visited = std::collections::HashSet::new();

                self.collect_captures_from_expr(
                    body,
                    &nested_local_bindings,
                    &mut nested_captures,
                    &mut nested_visited,
                );

                // DEBUG: Print what we found in the nested lambda
                println!("DEBUG: Nested lambda captures: {}", nested_captures.len());
                for (name, binding_id, _) in &nested_captures {
                    println!("  - {}: {:?}", self.interner.resolve(*name), binding_id);
                }

                // All captures found in the nested lambda (variables from outer scopes)
                // should also be captures of the current lambda, except for variables that
                // are already parameters of the current lambda
                for capture in nested_captures {
                    // Skip captures that are parameters of the current lambda
                    if !lambda_arg_binding_ids.contains(&capture.1) && !visited.contains(&capture.1)
                    {
                        captures.push(capture.clone());
                        visited.insert(capture.1);
                    }
                }
            }
            // For other expressions, recursively collect captures from sub-expressions
            TypedExprKind::BinOp { left, right, .. } => {
                self.collect_captures_from_expr(left, lambda_arg_binding_ids, captures, visited);
                self.collect_captures_from_expr(right, lambda_arg_binding_ids, captures, visited);
            }
            TypedExprKind::UnOp { operand, .. } => {
                self.collect_captures_from_expr(operand, lambda_arg_binding_ids, captures, visited);
            }
            TypedExprKind::Assign { l_val, r_val, .. } => {
                self.collect_captures_from_expr(l_val, lambda_arg_binding_ids, captures, visited);
                self.collect_captures_from_expr(r_val, lambda_arg_binding_ids, captures, visited);
            }
            TypedExprKind::Let {
                value, binding_id, ..
            } => {
                // Process the value being bound (it might reference outer variables)
                self.collect_captures_from_expr(value, lambda_arg_binding_ids, captures, visited);
                // The new binding itself is local to the let scope, so we don't treat it as a capture
                // but nested expressions might reference it
                let mut new_visited = visited.clone();
                new_visited.insert(*binding_id);
                // For expressions inside blocks that might reference the let binding,
                // we'd need more complex scoping, but for now we handle it simply
            }
            TypedExprKind::Array { elements, .. } => {
                for element in elements {
                    self.collect_captures_from_expr(
                        element,
                        lambda_arg_binding_ids,
                        captures,
                        visited,
                    );
                }
            }
            TypedExprKind::Tuple(elements) => {
                for element in elements {
                    self.collect_captures_from_expr(
                        element,
                        lambda_arg_binding_ids,
                        captures,
                        visited,
                    );
                }
            }
            TypedExprKind::Map { entries, .. } => {
                for (key, value) in entries {
                    self.collect_captures_from_expr(key, lambda_arg_binding_ids, captures, visited);
                    self.collect_captures_from_expr(
                        value,
                        lambda_arg_binding_ids,
                        captures,
                        visited,
                    );
                }
            }
            TypedExprKind::EnumConstruct { args, .. } => {
                for arg in args {
                    self.collect_captures_from_expr(arg, lambda_arg_binding_ids, captures, visited);
                }
            }
            TypedExprKind::StructConstruct { fields, .. } => {
                for (_, _, field_expr) in fields {
                    self.collect_captures_from_expr(
                        field_expr,
                        lambda_arg_binding_ids,
                        captures,
                        visited,
                    );
                }
            }
            TypedExprKind::Perform { args, .. } => {
                for arg in args {
                    self.collect_captures_from_expr(arg, lambda_arg_binding_ids, captures, visited);
                }
            }
            TypedExprKind::Handle { body, handlers, .. } => {
                self.collect_captures_from_expr(body, lambda_arg_binding_ids, captures, visited);
                for handler in handlers {
                    // Handle handler parameters
                    let mut handler_arg_bindings = lambda_arg_binding_ids.clone();
                    for (_, param_id, _) in &handler.params {
                        handler_arg_bindings.insert(*param_id);
                    }
                    handler_arg_bindings.insert(handler.resume_id);

                    self.collect_captures_from_expr(
                        &handler.body,
                        &handler_arg_bindings,
                        captures,
                        visited,
                    );
                }
            }
            TypedExprKind::Cast { expr, .. } => {
                self.collect_captures_from_expr(expr, lambda_arg_binding_ids, captures, visited);
            }
            TypedExprKind::IfElse {
                condition,
                then,
                else_,
            } => {
                self.collect_captures_from_expr(
                    condition,
                    lambda_arg_binding_ids,
                    captures,
                    visited,
                );
                self.collect_captures_from_expr(then, lambda_arg_binding_ids, captures, visited);
                if let Some(else_expr) = else_ {
                    self.collect_captures_from_expr(
                        else_expr,
                        lambda_arg_binding_ids,
                        captures,
                        visited,
                    );
                }
            }
            TypedExprKind::Block { expressions } => {
                let mut block_visited = visited.clone();
                for expr in expressions {
                    self.collect_captures_from_expr(
                        expr,
                        lambda_arg_binding_ids,
                        captures,
                        &mut block_visited,
                    );
                }
                // Update the main visited set with any new captures found in the block
                *visited = block_visited;
            }
            TypedExprKind::With {
                context,
                body,
                binding_id,
                ..
            } => {
                // Process context first (before the with binding is in scope)
                self.collect_captures_from_expr(context, lambda_arg_binding_ids, captures, visited);
                // Process body with the with binding added to the local set
                let mut with_bindings = lambda_arg_binding_ids.clone();
                with_bindings.insert(*binding_id);
                self.collect_captures_from_expr(body, &with_bindings, captures, visited);
            }
            TypedExprKind::Loop { body, .. } => {
                self.collect_captures_from_expr(body, lambda_arg_binding_ids, captures, visited);
            }
            TypedExprKind::Match { scrutinee, arms } => {
                self.collect_captures_from_expr(
                    scrutinee,
                    lambda_arg_binding_ids,
                    captures,
                    visited,
                );
                for arm in arms {
                    // For match arms, we'd need to process pattern bindings, but for now
                    // we just process the arm body
                    self.collect_captures_from_expr(
                        &arm.body,
                        lambda_arg_binding_ids,
                        captures,
                        visited,
                    );
                }
            }
            TypedExprKind::For {
                iterator,
                body,
                binding_id,
                ..
            } => {
                self.collect_captures_from_expr(
                    iterator,
                    lambda_arg_binding_ids,
                    captures,
                    visited,
                );
                let mut for_bindings = lambda_arg_binding_ids.clone();
                for_bindings.insert(*binding_id);
                self.collect_captures_from_expr(body, &for_bindings, captures, visited);
            }
            TypedExprKind::While { condition, body } => {
                self.collect_captures_from_expr(
                    condition,
                    lambda_arg_binding_ids,
                    captures,
                    visited,
                );
                self.collect_captures_from_expr(body, lambda_arg_binding_ids, captures, visited);
            }
            TypedExprKind::IfLet {
                expr, then, else_, ..
            } => {
                self.collect_captures_from_expr(expr, lambda_arg_binding_ids, captures, visited);
                self.collect_captures_from_expr(then, lambda_arg_binding_ids, captures, visited);
                if let Some(else_expr) = else_ {
                    self.collect_captures_from_expr(
                        else_expr,
                        lambda_arg_binding_ids,
                        captures,
                        visited,
                    );
                }
            }
            TypedExprKind::WhileLet { expr, body, .. } => {
                self.collect_captures_from_expr(expr, lambda_arg_binding_ids, captures, visited);
                self.collect_captures_from_expr(body, lambda_arg_binding_ids, captures, visited);
            }
            TypedExprKind::Return(expr) => {
                if let Some(return_expr) = expr {
                    self.collect_captures_from_expr(
                        return_expr,
                        lambda_arg_binding_ids,
                        captures,
                        visited,
                    );
                }
            }
            TypedExprKind::Break(expr) => {
                if let Some(break_expr) = expr {
                    self.collect_captures_from_expr(
                        break_expr,
                        lambda_arg_binding_ids,
                        captures,
                        visited,
                    );
                }
            }
            TypedExprKind::Call { function, args, .. } => {
                self.collect_captures_from_expr(
                    function,
                    lambda_arg_binding_ids,
                    captures,
                    visited,
                );
                for arg in args {
                    self.collect_captures_from_expr(arg, lambda_arg_binding_ids, captures, visited);
                }
            }
            TypedExprKind::Index { target, index, .. } => {
                self.collect_captures_from_expr(target, lambda_arg_binding_ids, captures, visited);
                self.collect_captures_from_expr(index, lambda_arg_binding_ids, captures, visited);
            }
            TypedExprKind::FieldAccess { target, .. } => {
                self.collect_captures_from_expr(target, lambda_arg_binding_ids, captures, visited);
            }
            TypedExprKind::OptionalChain { target, .. } => {
                self.collect_captures_from_expr(target, lambda_arg_binding_ids, captures, visited);
            }
            TypedExprKind::MacroCall { args, .. } => {
                for arg in args {
                    self.collect_captures_from_expr(arg, lambda_arg_binding_ids, captures, visited);
                }
            }
            // Primitive values don't have captures
            TypedExprKind::Int(_)
            | TypedExprKind::Float(_)
            | TypedExprKind::Bool(_)
            | TypedExprKind::String(_)
            | TypedExprKind::Continue
            | TypedExprKind::Error
            | TypedExprKind::Import(_) => {
                // Nothing to capture
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> TypedExpr {
        match &expr.expr {
            ExprKind::Int(n) => TypedExpr {
                span: expr.span.clone(),
                file: expr.file.clone(),
                expr: TypedExprKind::Int(*n),
                type_: self.int_type(),
            },

            ExprKind::Bool(b) => TypedExpr {
                span: expr.span.clone(),
                file: expr.file.clone(),
                expr: TypedExprKind::Bool(*b),
                type_: self.bool_type(),
            },

            ExprKind::Variable(name) => {
                let sym = self.interner.intern(name);
                match self.env.lookup(sym) {
                    Some(binding) => {
                        let binding = binding.clone();
                        // Instantiate polymorphic type - this should create fresh type variables for Forall types
                        let instantiated = self.instantiate(&binding.type_);

                        TypedExpr {
                            span: expr.span.clone(),
                            file: expr.file.clone(),
                            expr: TypedExprKind::Variable {
                                name: sym,
                                binding_id: binding.id,
                            },
                            type_: instantiated,
                        }
                    }
                    None => {
                        self.error(expr, TypeErrorKind::UnboundVariable { name: sym });
                        self.error_expr(expr)
                    }
                }
            }

            ExprKind::Let {
                var,
                type_annot,
                value,
            } => {
                let value_typed = self.check_expr(value);

                let var_type = if let Some(annot) = type_annot {
                    let expected = self.resolve_type_annot(annot);
                    self.unify(&value_typed.type_, &expected, &expr.span);
                    expected
                } else {
                    // Apply current substitution first, then generalize to capture polymorphism
                    let ty = self.substitution.apply(&value_typed.type_);
                    let env_vars = self.env_free_vars();
                    self.generalize(&ty, &env_vars)
                };

                let binding_id = self.id_gen.fresh_binding();
                let sym = self.interner.intern(var);
                self.env.add_binding(
                    sym,
                    Binding {
                        id: binding_id,
                        name: sym,
                        type_: var_type.clone(),
                        mutable: false,
                    },
                );

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Let {
                        var: sym,
                        binding_id,
                        var_type,
                        value: Box::new(value_typed),
                    },
                    type_: self.unit_type(),
                }
            }

            ExprKind::BinOp(left, op, right) => {
                let left_typed = self.check_expr(left);
                let right_typed = self.check_expr(right);

                // Determine result type based on operator
                let result_type = match op {
                    // Arithmetic operators: Int -> Int -> Int
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                        let int_type = self.int_type();
                        self.unify(&left_typed.type_, &int_type, &left.span);
                        self.unify(&right_typed.type_, &int_type, &right.span);
                        int_type
                    }

                    // Comparison operators: a -> a -> Bool
                    BinOp::Eq
                    | BinOp::NotEq
                    | BinOp::Less
                    | BinOp::LessEq
                    | BinOp::Greater
                    | BinOp::GreaterEq => {
                        // Both sides must have same type
                        self.unify(&left_typed.type_, &right_typed.type_, &expr.span);
                        self.bool_type()
                    }

                    // Logical operators: Bool -> Bool -> Bool
                    BinOp::And | BinOp::Or => {
                        let bool_type = self.bool_type();
                        self.unify(&left_typed.type_, &bool_type, &left.span);
                        self.unify(&right_typed.type_, &bool_type, &right.span);
                        bool_type
                    }

                    _ => {
                        self.diagnostics.add_type_error(TypeError {
                            span: expr.span.clone(),
                            file: expr.file.clone(),
                            kind: TypeErrorKind::InvalidOperator {
                                op: format!("{:?}", op),
                            },
                        });
                        self.fresh_type_var()
                    }
                };

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::BinOp {
                        left: Box::new(left_typed),
                        op: op.clone(),
                        right: Box::new(right_typed),
                    },
                    type_: result_type,
                }
            }

            ExprKind::Call(func, args) => {
                let func_typed = self.check_expr(func);
                let args_typed: Vec<_> = args.iter().map(|a| self.check_expr(a)).collect();

                // creating a new variable
                let instantiated_func_type = self.instantiate(&func_typed.type_);

                match &instantiated_func_type.type_ {
                    TypeKind::Function {
                        params,
                        return_type,
                        effects,
                    } => {
                        if params.len() != args_typed.len() {
                            self.diagnostics.add_type_error(TypeError {
                                span: expr.span.clone(),
                                file: expr.file.clone(),
                                kind: TypeErrorKind::TypeMismatch {
                                    expected: instantiated_func_type.clone(),
                                    found: func_typed.type_.clone(),
                                    context: "function call arity mismatch".to_string(),
                                },
                            });

                            return TypedExpr {
                                span: expr.span.clone(),
                                file: expr.file.clone(),
                                expr: TypedExprKind::Error,
                                type_: self.error_type(),
                            };
                        }

                        // idk if this is the issue
                        for (param_type, arg) in params.iter().zip(&args_typed) {
                            println!("unifying args");
                            self.unify(param_type, &arg.type_, &arg.span);
                        }

                        // Check if the effects of the called function are allowed by the current context
                        if !effects.is_subset_of(&self.current_function_effects) {
                            self.diagnostics.add_type_error(TypeError {
                                span: expr.span.clone(),
                                file: expr.file.clone(),
                                kind: TypeErrorKind::EffectMismatch {
                                    required: effects.clone(),
                                    found: self.current_function_effects.clone(),
                                },
                            });
                        }

                        let concrete_return = self.substitution.apply(return_type);

                        TypedExpr {
                            span: expr.span.clone(),
                            file: expr.file.clone(),
                            expr: TypedExprKind::Call {
                                function: Box::new(func_typed),
                                args: args_typed,
                                type_args: vec![],
                            },
                            type_: concrete_return,
                        }
                    }

                    TypeKind::Variable { .. } => {
                        let ret_type = self.fresh_type_var();
                        let param_types: Vec<_> =
                            args_typed.iter().map(|a| a.type_.clone()).collect();

                        let expected_func_type = Rc::new(Type {
                            span: None,
                            file: None,
                            type_: TypeKind::Function {
                                params: param_types,
                                return_type: ret_type.clone(),
                                effects: EffectSet::pure(),
                            },
                        });

                        self.unify(&instantiated_func_type, &expected_func_type, &expr.span);
                        let concrete_return = self.substitution.apply(&ret_type);

                        TypedExpr {
                            span: expr.span.clone(),
                            file: expr.file.clone(),
                            expr: TypedExprKind::Call {
                                function: Box::new(func_typed),
                                args: args_typed,
                                type_args: vec![],
                            },
                            type_: concrete_return,
                        }
                    }

                    _ => {
                        self.diagnostics.add_type_error(TypeError {
                            span: expr.span.clone(),
                            file: expr.file.clone(),
                            kind: TypeErrorKind::NotAFunction {
                                type_: func_typed.type_.clone(),
                            },
                        });

                        TypedExpr {
                            span: expr.span.clone(),
                            file: expr.file.clone(),
                            expr: TypedExprKind::Error,
                            type_: self.error_type(),
                        }
                    }
                }
            }

            ExprKind::Float(f) => TypedExpr {
                span: expr.span.clone(),
                file: expr.file.clone(),
                expr: TypedExprKind::Float(*f),
                type_: self.float_type(),
            },

            ExprKind::String(s) => TypedExpr {
                span: expr.span.clone(),
                file: expr.file.clone(),
                expr: TypedExprKind::String(s.clone()),
                type_: self.string_type(),
            },

            ExprKind::Block(exprs) => {
                self.env.push_scope();
                let typed_exprs: Vec<_> = exprs.iter().map(|e| self.check_expr(e)).collect();
                let ty = typed_exprs
                    .last()
                    .map(|e| e.type_.clone())
                    .unwrap_or_else(|| self.unit_type());
                self.env.pop_scope();

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Block {
                        expressions: typed_exprs,
                    },
                    type_: ty,
                }
            }

            ExprKind::IfElse {
                condition,
                then,
                else_,
            } => {
                let cond = self.check_expr(condition);
                self.unify(&cond.type_, &self.bool_type(), &condition.span);

                let then_typed = self.check_expr(then);
                let else_typed = else_.as_ref().map(|e| self.check_expr(e));

                let ty = if let Some(ref else_t) = else_typed {
                    self.unify(&then_typed.type_, &else_t.type_, &expr.span);
                    then_typed.type_.clone()
                } else {
                    self.unit_type()
                };

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::IfElse {
                        condition: Box::new(cond),
                        then: Box::new(then_typed),
                        else_: else_typed.map(Box::new),
                    },
                    type_: ty,
                }
            }

            ExprKind::Lambda { args, expression } => {
                self.env.push_scope();

                let typed_args: Vec<_> = args
                    .iter()
                    .map(|arg| {
                        let binding_id = self.id_gen.fresh_binding();
                        let ty = arg
                            .type_
                            .as_ref()
                            .map(|t| self.resolve_type_annot(t))
                            .unwrap_or_else(|| self.fresh_type_var());

                        let sym = self.interner.intern(&arg.name);
                        self.env.add_binding(
                            sym,
                            Binding {
                                id: binding_id,
                                name: sym,
                                type_: ty.clone(),
                                mutable: false,
                            },
                        );

                        TypedFnArg {
                            span: arg.span.clone(),
                            file: arg.file.clone(),
                            name: sym,
                            binding_id,
                            type_: ty,
                        }
                    })
                    .collect();

                let body = self.check_expr(expression);

                let func_type = self.function_type(
                    typed_args.iter().map(|a| a.type_.clone()).collect(),
                    body.type_.clone(),
                );

                // Perform capture analysis on the lambda body
                // We need to identify which variables in the body come from outer scopes
                let lambda_arg_binding_ids: std::collections::HashSet<BindingId> =
                    typed_args.iter().map(|arg| arg.binding_id).collect();

                let captures = self.find_captures_in_lambda_body(&body, &lambda_arg_binding_ids);

                self.env.pop_scope();

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Lambda {
                        args: typed_args,
                        body: Box::new(body),
                        captures,
                        function_type: func_type.clone(),
                    },
                    type_: func_type,
                }
            }

            ExprKind::Match(scrutinee, arms) => {
                let scrut_typed = self.check_expr(scrutinee);
                let result_type = self.fresh_type_var();

                let typed_arms: Vec<_> = arms
                    .iter()
                    .map(|arm| {
                        self.env.push_scope();
                        let pattern = self.check_pattern(&arm.pattern, &scrut_typed.type_);
                        let guard = arm.guard.as_ref().map(|g| {
                            let g_typed = self.check_expr(g);
                            self.unify(&g_typed.type_, &self.bool_type(), &g.span);
                            g_typed
                        });
                        let body = self.check_expr(&arm.body);
                        self.unify(&body.type_, &result_type, &arm.body.span);
                        self.env.pop_scope();

                        TypedMatchArm {
                            pattern,
                            guard,
                            body: Box::new(body),
                            span: arm.span.clone(),
                        }
                    })
                    .collect();

                // Check exhaustiveness
                let checker = ExhaustivenessChecker::new(self.interner.clone(), self.env.clone());
                checker.check_match(
                    &scrut_typed.type_,
                    &typed_arms,
                    &expr.span,
                    &mut self.diagnostics,
                );

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Match {
                        scrutinee: Box::new(scrut_typed),
                        arms: typed_arms,
                    },
                    type_: result_type,
                }
            }

            ExprKind::Array(elements) => {
                if elements.is_empty() {
                    let elem_ty = self.fresh_type_var();
                    return TypedExpr {
                        span: expr.span.clone(),
                        file: expr.file.clone(),
                        expr: TypedExprKind::Array {
                            elements: vec![],
                            element_type: elem_ty.clone(),
                        },
                        type_: self.array_type(elem_ty),
                    };
                }

                let typed_elems: Vec<_> = elements.iter().map(|e| self.check_expr(e)).collect();
                let elem_ty = typed_elems[0].type_.clone();

                for (i, e) in typed_elems.iter().enumerate().skip(1) {
                    self.unify(&e.type_, &elem_ty, &elements[i].span);
                }

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Array {
                        elements: typed_elems,
                        element_type: elem_ty.clone(),
                    },
                    type_: self.array_type(elem_ty),
                }
            }

            ExprKind::Tuple(elements) => {
                let typed_elems: Vec<_> = elements.iter().map(|e| self.check_expr(e)).collect();
                let elem_types: Vec<_> = typed_elems.iter().map(|e| e.type_.clone()).collect();

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Tuple(typed_elems),
                    type_: Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Tuple(elem_types),
                    }),
                }
            }

            ExprKind::Index(target, index) => {
                let target_typed = self.check_expr(target);
                let index_typed = self.check_expr(index);
                self.unify(&index_typed.type_, &self.int_type(), &index.span);

                let elem_type = self.fresh_type_var();
                let expected_array = self.array_type(elem_type.clone());
                self.unify(&target_typed.type_, &expected_array, &target.span);

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Index {
                        target: Box::new(target_typed),
                        index: Box::new(index_typed),
                        element_type: elem_type.clone(),
                    },
                    type_: elem_type,
                }
            }

            ExprKind::Return(val) => {
                let val_typed = val.as_ref().map(|v| self.check_expr(v));
                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Return(val_typed.map(Box::new)),
                    type_: Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Never,
                    }),
                }
            }

            ExprKind::UnOp(op, operand) => {
                let operand_typed = self.check_expr(operand);
                let result_type = match op {
                    UnOp::Not => {
                        self.unify(&operand_typed.type_, &self.bool_type(), &operand.span);
                        self.bool_type()
                    }
                    UnOp::Plus | UnOp::Minus => {
                        // Allow numeric types
                        operand_typed.type_.clone()
                    }
                    UnOp::Unwrap => {
                        let inner = self.fresh_type_var();
                        let option_ty = self.option_type(inner.clone());
                        self.unify(&operand_typed.type_, &option_ty, &operand.span);
                        inner
                    }
                };

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::UnOp {
                        op: op.clone(),
                        operand: Box::new(operand_typed),
                    },
                    type_: result_type,
                }
            }

            ExprKind::Assign { l_val, r_val, op } => {
                let l_typed = self.check_expr(l_val);
                let r_typed = self.check_expr(r_val);

                match op {
                    AssignOp::Assign => {
                        self.unify(&l_typed.type_, &r_typed.type_, &expr.span);
                    }
                    _ => {
                        // AddAssign, SubAssign, etc. - ensure numeric types
                        self.unify(&l_typed.type_, &r_typed.type_, &expr.span);
                    }
                }

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Assign {
                        l_val: Box::new(l_typed),
                        r_val: Box::new(r_typed),
                        op: op.clone(),
                    },
                    type_: self.unit_type(),
                }
            }

            ExprKind::Map(entries) => {
                if entries.is_empty() {
                    let key_ty = self.fresh_type_var();
                    let val_ty = self.fresh_type_var();
                    return TypedExpr {
                        span: expr.span.clone(),
                        file: expr.file.clone(),
                        expr: TypedExprKind::Map {
                            entries: vec![],
                            key_type: key_ty.clone(),
                            value_type: val_ty.clone(),
                        },
                        type_: self.map_type(key_ty, val_ty),
                    };
                }

                let typed_entries: Vec<_> = entries
                    .iter()
                    .map(|(k, v)| (self.check_expr(k), self.check_expr(v)))
                    .collect();

                let key_ty = typed_entries[0].0.type_.clone();
                let val_ty = typed_entries[0].1.type_.clone();

                for (i, (k, v)) in typed_entries.iter().enumerate().skip(1) {
                    self.unify(&k.type_, &key_ty, &entries[i].0.span);
                    self.unify(&v.type_, &val_ty, &entries[i].1.span);
                }

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Map {
                        entries: typed_entries,
                        key_type: key_ty.clone(),
                        value_type: val_ty.clone(),
                    },
                    type_: self.map_type(key_ty, val_ty),
                }
            }

            ExprKind::EnumConstruct {
                name,
                variant,
                args,
            } => {
                let enum_sym = self.interner.intern(name);
                let variant_sym = self.interner.intern(variant);

                // Lookup enum
                let enum_info = self
                    .env
                    .enums
                    .iter()
                    .find(|(_, info)| info.name == enum_sym)
                    .map(|(id, info)| (*id, info.clone()));

                if let Some((enum_id, enum_info)) = enum_info
                    && let Some((variant_id, variant_types)) = enum_info.variants.get(&variant_sym)
                {
                    let typed_args: Vec<_> = args.iter().map(|a| self.check_expr(a)).collect();

                    // Check argument count
                    if typed_args.len() != variant_types.len() {
                        self.error(
                            expr,
                            TypeErrorKind::ArityMismatch {
                                expected: variant_types.len(),
                                found: typed_args.len(),
                                function: variant_sym,
                            },
                        );
                    } else {
                        // Unify each argument
                        for (arg, expected) in typed_args.iter().zip(variant_types.iter()) {
                            self.unify(&arg.type_, expected, &arg.span);
                        }
                    }

                    let enum_type = Rc::new(Type {
                        span: Some(expr.span.clone()),
                        file: Some(expr.file.clone()),
                        type_: TypeKind::Constructor {
                            name: enum_sym.0,
                            args: vec![],
                            kind: Kind::Star,
                        },
                    });

                    return TypedExpr {
                        span: expr.span.clone(),
                        file: expr.file.clone(),
                        expr: TypedExprKind::EnumConstruct {
                            enum_name: enum_sym,
                            enum_id,
                            variant: variant_sym,
                            variant_id: *variant_id,
                            args: typed_args,
                        },
                        type_: enum_type,
                    };
                }

                self.error(expr, TypeErrorKind::UnboundVariable { name: variant_sym });
                self.error_expr(expr)
            }

            ExprKind::StructConstruct { name, fields } => {
                let struct_sym = self.interner.intern(name);

                // Lookup struct in environment
                let struct_info = self
                    .env
                    .structs
                    .iter()
                    .find(|(_, info)| info.name == struct_sym)
                    .map(|(id, info)| (*id, info.clone()));

                if let Some((struct_id, struct_info)) = struct_info {
                    // Create a mapping from field names to their expected types
                    let expected_fields: std::collections::HashMap<
                        Symbol,
                        (FieldId, Rc<Type>, Visibility),
                    > = struct_info.fields.clone();

                    // Type check each field expression
                    let mut typed_fields = Vec::new();
                    let mut processed_field_names = std::collections::HashSet::new();

                    for (field_name, field_expr) in fields {
                        let field_sym = self.interner.intern(field_name);

                        // Check for duplicate field names
                        if processed_field_names.contains(&field_sym) {
                            self.error(expr, TypeErrorKind::DuplicateField { field: field_sym });
                            continue;
                        }
                        processed_field_names.insert(field_sym);

                        // Check if the field exists in the struct
                        if let Some((field_id, expected_type, _vis)) =
                            expected_fields.get(&field_sym)
                        {
                            let typed_field_expr = self.check_expr(field_expr);

                            // Unify the field expression type with expected type
                            self.unify(&typed_field_expr.type_, expected_type, &field_expr.span);

                            typed_fields.push((field_sym, *field_id, typed_field_expr));
                        } else {
                            self.error(
                                expr,
                                TypeErrorKind::InvalidFieldAccess {
                                    type_: Rc::new(Type {
                                        span: Some(expr.span.clone()),
                                        file: Some(expr.file.clone()),
                                        type_: TypeKind::Constructor {
                                            name: struct_sym.0,
                                            args: vec![],
                                            kind: Kind::Star,
                                        },
                                    }),
                                    field: field_sym,
                                },
                            );
                        }
                    }

                    // Check that all required fields are provided
                    for (field_sym, (_field_id, _field_type, _vis)) in &struct_info.fields {
                        if !processed_field_names.contains(field_sym) {
                            self.error(
                                expr,
                                TypeErrorKind::MissingField {
                                    struct_name: struct_sym,
                                    field: *field_sym,
                                },
                            );
                        }
                    }

                    // Create the struct type
                    let struct_type = Rc::new(Type {
                        span: Some(expr.span.clone()),
                        file: Some(expr.file.clone()),
                        type_: TypeKind::Constructor {
                            name: struct_sym.0,
                            args: vec![], // For now, no type parameters
                            kind: Kind::Star,
                        },
                    });

                    return TypedExpr {
                        span: expr.span.clone(),
                        file: expr.file.clone(),
                        expr: TypedExprKind::StructConstruct {
                            struct_name: struct_sym,
                            struct_id,
                            fields: typed_fields,
                        },
                        type_: struct_type,
                    };
                }

                // Struct not found in environment
                self.error(expr, TypeErrorKind::UnboundType { name: struct_sym });
                self.error_expr(expr)
            }

            ExprKind::FieldAccess(target, field) => {
                let target_typed = self.check_expr(target);
                let field_sym = self.interner.intern(field);

                // Extract struct from type
                let struct_id = match &target_typed.type_.type_ {
                    TypeKind::Constructor { name, .. } => {
                        // Lookup struct by name
                        self.env
                            .structs
                            .iter()
                            .find(|(_, info)| info.name.0 == *name)
                            .map(|(id, _)| *id)
                    }
                    _ => None,
                };

                if let Some(sid) = struct_id
                    && let Some(struct_info) = self.env.get_struct(sid)
                    && let Some((field_id, field_type, _vis)) = struct_info.fields.get(&field_sym)
                {
                    return TypedExpr {
                        span: expr.span.clone(),
                        file: expr.file.clone(),
                        expr: TypedExprKind::FieldAccess {
                            target: Box::new(target_typed),
                            field: field_sym,
                            field_id: *field_id,
                            field_type: field_type.clone(),
                        },
                        type_: field_type.clone(),
                    };
                }

                self.error(
                    expr,
                    TypeErrorKind::InvalidFieldAccess {
                        type_: target_typed.type_.clone(),
                        field: field_sym,
                    },
                );
                self.error_expr(expr)
            }

            ExprKind::OptionalChain(target, field) => {
                let target_typed = self.check_expr(target);
                let field_sym = self.interner.intern(field);

                // Extract Option<StructType>
                let inner_type = match &target_typed.type_.type_ {
                    TypeKind::Constructor { name, args, .. }
                        if self.interner.resolve(Symbol(*name)) == "Option" && !args.is_empty() =>
                    {
                        args[0].clone()
                    }
                    _ => {
                        let tv = self.fresh_type_var();
                        let expected = self.option_type(tv);
                        self.error(
                            expr,
                            TypeErrorKind::TypeMismatch {
                                expected,
                                found: target_typed.type_.clone(),
                                context: "optional chain requires Option type".to_string(),
                            },
                        );
                        return self.error_expr(expr);
                    }
                };

                // Lookup field in inner struct type
                let struct_id = match &inner_type.type_ {
                    TypeKind::Constructor { name, .. } => self
                        .env
                        .structs
                        .iter()
                        .find(|(_, info)| info.name.0 == *name)
                        .map(|(id, _)| *id),
                    _ => None,
                };

                if let Some(sid) = struct_id
                    && let Some(struct_info) = self.env.get_struct(sid)
                {
                    let struct_info = struct_info.clone();
                    if let Some((field_id, field_type, _vis)) = struct_info.fields.get(&field_sym) {
                        let field_id = *field_id;
                        let result_type = self.option_type(field_type.clone());

                        return TypedExpr {
                            span: expr.span.clone(),
                            file: expr.file.clone(),
                            expr: TypedExprKind::OptionalChain {
                                target: Box::new(target_typed),
                                field: field_sym,
                                field_id,
                                field_type: field_type.clone(),
                            },
                            type_: result_type,
                        };
                    }
                }

                self.error(
                    expr,
                    TypeErrorKind::InvalidFieldAccess {
                        type_: inner_type,
                        field: field_sym,
                    },
                );
                self.error_expr(expr)
            }

            ExprKind::Cast {
                expr: inner,
                target_type,
            } => {
                let expr_typed = self.check_expr(inner);
                let target_ty = self.resolve_type_annot(target_type);

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Cast {
                        expr: Box::new(expr_typed),
                        target_type: target_ty.clone(),
                    },
                    type_: target_ty,
                }
            }
            ExprKind::With { context, var, body } => {
                let context_typed = self.check_expr(context);
                self.env.push_scope();

                let binding_id = self.id_gen.fresh_binding();
                let var_sym = self.interner.intern(var);
                self.env.add_binding(
                    var_sym,
                    Binding {
                        id: binding_id,
                        name: var_sym,
                        type_: context_typed.type_.clone(),
                        mutable: false,
                    },
                );

                let body_typed = self.check_expr(body);
                self.env.pop_scope();

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::With {
                        context: Box::new(context_typed.clone()),
                        var: var_sym,
                        binding_id,
                        var_type: context_typed.type_.clone(),
                        body: Box::new(body_typed.clone()),
                    },
                    type_: body_typed.type_.clone(),
                }
            }

            ExprKind::Loop { label, body } => {
                self.env.push_scope();
                let label_sym = label.as_ref().map(|l| self.interner.intern(l));
                let body_typed = self.check_expr(body);
                self.env.pop_scope();

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Loop {
                        label: label_sym,
                        body: Box::new(body_typed),
                    },
                    type_: Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Never,
                    }),
                }
            }

            ExprKind::For {
                iterator,
                value,
                expression,
            } => {
                let iter_typed = self.check_expr(iterator);

                if let Some(iterable_trait) = self
                    .env
                    .traits
                    .iter()
                    .find(|(_, info)| self.interner.resolve(info.name) == "Iterable")
                    .map(|(id, _)| *id)
                {
                    self.constraint_solver.add_constraint(TraitConstraint {
                        type_: iter_typed.type_.clone(),
                        trait_id: iterable_trait,
                        span: iterator.span.clone(),
                    });
                }

                self.env.push_scope();

                let elem_type = self.fresh_type_var();

                // Check for Iterable trait implementation
                // Extract element type from Array, List, or types implementing Iterable
                let inferred_elem = match &iter_typed.type_.type_ {
                    TypeKind::Constructor { name, args, .. } => {
                        let type_name = self.interner.resolve(Symbol(*name));
                        match type_name {
                            "Array" | "List" | "Vec" if !args.is_empty() => args[0].clone(),
                            _ => {
                                // Check if type implements Iterable<T> trait
                                // First look up the Iterable trait in the environment
                                let iterable_trait = self
                                    .env
                                    .traits
                                    .iter()
                                    .find(|(_, info)| {
                                        self.interner.resolve(info.name) == "Iterable"
                                    })
                                    .map(|(id, _)| *id);

                                if let Some(trait_id) = iterable_trait {
                                    // Add a constraint that the iterator type implements Iterable<elem_type>
                                    // We need to extract the type ID from the iterator type
                                    // For now, use a placeholder for the type ID
                                    let type_id = match &iter_typed.type_.type_ {
                                        TypeKind::Constructor { name, .. } => *name, // Use constructor name as ID
                                        TypeKind::Variable { id, .. } => *id, // Use variable ID
                                        _ => 0,                               // Fallback
                                    };
                                    let constraint = Constraint::Trait(type_id, trait_id.0);
                                    self.constraints.push(constraint);
                                    // For now, return the element type as the iterable element type
                                    elem_type.clone()
                                } else {
                                    // If Iterable trait doesn't exist, fall back to the original behavior
                                    let expected = self.array_type(elem_type.clone());
                                    self.error(
                                        expr,
                                        TypeErrorKind::TypeMismatch {
                                            expected,
                                            found: iter_typed.type_.clone(),
                                            context: "for loop requires iterable type".to_string(),
                                        },
                                    );
                                    elem_type.clone()
                                }
                            }
                        }
                    }
                    _ => {
                        let expected = self.array_type(elem_type.clone());

                        self.error(
                            expr,
                            TypeErrorKind::TypeMismatch {
                                expected,
                                found: iter_typed.type_.clone(),
                                context: "for loop requires iterable type".to_string(),
                            },
                        );
                        elem_type.clone()
                    }
                };

                self.unify(&elem_type, &inferred_elem, &iterator.span);

                let binding_id = self.id_gen.fresh_binding();
                let value_sym = self.interner.intern(value);
                self.env.add_binding(
                    value_sym,
                    Binding {
                        id: binding_id,
                        name: value_sym,
                        type_: elem_type.clone(),
                        mutable: false,
                    },
                );

                let body_typed = self.check_expr(expression);
                self.env.pop_scope();

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::For {
                        iterator: Box::new(iter_typed.clone()),
                        iterator_type: iter_typed.type_.clone(),
                        value: value_sym,
                        binding_id,
                        value_type: elem_type,
                        body: Box::new(body_typed),
                    },
                    type_: self.unit_type(),
                }
            }

            ExprKind::While(condition, body) => {
                let cond_typed = self.check_expr(condition);
                self.unify(&cond_typed.type_, &self.bool_type(), &condition.span);

                let body_typed = self.check_expr(body);

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::While {
                        condition: Box::new(cond_typed),
                        body: Box::new(body_typed),
                    },
                    type_: self.unit_type(),
                }
            }

            ExprKind::IfLet {
                pattern,
                expr: scrutinee,
                then,
                else_,
            } => {
                let scrut_typed = self.check_expr(scrutinee);
                self.env.push_scope();
                let pattern_typed = self.check_pattern(pattern, &scrut_typed.type_);
                let then_typed = self.check_expr(then);
                self.env.pop_scope();

                let else_typed = else_.as_ref().map(|e| self.check_expr(e));
                let result_type = if let Some(ref e) = else_typed {
                    self.unify(&then_typed.type_, &e.type_, &expr.span);
                    then_typed.type_.clone()
                } else {
                    self.unit_type()
                };

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::IfLet {
                        pattern: pattern_typed,
                        expr: Box::new(scrut_typed),
                        then: Box::new(then_typed),
                        else_: else_typed.map(Box::new),
                    },
                    type_: result_type,
                }
            }

            ExprKind::WhileLet {
                pattern,
                expr: scrutinee,
                body,
            } => {
                let scrut_typed = self.check_expr(scrutinee);
                self.env.push_scope();
                let pattern_typed = self.check_pattern(pattern, &scrut_typed.type_);
                let body_typed = self.check_expr(body);
                self.env.pop_scope();

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::WhileLet {
                        pattern: pattern_typed,
                        expr: Box::new(scrut_typed),
                        body: Box::new(body_typed),
                    },
                    type_: self.unit_type(),
                }
            }

            ExprKind::Break(val) => {
                let val_typed = val.as_ref().map(|v| self.check_expr(v));
                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Break(val_typed.map(Box::new)),
                    type_: Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Never,
                    }),
                }
            }

            ExprKind::Continue => TypedExpr {
                span: expr.span.clone(),
                file: expr.file.clone(),
                expr: TypedExprKind::Continue,
                type_: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Never,
                }),
            },

            ExprKind::Perform { effect, args } => {
                let op_sym = self.interner.intern(effect);

                // Find which effect contains this operation
                let mut found_operation: Option<(Rc<Type>, EffectId, EffectInfo)> = None;

                for (effect_id, effect_info) in &self.env.effects {
                    if let Some(op_type) = effect_info.operations.get(&op_sym) {
                        found_operation = Some((op_type.clone(), *effect_id, effect_info.clone()));
                        break;
                    }
                }

                if let Some((op_type, effect_id, _effect_info)) = found_operation {
                    println!("DEBUG: Processing perform operation");
                    println!("DEBUG: Original op type: {:?}", op_type);
                    println!(
                        "DEBUG: Current substitution map: {:?}",
                        self.substitution.map
                    );

                    // Apply current substitution to resolve type variables
                    let substituted_op_type = self.substitution.apply(&op_type);
                    println!("DEBUG: Substituted op type: {:?}", substituted_op_type);

                    // Check that op_type is a function type
                    if let TypeKind::Function {
                        params,
                        return_type,
                        ..
                    } = &substituted_op_type.type_
                    {
                        let typed_args: Vec<_> = args.iter().map(|a| self.check_expr(a)).collect();

                        // Check that the number of arguments matches
                        if typed_args.len() != params.len() {
                            self.error(
                                expr,
                                TypeErrorKind::ArityMismatch {
                                    expected: params.len(),
                                    found: typed_args.len(),
                                    function: op_sym, // Use the operation symbol as the function name
                                },
                            );
                            return self.error_expr(expr);
                        }

                        // Check that each argument matches the expected type
                        for (arg, expected_type) in typed_args.iter().zip(params.iter()) {
                            self.unify(&arg.type_, expected_type, &arg.span);
                        }

                        // Add the effect to the current function's allowed effects if not already present
                        if !self.current_function_effects.effects.contains(&effect_id.0) {
                            self.current_function_effects.effects.push(effect_id.0);
                        }

                        TypedExpr {
                            span: expr.span.clone(),
                            file: expr.file.clone(),
                            expr: TypedExprKind::Perform {
                                effect: op_sym, // This should be the operation name
                                effect_id,
                                args: typed_args,
                            },
                            type_: return_type.clone(),
                        }
                    } else {
                        self.error(
                            expr,
                            TypeErrorKind::NotAFunction {
                                type_: op_type.clone(),
                            },
                        );
                        self.error_expr(expr)
                    }
                } else {
                    self.error(expr, TypeErrorKind::UnboundVariable { name: op_sym });
                    self.error_expr(expr)
                }
            }

            ExprKind::Handle { body, handlers } => {
                // Save the current allowed effects
                let saved_effects = self.current_function_effects.clone();

                // When handling effects, we remove the handled effects from the allowed set
                // This simulates that these effects are now being handled and don't need to be declared
                let mut handled_effects = Vec::new();
                for handler in handlers {
                    let effect_sym = self.interner.intern(&handler.effect);
                    handled_effects.push(effect_sym.0);
                }

                // Remove handled effects from current_function_effects
                let mut new_effects = saved_effects.clone();
                new_effects.effects.retain(|e| !handled_effects.contains(e));

                // Update the current allowed effects to exclude handled ones
                self.current_function_effects = new_effects;

                let body_typed = self.check_expr(body);
                let typed_handlers: Vec<_> = handlers
                    .iter()
                    .map(|h| self.check_effect_handler(h))
                    .collect();

                // Restore the original effects
                self.current_function_effects = saved_effects;

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Handle {
                        body: Box::new(body_typed.clone()),
                        handlers: typed_handlers,
                        return_type: body_typed.type_.clone(),
                    },
                    type_: body_typed.type_.clone(),
                }
            }

            ExprKind::MacroCall(name, args, delimiter) => {
                let name_sym = self.interner.intern(name);

                // Macros should be expanded before type checking, so at this point
                // we just type check the arguments and return a fresh type variable
                // since the macro expansion will determine the final type
                let typed_args: Vec<_> = args.iter().map(|a| self.check_expr(a)).collect();

                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::MacroCall {
                        name: name_sym,
                        macro_id: MacroId(0), // Placeholder - macros expanded before type checking
                        args: typed_args,
                        delimiter: delimiter.clone(),
                    },
                    type_: self.fresh_type_var(), // Return type determined after macro expansion
                }
            }

            ExprKind::Import(import) => {
                // Import expressions are handled during earlier phases (parsing/resolution)
                // For type checking, we just need to create a typed import expression
                TypedExpr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: TypedExprKind::Import(TypedImport {
                        span: import.span.clone(),
                        file: import.file.clone(),
                        path: import
                            .path
                            .iter()
                            .map(|s| self.interner.intern(s))
                            .collect(),
                        items: import
                            .items
                            .iter()
                            .map(|(name, alias)| {
                                // In the typed version, we convert string names to symbols
                                (
                                    self.interner.intern(name),
                                    alias.as_ref().map(|a| self.interner.intern(a)),
                                    TypeId(0), // Placeholder, would be filled during type resolution
                                    self.fresh_type_var(),
                                ) // Placeholder type
                            })
                            .collect(),
                        alias: import.alias.as_ref().map(|a| self.interner.intern(a)),
                    }),
                    type_: self.unit_type(), // Import expressions have unit type
                }
            }

            ExprKind::Error => {
                // Error expressions should not normally reach type checking
                // but if they do, return an error type
                self.error_expr(&Expr {
                    span: expr.span.clone(),
                    file: expr.file.clone(),
                    expr: ExprKind::Error,
                })
            }
        }
    }

    fn check_pattern(&mut self, pat: &Pattern, expected_ty: &Rc<Type>) -> TypedPattern {
        match &pat.pat {
            PatKind::Wildcard => TypedPattern {
                span: pat.span.clone(),
                file: pat.file.clone(),
                pat: TypedPatKind::Wildcard,
                type_: expected_ty.clone(),
            },

            PatKind::Bind(name) => {
                let sym = self.interner.intern(name);
                let binding_id = self.id_gen.fresh_binding();
                self.env.add_binding(
                    sym,
                    Binding {
                        id: binding_id,
                        name: sym,
                        type_: expected_ty.clone(),
                        mutable: false,
                    },
                );

                TypedPattern {
                    span: pat.span.clone(),
                    file: pat.file.clone(),
                    pat: TypedPatKind::Bind {
                        name: sym,
                        binding_id,
                    },
                    type_: expected_ty.clone(),
                }
            }

            PatKind::Literal(lit) => {
                let lit_type = match lit {
                    Literal::Int(_) => self.int_type(),
                    Literal::Float(_) => self.float_type(),
                    Literal::Bool(_) => self.bool_type(),
                    Literal::String(_) => self.string_type(),
                };
                self.unify(&lit_type, expected_ty, &pat.span);

                TypedPattern {
                    span: pat.span.clone(),
                    file: pat.file.clone(),
                    pat: TypedPatKind::Literal(lit.clone()),
                    type_: lit_type,
                }
            }

            PatKind::Array(patterns) => {
                let elem_type = self.fresh_type_var();
                let array_type = self.array_type(elem_type.clone());
                self.unify(&array_type, expected_ty, &pat.span);

                let typed_patterns: Vec<_> = patterns
                    .iter()
                    .map(|p| self.check_pattern(p, &elem_type))
                    .collect();

                TypedPattern {
                    span: pat.span.clone(),
                    file: pat.file.clone(),
                    pat: TypedPatKind::Array {
                        patterns: typed_patterns,
                        element_type: elem_type.clone(),
                    },
                    type_: array_type,
                }
            }

            PatKind::Or(patterns) => {
                let typed_patterns: Vec<_> = patterns
                    .iter()
                    .map(|p| self.check_pattern(p, expected_ty))
                    .collect();

                TypedPattern {
                    span: pat.span.clone(),
                    file: pat.file.clone(),
                    pat: TypedPatKind::Or(typed_patterns),
                    type_: expected_ty.clone(),
                }
            }

            PatKind::As { name, pattern } => {
                let sym = self.interner.intern(name);
                let binding_id = self.id_gen.fresh_binding();
                let inner = self.check_pattern(pattern, expected_ty);

                self.env.add_binding(
                    sym,
                    Binding {
                        id: binding_id,
                        name: sym,
                        type_: expected_ty.clone(),
                        mutable: false,
                    },
                );

                TypedPattern {
                    span: pat.span.clone(),
                    file: pat.file.clone(),
                    pat: TypedPatKind::As {
                        name: sym,
                        binding_id,
                        pattern: Box::new(inner),
                    },
                    type_: expected_ty.clone(),
                }
            }

            PatKind::Struct { name, fields } => {
                let struct_sym = self.interner.intern(name);

                let struct_info = self
                    .env
                    .structs
                    .iter()
                    .find(|(_, info)| info.name == struct_sym)
                    .map(|(id, info)| (*id, info.clone()));

                if let Some((struct_id, struct_info)) = struct_info {
                    let typed_fields: Vec<_> = fields
                        .iter()
                        .map(|(field_name, field_pat)| {
                            let field_sym = self.interner.intern(field_name);

                            if let Some((field_id, field_type, _)) =
                                struct_info.fields.get(&field_sym)
                            {
                                let typed_pat = self.check_pattern(field_pat, field_type);
                                (field_sym, *field_id, typed_pat)
                            } else {
                                let binding = &self.fresh_type_var();
                                let typed_pat = self.check_pattern(field_pat, binding);
                                (field_sym, FieldId(0), typed_pat)
                            }
                        })
                        .collect();

                    return TypedPattern {
                        span: pat.span.clone(),
                        file: pat.file.clone(),
                        pat: TypedPatKind::Struct {
                            name: struct_sym,
                            struct_id,
                            fields: typed_fields,
                        },
                        type_: expected_ty.clone(),
                    };
                }

                TypedPattern {
                    span: pat.span.clone(),
                    file: pat.file.clone(),
                    pat: TypedPatKind::Error,
                    type_: Rc::new(Type::new_error()),
                }
            }

            PatKind::Enum {
                name,
                variant,
                params,
            } => {
                let enum_sym = self.interner.intern(name);
                let variant_sym = self.interner.intern(variant);

                // Lookup enum variant
                let enum_info = self
                    .env
                    .enums
                    .iter()
                    .find(|(_, info)| info.name == enum_sym)
                    .map(|(id, info)| (*id, info.clone()));

                if let Some((enum_id, enum_info)) = enum_info
                    && let Some((variant_id, variant_types)) = enum_info.variants.get(&variant_sym)
                {
                    let typed_params: Vec<_> = params
                        .iter()
                        .zip(variant_types.iter())
                        .map(|(p, vt)| self.check_pattern(p, vt))
                        .collect();

                    return TypedPattern {
                        span: pat.span.clone(),
                        file: pat.file.clone(),
                        pat: TypedPatKind::Enum {
                            enum_name: enum_sym,
                            enum_id,
                            variant: variant_sym,
                            variant_id: *variant_id,
                            params: typed_params,
                        },
                        type_: expected_ty.clone(),
                    };
                }

                TypedPattern {
                    span: pat.span.clone(),
                    file: pat.file.clone(),
                    pat: TypedPatKind::Error,
                    type_: Rc::new(Type::new_error()),
                }
            }

            PatKind::Range(start, end) => {
                let start_typed = self.check_expr(start);
                let end_typed = self.check_expr(end);
                self.unify(&start_typed.type_, &end_typed.type_, &pat.span);
                self.unify(&start_typed.type_, expected_ty, &pat.span);

                TypedPattern {
                    span: pat.span.clone(),
                    file: pat.file.clone(),
                    pat: TypedPatKind::Range {
                        start: Box::new(start_typed),
                        end: Box::new(end_typed),
                    },
                    type_: expected_ty.clone(),
                }
            }

            PatKind::Rest(name) => {
                let sym = self.interner.intern(name);
                let binding_id = self.id_gen.fresh_binding();
                self.env.add_binding(
                    sym,
                    Binding {
                        id: binding_id,
                        name: sym,
                        type_: expected_ty.clone(),
                        mutable: false,
                    },
                );

                TypedPattern {
                    span: pat.span.clone(),
                    file: pat.file.clone(),
                    pat: TypedPatKind::Rest {
                        name: sym,
                        binding_id,
                    },
                    type_: expected_ty.clone(),
                }
            }

            PatKind::Tuple(patterns) => {
                if let TypeKind::Tuple(element_types) = &expected_ty.type_ {
                    if patterns.len() != element_types.len() {
                        // Error: tuple pattern length doesn't match expected type
                        return TypedPattern {
                            span: pat.span.clone(),
                            file: pat.file.clone(),
                            pat: TypedPatKind::Error,
                            type_: Rc::new(Type::new_error()),
                        };
                    }

                    let typed_patterns: Vec<_> = patterns
                        .iter()
                        .zip(element_types.iter())
                        .map(|(p, ty)| self.check_pattern(p, ty))
                        .collect();

                    TypedPattern {
                        span: pat.span.clone(),
                        file: pat.file.clone(),
                        pat: TypedPatKind::Tuple {
                            patterns: typed_patterns,
                            element_types: element_types.clone(),
                        },
                        type_: expected_ty.clone(),
                    }
                } else {
                    // Error: trying to match against non-tuple type
                    TypedPattern {
                        span: pat.span.clone(),
                        file: pat.file.clone(),
                        pat: TypedPatKind::Error,
                        type_: Rc::new(Type::new_error()),
                    }
                }
            }

            PatKind::Error => TypedPattern {
                span: pat.span.clone(),
                file: pat.file.clone(),
                pat: TypedPatKind::Error,
                type_: Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Error,
                }),
            },
        }
    }

    fn check_effect_handler(&mut self, handler: &EffectHandler) -> TypedEffectHandler {
        self.env.push_scope();

        // In the handler syntax like `read_line(k) => ...`, the `effect` field contains the operation name
        // (e.g., "read_line"), not the effect type name. We need to find which effect contains this operation.
        let op_sym = self.interner.intern(&handler.effect);

        // Find which effect contains this operation
        let mut found_operation = None;

        for (effect_id, effect_info) in &self.env.effects {
            if let Some(op_type) = effect_info.operations.get(&op_sym) {
                found_operation = Some((*effect_id, effect_info.clone(), op_type.clone()));
                break;
            }
        }

        let (effect_id, _effect_info, op_type) =
            if let Some((effect_id, effect_info, op_type)) = found_operation {
                (effect_id, effect_info, op_type)
            } else {
                // Error: operation not found in any effect
                self.error(
                    &Expr {
                        span: handler.span.clone(),
                        file: String::new(),
                        expr: ExprKind::Error,
                    },
                    TypeErrorKind::UnboundVariable { name: op_sym },
                );
                // Return with error state
                self.env.pop_scope();
                return TypedEffectHandler {
                    span: handler.span.clone(),
                    effect: op_sym,
                    effect_id: EffectId(0),
                    params: vec![],
                    resume_param: self.interner.intern(&handler.resume_param),
                    resume_id: self.id_gen.fresh_binding(),
                    resume_type: self.fresh_type_var(),
                    body: self.error_expr(&handler.body),
                };
            };

        // Check that op_type is a function type
        let (op_params, op_return_type) = if let TypeKind::Function {
            params: ref op_params,
            return_type: ref op_return_type,
            ..
        } = op_type.type_
        {
            (op_params, op_return_type)
        } else {
            // Error: operation is not a function type
            self.error(
                &Expr {
                    span: handler.span.clone(),
                    file: String::new(),
                    expr: ExprKind::Error,
                },
                TypeErrorKind::NotAFunction {
                    type_: op_type.clone(),
                },
            );
            self.env.pop_scope();
            return TypedEffectHandler {
                span: handler.span.clone(),
                effect: op_sym,
                effect_id,
                params: vec![],
                resume_param: self.interner.intern(&handler.resume_param),
                resume_id: self.id_gen.fresh_binding(),
                resume_type: self.fresh_type_var(),
                body: self.error_expr(&handler.body),
            };
        };

        // Check that the number of handler parameters matches the operation parameters
        if handler.params.len() != op_params.len() {
            self.error(
                &Expr {
                    span: handler.span.clone(),
                    file: String::new(),
                    expr: ExprKind::Error,
                },
                TypeErrorKind::ArityMismatch {
                    expected: op_params.len(),
                    found: handler.params.len(),
                    function: op_sym,
                },
            );
        }

        // Create parameters with the correct types from the operation signature
        let params: Vec<_> = handler
            .params
            .iter()
            .zip(op_params.iter())
            .map(|(p, expected_type)| {
                let binding_id = self.id_gen.fresh_binding();
                let param_sym = self.interner.intern(p);

                self.env.add_binding(
                    param_sym,
                    Binding {
                        id: binding_id,
                        name: param_sym,
                        type_: expected_type.clone(),
                        mutable: false,
                    },
                );

                (param_sym, binding_id, expected_type.clone())
            })
            .collect();

        // The resume parameter should have a function type that takes the operation's return type
        // and returns the same type as the handler body
        let resume_return_type = self.fresh_type_var();
        let resume_type = Rc::new(Type {
            span: Some(handler.span.clone()),
            file: Some(handler.body.file.clone()), // Use the body's file
            type_: TypeKind::Function {
                params: vec![op_return_type.clone()], // Takes the operation's return type
                return_type: resume_return_type.clone(), // Returns the handler's return type
                effects: EffectSet::pure(),           // Resume functions are pure
            },
        });

        let resume_id = self.id_gen.fresh_binding();
        let resume_sym = self.interner.intern(&handler.resume_param);
        self.env.add_binding(
            resume_sym,
            Binding {
                id: resume_id,
                name: resume_sym,
                type_: resume_type.clone(),
                mutable: false,
            },
        );

        let body_typed = self.check_expr(&handler.body);

        // Unify the resume return type with the body type
        self.unify(&resume_return_type, &body_typed.type_, &handler.body.span);

        self.env.pop_scope();

        TypedEffectHandler {
            span: handler.span.clone(),
            effect: op_sym, // The operation name
            effect_id,
            params,
            resume_param: resume_sym,
            resume_id,
            resume_type,
            body: body_typed,
        }
    }

    fn resolve_type_annot(&mut self, annot: &TypeAnnot) -> Rc<Type> {
        self.resolve_type_annot_with_params(annot, &HashMap::new())
    }

    fn resolve_type_annot_with_params(
        &mut self,
        annot: &TypeAnnot,
        type_param_map: &HashMap<Symbol, Rc<Type>>,
    ) -> Rc<Type> {
        match &annot.type_ {
            TypeAnnotKind::Bool => self.bool_type(),
            TypeAnnotKind::Int => self.int_type(),
            TypeAnnotKind::Float => self.float_type(),
            TypeAnnotKind::String => self.string_type(),
            TypeAnnotKind::Unit => self.unit_type(),
            TypeAnnotKind::Never => Rc::new(Type {
                span: Some(annot.span.clone()),
                file: Some(annot.file.clone()),
                type_: TypeKind::Never,
            }),

            TypeAnnotKind::Named(name) => {
                let sym = self.interner.intern(name);

                // Check if this is a type parameter that should be substituted
                if let Some(type_var) = type_param_map.get(&sym) {
                    return type_var.clone();
                }

                // Check if it's a type alias
                for alias_info in self.env.type_aliases.values() {
                    if alias_info.name == sym {
                        return alias_info.expanded_type.clone();
                    }
                }

                // Check if it's a struct
                for struct_info in self.env.structs.values() {
                    if struct_info.name == sym {
                        return Rc::new(Type {
                            span: Some(annot.span.clone()),
                            file: Some(annot.file.clone()),
                            type_: TypeKind::Constructor {
                                name: sym.0,
                                args: vec![],
                                kind: Kind::Star,
                            },
                        });
                    }
                }

                // Check if it's an enum
                for enum_info in self.env.enums.values() {
                    if enum_info.name == sym {
                        return Rc::new(Type {
                            span: Some(annot.span.clone()),
                            file: Some(annot.file.clone()),
                            type_: TypeKind::Constructor {
                                name: sym.0,
                                args: vec![],
                                kind: Kind::Star,
                            },
                        });
                    }
                }

                // Not found, return fresh type var
                self.fresh_type_var()
                // or should it be an error?
            }

            TypeAnnotKind::Generic {
                name,
                args,
                kind: _,
            } => {
                let name_sym = self.interner.intern(name);
                let arg_types: Vec<_> = args
                    .iter()
                    .map(|a| self.resolve_type_annot_with_params(a, type_param_map))
                    .collect();

                Rc::new(Type {
                    span: Some(annot.span.clone()),
                    file: Some(annot.file.clone()),
                    type_: TypeKind::Constructor {
                        name: name_sym.0,
                        args: arg_types,
                        kind: Kind::Star, // TODO: use actual kind
                    },
                })
            }

            TypeAnnotKind::Function {
                params,
                return_type,
                effects,
            } => {
                let param_types: Vec<_> = params
                    .iter()
                    .map(|p| self.resolve_type_annot_with_params(p, type_param_map))
                    .collect();
                let ret_type = self.resolve_type_annot_with_params(return_type, type_param_map);
                let effect_set = self.convert_effect_annot(effects);

                Rc::new(Type {
                    span: Some(annot.span.clone()),
                    file: Some(annot.file.clone()),
                    type_: TypeKind::Function {
                        params: param_types,
                        return_type: ret_type,
                        effects: effect_set,
                    },
                })
            }

            TypeAnnotKind::Tuple(types) => {
                let elem_types: Vec<_> = types
                    .iter()
                    .map(|t| self.resolve_type_annot_with_params(t, type_param_map))
                    .collect();

                Rc::new(Type {
                    span: Some(annot.span.clone()),
                    file: Some(annot.file.clone()),
                    type_: TypeKind::Tuple(elem_types),
                })
            }

            TypeAnnotKind::Union(types) => {
                let variant_types: Vec<_> = types
                    .iter()
                    .map(|t| self.resolve_type_annot_with_params(t, type_param_map))
                    .collect();

                Rc::new(Type {
                    span: Some(annot.span.clone()),
                    file: Some(annot.file.clone()),
                    type_: TypeKind::Union(variant_types),
                })
            }

            TypeAnnotKind::Variable { name, kind } => {
                // Check if this is a type parameter that should be substituted
                let sym = self.interner.intern(name);
                if let Some(type_var) = type_param_map.get(&sym) {
                    return type_var.clone();
                }

                let var_id = self.id_gen.fresh_type();
                let k = kind
                    .as_ref()
                    .map(|k| self.convert_kind_annot(k))
                    .unwrap_or(Kind::Star);

                Rc::new(Type {
                    span: Some(annot.span.clone()),
                    file: Some(annot.file.clone()),
                    type_: TypeKind::Variable {
                        id: var_id.0,
                        kind: k,
                    },
                })
            }

            TypeAnnotKind::Pointer(inner_annot) => {
                let inner_type = self.resolve_type_annot_with_params(inner_annot, type_param_map);

                Rc::new(Type {
                    span: Some(annot.span.clone()),
                    file: Some(annot.file.clone()),
                    type_: TypeKind::Pointer(inner_type),
                })
            }

            _ => {
                // Forall, Exists, Trait, Constructor - advanced types
                self.fresh_type_var()
            }
        }
    }

    fn check_function(&mut self, func: Function) -> TypedFunction {
        let name_sym = self.interner.intern(&func.name);

        // Find or auto-register function
        let func_id = match self
            .env
            .functions
            .iter()
            .find(|(_, info)| info.name == name_sym)
            .map(|(id, _)| *id)
        {
            Some(id) => id,
            None => {
                self.pre_register_function(&func);
                self.env
                    .functions
                    .iter()
                    .find(|(_, info)| info.name == name_sym)
                    .map(|(id, _)| *id)
                    .expect("Just registered")
            }
        };

        self.env.push_scope();

        // Create type parameters during actual function checking
        let typeparams: Vec<TypedTypeParam> = func
            .type_params
            .iter()
            .map(|tp| {
                let var_id = self.id_gen.fresh_type();
                TypedTypeParam {
                    name: self.interner.intern(&tp.name),
                    var_id,
                    kind: tp
                        .kind
                        .as_ref()
                        .map(|k| self.convert_kind_annot(k))
                        .unwrap_or(Kind::Star),
                    bounds: tp
                        .bounds
                        .iter()
                        .map(|b| self.resolve_type_annot(b))
                        .collect(),
                }
            })
            .collect();

        // Create type parameter mapping for type annotation resolution
        let mut type_param_map = std::collections::HashMap::new();
        for tp in &typeparams {
            type_param_map.insert(
                tp.name,
                Rc::new(Type {
                    span: None,
                    file: None,
                    type_: TypeKind::Variable {
                        id: tp.var_id.0,
                        kind: tp.kind.clone(),
                    },
                }),
            );
        }

        // Create fresh types for checking (don't use pre-registered)
        let param_types: Vec<Rc<Type>> = func
            .args
            .iter()
            .map(|arg| {
                if let Some(type_annot) = &arg.type_ {
                    self.resolve_type_annot_with_params(type_annot, &type_param_map)
                } else {
                    self.fresh_type_var()
                }
            })
            .collect();

        let return_type = if let Some(ret) = &func.return_type {
            self.resolve_type_annot_with_params(ret, &type_param_map)
        } else {
            self.fresh_type_var()
        };

        let args: Vec<TypedFnArg> = func
            .args
            .iter()
            .zip(&param_types)
            .map(|(arg, param_type)| {
                let binding_id = self.id_gen.fresh_binding();
                let arg_sym = self.interner.intern(&arg.name);

                self.env.add_binding(
                    arg_sym,
                    Binding {
                        id: binding_id,
                        name: arg_sym,
                        type_: param_type.clone(),
                        mutable: false,
                    },
                );

                TypedFnArg {
                    span: arg.span.clone(),
                    file: arg.file.clone(),
                    name: arg_sym,
                    binding_id,
                    type_: param_type.clone(),
                }
            })
            .collect();

        // Set the current function's allowed effects for checking the body
        let saved_effects = self.current_function_effects.clone();
        self.current_function_effects = self.convert_effect_annot(&func.effects);

        let body = func.body.map(|b| self.check_expr(&b));

        // Restore the previous function's effects
        self.current_function_effects = saved_effects;
        self.env.pop_scope();

        // Now unify the return type with the body type (this happens inside the function type)
        if let (Some(body_typed), Some(_)) = (&body, &func.return_type) {
            // Only unify if return type was explicitly specified
            self.unify(&body_typed.type_, &return_type, &func.span);
        } else if let Some(body_typed) = &body {
            // If no return type was specified, the body determines the return type
            self.unify(&body_typed.type_, &return_type, &func.span);
        }

        // Now unify the return type with the body type (this happens inside the function type)
        if let (Some(body_typed), Some(_)) = (&body, &func.return_type) {
            // Only unify if return type was explicitly specified
            self.unify(&body_typed.type_, &return_type, &func.span);
        } else if let Some(body_typed) = &body {
            // If no return type was specified, the body determines the return type
            self.unify(&body_typed.type_, &return_type, &func.span);
        }

        // Create the function type (after unification)
        let base_func_type = Rc::new(Type {
            span: Some(func.span.clone()),
            file: Some(func.file.clone()),
            type_: TypeKind::Function {
                params: param_types.clone(),
                return_type: return_type.clone(),
                effects: self.convert_effect_annot(&func.effects),
            },
        });

        // For generic functions, wrap in Forall; otherwise just use the base type
        let func_type = if !typeparams.is_empty() {
            // Create polymorphic type for explicitly generic functions
            // Convert type parameter bounds to trait constraints
            let mut constraints = Vec::new();
            for tp in &typeparams {
                for bound in &tp.bounds {
                    // Find the trait that this bound refers to
                    // The bound should be a trait type, so we need to extract the trait id
                    if let TypeKind::Constructor { name, .. } = &bound.type_ {
                        // Create a trait constraint: this type parameter implements this trait
                        constraints.push(Constraint::Trait(tp.var_id.0, *name));
                    }
                }
            }

            Rc::new(Type {
                span: Some(func.span.clone()),
                file: Some(func.file.clone()),
                type_: TypeKind::Forall {
                    vars: typeparams
                        .iter()
                        .map(|tp| (tp.var_id.0, tp.kind.clone()))
                        .collect(),
                    constraints, // Now with proper trait constraints
                    body: base_func_type,
                },
            })
        } else {
            // For implicit polymorphism, we need to generalize over free type variables
            // At this point, param_types and return_type have been unified internally
            // so we now generalize the function type to capture polymorphism
            let env_vars = self.env_free_vars();
            self.generalize(&base_func_type, &env_vars)
        };

        // Update environment with the final function type (polymorphic or not)
        if let Some(info) = self.env.functions.get_mut(&func_id) {
            info.type_ = func_type.clone();
        }

        let binding_id = BindingId(func_id.0);
        if let Some(binding) = self.env.bindings.get_mut(&binding_id) {
            binding.type_ = func_type.clone();
        }

        TypedFunction {
            span: func.span,
            file: func.file,
            vis: func.vis,
            name: name_sym,
            function_id: func_id,
            type_params: typeparams,
            args,
            return_type,
            where_constraints: vec![],
            effects: self.convert_effect_annot(&func.effects),
            function_type: func_type,
            body,
        }
    }

    fn check_struct(&mut self, struct_def: Struct) -> TypedStruct {
        let struct_id = self.id_gen.fresh_struct();
        let struct_sym = self.interner.intern(&struct_def.name);

        // Add type parameters
        let type_params: Vec<_> = struct_def
            .type_params
            .iter()
            .map(|tp| {
                let var_id = self.id_gen.fresh_type();
                TypedTypeParam {
                    name: self.interner.intern(&tp.name),
                    var_id,
                    kind: tp
                        .kind
                        .as_ref()
                        .map(|k| self.convert_kind_annot(k))
                        .unwrap_or(Kind::Star),
                    bounds: tp
                        .bounds
                        .iter()
                        .map(|b| self.resolve_type_annot(b))
                        .collect(),
                }
            })
            .collect();

        // Check fields
        let mut field_map = HashMap::new();
        let typed_fields: Vec<_> = struct_def
            .fields
            .iter()
            .map(|(field, vis)| {
                let field_id = self.id_gen.fresh_field();
                let field_sym = self.interner.intern(&field.name);
                let field_type = field
                    .type_
                    .as_ref()
                    .map(|t| self.resolve_type_annot(t))
                    .unwrap_or_else(|| self.fresh_type_var());

                field_map.insert(field_sym, (field_id, field_type.clone(), *vis));

                let typed_arg = TypedFnArg {
                    span: field.span.clone(),
                    file: field.file.clone(),
                    name: field_sym,
                    binding_id: self.id_gen.fresh_binding(),
                    type_: field_type,
                };

                (typed_arg, *vis, field_id)
            })
            .collect();

        // Register struct in environment
        let struct_info = StructInfo {
            id: struct_id,
            name: struct_sym,
            type_params: type_params.iter().map(|tp| tp.var_id).collect(),
            fields: field_map,
        };
        self.env.add_struct(struct_info);

        // Check methods
        let typed_methods: Vec<_> = struct_def
            .methods
            .iter()
            .map(|m| self.check_function(m.clone()))
            .collect();

        // Build struct type constructor
        let struct_type = if type_params.is_empty() {
            Rc::new(Type {
                span: Some(struct_def.span.clone()),
                file: Some(struct_def.file.clone()),
                type_: TypeKind::Constructor {
                    name: struct_sym.0,
                    args: vec![],
                    kind: Kind::Star,
                },
            })
        } else {
            let type_args: Vec<_> = type_params
                .iter()
                .map(|tp| {
                    Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Variable {
                            id: tp.var_id.0,
                            kind: tp.kind.clone(),
                        },
                    })
                })
                .collect();

            Rc::new(Type {
                span: Some(struct_def.span.clone()),
                file: Some(struct_def.file.clone()),
                type_: TypeKind::Constructor {
                    name: struct_sym.0,
                    args: type_args,
                    kind: Kind::Star,
                },
            })
        };

        TypedStruct {
            span: struct_def.span,
            file: struct_def.file,
            name: struct_sym,
            struct_id,
            vis: struct_def.vis,
            type_params,
            fields: typed_fields,
            methods: typed_methods,
            struct_type,
        }
    }

    fn check_enum(&mut self, enum_def: Enum) -> TypedEnum {
        let enum_id = self.id_gen.fresh_enum();
        let enum_sym = self.interner.intern(&enum_def.name);

        // Add type parameters
        let type_params: Vec<_> = enum_def
            .type_params
            .iter()
            .map(|tp| {
                let var_id = self.id_gen.fresh_type();
                TypedTypeParam {
                    name: self.interner.intern(&tp.name),
                    var_id,
                    kind: tp
                        .kind
                        .as_ref()
                        .map(|k| self.convert_kind_annot(k))
                        .unwrap_or(Kind::Star),
                    bounds: tp
                        .bounds
                        .iter()
                        .map(|b| self.resolve_type_annot(b))
                        .collect(),
                }
            })
            .collect();

        // Check variants
        let mut variant_map = HashMap::new();
        let typed_variants: Vec<_> = enum_def
            .variants
            .iter()
            .map(|variant| {
                let variant_id = self.id_gen.fresh_variant();
                let variant_sym = self.interner.intern(&variant.name);

                let variant_types: Vec<_> = variant
                    .types
                    .iter()
                    .map(|t| self.resolve_type_annot(t))
                    .collect();

                let typed_constraints: Vec<_> = variant
                    .constraints
                    .iter()
                    .map(|c| TypedConstraint {
                        span: c.span.clone(),
                        constraint: Constraint::Equal(
                            self.resolve_type_annot(&c.left),
                            self.resolve_type_annot(&c.right),
                        ),
                    })
                    .collect();

                // Constructor type: (T1, T2, ...) -> EnumType
                let enum_type = Rc::new(Type {
                    span: Some(enum_def.span.clone()),
                    file: Some(enum_def.file.clone()),
                    type_: TypeKind::Constructor {
                        name: enum_sym.0,
                        args: type_params
                            .iter()
                            .map(|tp| {
                                Rc::new(Type {
                                    span: None,
                                    file: None,
                                    type_: TypeKind::Variable {
                                        id: tp.var_id.0,
                                        kind: tp.kind.clone(),
                                    },
                                })
                            })
                            .collect(),
                        kind: Kind::Star,
                    },
                });

                let constructor_type = if variant_types.is_empty() {
                    enum_type.clone()
                } else {
                    Rc::new(Type {
                        span: Some(variant.span.clone()),
                        file: Some(variant.file.clone()),
                        type_: TypeKind::Function {
                            params: variant_types.clone(),
                            return_type: enum_type,
                            effects: EffectSet::pure(),
                        },
                    })
                };

                variant_map.insert(variant_sym, (variant_id, variant_types.clone()));

                TypedEnumVariant {
                    span: variant.span.clone(),
                    file: variant.file.clone(),
                    name: variant_sym,
                    variant_id,
                    types: variant_types,
                    constraints: typed_constraints,
                    constructor_type,
                }
            })
            .collect();

        // Register enum in environment
        let enum_info = EnumInfo {
            id: enum_id,
            name: enum_sym,
            type_params: type_params.iter().map(|tp| tp.var_id).collect(),
            variants: variant_map,
        };
        self.env.add_enum(enum_info);

        // Check methods
        let typed_methods: Vec<_> = enum_def
            .methods
            .iter()
            .map(|m| self.check_function(m.clone()))
            .collect();

        // Build enum type
        let enum_type = Rc::new(Type {
            span: Some(enum_def.span.clone()),
            file: Some(enum_def.file.clone()),
            type_: TypeKind::Constructor {
                name: enum_sym.0,
                args: type_params
                    .iter()
                    .map(|tp| {
                        Rc::new(Type {
                            span: None,
                            file: None,
                            type_: TypeKind::Variable {
                                id: tp.var_id.0,
                                kind: tp.kind.clone(),
                            },
                        })
                    })
                    .collect(),
                kind: Kind::Star,
            },
        });

        TypedEnum {
            span: enum_def.span,
            file: enum_def.file,
            name: enum_sym,
            enum_id,
            vis: enum_def.vis,
            type_params,
            variants: typed_variants,
            methods: typed_methods,
            enum_type,
        }
    }

    fn check_type_alias(&mut self, alias: TypeAlias) -> TypedTypeAlias {
        let alias_id = self.id_gen.fresh_type_alias();
        let alias_sym = self.interner.intern(&alias.name);

        let type_params: Vec<_> = alias
            .type_params
            .iter()
            .map(|tp| {
                let var_id = self.id_gen.fresh_type();
                TypedTypeParam {
                    name: self.interner.intern(&tp.name),
                    var_id,
                    kind: tp
                        .kind
                        .as_ref()
                        .map(|k| self.convert_kind_annot(k))
                        .unwrap_or(Kind::Star),
                    bounds: tp
                        .bounds
                        .iter()
                        .map(|b| self.resolve_type_annot(b))
                        .collect(),
                }
            })
            .collect();

        let target_type = self.resolve_type_annot(&alias.target_type);
        let where_constraints: Vec<_> = alias
            .where_constraints
            .iter()
            .map(|c| TypedConstraint {
                span: c.span.clone(),
                constraint: Constraint::Equal(
                    self.resolve_type_annot(&c.left),
                    self.resolve_type_annot(&c.right),
                ),
            })
            .collect();

        TypedTypeAlias {
            span: alias.span,
            file: alias.file,
            name: alias_sym,
            alias_id,
            type_params,
            target_type: target_type.clone(),
            where_constraints,
            expanded_type: target_type,
        }
    }

    fn _resolve_method_call(
        &mut self,
        receiver: &TypedExpr,
        method_name: Symbol,
        // span: &Range<usize>,
    ) -> Option<FunctionId> {
        let receiver_ty = &receiver.type_;

        // First check if it's a direct method on the type
        if let TypeKind::Constructor { name, .. } = &receiver_ty.type_ {
            let type_id = TypeId(*name);

            // Check struct methods
            if let Some(_struct_info) = self.env.structs.get(&StructId(type_id.0)) {
                // TODO: lookup method in struct
            }

            // Check enum methods
            if let Some(_enum_info) = self.env.enums.get(&EnumId(type_id.0)) {
                // TODO: lookup method in enum
            }
        }

        // Check trait methods
        for (trait_id, trait_info) in &self.env.traits {
            if let Some(&method_id) = trait_info.methods.get(&method_name) {
                // Check if receiver implements this trait
                if self
                    .trait_resolver
                    .find_impl(*trait_id, receiver_ty)
                    .is_some()
                {
                    return Some(method_id);
                }
            }
        }

        None
    }

    // fn _check_method_call(
    //     &mut self,
    //     receiver: &Expr,
    //     method: &str,
    //     args: &[Expr],
    //     span: &Range<usize>,
    // ) -> TypedExpr {
    //     let receiver_typed = self.check_expr(receiver);
    //     let method_sym = self.interner.intern(method);

    //     if let Some(method_id) = self.resolve_method_call(&receiver_typed, method_sym) {
    //         // Get method signature
    //         if let Some(func_info) = self.env.functions.get(&method_id) {
    //             let func_info = func_info.clone();
    //             let args_typed: Vec<_> = args.iter().map(|a| self.check_expr(a)).collect();

    //             // Check argument types
    //             // TODO: full method call type checking

    //             let result_type = func_info.type_.clone();

    //             return TypedExpr {
    //                 span: span.clone(),
    //                 file: receiver.file.clone(),
    //                 expr: TypedExprKind::Call {
    //                     function: Box::new(receiver_typed),
    //                     args: args_typed,
    //                     type_args: vec![],
    //                 },
    //                 type_: result_type,
    //             };
    //         }
    //     }

    //     self.error(
    //         &Expr {
    //             span: span.clone(),
    //             file: receiver.file.clone(),
    //             expr: ExprKind::Error,
    //         },
    //         TypeErrorKind::UnboundVariable { name: method_sym },
    //     );

    //     self.error_expr(&Expr {
    //         span: span.clone(),
    //         file: receiver.file.clone(),
    //         expr: ExprKind::Error,
    //     })
    // }

    // Check impl blocks
    fn check_impl(&mut self, impl_block: Impl) -> TypedImpl {
        let type_sym = self.interner.intern(&impl_block.type_name);
        let type_id = self.id_gen.fresh_type();

        let type_params: Vec<_> = impl_block
            .type_params
            .iter()
            .map(|tp| {
                let var_id = self.id_gen.fresh_type();
                TypedTypeParam {
                    name: self.interner.intern(&tp.name),
                    var_id,
                    kind: tp
                        .kind
                        .as_ref()
                        .map(|k| self.convert_kind_annot(k))
                        .unwrap_or(Kind::Star),
                    bounds: tp
                        .bounds
                        .iter()
                        .map(|b| self.resolve_type_annot(b))
                        .collect(),
                }
            })
            .collect();

        let implementing_type = Rc::new(Type {
            span: Some(impl_block.span.clone()),
            file: Some(impl_block.file.clone()),
            type_: TypeKind::Constructor {
                name: type_sym.0,
                args: type_params
                    .iter()
                    .map(|tp| {
                        Rc::new(Type {
                            span: None,
                            file: None,
                            type_: TypeKind::Variable {
                                id: tp.var_id.0,
                                kind: tp.kind.clone(),
                            },
                        })
                    })
                    .collect(),
                kind: Kind::Star,
            },
        });

        let trait_info = impl_block.trait_.as_ref().map(|t| {
            let trait_sym = self.interner.intern(t);
            let trait_id = self
                .env
                .traits
                .iter()
                .find(|(_, info)| info.name == trait_sym)
                .map(|(id, _)| *id)
                .unwrap_or(TraitId(0));
            (trait_sym, trait_id)
        });

        // Check all methods match trait signature if implementing a trait
        let typed_methods: Vec<_> = impl_block
            .methods
            .iter()
            .map(|m| {
                if let Some((_, trait_id)) = trait_info {
                    // Verify method exists in trait
                    if let Some(trait_info) = self.env.traits.get(&trait_id) {
                        let method_sym = self.interner.intern(&m.name);
                        if !trait_info.methods.contains_key(&method_sym) {
                            self.diagnostics.add_type_error(TypeError {
                                span: m.span.clone(),
                                file: m.file.clone(),
                                kind: TypeErrorKind::UnboundVariable { name: method_sym },
                            });
                        }
                    }
                }
                self.check_function(m.clone())
            })
            .collect();

        let where_constraints: Vec<_> = impl_block
            .where_constraints
            .iter()
            .map(|c| TypedConstraint {
                span: c.span.clone(),
                constraint: Constraint::Equal(
                    self.resolve_type_annot(&c.left),
                    self.resolve_type_annot(&c.right),
                ),
            })
            .collect();

        let typed_impl = TypedImpl {
            span: impl_block.span,
            file: impl_block.file,
            type_name: type_sym,
            type_id,
            type_params,
            implementing_type,
            trait_: trait_info,
            trait_type: None,
            methods: typed_methods,
            where_constraints,
        };

        // Register impl
        self.trait_resolver.register_impl(typed_impl.clone());

        typed_impl
    }

    fn check_trait(&mut self, trait_def: TraitDef) -> TypedTraitDef {
        let trait_id = self.id_gen.fresh_trait();
        let trait_sym = self.interner.intern(&trait_def.name);

        // Add type parameters
        let type_params: Vec<_> = trait_def
            .type_params
            .iter()
            .map(|tp| {
                let var_id = self.id_gen.fresh_type();
                TypedTypeParam {
                    name: self.interner.intern(&tp.name),
                    var_id,
                    kind: tp
                        .kind
                        .as_ref()
                        .map(|k| self.convert_kind_annot(k))
                        .unwrap_or(Kind::Star),
                    bounds: tp
                        .bounds
                        .iter()
                        .map(|b| self.resolve_type_annot(b))
                        .collect(),
                }
            })
            .collect();

        // Check super traits
        let super_traits: Vec<_> = trait_def
            .super_traits
            .iter()
            .map(|t| self.resolve_type_annot(t))
            .collect();

        // Check methods (only signatures, no bodies)
        let typed_methods: Vec<_> = trait_def
            .methods
            .iter()
            .map(|m| self.check_function(m.clone()))
            .collect();

        // Check associated types
        let associated_types: Vec<_> = trait_def
            .associated_types
            .iter()
            .map(|at| {
                let type_id = self.id_gen.fresh_type();
                let name_sym = self.interner.intern(&at.name);
                TypedAssociatedType {
                    name: name_sym,
                    type_id,
                    bounds: at
                        .bounds
                        .iter()
                        .map(|b| self.resolve_type_annot(b))
                        .collect(),
                }
            })
            .collect();

        // Register trait in environment
        let trait_info = TraitInfo {
            id: trait_id,
            name: trait_sym,
            methods: typed_methods
                .iter()
                .map(|m| (m.name, m.function_id))
                .collect(),
        };
        self.env.add_trait(trait_info);

        TypedTraitDef {
            span: trait_def.span,
            name: trait_sym,
            trait_id,
            type_params,
            super_traits,
            methods: typed_methods,
            associated_types,
        }
    }

    fn check_extern_function(&mut self, extern_func: ExternFunction) -> TypedExternFunction {
        let extern_func_id = self.id_gen.fresh_function();
        let name_sym = self.interner.intern(&extern_func.name);

        // Create type parameter mapping for type annotation resolution
        let mut type_param_map = std::collections::HashMap::new();
        let typeparams: Vec<TypedTypeParam> = extern_func
            .type_params
            .iter()
            .map(|tp| {
                let var_id = self.id_gen.fresh_type();
                let name = self.interner.intern(&tp.name);
                type_param_map.insert(
                    name,
                    Rc::new(Type {
                        span: None,
                        file: None,
                        type_: TypeKind::Variable {
                            id: var_id.0,
                            kind: tp
                                .kind
                                .as_ref()
                                .map(|k| self.convert_kind_annot(k))
                                .unwrap_or(Kind::Star),
                        },
                    }),
                );
                TypedTypeParam {
                    name,
                    var_id,
                    kind: tp
                        .kind
                        .as_ref()
                        .map(|k| self.convert_kind_annot(k))
                        .unwrap_or(Kind::Star),
                    bounds: tp
                        .bounds
                        .iter()
                        .map(|b| self.resolve_type_annot(b))
                        .collect(),
                }
            })
            .collect();

        // Resolve parameter types
        let param_types: Vec<Rc<Type>> = extern_func
            .args
            .iter()
            .map(|arg| {
                if let Some(type_annot) = &arg.type_ {
                    self.resolve_type_annot_with_params(type_annot, &type_param_map)
                } else {
                    self.fresh_type_var()
                }
            })
            .collect();

        // Resolve return type
        let return_type = if let Some(ret) = &extern_func.return_type {
            self.resolve_type_annot_with_params(ret, &type_param_map)
        } else {
            self.fresh_type_var()
        };

        // Convert effects
        let effects = self.convert_effect_annot(&extern_func.effects);

        // Build function type
        let base_func_type = Rc::new(Type {
            span: Some(extern_func.span.clone()),
            file: Some(extern_func.file.clone()),
            type_: TypeKind::Function {
                params: param_types.clone(),
                return_type: return_type.clone(),
                effects: effects.clone(),
            },
        });

        // For generic extern functions, wrap in Forall; otherwise just use the base type
        let func_type = if !extern_func.type_params.is_empty() {
            // Convert type parameter bounds to trait constraints
            let mut constraints = Vec::new();
            for tp in typeparams.iter() {
                for bound in &tp.bounds {
                    // Find the trait that this bound refers to
                    // The bound should be a trait type, so we need to extract the trait id
                    if let TypeKind::Constructor { name, .. } = &bound.type_ {
                        // Create a trait constraint: this type parameter implements this trait
                        constraints.push(Constraint::Trait(tp.var_id.0, *name));
                    }
                }
            }

            Rc::new(Type {
                span: Some(extern_func.span.clone()),
                file: Some(extern_func.file.clone()),
                type_: TypeKind::Forall {
                    vars: typeparams
                        .iter()
                        .map(|tp| (tp.var_id.0, tp.kind.clone()))
                        .collect(),
                    constraints, // Now with proper trait constraints
                    body: base_func_type,
                },
            })
        } else {
            base_func_type
        };

        // Create typed args
        let args: Vec<TypedFnArg> = extern_func
            .args
            .iter()
            .zip(&param_types)
            .map(|(arg, param_type)| {
                let binding_id = self.id_gen.fresh_binding();
                let arg_sym = self.interner.intern(&arg.name);

                TypedFnArg {
                    span: arg.span.clone(),
                    file: arg.file.clone(),
                    name: arg_sym,
                    binding_id,
                    type_: param_type.clone(),
                }
            })
            .collect();

        // Register the extern function in the environment
        let extern_func_info = FunctionInfo {
            id: extern_func_id,
            name: name_sym,
            type_: func_type.clone(),
        };
        self.env.functions.insert(extern_func_id, extern_func_info);

        // Add binding to environment
        self.env.add_binding(
            name_sym,
            Binding {
                id: BindingId(extern_func_id.0),
                name: name_sym,
                type_: func_type.clone(),
                mutable: false,
            },
        );

        TypedExternFunction {
            span: extern_func.span,
            file: extern_func.file,
            vis: extern_func.vis,
            name: name_sym,
            function_id: extern_func_id,
            type_params: typeparams,
            args,
            return_type,
            where_constraints: extern_func
                .where_constraints
                .iter()
                .map(|c| TypedConstraint {
                    span: c.span.clone(),
                    constraint: Constraint::Equal(
                        self.resolve_type_annot(&c.left),
                        self.resolve_type_annot(&c.right),
                    ),
                })
                .collect(),
            effects,
            function_type: func_type,
            library: extern_func.library,
            symbol_name: extern_func.symbol_name,
        }
    }

    fn check_effect_def(&mut self, effect: EffectDef) -> TypedEffectDef {
        let effect_id = self.id_gen.fresh_effect();
        let effect_sym = self.interner.intern(&effect.name);

        // Add type parameters
        let type_params: Vec<_> = effect
            .type_params
            .iter()
            .map(|tp| {
                let var_id = self.id_gen.fresh_type();
                TypedTypeParam {
                    name: self.interner.intern(&tp.name),
                    var_id,
                    kind: tp
                        .kind
                        .as_ref()
                        .map(|k| self.convert_kind_annot(k))
                        .unwrap_or(Kind::Star),
                    bounds: tp
                        .bounds
                        .iter()
                        .map(|b| self.resolve_type_annot(b))
                        .collect(),
                }
            })
            .collect();

        // Check operations
        let mut operation_map = HashMap::new();
        let typed_operations: Vec<_> = effect
            .operations
            .iter()
            .map(|op| {
                let op_sym = self.interner.intern(&op.name);
                let op_id = self.id_gen.fresh_type().0; // Reuse TypeId counter

                let param_types: Vec<_> = op
                    .params
                    .iter()
                    .map(|p| self.resolve_type_annot(p))
                    .collect();
                let return_type = self.resolve_type_annot(&op.return_type);

                let operation_type = Rc::new(Type {
                    span: Some(op.span.clone()),
                    file: None,
                    type_: TypeKind::Function {
                        params: param_types.clone(),
                        return_type: return_type.clone(),
                        effects: EffectSet::pure(),
                    },
                });

                operation_map.insert(op_sym, operation_type.clone());

                TypedEffectOperation {
                    span: op.span.clone(),
                    name: op_sym,
                    operation_id: op_id,
                    params: param_types,
                    return_type,
                    operation_type,
                }
            })
            .collect();

        let where_constraints: Vec<_> = effect
            .where_constraints
            .iter()
            .map(|c| TypedConstraint {
                span: c.span.clone(),
                constraint: Constraint::Equal(
                    self.resolve_type_annot(&c.left),
                    self.resolve_type_annot(&c.right),
                ),
            })
            .collect();

        // Register effect in environment
        let effect_info = EffectInfo {
            id: effect_id,
            name: effect_sym,
            operations: operation_map,
            type_params: type_params.iter().map(|tp| (tp.name, tp.var_id)).collect(),
        };
        self.env.add_effect(effect_info);

        TypedEffectDef {
            span: effect.span,
            file: effect.file,
            vis: effect.vis,
            name: effect_sym,
            effect_id,
            type_params,
            operations: typed_operations,
            where_constraints,
        }
    }

    fn check_macro_def(&mut self, macro_def: MacroDef) -> TypedMacroDef {
        let macro_id = self.id_gen.fresh_macro();
        let name_sym = self.interner.intern(&macro_def.name);

        let typed_rules: Vec<_> = macro_def
            .rules
            .iter()
            .map(|rule| TypedMacroRule {
                span: rule.span.clone(),
                pattern: self.convert_macro_tokens(&rule.pattern),
                body: self.convert_macro_tokens(&rule.body),
                scope_id: self.id_gen.fresh_scope(),
            })
            .collect();

        TypedMacroDef {
            span: macro_def.span,
            file: macro_def.file,
            name: name_sym,
            macro_id,
            rules: typed_rules,
            hygiene: macro_def.hygiene,
        }
    }

    fn convert_macro_tokens(&mut self, tokens: &[MacroToken]) -> Vec<TypedMacroToken> {
        tokens
            .iter()
            .map(|tok| match tok {
                MacroToken::Ident(s) => TypedMacroToken::Ident(self.interner.intern(s)),
                MacroToken::Literal(lit) => TypedMacroToken::Literal(lit.clone()),
                MacroToken::Punct(s) => TypedMacroToken::Punct(s.clone()),
                MacroToken::Group { delimiter, tokens } => TypedMacroToken::Group {
                    delimiter: delimiter.clone(),
                    tokens: self.convert_macro_tokens(tokens),
                    scope_id: self.id_gen.fresh_scope(),
                },
                MacroToken::MetaVar { name, kind } => TypedMacroToken::MetaVar {
                    name: self.interner.intern(name),
                    kind: self.convert_macro_var_kind(kind),
                    binding_id: self.id_gen.fresh_binding(),
                },
                MacroToken::Repeat {
                    tokens,
                    separator,
                    kind,
                } => TypedMacroToken::Repeat {
                    tokens: self.convert_macro_tokens(tokens),
                    separator: separator.clone(),
                    kind: kind.clone(),
                },
            })
            .collect()
    }

    fn convert_macro_var_kind(&self, kind: &str) -> MacroVarKind {
        match kind {
            "expr" => MacroVarKind::Expr,
            "ty" => MacroVarKind::Type,
            "pat" => MacroVarKind::Pattern,
            "stmt" => MacroVarKind::Statement,
            "item" => MacroVarKind::Item,
            "block" => MacroVarKind::Block,
            "ident" => MacroVarKind::Ident,
            "path" => MacroVarKind::Path,
            "literal" => MacroVarKind::Literal,
            "meta" => MacroVarKind::Meta,
            _ => MacroVarKind::Expr, // default
        }
    }

    fn occurs_check(&mut self, id: usize, ty: &Rc<Type>) -> bool {
        let ty = self.substitution.apply(ty);
        self.occurs_check_inner(id, &ty)
    }

    #[allow(clippy::only_used_in_recursion)]
    fn occurs_check_inner(&self, id: usize, ty: &Rc<Type>) -> bool {
        match &ty.type_ {
            TypeKind::Variable { id: vid, .. } => *vid == id,
            TypeKind::Constructor { args, .. } => {
                args.iter().any(|a| self.occurs_check_inner(id, a))
            }
            TypeKind::Function {
                params,
                return_type,
                ..
            } => {
                params.iter().any(|p| self.occurs_check_inner(id, p))
                    || self.occurs_check_inner(id, return_type)
            }
            TypeKind::Tuple(types) | TypeKind::Union(types) => {
                types.iter().any(|t| self.occurs_check_inner(id, t))
            }
            TypeKind::Pointer(inner) => self.occurs_check_inner(id, inner),
            _ => false,
        }
    }

    fn unify(&mut self, t1: &Rc<Type>, t2: &Rc<Type>, span: &Range<usize>) {
        let t1 = self.substitution.apply(t1);
        let t2 = self.substitution.apply(t2);

        // Skip if both are errors
        if matches!(t1.type_, TypeKind::Error) || matches!(t2.type_, TypeKind::Error) {
            return;
        }

        match (&t1.type_, &t2.type_) {
            (TypeKind::Variable { id: id1, .. }, TypeKind::Variable { id: id2, .. })
                if id1 == id2 => {}

            (TypeKind::Variable { id, .. }, _) => {
                if self.occurs_check(*id, &t2) {
                    self.diagnostics.add_type_error(TypeError {
                        span: span.clone(),
                        file: String::new(),
                        kind: TypeErrorKind::OccursCheck {
                            var: TypeId(*id),
                            in_type: t2,
                        },
                    });
                } else {
                    self.substitution.bind(TypeId(*id), t2);
                }
            }

            (_, TypeKind::Variable { id, .. }) => {
                if self.occurs_check(*id, &t1) {
                    self.diagnostics.add_type_error(TypeError {
                        span: span.clone(),
                        file: String::new(),
                        kind: TypeErrorKind::OccursCheck {
                            var: TypeId(*id),
                            in_type: t1,
                        },
                    });
                } else {
                    self.substitution.bind(TypeId(*id), t1);
                }
            }

            (
                TypeKind::Constructor {
                    name: n1, args: a1, ..
                },
                TypeKind::Constructor {
                    name: n2, args: a2, ..
                },
            ) if n1 == n2 && a1.len() == a2.len() => {
                for (arg1, arg2) in a1.iter().zip(a2.iter()) {
                    self.unify(arg1, arg2, span);
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
                    self.unify(param1, param2, span);
                }
                self.unify(r1, r2, span);
            }

            (TypeKind::Tuple(t1), TypeKind::Tuple(t2)) if t1.len() == t2.len() => {
                for (ty1, ty2) in t1.iter().zip(t2.iter()) {
                    self.unify(ty1, ty2, span);
                }
            }

            (TypeKind::Pointer(inner1), TypeKind::Pointer(inner2)) => {
                self.unify(inner1, inner2, span);
            }

            _ => {
                println!("mismatch during unification");
                self.diagnostics.add_type_error(TypeError {
                    span: span.clone(),
                    file: String::new(),
                    kind: TypeErrorKind::TypeMismatch {
                        expected: t1,
                        found: t2,
                        context: "unification".to_string(),
                    },
                });
            }
        }
    }

    fn int_type(&self) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: 0, // intern "Int"
                args: vec![],
                kind: Kind::Star,
            },
        })
    }

    fn bool_type(&self) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: 1, // intern "Bool"
                args: vec![],
                kind: Kind::Star,
            },
        })
    }

    fn unit_type(&self) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: 2, // intern "Unit"
                args: vec![],
                kind: Kind::Star,
            },
        })
    }

    fn function_type(&self, params: Vec<Rc<Type>>, ret: Rc<Type>) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Function {
                params,
                return_type: ret,
                effects: EffectSet {
                    effects: vec![],
                    rest: None,
                },
            },
        })
    }

    fn error_expr(&self, expr: &Expr) -> TypedExpr {
        TypedExpr {
            span: expr.span.clone(),
            file: expr.file.clone(),
            expr: TypedExprKind::Error,
            type_: Rc::new(Type {
                span: None,
                file: None,
                type_: TypeKind::Error,
            }),
        }
    }

    fn error(&mut self, expr: &Expr, kind: TypeErrorKind) {
        self.diagnostics.add_type_error(TypeError {
            span: expr.span.clone(),
            file: expr.file.clone(),
            kind,
        });
    }

    fn float_type(&mut self) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: self.interner.intern("Float").0,
                args: vec![],
                kind: Kind::Star,
            },
        })
    }

    fn string_type(&mut self) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: self.interner.intern("String").0,
                args: vec![],
                kind: Kind::Star,
            },
        })
    }

    fn array_type(&mut self, elem: Rc<Type>) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: self.interner.intern("Array").0,
                args: vec![elem],
                kind: Kind::Star,
            },
        })
    }

    fn option_type(&mut self, inner: Rc<Type>) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: self.interner.intern("Option").0,
                args: vec![inner],
                kind: Kind::Star,
            },
        })
    }

    fn error_type(&self) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Error,
        })
    }

    fn map_type(&mut self, key: Rc<Type>, value: Rc<Type>) -> Rc<Type> {
        Rc::new(Type {
            span: None,
            file: None,
            type_: TypeKind::Constructor {
                name: self.interner.intern("Map").0,
                args: vec![key, value],
                kind: Kind::Star,
            },
        })
    }

    fn apply_substitution_to_node(&self, node: TypedASTNode) -> TypedASTNode {
        TypedASTNode {
            span: node.span,
            file: node.file,
            node: self.apply_substitution_to_node_kind(node.node),
            attributes: node.attributes,
        }
    }

    fn apply_substitution_to_node_kind(&self, node_kind: TypedASTNodeKind) -> TypedASTNodeKind {
        match node_kind {
            TypedASTNodeKind::Function(func) => {
                TypedASTNodeKind::Function(Box::new(self.apply_substitution_to_function(*func)))
            }
            TypedASTNodeKind::Struct(s) => {
                TypedASTNodeKind::Struct(self.apply_substitution_to_struct(s))
            }
            TypedASTNodeKind::Enum(e) => TypedASTNodeKind::Enum(self.apply_substitution_to_enum(e)),
            TypedASTNodeKind::Impl(impl_) => {
                TypedASTNodeKind::Impl(self.apply_substitution_to_impl(impl_))
            }
            TypedASTNodeKind::Trait(t) => {
                TypedASTNodeKind::Trait(self.apply_substitution_to_trait(t))
            }
            TypedASTNodeKind::EffectDef(e) => {
                TypedASTNodeKind::EffectDef(self.apply_substitution_to_effect_def(e))
            }
            TypedASTNodeKind::TypeAlias(a) => {
                TypedASTNodeKind::TypeAlias(self.apply_substitution_to_type_alias(a))
            }
            TypedASTNodeKind::MacroDef(m) => {
                TypedASTNodeKind::MacroDef(self.apply_substitution_to_macro_def(m))
            }
            TypedASTNodeKind::ExternFunction(extern_func) => TypedASTNodeKind::ExternFunction(
                Box::new(self.apply_substitution_to_extern_function(*extern_func)),
            ),
            TypedASTNodeKind::Expr(expr) => {
                TypedASTNodeKind::Expr(self.apply_substitution_to_expr(expr))
            }
            TypedASTNodeKind::Error => TypedASTNodeKind::Error,
        }
    }

    fn apply_substitution_to_function(&self, func: TypedFunction) -> TypedFunction {
        TypedFunction {
            span: func.span,
            file: func.file,
            vis: func.vis,
            name: func.name,
            function_id: func.function_id,
            type_params: func.type_params,
            args: func
                .args
                .into_iter()
                .map(|arg| TypedFnArg {
                    span: arg.span,
                    file: arg.file,
                    name: arg.name,
                    binding_id: arg.binding_id,
                    type_: self.substitution.apply(&arg.type_),
                })
                .collect(),
            return_type: self.substitution.apply(&func.return_type),
            where_constraints: func.where_constraints,
            effects: func.effects,
            function_type: self.substitution.apply(&func.function_type),
            body: func.body.map(|b| self.apply_substitution_to_expr(b)),
        }
    }

    fn apply_substitution_to_extern_function(
        &self,
        extern_func: TypedExternFunction,
    ) -> TypedExternFunction {
        TypedExternFunction {
            span: extern_func.span,
            file: extern_func.file,
            vis: extern_func.vis,
            name: extern_func.name,
            function_id: extern_func.function_id,
            type_params: extern_func.type_params,
            args: extern_func
                .args
                .into_iter()
                .map(|arg| TypedFnArg {
                    span: arg.span,
                    file: arg.file,
                    name: arg.name,
                    binding_id: arg.binding_id,
                    type_: self.substitution.apply(&arg.type_),
                })
                .collect(),
            return_type: self.substitution.apply(&extern_func.return_type),
            where_constraints: extern_func.where_constraints,
            effects: extern_func.effects,
            function_type: self.substitution.apply(&extern_func.function_type),
            library: extern_func.library,
            symbol_name: extern_func.symbol_name,
        }
    }

    fn apply_substitution_to_struct(&self, s: TypedStruct) -> TypedStruct {
        TypedStruct {
            span: s.span,
            file: s.file,
            name: s.name,
            struct_id: s.struct_id,
            vis: s.vis,
            type_params: s.type_params,
            fields: s
                .fields
                .into_iter()
                .map(|(arg, vis, field_id)| {
                    (
                        TypedFnArg {
                            span: arg.span,
                            file: arg.file,
                            name: arg.name,
                            binding_id: arg.binding_id,
                            type_: self.substitution.apply(&arg.type_),
                        },
                        vis,
                        field_id,
                    )
                })
                .collect(),
            methods: s
                .methods
                .into_iter()
                .map(|m| self.apply_substitution_to_function(m))
                .collect(),
            struct_type: self.substitution.apply(&s.struct_type),
        }
    }

    fn apply_substitution_to_enum(&self, e: TypedEnum) -> TypedEnum {
        TypedEnum {
            span: e.span,
            file: e.file,
            name: e.name,
            enum_id: e.enum_id,
            vis: e.vis,
            type_params: e.type_params,
            variants: e
                .variants
                .into_iter()
                .map(|v| TypedEnumVariant {
                    span: v.span,
                    file: v.file,
                    name: v.name,
                    variant_id: v.variant_id,
                    types: v
                        .types
                        .into_iter()
                        .map(|t| self.substitution.apply(&t))
                        .collect(),
                    constraints: v.constraints,
                    constructor_type: self.substitution.apply(&v.constructor_type),
                })
                .collect(),
            methods: e
                .methods
                .into_iter()
                .map(|m| self.apply_substitution_to_function(m))
                .collect(),
            enum_type: self.substitution.apply(&e.enum_type),
        }
    }

    fn apply_substitution_to_impl(&self, impl_: TypedImpl) -> TypedImpl {
        TypedImpl {
            span: impl_.span,
            file: impl_.file,
            type_name: impl_.type_name,
            type_id: impl_.type_id,
            type_params: impl_.type_params,
            implementing_type: self.substitution.apply(&impl_.implementing_type),
            trait_: impl_.trait_,
            trait_type: impl_.trait_type,
            methods: impl_
                .methods
                .into_iter()
                .map(|m| self.apply_substitution_to_function(m))
                .collect(),
            where_constraints: impl_.where_constraints,
        }
    }

    fn apply_substitution_to_trait(&self, t: TypedTraitDef) -> TypedTraitDef {
        TypedTraitDef {
            span: t.span,
            name: t.name,
            trait_id: t.trait_id,
            type_params: t.type_params,
            super_traits: t
                .super_traits
                .into_iter()
                .map(|st| self.substitution.apply(&st))
                .collect(),
            methods: t
                .methods
                .into_iter()
                .map(|m| self.apply_substitution_to_function(m))
                .collect(),
            associated_types: t.associated_types,
        }
    }

    fn apply_substitution_to_effect_def(&self, e: TypedEffectDef) -> TypedEffectDef {
        TypedEffectDef {
            span: e.span,
            file: e.file,
            vis: e.vis,
            name: e.name,
            effect_id: e.effect_id,
            type_params: e.type_params,
            operations: e
                .operations
                .into_iter()
                .map(|op| TypedEffectOperation {
                    span: op.span,
                    name: op.name,
                    operation_id: op.operation_id,
                    params: op
                        .params
                        .into_iter()
                        .map(|p| self.substitution.apply(&p))
                        .collect(),
                    return_type: self.substitution.apply(&op.return_type),
                    operation_type: self.substitution.apply(&op.operation_type),
                })
                .collect(),
            where_constraints: e.where_constraints,
        }
    }

    fn apply_substitution_to_type_alias(&self, a: TypedTypeAlias) -> TypedTypeAlias {
        TypedTypeAlias {
            span: a.span,
            file: a.file,
            name: a.name,
            alias_id: a.alias_id,
            type_params: a.type_params,
            target_type: self.substitution.apply(&a.target_type),
            where_constraints: a.where_constraints,
            expanded_type: self.substitution.apply(&a.expanded_type),
        }
    }

    fn apply_substitution_to_macro_def(&self, m: TypedMacroDef) -> TypedMacroDef {
        TypedMacroDef {
            span: m.span,
            file: m.file,
            name: m.name,
            macro_id: m.macro_id,
            rules: m.rules,
            hygiene: m.hygiene,
        }
    }

    fn apply_substitution_to_expr(&self, expr: TypedExpr) -> TypedExpr {
        TypedExpr {
            span: expr.span,
            file: expr.file,
            expr: match expr.expr {
                TypedExprKind::BinOp { left, right, op } => TypedExprKind::BinOp {
                    left: Box::new(self.apply_substitution_to_expr(*left)),
                    right: Box::new(self.apply_substitution_to_expr(*right)),
                    op,
                },
                TypedExprKind::UnOp { op, operand } => TypedExprKind::UnOp {
                    op,
                    operand: Box::new(self.apply_substitution_to_expr(*operand)),
                },
                TypedExprKind::Call {
                    function,
                    args,
                    type_args,
                } => TypedExprKind::Call {
                    function: Box::new(self.apply_substitution_to_expr(*function)),
                    args: args
                        .into_iter()
                        .map(|a| self.apply_substitution_to_expr(a))
                        .collect(),
                    type_args, // type_args might also need substitution
                },
                TypedExprKind::IfElse {
                    condition,
                    then,
                    else_,
                } => TypedExprKind::IfElse {
                    condition: Box::new(self.apply_substitution_to_expr(*condition)),
                    then: Box::new(self.apply_substitution_to_expr(*then)),
                    else_: else_.map(|e| Box::new(self.apply_substitution_to_expr(*e))),
                },
                TypedExprKind::Match { scrutinee, arms } => TypedExprKind::Match {
                    scrutinee: Box::new(self.apply_substitution_to_expr(*scrutinee)),
                    arms: arms
                        .into_iter()
                        .map(|arm| TypedMatchArm {
                            pattern: arm.pattern, // patterns might have types too
                            guard: arm.guard.map(|g| self.apply_substitution_to_expr(g)),
                            body: Box::new(self.apply_substitution_to_expr(*arm.body)),
                            span: arm.span,
                        })
                        .collect(),
                },
                TypedExprKind::Block { expressions } => TypedExprKind::Block {
                    expressions: expressions
                        .into_iter()
                        .map(|e| self.apply_substitution_to_expr(e))
                        .collect(),
                },
                TypedExprKind::Lambda {
                    args,
                    body,
                    captures,
                    function_type,
                } => TypedExprKind::Lambda {
                    args: args
                        .into_iter()
                        .map(|arg| TypedFnArg {
                            span: arg.span,
                            file: arg.file,
                            name: arg.name,
                            binding_id: arg.binding_id,
                            type_: self.substitution.apply(&arg.type_),
                        })
                        .collect(),
                    body: Box::new(self.apply_substitution_to_expr(*body)),
                    captures,
                    function_type: self.substitution.apply(&function_type),
                },
                TypedExprKind::FieldAccess {
                    target,
                    field,
                    field_id,
                    field_type,
                } => TypedExprKind::FieldAccess {
                    target: Box::new(self.apply_substitution_to_expr(*target)),
                    field,
                    field_id,
                    field_type: self.substitution.apply(&field_type),
                },
                TypedExprKind::Array {
                    elements,
                    element_type,
                } => TypedExprKind::Array {
                    elements: elements
                        .into_iter()
                        .map(|e| self.apply_substitution_to_expr(e))
                        .collect(),
                    element_type: self.substitution.apply(&element_type),
                },
                TypedExprKind::Tuple(exprs) => TypedExprKind::Tuple(
                    exprs
                        .into_iter()
                        .map(|e| self.apply_substitution_to_expr(e))
                        .collect(),
                ),
                TypedExprKind::Index {
                    target,
                    index,
                    element_type,
                } => TypedExprKind::Index {
                    target: Box::new(self.apply_substitution_to_expr(*target)),
                    index: Box::new(self.apply_substitution_to_expr(*index)),
                    element_type: self.substitution.apply(&element_type),
                },
                TypedExprKind::Return(value) => TypedExprKind::Return(
                    value.map(|v| Box::new(self.apply_substitution_to_expr(*v))),
                ),
                TypedExprKind::Assign { l_val, r_val, op } => TypedExprKind::Assign {
                    l_val: Box::new(self.apply_substitution_to_expr(*l_val)),
                    r_val: Box::new(self.apply_substitution_to_expr(*r_val)),
                    op,
                },
                TypedExprKind::Map {
                    entries,
                    key_type,
                    value_type,
                } => TypedExprKind::Map {
                    entries: entries
                        .into_iter()
                        .map(|(k, v)| {
                            (
                                self.apply_substitution_to_expr(k),
                                self.apply_substitution_to_expr(v),
                            )
                        })
                        .collect(),
                    key_type: self.substitution.apply(&key_type),
                    value_type: self.substitution.apply(&value_type),
                },
                TypedExprKind::EnumConstruct {
                    enum_name,
                    enum_id,
                    variant,
                    variant_id,
                    args,
                } => TypedExprKind::EnumConstruct {
                    enum_name,
                    enum_id,
                    variant,
                    variant_id,
                    args: args
                        .into_iter()
                        .map(|a| self.apply_substitution_to_expr(a))
                        .collect(),
                },
                TypedExprKind::OptionalChain {
                    target,
                    field,
                    field_id,
                    field_type,
                } => TypedExprKind::OptionalChain {
                    target: Box::new(self.apply_substitution_to_expr(*target)),
                    field,
                    field_id,
                    field_type: self.substitution.apply(&field_type),
                },
                TypedExprKind::Cast { expr, target_type } => TypedExprKind::Cast {
                    expr: Box::new(self.apply_substitution_to_expr(*expr)),
                    target_type: self.substitution.apply(&target_type),
                },
                TypedExprKind::With {
                    context,
                    var,
                    binding_id,
                    var_type,
                    body,
                } => TypedExprKind::With {
                    context: Box::new(self.apply_substitution_to_expr(*context)),
                    var,
                    binding_id,
                    var_type: self.substitution.apply(&var_type),
                    body: Box::new(self.apply_substitution_to_expr(*body)),
                },
                TypedExprKind::Loop { label, body } => TypedExprKind::Loop {
                    label,
                    body: Box::new(self.apply_substitution_to_expr(*body)),
                },
                TypedExprKind::For {
                    iterator,
                    iterator_type,
                    value,
                    binding_id,
                    value_type,
                    body,
                } => TypedExprKind::For {
                    iterator: Box::new(self.apply_substitution_to_expr(*iterator)),
                    iterator_type: self.substitution.apply(&iterator_type),
                    value,
                    binding_id,
                    value_type: self.substitution.apply(&value_type),
                    body: Box::new(self.apply_substitution_to_expr(*body)),
                },
                TypedExprKind::While { condition, body } => TypedExprKind::While {
                    condition: Box::new(self.apply_substitution_to_expr(*condition)),
                    body: Box::new(self.apply_substitution_to_expr(*body)),
                },
                TypedExprKind::IfLet {
                    pattern,
                    expr,
                    then,
                    else_,
                } => TypedExprKind::IfLet {
                    pattern, // pattern might need type substitution
                    expr: Box::new(self.apply_substitution_to_expr(*expr)),
                    then: Box::new(self.apply_substitution_to_expr(*then)),
                    else_: else_.map(|e| Box::new(self.apply_substitution_to_expr(*e))),
                },
                TypedExprKind::WhileLet {
                    pattern,
                    expr,
                    body,
                } => TypedExprKind::WhileLet {
                    pattern, // pattern might need type substitution
                    expr: Box::new(self.apply_substitution_to_expr(*expr)),
                    body: Box::new(self.apply_substitution_to_expr(*body)),
                },
                TypedExprKind::Break(value) => TypedExprKind::Break(
                    value.map(|v| Box::new(self.apply_substitution_to_expr(*v))),
                ),
                TypedExprKind::Continue => TypedExprKind::Continue,
                TypedExprKind::Perform {
                    effect,
                    effect_id,
                    args,
                } => TypedExprKind::Perform {
                    effect,
                    effect_id,
                    args: args
                        .into_iter()
                        .map(|a| self.apply_substitution_to_expr(a))
                        .collect(),
                },
                TypedExprKind::Handle {
                    body,
                    handlers,
                    return_type,
                } => TypedExprKind::Handle {
                    body: Box::new(self.apply_substitution_to_expr(*body)),
                    handlers: handlers
                        .into_iter()
                        .map(|h| TypedEffectHandler {
                            span: h.span,
                            effect: h.effect,
                            effect_id: h.effect_id,
                            params: h.params,
                            resume_param: h.resume_param,
                            resume_id: h.resume_id,
                            resume_type: self.substitution.apply(&h.resume_type),
                            body: self.apply_substitution_to_expr(h.body),
                        })
                        .collect(),
                    return_type: self.substitution.apply(&return_type),
                },
                TypedExprKind::MacroCall {
                    name,
                    macro_id,
                    args,
                    delimiter,
                } => TypedExprKind::MacroCall {
                    name,
                    macro_id,
                    args: args
                        .into_iter()
                        .map(|a| self.apply_substitution_to_expr(a))
                        .collect(),
                    delimiter,
                },
                TypedExprKind::Variable { name, binding_id } => {
                    TypedExprKind::Variable { name, binding_id }
                }
                TypedExprKind::Int(n) => TypedExprKind::Int(n),
                TypedExprKind::Float(f) => TypedExprKind::Float(f),
                TypedExprKind::Bool(b) => TypedExprKind::Bool(b),
                TypedExprKind::String(s) => TypedExprKind::String(s),
                TypedExprKind::Let {
                    var,
                    binding_id,
                    var_type,
                    value,
                } => TypedExprKind::Let {
                    var,
                    binding_id,
                    var_type: self.substitution.apply(&var_type),
                    value: Box::new(self.apply_substitution_to_expr(*value)),
                },
                TypedExprKind::Error => TypedExprKind::Error,
                TypedExprKind::Import(_) => expr.expr, // Import doesn't have type variables to substitute
                TypedExprKind::StructConstruct {
                    struct_name,
                    struct_id,
                    fields,
                } => TypedExprKind::StructConstruct {
                    struct_name,
                    struct_id,
                    fields: fields
                        .into_iter()
                        .map(|(field_name, field_id, field_expr)| {
                            (
                                field_name,
                                field_id,
                                self.apply_substitution_to_expr(field_expr),
                            )
                        })
                        .collect(),
                },
            },
            type_: self.substitution.apply(&expr.type_),
        }
    }
}
