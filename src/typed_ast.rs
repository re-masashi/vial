use crate::ast::{BinOp, UnOp};
use crate::meta::Meta;

#[derive(Debug, Clone)]
pub struct Type {
    pub type_variant: TypeVariant,
    pub kind: Kind,
}

#[derive(Debug, Clone)]
pub enum TypeVariant {
    // List, HashMap, int, etc.
    Named(String),
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    Tuple(Vec<Type>),
    Record(Vec<(String, Type)>),
    Enum(Vec<TypedEnumVariant>),
    TypeVar(TypeVariable),
    TypeConstructorVar(TypeVariable),
    // associated type projection
    AssociatedType {
        trait_name: String,
        assoc_type_name: String,
        self_type: Box<Type>,
        args: Vec<Type>,
    },
    // trait constraint
    TraitConstraint {
        trait_name: String,
        self_type: Box<Type>,
        args: Vec<Type>,
    },
    // higher-kinded type application (F<A> where F is a type constructor)
    Apply {
        func: Box<Type>, // a type constructor
        arg: Box<Type>,  // an argument type
    },
    Forall {
        bound_vars: Vec<BoundedTypeVar>, // type vars with their kinds and constraints
        body: Box<Type>,
    },
    Exists {
        bound_vars: Vec<BoundedTypeVar>,
        body: Box<Type>,
    },
    // deferred type (to be resolved later)
    Deferred(Box<DeferredType>),
    // error type for type checking errors
    Error(TypeError),
}

#[derive(Debug, Clone)]
pub struct TypeVariable {
    pub id: usize,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct BoundedTypeVar {
    pub var: TypeVariable,
    pub kind: Kind,
    pub bounds: Vec<Type>,
}

#[derive(Debug, Clone)]
pub enum DeferredType {
    // type to be inferred during constraint solving
    Unification(usize), // UID for unification variable
    // type to be computed at compile time
    Comptime(usize),
    // pending trait resolution
    TraitResolution {
        trait_name: String,
        self_type: Box<Type>,
    },
}

#[derive(Debug, Clone)]
pub enum TypeError {
    Other(String),
}

impl Type {
    pub fn new_named(name: String, kind: Kind) -> Self {
        Type {
            type_variant: TypeVariant::Named(name),
            kind,
        }
    }

    pub fn new_function(params: Vec<Type>, return_type: Type, kind: Kind) -> Self {
        Type {
            type_variant: TypeVariant::Function {
                params,
                return_type: Box::new(return_type),
            },
            kind,
        }
    }

    pub fn new_tuple(types: Vec<Type>, kind: Kind) -> Self {
        Type {
            type_variant: TypeVariant::Tuple(types),
            kind,
        }
    }

    pub fn new_type_variable(id: usize, name: String, kind: Kind) -> Self {
        Type {
            type_variant: TypeVariant::TypeVar(TypeVariable { id, name }),
            kind,
        }
    }

    pub fn new_type_constructor_variable(id: usize, name: String, kind: Kind) -> Self {
        Type {
            type_variant: TypeVariant::TypeConstructorVar(TypeVariable { id, name }),
            kind,
        }
    }

    pub fn new_associated_type(
        trait_name: String,
        assoc_type_name: String,
        self_type: Type,
        args: Vec<Type>,
        kind: Kind,
    ) -> Self {
        Type {
            type_variant: TypeVariant::AssociatedType {
                trait_name,
                assoc_type_name,
                self_type: Box::new(self_type),
                args,
            },
            kind,
        }
    }

    pub fn new_trait_constraint(
        trait_name: String,
        self_type: Type,
        args: Vec<Type>,
        kind: Kind,
    ) -> Self {
        Type {
            type_variant: TypeVariant::TraitConstraint {
                trait_name,
                self_type: Box::new(self_type),
                args,
            },
            kind,
        }
    }

    pub fn new_apply(func: Type, arg: Type, kind: Kind) -> Self {
        Type {
            type_variant: TypeVariant::Apply {
                func: Box::new(func),
                arg: Box::new(arg),
            },
            kind,
        }
    }

    pub fn new_forall(bound_vars: Vec<BoundedTypeVar>, body: Type, kind: Kind) -> Self {
        Type {
            type_variant: TypeVariant::Forall {
                bound_vars,
                body: Box::new(body),
            },
            kind,
        }
    }

    pub fn new_exists(bound_vars: Vec<BoundedTypeVar>, body: Type, kind: Kind) -> Self {
        Type {
            type_variant: TypeVariant::Exists {
                bound_vars,
                body: Box::new(body),
            },
            kind,
        }
    }

    pub fn new_deferred_unification(id: usize, kind: Kind) -> Self {
        Type {
            type_variant: TypeVariant::Deferred(Box::new(DeferredType::Unification(id))),
            kind,
        }
    }

    pub fn new_deferred_comptime(id: usize, kind: Kind) -> Self {
        Type {
            type_variant: TypeVariant::Deferred(Box::new(DeferredType::Comptime(id))),
            kind,
        }
    }

    pub fn new_deferred_trait_resolution(trait_name: String, self_type: Type, kind: Kind) -> Self {
        Type {
            type_variant: TypeVariant::Deferred(Box::new(DeferredType::TraitResolution {
                trait_name,
                self_type: Box::new(self_type),
            })),
            kind,
        }
    }

    pub fn new_error(error: TypeError, kind: Kind) -> Self {
        Type {
            type_variant: TypeVariant::Error(error),
            kind,
        }
    }

    pub fn is_type_var(&self) -> bool {
        matches!(
            self.type_variant,
            TypeVariant::TypeVar(_) | TypeVariant::TypeConstructorVar(_)
        )
    }

    pub fn is_associated_type(&self) -> bool {
        matches!(self.type_variant, TypeVariant::AssociatedType { .. })
    }

    pub fn get_name(&self) -> Option<&str> {
        match &self.type_variant {
            TypeVariant::Named(name) => Some(name),
            TypeVariant::AssociatedType {
                assoc_type_name, ..
            } => Some(assoc_type_name),
            _ => None,
        }
    }

    pub fn likely_unifies(&self, other: &Type) -> bool {
        std::mem::discriminant(&self.type_variant) == std::mem::discriminant(&other.type_variant)
    }

    pub fn get_trait_constraint_name(&self) -> Option<String> {
        match &self.type_variant {
            TypeVariant::TraitConstraint { trait_name, .. } => Some(trait_name.clone()),
            _ => None,
        }
    }

    pub fn new_star_type(type_variant: TypeVariant) -> Self {
        Type {
            type_variant,
            kind: Kind::Star,
        }
    }

    pub fn new_arrow_kind_type(type_variant: TypeVariant, from: Kind, to: Kind) -> Self {
        Type {
            type_variant,
            kind: Kind::Arrow(Box::new(from), Box::new(to)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedNode<T> {
    pub data: T,
    pub meta: Meta,
    pub ty: Type,
}

pub type TypedProgram = Vec<TypedNode<TypedItem>>;

#[derive(Debug, Clone)]
pub enum TypedItem {
    Function(Box<TypedFunction>),
    ExternFunction(TypedExternFunction),
    TypeAlias(TypedTypeAlias),
    Trait(TypedTrait),
    Impl(TypedImpl),
    Use(TypedUse),
}

#[derive(Debug, Clone)]
pub struct TypedFunction {
    pub name: String,
    pub generics: Vec<TypedGenericParam>,
    pub params: Vec<TypedFunctionArg>,
    pub return_type: Type,
    pub body: TypedNode<TypedExpr>,
    pub signature: Type,
    pub is_method: bool,         // is this a method (first param is self)?
    pub self_type: Option<Type>, // the type of self if this is a method
}

#[derive(Debug, Clone)]
pub struct TypedExternFunction {
    pub name: String,
    pub params: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct TypedTypeAlias {
    pub name: String,
    pub generics: Vec<TypedGenericParam>,
    pub definition: Type,
    pub aliased_type: Type,
}

#[derive(Debug, Clone)]
pub struct TypedStructField {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub enum TypedEnumVariant {
    Unit(String),
    Tuple(String, Vec<Type>),
    Struct(String, Vec<TypedStructField>),
}

#[derive(Debug, Clone)]
pub struct TypedTrait {
    pub name: String,
    pub generics: Vec<TypedGenericParam>,
    pub items: Vec<TypedNode<TypedTraitItem>>,
    pub trait_type: Type,
    pub super_traits: Vec<Type>, // For trait inheritance
}

#[derive(Debug, Clone)]
pub enum TypedTraitItem {
    TypeAlias(TypedTypeAlias),
    FunctionSignature(TypedFunctionSignature),
    AssociatedType(TypedAssociatedType),
}

#[derive(Debug, Clone)]
pub struct TypedFunctionSignature {
    pub name: String,
    pub generics: Vec<TypedGenericParam>,
    pub params: Vec<TypedFunctionArg>,
    pub return_type: Type,
    pub is_method: bool,         // is this a method (first param is self)?
    pub self_type: Option<Type>, // the type of self if this is a method
}

#[derive(Debug, Clone)]
pub struct TypedImpl {
    pub trait_name: Option<String>,
    pub target_type: String,
    pub generics: Vec<TypedGenericParam>,
    pub items: Vec<TypedNode<TypedImplItem>>,
    pub impl_type: Type,
    pub trait_args: Vec<Type>, // arguments to the trait if it has generics
}

#[derive(Debug, Clone)]
pub enum TypedImplItem {
    TypeAlias(TypedTypeAlias),
    Function(Box<TypedFunction>),
    AssociatedTypeValue(TypedAssociatedTypeValue),
}

#[derive(Debug, Clone)]
pub struct TypedAssociatedTypeValue {
    pub name: String,
    pub value: Type,
    pub implemented_type: Type,
}

#[derive(Debug, Clone)]
pub struct TypedUse {
    pub path: String,
    pub imports: Option<Vec<TypedImportItem>>,
}

#[derive(Debug, Clone)]
pub struct TypedImportItem {
    pub name: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone)]
pub enum TypedExpr {
    Binary {
        left: Box<TypedNode<TypedExpr>>,
        op: BinOp,
        right: Box<TypedNode<TypedExpr>>,
        result_type: Type,
    },
    Unary {
        op: UnOp,
        expr: Box<TypedNode<TypedExpr>>,
        result_type: Type,
    },
    Literal(Literal, Type),
    Variable(String, Type),
    Call {
        fun: Box<TypedNode<TypedExpr>>,
        args: Vec<TypedNode<TypedExpr>>,
        result_type: Type,
    },
    Lambda {
        params: Vec<TypedFunctionArg>,
        body: Box<TypedNode<TypedExpr>>,
        lambda_type: Type,
    },
    If {
        cond: Box<TypedNode<TypedExpr>>,
        then_branch: Box<TypedNode<TypedExpr>>,
        else_branch: Option<Box<TypedNode<TypedExpr>>>,
        result_type: Type,
    },
    While {
        cond: Box<TypedNode<TypedExpr>>,
        body: Box<TypedNode<TypedExpr>>,
    },
    For {
        var: String,
        iter: Box<TypedNode<TypedExpr>>,
        body: Box<TypedNode<TypedExpr>>,
        loop_var_type: Type,
    },
    Match {
        expr: Box<TypedNode<TypedExpr>>,
        arms: Vec<TypedMatchArm>,
        result_type: Type,
    },
    Block(Vec<TypedNode<TypedExpr>>, Type), // result type of the block
    Tuple(Vec<TypedNode<TypedExpr>>, Type), // tuple type
    Array(Vec<TypedNode<TypedExpr>>, Type), // array type
    Pipe {
        left: Box<TypedNode<TypedExpr>>,
        right: Box<TypedNode<TypedExpr>>,
        result_type: Type,
    },
    FieldAccess {
        expr: Box<TypedNode<TypedExpr>>,
        field: String,
        result_type: Type,
    },
    Index {
        expr: Box<TypedNode<TypedExpr>>,
        index: Box<TypedNode<TypedExpr>>,
        result_type: Type,
    },
    StructLiteral {
        name: String,
        fields: Vec<TypedStructLitField>,
        base: Option<Box<TypedNode<TypedExpr>>>,
        struct_type: Type,
    },
    EnumVariant {
        enum_name: String,
        variant_name: String,
        data: Option<TypedEnumVariantData>,
        variant_type: Type,
    },
    Assign {
        left: Box<TypedNode<TypedExpr>>,
        right: Box<TypedNode<TypedExpr>>,
        result_type: Type,
    },
    Let {
        name: String,
        value: Box<TypedNode<TypedExpr>>,
        binding_type: Type,
    },
    StaticCall {
        type_name: String,
        method: String,
        args: Vec<TypedNode<TypedExpr>>,
        result_type: Type,
    },
    /// Cast expression (for explicit type coercion)
    Cast {
        expr: Box<TypedNode<TypedExpr>>,
        target_type: Type,
        cast_type: Type,
    },
    Break,
    Continue,
    Return(Option<Box<TypedNode<TypedExpr>>>, Type), // result type of the return expr
}

#[derive(Debug, Clone)]
pub enum TypedEnumVariantData {
    Tuple(Vec<TypedNode<TypedExpr>>),
    Struct(Vec<TypedStructLitField>),
}

#[derive(Debug, Clone)]
pub struct TypedStructLitField {
    pub name: String,
    pub value: Option<TypedNode<TypedExpr>>,
    pub field_type: Type,
}

#[derive(Debug, Clone)]
pub struct TypedMatchArm {
    pub pattern: TypedNode<TypedPat>,
    pub body: TypedNode<TypedExpr>,
    pub arm_type: Type,
}

#[derive(Debug, Clone)]
pub enum TypedPat {
    Wildcard(Type),
    Identifier(String, Type),
    Literal(Literal, Type),
    Tuple(Vec<TypedNode<TypedPat>>, Type),
    Struct {
        name: String,
        fields: Vec<TypedStructPatField>,
        struct_type: Type,
    },
    EnumVariant {
        enum_name: String,
        variant_name: String,
        data: Option<TypedEnumPatData>,
        variant_type: Type,
    },
    Array(Vec<TypedNode<TypedPat>>, Type),
    ArrayRest(Vec<TypedNode<TypedPat>>, Option<String>, Type),
    Range {
        start: Box<TypedNode<TypedPat>>,
        end: Box<TypedNode<TypedPat>>,
        range_type: Type,
    },
    Union(Vec<TypedNode<TypedPat>>, Type),
}

#[derive(Debug, Clone)]
pub enum TypedEnumPatData {
    Tuple(Vec<TypedNode<TypedPat>>),
    Struct(Vec<TypedStructPatField>),
}

#[derive(Debug, Clone)]
pub struct TypedStructPatField {
    pub name: String,
    pub pattern: TypedNode<TypedPat>,
    pub field_type: Type,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Default, Debug, Clone, PartialEq)]
pub enum Kind {
    // *
    #[default]
    Star,
    // K1 -> K2
    Arrow(Box<Kind>, Box<Kind>),
    Var(KindVariable),
}

#[derive(Debug, Clone, PartialEq)]
pub struct KindVariable {
    pub id: usize,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct TypedGenericParam {
    pub name: String,
    pub kind: Kind,
    pub bounds: Vec<Type>,
    pub trait_constraints: Vec<Type>,
    pub param_type: Type,
    pub associated_types: Vec<TypedAssociatedType>,
}

#[derive(Debug, Clone)]
pub struct TypedAssociatedType {
    pub name: String,
    pub generics: Vec<TypedGenericParam>,
    pub definition: Type,
    pub associated_type_type: Type,
}

#[derive(Debug, Clone)]
pub struct TypedFunctionArg {
    pub name: String,
    pub type_: Type,
    pub is_self: bool, // is this the self parameter?
}
