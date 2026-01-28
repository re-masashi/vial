use crate::meta::Meta;

#[derive(Debug, Clone)]
pub struct Node<T> {
    pub data: T,
    pub meta: Meta,
}

pub type Program = Vec<Node<Item>>;

#[derive(Debug, Clone)]
pub enum Item {
    Function(Box<Function>),
    ExternFunction(ExternFunction),
    TypeAlias(TypeAlias),
    Trait(Trait),
    Impl(Impl),
    Use(Use),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub params: Vec<FunctionArg>,
    pub return_type: Option<Node<TypeAnn>>,
    pub body: Node<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
    pub name: String,
    pub params: Vec<Node<TypeAnn>>,
    pub return_type: Node<TypeAnn>,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub definition: Node<TypeAnn>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub type_ann: Node<TypeAnn>,
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Unit(String),
    Tuple(String, Vec<Node<TypeAnn>>),
    Struct(String, Vec<StructField>),
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub inherits: Vec<Node<TypeAnn>>, // supertraits
    pub items: Vec<Node<TraitItem>>,
}

#[derive(Debug, Clone)]
pub enum TraitItem {
    TypeAlias(TypeAlias),
    FunctionSignature(FunctionSignature),
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub params: Vec<FunctionArg>,
    pub return_type: Option<Node<TypeAnn>>,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub trait_name: Option<String>,
    pub target_type: String,
    pub generics: Vec<GenericParam>,
    pub items: Vec<Node<ImplItem>>,
}

#[derive(Debug, Clone)]
pub enum ImplItem {
    TypeAlias(TypeAlias),
    Function(Box<Function>),
}

#[derive(Debug, Clone)]
pub struct Use {
    pub path: String,
    pub imports: Option<Vec<ImportItem>>,
}

#[derive(Debug, Clone)]
pub struct ImportItem {
    pub name: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Node<Expr>>,
        op: BinOp,
        right: Box<Node<Expr>>,
    },
    Unary {
        op: UnOp,
        expr: Box<Node<Expr>>,
    },
    Literal(Literal),
    Variable(String),
    Call {
        fun: Box<Node<Expr>>,
        args: Vec<Node<Expr>>,
    },
    Lambda {
        params: Vec<FunctionArg>,
        return_type: Option<Node<TypeAnn>>,
        body: Box<Node<Expr>>,
    },
    If {
        cond: Box<Node<Expr>>,
        then_branch: Box<Node<Expr>>,
        else_branch: Option<Box<Node<Expr>>>,
    },
    While {
        cond: Box<Node<Expr>>,
        body: Box<Node<Expr>>,
    },
    For {
        var: String,
        iter: Box<Node<Expr>>,
        body: Box<Node<Expr>>,
    },
    Match {
        expr: Box<Node<Expr>>,
        arms: Vec<MatchArm>,
    },
    Block(Vec<Node<Expr>>),
    Tuple(Vec<Node<Expr>>),
    Array(Vec<Node<Expr>>),
    Pipe {
        left: Box<Node<Expr>>,
        right: Box<Node<Expr>>,
    },
    FieldAccess {
        expr: Box<Node<Expr>>,
        field: String,
    },
    Index {
        expr: Box<Node<Expr>>,
        index: Box<Node<Expr>>,
    },
    StructLiteral {
        name: String,
        fields: Vec<StructLitField>,
        base: Option<Box<Node<Expr>>>,
    },
    EnumVariant {
        enum_name: String,
        variant_name: String,
        data: Option<EnumVariantData>,
    },
    Assign {
        left: Box<Node<Expr>>,
        right: Box<Node<Expr>>,
    },
    Let {
        name: String,
        type_ann: Option<Node<TypeAnn>>,
        value: Box<Node<Expr>>,
    },
    StaticCall {
        type_name: String,
        method: String,
        args: Vec<Node<Expr>>,
    },
    Break,
    Continue,
    Return(Option<Box<Node<Expr>>>),
}

#[derive(Debug, Clone)]
pub enum EnumVariantData {
    Tuple(Vec<Node<Expr>>),
    Struct(Vec<StructLitField>),
}

#[derive(Debug, Clone)]
pub struct StructLitField {
    pub name: String,
    pub value: Option<Node<Expr>>,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Node<Pat>,
    pub body: Node<Expr>,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Xor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Lt,
    Gt,
    Leq,
    Geq,
    Eq,
    Neq,
    And,
    Or,
    Range,
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum Pat {
    Wildcard,
    Identifier(String),
    Literal(Literal),
    Tuple(Vec<Node<Pat>>),
    Struct {
        name: String,
        fields: Vec<StructPatField>,
    },
    EnumVariant {
        enum_name: String,
        variant_name: String,
        data: Option<EnumPatData>,
    },
    Array(Vec<Node<Pat>>),
    ArrayRest(Vec<Node<Pat>>, Option<String>),
    Range {
        start: Box<Node<Pat>>,
        end: Box<Node<Pat>>,
    },
    Union(Vec<Node<Pat>>),
}

#[derive(Debug, Clone)]
pub enum EnumPatData {
    Tuple(Vec<Node<Pat>>),
    Struct(Vec<StructPatField>),
}

#[derive(Debug, Clone)]
pub struct StructPatField {
    pub name: String,
    pub pattern: Node<Pat>,
}

#[derive(Debug, Clone)]
pub enum TypeAnn {
    Primary(String, Vec<Node<TypeAnn>>), // name, generics
    Function {
        params: Vec<Node<TypeAnn>>,
        return_type: Box<Node<TypeAnn>>,
    },
    Tuple(Vec<Node<TypeAnn>>),
    TraitBound(Vec<Node<TypeAnn>>),
    Struct(Vec<StructField>),
    Enum(Vec<EnumVariant>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum KindAnnot {
    #[default]
    Star, // *
    Arrow(Box<KindAnnot>, Box<KindAnnot>), // * -> * or * -> (* -> *)
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: String,
    pub kind: KindAnnot,
    pub bounds: Vec<Node<TypeAnn>>,
    pub trait_constraints: Vec<Node<TypeAnn>>,
}

#[derive(Debug, Clone)]
pub struct FunctionArg {
    pub name: String,
    pub type_ann: Option<Node<TypeAnn>>,
}
