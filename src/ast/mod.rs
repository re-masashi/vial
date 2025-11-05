use std::collections::HashMap;
use std::ops::Range;
use std::rc::Rc;

use crate::typechecker::{Constraint, EffectSet, Type};

#[derive(Debug, Clone)]
pub struct ASTNode {
    pub span: Range<usize>,
    pub file: String,
    pub node: ASTNodeKind,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub enum ASTNodeKind {
    Expr(Expr),
    Function(Box<Function>),
    ExternFunction(Box<ExternFunction>),
    Struct(Struct),
    Enum(Enum),
    TypeAlias(TypeAlias),
    Impl(Impl),
    Trait(TraitDef),
    MacroDef(MacroDef),
    EffectDef(EffectDef),

    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnot {
    pub span: Range<usize>,
    pub file: String,
    pub type_: TypeAnnotKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotKind {
    // Primitives
    Bool,
    Int,
    Float,
    String,
    Unit,
    Never,

    // Named types
    Named(String),

    // Generic types: List<T>, Option<T>
    Generic {
        name: String,
        args: Vec<TypeAnnot>,
        kind: Option<KindAnnot>,
    },

    // Function types with effects
    Function {
        params: Vec<TypeAnnot>,
        return_type: Box<TypeAnnot>,
        effects: EffectAnnot,
    },

    // Tuple types
    Tuple(Vec<TypeAnnot>),

    // Union types
    Union(Vec<TypeAnnot>),

    // Trait bounds
    Trait(Vec<String>),

    // HKT: Type constructor with explicit kind
    Constructor {
        name: String,
        kind: KindAnnot,
    },

    // Type variable (in forall/exists)
    Variable {
        name: String,
        kind: Option<KindAnnot>,
    },

    // GADT: Universal quantification
    // forall T. T ~ Int => T -> T
    // Forall {
    //     vars: Vec<(String, KindAnnot)>,
    //     constraints: Vec<ConstraintAnnot>,
    //     body: Box<TypeAnnot>,
    // },

    // // GADT: Existential types
    // // exists T. (T, T -> Int)
    // Exists {
    //     vars: Vec<(String, KindAnnot)>,
    //     constraints: Vec<ConstraintAnnot>,
    //     body: Box<TypeAnnot>,
    // },

    // Pointer type for FFI
    Pointer(Box<TypeAnnot>),

    Error,
}

// Kind annotations for HKTs
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KindAnnot {
    Star,                                  // *
    Arrow(Box<KindAnnot>, Box<KindAnnot>), // * -> *
}

// Effect annotations
#[derive(Debug, Clone, PartialEq)]
pub struct EffectAnnot {
    pub effects: Vec<(String, Vec<TypeAnnot>)>, // (effect_name, type_args) - empty vec means simple effect
    pub rest: Option<String>,                   // todo
}

impl EffectAnnot {
    pub fn pure() -> Self {
        Self {
            effects: vec![],
            rest: None,
        }
    }

    pub fn closed(effects: Vec<(String, Vec<TypeAnnot>)>) -> Self {
        Self {
            effects,
            rest: None,
        }
    }

    pub fn closed_simple(effects: Vec<String>) -> Self {
        Self {
            effects: effects.into_iter().map(|name| (name, vec![])).collect(),
            rest: None,
        }
    }

    pub fn open(effects: Vec<(String, Vec<TypeAnnot>)>, rest: String) -> Self {
        Self {
            effects,
            rest: Some(rest),
        }
    }

    pub fn open_simple(effects: Vec<String>, rest: String) -> Self {
        Self {
            effects: effects.into_iter().map(|name| (name, vec![])).collect(),
            rest: Some(rest),
        }
    }
}

// Type constraints (for GADTs)
#[derive(Debug, Clone)]
pub enum ConstraintAnnot {
    // Type equality: T ~ Int
    Equal(TypeAnnot, TypeAnnot),

    // Trait bound: T: Show
    Trait(String, String), // (type_var, trait_name)
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Range<usize>,
    pub file: String,
    pub expr: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),

    Variable(String),

    BinOp(Box<Expr>, BinOp, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    Spread(Box<Expr>), // Represents the ...expr syntax in array expressions
    Assign {
        l_val: Box<Expr>,
        r_val: Box<Expr>,
        op: AssignOp,
    },
    Let {
        var: String,
        type_annot: Option<TypeAnnot>,
        value: Box<Expr>,
    },

    Array(Vec<Expr>), // Could contain regular elements and/or spread elements
    Tuple(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
    EnumConstruct {
        name: String,
        variant: String,
        args: Vec<Expr>,
    },
    StructConstruct {
        name: String,
        fields: Vec<(String, Expr)>,
    },

    Perform {
        effect: String,
        args: Vec<Expr>,
    },
    Handle {
        body: Box<Expr>,
        handlers: Vec<EffectHandler>,
    },

    Lambda {
        args: Vec<FnArg>,
        expression: Box<Expr>,
    },
    Cast {
        expr: Box<Expr>,
        target_type: TypeAnnot,
    },

    IfElse {
        condition: Box<Expr>,
        then: Box<Expr>,
        else_: Option<Box<Expr>>,
    },
    Block(Vec<Expr>),
    With {
        context: Box<Expr>,
        var: String,
        body: Box<Expr>,
    },
    Loop {
        label: Option<String>,
        body: Box<Expr>,
    },
    Match(Box<Expr>, Vec<MatchArm>),
    For {
        iterator: Box<Expr>,
        value: String,
        expression: Box<Expr>,
    },
    While(Box<Expr>, Box<Expr>),
    IfLet {
        pattern: Pattern,
        expr: Box<Expr>,
        then: Box<Expr>,
        else_: Option<Box<Expr>>,
    },
    WhileLet {
        pattern: Pattern,
        expr: Box<Expr>,
        body: Box<Expr>,
    },

    Return(Option<Box<Expr>>),
    Break(Option<Box<Expr>>),
    Continue,

    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    FieldAccess(Box<Expr>, String),
    OptionalChain(Box<Expr>, String),

    MacroCall(String, Vec<Expr>, Delimiter),
    Import(Import),

    Error,
}

#[derive(Debug, Clone)]
pub struct EffectHandler {
    pub span: Range<usize>,
    pub effect: String,
    pub params: Vec<String>,
    pub resume_param: String, // The continuation parameter
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub span: Range<usize>,
    pub file: String,
    pub path: Vec<String>,                    // ["abc", "ab"] for "abc/ab.vi"
    pub items: Vec<(String, Option<String>)>, // for selective imports + aliases
    // none implies all being imported
    pub alias: Option<String>,
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    pub span: Range<usize>,
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub super_traits: Vec<TypeAnnot>,
    pub methods: Vec<Function>,
    pub associated_types: Vec<AssociatedType>,
}

#[derive(Debug, Clone)]
pub struct AssociatedType {
    pub name: String,
    pub bounds: Vec<TypeAnnot>,
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Not,
    Plus,
    Minus,
    Unwrap,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    Eq,
    Greater,
    Less,
    GreaterEq,
    LessEq,
    NotEq,

    Pipe,

    And,
    Or,
    Nor,
    Xor,
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Box<Expr>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub span: Range<usize>,
    pub file: String,
    pub pat: PatKind,
}

#[derive(Debug, Clone)]
pub enum PatKind {
    Wildcard,
    Bind(String),
    Literal(Literal),
    Array(Vec<ArrayPatElement>),
    Tuple(Vec<Pattern>),
    Or(Vec<Pattern>),
    As {
        name: String,
        pattern: Box<Pattern>,
    },
    Struct {
        name: String,
        fields: Vec<(String, Pattern)>,
    },
    Enum {
        name: String,
        variant: String,
        params: Vec<Pattern>,
    },
    Range(Box<Expr>, Box<Expr>),
    Rest(String),

    Error,
}

#[derive(Debug, Clone)]
pub enum ArrayPatElement {
    Pattern(Pattern),
    Spread(Pattern),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub span: Range<usize>,
    pub file: String,

    pub vis: Visibility,
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub args: Vec<FnArg>,
    pub return_type: Option<TypeAnnot>,
    pub where_constraints: Vec<TypeConstraint>,
    pub effects: EffectAnnot,
    pub body: Option<Expr>, // no body implies declaration
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
    pub span: Range<usize>,
    pub file: String,

    pub vis: Visibility,
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub args: Vec<FnArg>,
    pub return_type: Option<TypeAnnot>,
    pub where_constraints: Vec<TypeConstraint>,
    pub effects: EffectAnnot,
    pub library: String,             // library name where the function is defined
    pub symbol_name: Option<String>, // optional C symbol name (if different from function name)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub struct TypeParam {
    pub name: String,
    pub kind: Option<KindAnnot>,
    pub bounds: Vec<TypeAnnot>,
}

#[derive(Debug, Clone)]
pub struct TypeConstraint {
    pub span: Range<usize>,
    pub left: TypeAnnot,
    pub right: TypeAnnot,
}

#[derive(Debug, Clone)]
pub struct FnArg {
    pub span: Range<usize>,
    pub file: String,
    pub name: String,
    pub type_: Option<TypeAnnot>,
    // pub default: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub span: Range<usize>,
    pub file: String,
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub fields: Vec<(FnArg, Visibility)>,
    pub methods: Vec<Function>, // (desugar to impl)
    pub vis: Visibility,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub span: Range<usize>,
    pub file: String,
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub variants: Vec<EnumVariant>,
    pub methods: Vec<Function>, // (desugar to impl)
    pub vis: Visibility,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub span: Range<usize>,
    pub file: String,
    pub name: String,
    pub types: Vec<TypeAnnot>,
    pub constraints: Vec<TypeConstraint>,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub span: Range<usize>,
    pub file: String,
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub target_type: TypeAnnot,
    pub where_constraints: Vec<TypeConstraint>,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub span: Range<usize>,
    pub file: String,
    pub type_name: String,
    pub type_params: Vec<TypeParam>,
    pub methods: Vec<Function>,
    pub trait_: Option<String>,
    pub where_constraints: Vec<TypeConstraint>,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub span: Range<usize>,
    pub name: String,
    pub args: Vec<AttributeArg>,
}

#[derive(Debug, Clone)]
pub enum AttributeArg {
    Ident(String),
    Literal(Literal),
    KeyValue(String, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct MacroDef {
    pub span: Range<usize>,
    pub file: String,
    pub name: String,
    pub rules: Vec<MacroRule>,
    pub hygiene: MacroHygiene,
}

#[derive(Debug, Clone)]
pub enum MacroHygiene {
    Hygienic,
    Unhygienic,
}

#[derive(Debug, Clone)]
pub struct MacroRule {
    pub span: Range<usize>,
    pub pattern: Vec<MacroToken>,
    pub body: Vec<MacroToken>,
}

#[derive(Debug, Clone)]
pub enum MacroToken {
    Ident(String),
    Literal(Literal),
    Punct(String),
    Group {
        delimiter: Delimiter,
        tokens: Vec<MacroToken>,
    },
    MetaVar {
        name: String,
        kind: String,
    }, // $x:expr, $t:ty
    Repeat {
        tokens: Vec<MacroToken>,
        separator: Option<String>,
        kind: RepeatKind,
    },
}

#[derive(Debug, Clone)]
pub enum Delimiter {
    Paren,   // ()
    Bracket, // []
    Brace,   // {}
}

#[derive(Debug, Clone)]
pub enum RepeatKind {
    ZeroOrMore, // *
    OneOrMore,  // +
    ZeroOrOne,  // ?
}

#[derive(Debug, Clone)]
pub struct EffectDef {
    pub span: Range<usize>,
    pub file: String,
    pub vis: Visibility,
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub operations: Vec<EffectOperation>,
    pub where_constraints: Vec<TypeConstraint>,
}

#[derive(Debug, Clone)]
pub struct EffectOperation {
    pub span: Range<usize>,
    pub name: String,
    pub params: Vec<TypeAnnot>,
    pub return_type: TypeAnnot,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
}

// TYPED STUFF

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(pub usize);

#[derive(Debug, Default, Clone)]
pub struct Interner {
    map: HashMap<String, Symbol>,
    vec: Vec<String>,
}

impl Interner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(&sym) = self.map.get(name) {
            return sym;
        }
        let sym = Symbol(self.vec.len());
        self.map.insert(name.to_string(), sym);
        self.vec.push(name.to_string());
        sym
    }

    pub fn resolve(&self, sym: Symbol) -> &str {
        &self.vec[sym.0]
    }
}

// Unique ID Generators

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BindingId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TraitId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EffectId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariantId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MacroId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeAliasId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

// ID Generator Helper
#[derive(Debug, Default)]
pub struct IdGen {
    next_binding: usize,
    next_type: usize,
    next_function: usize,
    next_struct: usize,
    next_enum: usize,
    next_trait: usize,
    next_effect: usize,
    next_variant: usize,
    next_field: usize,
    next_macro: usize,
    next_type_alias: usize,
    next_scope: usize,
}

impl IdGen {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn fresh_binding(&mut self) -> BindingId {
        let id = BindingId(self.next_binding);
        self.next_binding += 1;
        id
    }

    pub fn fresh_type(&mut self) -> TypeId {
        let id = TypeId(self.next_type);
        self.next_type += 1;
        id
    }

    pub fn fresh_function(&mut self) -> FunctionId {
        let id = FunctionId(self.next_function);
        self.next_function += 1;
        id
    }

    pub fn fresh_struct(&mut self) -> StructId {
        let id = StructId(self.next_struct);
        self.next_struct += 1;
        id
    }

    pub fn fresh_enum(&mut self) -> EnumId {
        let id = EnumId(self.next_enum);
        self.next_enum += 1;
        id
    }

    pub fn fresh_trait(&mut self) -> TraitId {
        let id = TraitId(self.next_trait);
        self.next_trait += 1;
        id
    }

    pub fn fresh_effect(&mut self) -> EffectId {
        let id = EffectId(self.next_effect);
        self.next_effect += 1;
        id
    }

    pub fn fresh_variant(&mut self) -> VariantId {
        let id = VariantId(self.next_variant);
        self.next_variant += 1;
        id
    }

    pub fn fresh_field(&mut self) -> FieldId {
        let id = FieldId(self.next_field);
        self.next_field += 1;
        id
    }

    pub fn fresh_macro(&mut self) -> MacroId {
        let id = MacroId(self.next_macro);
        self.next_macro += 1;
        id
    }

    pub fn fresh_type_alias(&mut self) -> TypeAliasId {
        let id = TypeAliasId(self.next_type_alias);
        self.next_type_alias += 1;
        id
    }

    pub fn fresh_scope(&mut self) -> ScopeId {
        let id = ScopeId(self.next_scope);
        self.next_scope += 1;
        id
    }
}

#[derive(Debug, Clone)]
pub struct TypedASTNode {
    pub span: Range<usize>,
    pub file: String,
    pub node: TypedASTNodeKind,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub enum TypedASTNodeKind {
    Expr(TypedExpr),
    Function(Box<TypedFunction>),
    ExternFunction(Box<TypedExternFunction>),
    Struct(TypedStruct),
    Enum(TypedEnum),
    TypeAlias(TypedTypeAlias),
    Impl(TypedImpl),
    Trait(TypedTraitDef),
    MacroDef(TypedMacroDef),
    EffectDef(TypedEffectDef),

    Error,
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub span: Range<usize>,
    pub file: String,
    pub expr: TypedExprKind,
    pub type_: Rc<Type>,
}

#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),

    Variable {
        name: Symbol,          // For printing/debugging
        binding_id: BindingId, // The actual reference
    },

    BinOp {
        left: Box<TypedExpr>,
        op: BinOp,
        right: Box<TypedExpr>,
    },
    UnOp {
        op: UnOp,
        operand: Box<TypedExpr>,
    },
    Spread {
        value: Box<TypedExpr>,
        element_type: Rc<Type>,
    },
    Assign {
        l_val: Box<TypedExpr>,
        r_val: Box<TypedExpr>,
        op: AssignOp,
    },
    Let {
        var: Symbol,
        binding_id: BindingId,
        var_type: Rc<Type>,
        value: Box<TypedExpr>,
    },

    Array {
        elements: Vec<TypedExpr>,
        element_type: Rc<Type>,
    },
    Tuple(Vec<TypedExpr>),
    Map {
        entries: Vec<(TypedExpr, TypedExpr)>,
        key_type: Rc<Type>,
        value_type: Rc<Type>,
    },
    EnumConstruct {
        enum_name: Symbol,
        enum_id: EnumId,
        variant: Symbol,
        variant_id: VariantId,
        args: Vec<TypedExpr>,
    },
    StructConstruct {
        struct_name: Symbol,
        struct_id: StructId,
        fields: Vec<(Symbol, FieldId, TypedExpr)>,
    },

    Perform {
        effect: Symbol,
        effect_id: EffectId,
        args: Vec<TypedExpr>,
    },
    Handle {
        body: Box<TypedExpr>,
        handlers: Vec<TypedEffectHandler>,
        return_type: Rc<Type>,
    },

    Lambda {
        args: Vec<TypedFnArg>,
        body: Box<TypedExpr>,
        captures: Vec<(Symbol, BindingId, Rc<Type>)>,
        function_type: Rc<Type>,
    },
    Cast {
        expr: Box<TypedExpr>,
        target_type: Rc<Type>,
    },

    IfElse {
        condition: Box<TypedExpr>,
        then: Box<TypedExpr>,
        else_: Option<Box<TypedExpr>>,
    },
    Block {
        expressions: Vec<TypedExpr>,
    },
    With {
        context: Box<TypedExpr>,
        var: Symbol,
        binding_id: BindingId,
        var_type: Rc<Type>,
        body: Box<TypedExpr>,
    },
    Loop {
        label: Option<Symbol>,
        body: Box<TypedExpr>,
    },
    Match {
        scrutinee: Box<TypedExpr>,
        arms: Vec<TypedMatchArm>,
    },
    For {
        iterator: Box<TypedExpr>,
        iterator_type: Rc<Type>,
        value: Symbol,
        binding_id: BindingId,
        value_type: Rc<Type>,
        body: Box<TypedExpr>,
    },
    While {
        condition: Box<TypedExpr>,
        body: Box<TypedExpr>,
    },
    IfLet {
        pattern: TypedPattern,
        expr: Box<TypedExpr>,
        then: Box<TypedExpr>,
        else_: Option<Box<TypedExpr>>,
    },
    WhileLet {
        pattern: TypedPattern,
        expr: Box<TypedExpr>,
        body: Box<TypedExpr>,
    },

    Return(Option<Box<TypedExpr>>),
    Break(Option<Box<TypedExpr>>),
    Continue,

    Call {
        function: Box<TypedExpr>,
        args: Vec<TypedExpr>,
        type_args: Vec<Rc<Type>>,
    },
    Index {
        target: Box<TypedExpr>,
        index: Box<TypedExpr>,
        element_type: Rc<Type>,
    },
    FieldAccess {
        target: Box<TypedExpr>,
        field: Symbol,
        field_id: FieldId,
        field_type: Rc<Type>,
    },
    OptionalChain {
        target: Box<TypedExpr>,
        field: Symbol,
        field_id: FieldId,
        field_type: Rc<Type>,
    },

    MacroCall {
        name: Symbol,
        macro_id: MacroId,
        args: Vec<TypedExpr>,
        delimiter: Delimiter,
    },
    Import(TypedImport),

    Error,
}

#[derive(Debug, Clone)]
pub struct TypedEffectHandler {
    pub span: Range<usize>,
    pub effect: Symbol,
    pub effect_id: EffectId,
    pub params: Vec<(Symbol, BindingId, Rc<Type>)>,
    pub resume_param: Symbol,
    pub resume_id: BindingId,
    pub resume_type: Rc<Type>,
    pub body: TypedExpr,
}

#[derive(Debug, Clone)]
pub struct TypedPattern {
    pub span: Range<usize>,
    pub file: String,
    pub pat: TypedPatKind,
    pub type_: Rc<Type>,
}

#[derive(Debug, Clone)]
pub enum TypedPatKind {
    Wildcard,
    Bind {
        name: Symbol,
        binding_id: BindingId,
    },
    Literal(Literal),
    Array {
        elements: Vec<TypedArrayPatElement>,
        element_type: Rc<Type>,
    },
    Tuple {
        patterns: Vec<TypedPattern>,
        element_types: Vec<Rc<Type>>,
    },
    Or(Vec<TypedPattern>),
    As {
        name: Symbol,
        binding_id: BindingId,
        pattern: Box<TypedPattern>,
    },
    Struct {
        name: Symbol,
        struct_id: StructId,
        fields: Vec<(Symbol, FieldId, TypedPattern)>,
    },
    Enum {
        enum_name: Symbol,
        enum_id: EnumId,
        variant: Symbol,
        variant_id: VariantId,
        params: Vec<TypedPattern>,
    },
    Range {
        start: Box<TypedExpr>,
        end: Box<TypedExpr>,
    },
    Rest {
        name: Symbol,
        binding_id: BindingId,
    },

    Error,
}

#[derive(Debug, Clone)]
pub enum TypedArrayPatElement {
    Pattern(TypedPattern),
    Spread(TypedPattern),
}

#[derive(Debug, Clone)]
pub struct TypedMatchArm {
    pub pattern: TypedPattern,
    pub guard: Option<TypedExpr>,
    pub body: Box<TypedExpr>,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct TypedFunction {
    pub span: Range<usize>,
    pub file: String,
    pub vis: Visibility,
    pub name: Symbol,
    pub function_id: FunctionId,

    pub type_params: Vec<TypedTypeParam>,
    pub args: Vec<TypedFnArg>,
    pub return_type: Rc<Type>,
    pub where_constraints: Vec<TypedConstraint>,
    pub effects: EffectSet,
    pub function_type: Rc<Type>,
    pub body: Option<TypedExpr>,
}

#[derive(Debug, Clone)]
pub struct TypedExternFunction {
    pub span: Range<usize>,
    pub file: String,
    pub vis: Visibility,
    pub name: Symbol,
    pub function_id: FunctionId,

    pub type_params: Vec<TypedTypeParam>,
    pub args: Vec<TypedFnArg>,
    pub return_type: Rc<Type>,
    pub where_constraints: Vec<TypedConstraint>,
    pub effects: EffectSet,
    pub function_type: Rc<Type>,
    pub library: String,             // library name where the function is defined
    pub symbol_name: Option<String>, // optional C symbol name (if different from function name)
}

#[derive(Debug, Clone)]
pub struct TypedTypeParam {
    pub name: Symbol,
    pub var_id: TypeId,
    pub kind: Kind,
    pub bounds: Vec<Rc<Type>>,
}

#[derive(Debug, Clone)]
pub struct TypedFnArg {
    pub span: Range<usize>,
    pub file: String,
    pub name: Symbol,
    pub binding_id: BindingId,
    pub type_: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct TypedConstraint {
    pub span: Range<usize>,
    pub constraint: Constraint,
}

#[derive(Debug, Clone)]
pub struct TypedStruct {
    pub span: Range<usize>,
    pub file: String,
    pub name: Symbol,
    pub struct_id: StructId,
    pub vis: Visibility,
    pub type_params: Vec<TypedTypeParam>,
    pub fields: Vec<(TypedFnArg, Visibility, FieldId)>,
    pub methods: Vec<TypedFunction>,
    pub struct_type: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct TypedEnum {
    pub span: Range<usize>,
    pub file: String,
    pub name: Symbol,
    pub enum_id: EnumId,
    pub vis: Visibility,
    pub type_params: Vec<TypedTypeParam>,
    pub variants: Vec<TypedEnumVariant>,
    pub methods: Vec<TypedFunction>,
    pub enum_type: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct TypedEnumVariant {
    pub span: Range<usize>,
    pub file: String,
    pub name: Symbol,
    pub variant_id: VariantId,
    pub types: Vec<Rc<Type>>,
    pub constraints: Vec<TypedConstraint>,
    pub constructor_type: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct TypedTraitDef {
    pub span: Range<usize>,
    pub name: Symbol,
    pub trait_id: TraitId,
    pub type_params: Vec<TypedTypeParam>,
    pub super_traits: Vec<Rc<Type>>,
    pub methods: Vec<TypedFunction>,
    pub associated_types: Vec<TypedAssociatedType>,
}

#[derive(Debug, Clone)]
pub struct TypedAssociatedType {
    pub name: Symbol,
    pub type_id: TypeId,
    pub bounds: Vec<Rc<Type>>,
}

#[derive(Debug, Clone)]
pub struct TypedEffectDef {
    pub span: Range<usize>,
    pub file: String,
    pub vis: Visibility,
    pub name: Symbol,
    pub effect_id: EffectId,
    pub type_params: Vec<TypedTypeParam>,
    pub operations: Vec<TypedEffectOperation>,
    pub where_constraints: Vec<TypedConstraint>,
}

#[derive(Debug, Clone)]
pub struct TypedEffectOperation {
    pub span: Range<usize>,
    pub name: Symbol,
    pub operation_id: usize,
    pub params: Vec<Rc<Type>>,
    pub return_type: Rc<Type>,
    pub operation_type: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct TypedImport {
    pub span: Range<usize>,
    pub file: String,
    pub path: Vec<Symbol>,
    pub items: Vec<(Symbol, Option<Symbol>, TypeId, Rc<Type>)>,
    pub alias: Option<Symbol>,
}

#[derive(Debug, Clone)]
pub struct TypedImpl {
    pub span: Range<usize>,
    pub file: String,
    pub type_name: Symbol,
    pub type_id: TypeId,
    pub type_params: Vec<TypedTypeParam>,
    pub implementing_type: Rc<Type>,
    pub trait_: Option<(Symbol, TraitId)>,
    pub trait_type: Option<Rc<Type>>,
    pub methods: Vec<TypedFunction>,
    pub where_constraints: Vec<TypedConstraint>,
}

#[derive(Debug, Clone)]
pub struct TypedTypeAlias {
    pub span: Range<usize>,
    pub file: String,
    pub name: Symbol,
    pub alias_id: TypeAliasId,

    pub type_params: Vec<TypedTypeParam>,
    pub target_type: Rc<Type>,
    pub where_constraints: Vec<TypedConstraint>,

    // The fully expanded type (after resolving all aliases)
    pub expanded_type: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct TypedMacroDef {
    pub span: Range<usize>,
    pub file: String,
    pub name: Symbol,
    pub macro_id: MacroId,
    pub rules: Vec<TypedMacroRule>,
    pub hygiene: MacroHygiene,
}

#[derive(Debug, Clone)]
pub struct TypedMacroRule {
    pub span: Range<usize>,
    pub pattern: Vec<TypedMacroToken>,
    pub body: Vec<TypedMacroToken>,

    // hygiene scope for this rule (idk if its needed tho)
    pub scope_id: ScopeId,
}

#[derive(Debug, Clone)]
pub enum TypedMacroToken {
    Ident(Symbol),
    Literal(Literal),
    Punct(String),

    Group {
        delimiter: Delimiter,
        tokens: Vec<TypedMacroToken>,
        scope_id: ScopeId,
    },

    MetaVar {
        name: Symbol,
        kind: MacroVarKind,
        binding_id: BindingId,
    },

    Repeat {
        tokens: Vec<TypedMacroToken>,
        separator: Option<String>,
        kind: RepeatKind,
    },
}

#[derive(Debug, Clone)]
pub enum MacroVarKind {
    Expr,      // $x:expr
    Type,      // $t:ty
    Pattern,   // $p:pat
    Statement, // $s:stmt
    Item,      // $i:item
    Block,     // $b:block
    Ident,     // $i:ident
    Path,      // $p:path
    Literal,   // $l:literal
    Meta,      // $m:meta
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_interner() {
        let mut interner = Interner::new();
        let sym1 = interner.intern("hello");
        let sym2 = interner.intern("world");
        let sym3 = interner.intern("hello");

        assert_eq!(interner.resolve(sym1), "hello");
        assert_eq!(interner.resolve(sym2), "world");
        assert_eq!(sym1, sym3);
    }

    #[test]
    fn test_id_gen() {
        let mut id_gen = IdGen::new();

        let id1 = id_gen.fresh_binding();
        let id2 = id_gen.fresh_binding();
        assert_ne!(id1.0, id2.0);

        let type_id1 = id_gen.fresh_type();
        let type_id2 = id_gen.fresh_type();
        assert_ne!(type_id1.0, type_id2.0);

        let func_id1 = id_gen.fresh_function();
        let func_id2 = id_gen.fresh_function();
        assert_ne!(func_id1.0, func_id2.0);
    }

    #[test]
    fn test_effect_annot() {
        let pure = EffectAnnot::pure();
        assert_eq!(pure.effects.len(), 0);
        assert_eq!(pure.rest, None);

        let closed = EffectAnnot::closed_simple(vec!["IO".to_string()]);
        assert_eq!(closed.effects, vec![("IO".to_string(), vec![])]);
        assert_eq!(closed.rest, None);

        let open = EffectAnnot::open_simple(vec!["State".to_string()], "e".to_string());
        assert_eq!(open.effects, vec![("State".to_string(), vec![])]);
        assert_eq!(open.rest, Some("e".to_string()));
    }

    #[test]
    fn test_kind_annot() {
        let star = KindAnnot::Star;
        assert_eq!(star, KindAnnot::Star);

        let arrow = KindAnnot::Arrow(Box::new(KindAnnot::Star), Box::new(KindAnnot::Star));
        match arrow {
            KindAnnot::Arrow(left, right) => {
                assert_eq!(*left, KindAnnot::Star);
                assert_eq!(*right, KindAnnot::Star);
            }
            _ => panic!("Expected Arrow kind"),
        }
    }
}
