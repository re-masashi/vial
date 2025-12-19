use std::ops::Range;

/// Source location information
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub file: String,
    pub range: Range<usize>,
}

impl Span {
    pub fn new(file: String, range: Range<usize>) -> Self {
        Self { file, range }
    }

    pub fn dummy() -> Self {
        Self {
            file: String::from("<unknown>"),
            range: 0..0,
        }
    }

    /// Merge two spans into a span covering both
    pub fn merge(&self, other: &Span) -> Span {
        Span {
            file: self.file.clone(),
            range: self.range.start.min(other.range.start)..self.range.end.max(other.range.end),
        }
    }
}

/// Attribute applied to declarations, expressions, etc.
#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub span: Span,
    pub name: String,
    pub args: Vec<AttrArg>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttrArg {
    Ident(String),
    Literal(Literal),
    KeyValue { key: String, value: Box<AttrArg> },
    List(Vec<AttrArg>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    // Literals
    Literal(Literal),

    // Identifiers and paths
    Ident(String),
    Path(Path),

    // Variable binding
    Let {
        mode: BindingMode,
        pattern: Pattern,
        type_ann: Option<Type>,
        value: Box<Expr>,
    },

    // Assignment
    Assign {
        target: Box<Expr>,
        op: Option<BinOp>, // None for =, Some for +=, -=, etc.
        value: Box<Expr>,
    },

    // Block expression
    Block {
        exprs: Vec<Expr>,
    },

    // Conditional
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },

    // Pattern matching
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },

    // Loops
    For {
        pattern: Pattern,
        iterator: Box<Expr>,
        body: Box<Expr>,
    },
    While {
        condition: Box<Expr>,
        body: Box<Expr>,
    },

    // Loop control
    Break {
        value: Option<Box<Expr>>,
    },
    Continue,

    // Function-related
    Return {
        value: Option<Box<Expr>>,
    },
    Defer {
        expr: Box<Expr>,
    },
    Lambda {
        params: Vec<FnParam>,
        return_type: Option<Type>,
        body: Box<Expr>,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },

    // Operators
    Unary {
        op: UnOp,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },

    // Pipe operator
    Pipe {
        left: Box<Expr>,
        right: Box<Expr>,
    },

    // Range
    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        inclusive: bool, // false for .., true for ..=
    },

    // Field access
    Field {
        base: Box<Expr>,
        field: String,
    },

    // Index
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },

    // Array literal
    Array {
        elements: Vec<Expr>,
    },

    // Struct construction
    Struct {
        path: Path,
        fields: Vec<StructField>,
        spread: Option<Box<Expr>>, // ..other
    },

    // Map literal
    Map {
        entries: Vec<(Expr, Expr)>,
    },

    // Tuple (represented by unit or multi-element)
    Tuple {
        elements: Vec<Expr>,
    },

    // Enum variant construction
    EnumVariant {
        path: Path,
        args: Vec<Expr>,
    },

    // Type annotation
    Typed {
        expr: Box<Expr>,
        ty: Type,
    },

    // Concurrency
    Spawn {
        expr: Box<Expr>,
    },
    Select {
        arms: Vec<SelectArm>,
    },

    // Compile-time evaluation
    Comptime {
        expr: Box<Expr>,
    },

    // Runtime evaluation
    Runtime {
        runtime: Box<Expr>,
        body: Box<Expr>,
    },

    // Macro invocation
    MacroCall {
        name: String,
        args: Vec<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub pattern: Pattern,
    pub guard: Option<Box<Expr>>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelectArm {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub kind: SelectArmKind,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SelectArmKind {
    Recv {
        channel: Expr,
        binding: Option<String>,
    },
    Send {
        channel: Expr,
        value: Expr,
    },
    After {
        duration: Expr,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub span: Span,
    pub name: Option<String>, // None for positional
    pub value: Expr,
}

// PATTERNS

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub kind: PatternKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    // Wildcard
    Wildcard,

    // Literal pattern
    Literal(Literal),

    // Identifier binding
    Ident {
        name: String,
        mutable: bool,
    },

    // Path (for enum variants, etc.)
    Path(Path),

    // Struct pattern
    Struct {
        path: Path,
        fields: Vec<FieldPattern>,
        rest: bool, // .. rest
    },

    // Tuple pattern
    Tuple {
        elements: Vec<Pattern>,
    },

    // Enum variant pattern
    EnumVariant {
        path: Path,
        args: Vec<Pattern>,
    },

    // Array pattern
    Array {
        elements: Vec<Pattern>,
        rest: Option<Box<Pattern>>, // ..rest
    },

    // Or pattern
    Or {
        patterns: Vec<Pattern>,
    },

    // Type annotation
    Typed {
        pattern: Box<Pattern>,
        ty: Type,
    },

    // Range pattern
    Range {
        start: Box<Literal>,
        end: Box<Literal>,
        inclusive: bool,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldPattern {
    pub span: Span,
    pub name: String,
    pub pattern: Option<Pattern>, // None for shorthand
}

// TYPES

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    // Path (for named types)
    Path(Path),

    // Primitive types
    Int(IntType),
    Float(FloatType),
    Bool,
    Char,
    String,
    Unit,
    Never, // !

    // Array
    Array {
        element: Box<Type>,
        size: Option<Box<Expr>>, // None for unsized
    },

    // Function type
    Fn {
        params: Vec<Type>,
        return_type: Box<Type>,
    },

    // Reference
    Ref {
        mutable: bool,
        ty: Box<Type>,
    },

    // Tuple
    Tuple {
        elements: Vec<Type>,
    },

    // Generic application
    Generic {
        base: Box<Type>,
        args: Vec<Type>,
    },

    // Map type
    Map {
        key: Box<Type>,
        value: Box<Type>,
    },

    // Range type
    Range {
        element: Box<Type>,
        inclusive: bool,
    },

    // Inferred type
    Infer,

    // Higher-kinded type parameter (F<_>)
    HigherKinded {
        name: String,
        arity: usize,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
    Int,
    U8,
    U16,
    U32,
    U64,
    Uint,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatType {
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    pub span: Span,
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathSegment {
    pub span: Span,
    pub ident: String,
    pub generics: Option<Vec<Type>>,
}

// DECLARATIONS

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub visibility: Visibility,
    pub kind: ItemKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    // Function declaration
    Fn(FnDecl),

    // Struct declaration
    Struct(StructDecl),

    // Enum declaration
    Enum(EnumDecl),

    // Type alias
    TypeAlias(TypeAliasDecl),

    // Trait declaration
    Trait(TraitDecl),

    // Trait implementation
    Impl(ImplDecl),

    // Constant
    Const {
        name: String,
        ty: Option<Type>,
        value: Expr,
    },

    // Use/import
    Use(UseDecl),

    // Macro definition
    Macro(MacroDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    pub span: Span,
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub params: Vec<FnParam>,
    pub return_type: Option<Type>,
    pub body: Option<Expr>, // None for trait method declarations
    pub is_comptime: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnParam {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub pattern: Pattern,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub span: Span,
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub fields: Vec<StructFieldDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldDecl {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub visibility: Visibility,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub span: Span,
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub variants: Vec<EnumVariant>,
    pub is_gadt: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub name: String,
    pub fields: Vec<Type>,
    pub gadt_return: Option<Type>, // For GADT syntax: Variant(args) -> Type
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasDecl {
    pub span: Span,
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitDecl {
    pub span: Span,
    pub name: String,
    pub generics: Vec<GenericParam>,
    pub supertraits: Vec<Path>,
    pub associated_types: Vec<AssociatedType>,
    pub methods: Vec<FnDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssociatedType {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub name: String,
    pub bounds: Vec<Path>,
    pub default: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplDecl {
    pub span: Span,
    pub generics: Vec<GenericParam>,
    pub trait_path: Option<Path>, // None for inherent impl
    pub self_ty: Type,
    pub items: Vec<ImplItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplItem {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub visibility: Visibility,
    pub kind: ImplItemKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImplItemKind {
    Fn(FnDecl),
    Type { name: String, ty: Type },
    Const { name: String, ty: Type, value: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseDecl {
    pub span: Span,
    pub path: String, // String-based import
    pub alias: Option<String>,
    pub items: UseItems,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UseItems {
    All,                   // .*
    Single(String),        // .name
    Multiple(Vec<String>), // .{name1, name2}
    None,                  // use "path"
}

#[derive(Debug, Clone, PartialEq)]
pub struct MacroDecl {
    pub span: Span,
    pub name: String,
    pub params: Vec<String>,
    pub body: Expr,
}

// GENERICS AND CONSTRAINTS

#[derive(Debug, Clone, PartialEq)]
pub struct GenericParam {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub kind: GenericParamKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericParamKind {
    // Type parameter
    Type {
        name: String,
        bounds: Vec<Path>,
        default: Option<Type>,
    },

    // Lifetime parameter (for future use)
    Lifetime {
        name: String,
    },

    // Const parameter (for future use)
    Const {
        name: String,
        ty: Type,
    },

    // Higher-kinded type parameter
    HigherKinded {
        name: String,
        arity: usize,
    },
}

// LITERALS

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int {
        value: String,
        suffix: Option<IntType>,
    },
    Float {
        value: String,
        suffix: Option<FloatType>,
    },
    Bool(bool),
    Char(String),   // Unescaped character literal (escape sequences processed by lexer)
    String(String), // Unescaped string (escape sequences processed by lexer, interpolations still preserved as #{...})
    Unit,
}

// OPERATORS

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,    // -
    Not,    // not
    BitNot, // ~
    Ref,    // &
    RefMut, // &mut
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Rem, // %

    // Comparison
    Eq, // ==
    Ne, // !=
    Lt, // <
    Le, // <=
    Gt, // >
    Ge, // >=

    // Logical
    And, // and
    Or,  // or

    // Bitwise
    BitAnd, // &
    BitOr,  // |
    BitXor, // ^
    Shl,    // <<
    Shr,    // >>
}

// VISIBILITY

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

// BINDING MODES

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingMode {
    Immutable, // let
    Mutable,   // let mut
    Once,      // let once
    Unique,    // let uniq
}

// MODULE

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub span: Span,
    pub file: String,
    pub items: Vec<Item>,
}

// CONVENIENCE CONSTRUCTORS

impl Expr {
    pub fn new(span: Span, kind: ExprKind) -> Self {
        Self {
            span,
            attributes: Vec::new(),
            kind,
        }
    }

    pub fn with_attributes(mut self, attributes: Vec<Attribute>) -> Self {
        self.attributes = attributes;
        self
    }
}

impl Pattern {
    pub fn new(span: Span, kind: PatternKind) -> Self {
        Self {
            span,
            attributes: Vec::new(),
            kind,
        }
    }

    pub fn with_attributes(mut self, attributes: Vec<Attribute>) -> Self {
        self.attributes = attributes;
        self
    }
}

impl Type {
    pub fn new(span: Span, kind: TypeKind) -> Self {
        Self { span, kind }
    }
}

impl Item {
    pub fn new(span: Span, visibility: Visibility, kind: ItemKind) -> Self {
        Self {
            span,
            attributes: Vec::new(),
            visibility,
            kind,
        }
    }

    pub fn with_attributes(mut self, attributes: Vec<Attribute>) -> Self {
        self.attributes = attributes;
        self
    }
}

// VISITOR PATTERN (for traversal)
pub trait Visitor: Sized {
    fn visit_module(&mut self, module: &Module) {
        walk_module(self, module);
    }

    fn visit_item(&mut self, item: &Item) {
        walk_item(self, item);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }

    fn visit_pattern(&mut self, pattern: &Pattern) {
        walk_pattern(self, pattern);
    }

    fn visit_type(&mut self, ty: &Type) {
        walk_type(self, ty);
    }
}

pub fn walk_module<V: Visitor>(visitor: &mut V, module: &Module) {
    for item in &module.items {
        visitor.visit_item(item);
    }
}

pub fn walk_item<V: Visitor>(visitor: &mut V, item: &Item) {
    match &item.kind {
        ItemKind::Fn(fn_decl) => {
            for param in &fn_decl.params {
                visitor.visit_pattern(&param.pattern);
                visitor.visit_type(&param.ty);
            }
            if let Some(ret_ty) = &fn_decl.return_type {
                visitor.visit_type(ret_ty);
            }
            if let Some(body) = &fn_decl.body {
                visitor.visit_expr(body);
            }
        }
        ItemKind::Struct(struct_decl) => {
            for field in &struct_decl.fields {
                visitor.visit_type(&field.ty);
            }
        }
        ItemKind::Enum(enum_decl) => {
            for variant in &enum_decl.variants {
                for field_ty in &variant.fields {
                    visitor.visit_type(field_ty);
                }
                if let Some(ret_ty) = &variant.gadt_return {
                    visitor.visit_type(ret_ty);
                }
            }
        }
        ItemKind::TypeAlias(alias) => {
            visitor.visit_type(&alias.ty);
        }
        ItemKind::Trait(trait_decl) => {
            for method in &trait_decl.methods {
                if let Some(body) = &method.body {
                    visitor.visit_expr(body);
                }
            }
        }
        ItemKind::Impl(impl_decl) => {
            visitor.visit_type(&impl_decl.self_ty);
            for impl_item in &impl_decl.items {
                match &impl_item.kind {
                    ImplItemKind::Fn(fn_decl) => {
                        if let Some(body) = &fn_decl.body {
                            visitor.visit_expr(body);
                        }
                    }
                    ImplItemKind::Type { ty, .. } => {
                        visitor.visit_type(ty);
                    }
                    ImplItemKind::Const { ty, value, .. } => {
                        visitor.visit_type(ty);
                        visitor.visit_expr(value);
                    }
                }
            }
        }
        ItemKind::Const { ty, value, .. } => {
            if let Some(ty) = ty {
                visitor.visit_type(ty);
            }
            visitor.visit_expr(value);
        }
        ItemKind::Use(_) => {}
        ItemKind::Macro(macro_decl) => {
            visitor.visit_expr(&macro_decl.body);
        }
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) {
    match &expr.kind {
        ExprKind::Literal(_) | ExprKind::Ident(_) => {}
        ExprKind::Path(_) => {}
        ExprKind::Let {
            pattern,
            type_ann,
            value,
            ..
        } => {
            visitor.visit_pattern(pattern);
            if let Some(ty) = type_ann {
                visitor.visit_type(ty);
            }
            visitor.visit_expr(value);
        }
        ExprKind::Assign { target, value, .. } => {
            visitor.visit_expr(target);
            visitor.visit_expr(value);
        }
        ExprKind::Block { exprs, .. } => {
            for e in exprs {
                visitor.visit_expr(e);
            }
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            visitor.visit_expr(condition);
            visitor.visit_expr(then_branch);
            if let Some(else_br) = else_branch {
                visitor.visit_expr(else_br);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            visitor.visit_expr(scrutinee);
            for arm in arms {
                visitor.visit_pattern(&arm.pattern);
                if let Some(guard) = &arm.guard {
                    visitor.visit_expr(guard);
                }
                visitor.visit_expr(&arm.body);
            }
        }
        ExprKind::For {
            pattern,
            iterator,
            body,
        } => {
            visitor.visit_pattern(pattern);
            visitor.visit_expr(iterator);
            visitor.visit_expr(body);
        }
        ExprKind::While { condition, body } => {
            visitor.visit_expr(condition);
            visitor.visit_expr(body);
        }
        ExprKind::Break { value } | ExprKind::Return { value } => {
            if let Some(v) = value {
                visitor.visit_expr(v);
            }
        }
        ExprKind::Defer { expr: inner } => {
            visitor.visit_expr(inner);
        }
        ExprKind::Continue => {}
        ExprKind::Lambda {
            params,
            return_type,
            body,
        } => {
            for param in params {
                visitor.visit_pattern(&param.pattern);
                visitor.visit_type(&param.ty);
            }
            if let Some(ret_ty) = return_type {
                visitor.visit_type(ret_ty);
            }
            visitor.visit_expr(body);
        }
        ExprKind::Call { func, args } => {
            visitor.visit_expr(func);
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        ExprKind::Unary { expr: inner, .. } => {
            visitor.visit_expr(inner);
        }
        ExprKind::Binary { left, right, .. } | ExprKind::Pipe { left, right } => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        ExprKind::Range { start, end, .. } => {
            if let Some(s) = start {
                visitor.visit_expr(s);
            }
            if let Some(e) = end {
                visitor.visit_expr(e);
            }
        }
        ExprKind::Field { base, .. }
        | ExprKind::Spawn { expr: base }
        | ExprKind::Comptime { expr: base } => {
            visitor.visit_expr(base);
        }
        ExprKind::Runtime { runtime, body } => {
            visitor.visit_expr(runtime);
            visitor.visit_expr(body);
        }
        ExprKind::Index { base, index } => {
            visitor.visit_expr(base);
            visitor.visit_expr(index);
        }
        ExprKind::Array { elements } | ExprKind::Tuple { elements } => {
            for elem in elements {
                visitor.visit_expr(elem);
            }
        }
        ExprKind::Struct { fields, spread, .. } => {
            for field in fields {
                visitor.visit_expr(&field.value);
            }
            if let Some(spr) = spread {
                visitor.visit_expr(spr);
            }
        }
        ExprKind::Map { entries } => {
            for (k, v) in entries {
                visitor.visit_expr(k);
                visitor.visit_expr(v);
            }
        }
        ExprKind::EnumVariant { args, .. } => {
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        ExprKind::Typed { expr: inner, ty } => {
            visitor.visit_expr(inner);
            visitor.visit_type(ty);
        }
        ExprKind::Select { arms } => {
            for arm in arms {
                match &arm.kind {
                    SelectArmKind::Recv { channel, .. } => visitor.visit_expr(channel),
                    SelectArmKind::Send { channel, value } => {
                        visitor.visit_expr(channel);
                        visitor.visit_expr(value);
                    }
                    SelectArmKind::After { duration } => visitor.visit_expr(duration),
                }
                visitor.visit_expr(&arm.body);
            }
        }
        ExprKind::MacroCall { args, .. } => {
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
    }
}

pub fn walk_pattern<V: Visitor>(visitor: &mut V, pattern: &Pattern) {
    match &pattern.kind {
        PatternKind::Wildcard
        | PatternKind::Literal(_)
        | PatternKind::Ident { .. }
        | PatternKind::Path(_) => {}
        PatternKind::Struct { fields, .. } => {
            for field in fields {
                if let Some(pat) = &field.pattern {
                    visitor.visit_pattern(pat);
                }
            }
        }
        PatternKind::Tuple { elements } | PatternKind::Or { patterns: elements } => {
            for elem in elements {
                visitor.visit_pattern(elem);
            }
        }
        PatternKind::EnumVariant { args, .. } => {
            for arg in args {
                visitor.visit_pattern(arg);
            }
        }
        PatternKind::Array { elements, rest } => {
            for elem in elements {
                visitor.visit_pattern(elem);
            }
            if let Some(r) = rest {
                visitor.visit_pattern(r);
            }
        }
        PatternKind::Typed { pattern: inner, ty } => {
            visitor.visit_pattern(inner);
            visitor.visit_type(ty);
        }
        PatternKind::Range { .. } => {}
    }
}

pub fn walk_type<V: Visitor>(visitor: &mut V, ty: &Type) {
    match &ty.kind {
        TypeKind::Path(_)
        | TypeKind::Int(_)
        | TypeKind::Float(_)
        | TypeKind::Bool
        | TypeKind::Char
        | TypeKind::String
        | TypeKind::Unit
        | TypeKind::Never
        | TypeKind::Infer
        | TypeKind::HigherKinded { .. } => {}
        TypeKind::Array { element, size } => {
            visitor.visit_type(element);
            if let Some(s) = size {
                visitor.visit_expr(s);
            }
        }
        TypeKind::Fn {
            params,
            return_type,
        } => {
            for param in params {
                visitor.visit_type(param);
            }
            visitor.visit_type(return_type);
        }
        TypeKind::Ref { ty: inner, .. } => {
            visitor.visit_type(inner);
        }
        TypeKind::Tuple { elements } => {
            for elem in elements {
                visitor.visit_type(elem);
            }
        }
        TypeKind::Generic { base, args } => {
            visitor.visit_type(base);
            for arg in args {
                visitor.visit_type(arg);
            }
        }
        TypeKind::Map { key, value } => {
            visitor.visit_type(key);
            visitor.visit_type(value);
        }
        TypeKind::Range { element, .. } => {
            visitor.visit_type(element);
        }
    }
}
