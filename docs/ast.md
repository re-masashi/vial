# Abstract Syntax Tree (AST) Structure

The Abstract Syntax Tree (AST) is a tree representation of the syntactic structure of Vial source code. This document describes the various components of the AST and how they represent different language constructs.

## Overview

The AST is defined in `src/ast/mod.rs` and consists of several key components:

- `ASTNode`: The base unit of the AST with span information and attributes
- `ASTNodeKind`: The different types of AST nodes
- Expression types: Represent expressions in the language
- Declaration types: Represent declarations like functions, structs, enums, etc.
- Type annotations: Represent type information

## Core AST Components

### ASTNode

The `ASTNode` is the fundamental building block of the AST:

```rust
pub struct ASTNode {
    pub span: Range<usize>,          // Source code location
    pub file: String,                // Source file name
    pub node: ASTNodeKind,           // The actual node content
    pub attributes: Vec<Attribute>,  // Node attributes
}
```

### ASTNodeKind

The `ASTNodeKind` enum represents different types of AST nodes:

- `Expr(Expr)`: Expression nodes
- `Function(Box<Function>)`: Function declarations
- `Struct(Struct)`: Struct declarations
- `Enum(Enum)`: Enum declarations
- `TypeAlias(TypeAlias)`: Type alias declarations
- `Impl(Impl)`: Implementation blocks
- `Trait(TraitDef)`: Trait definitions
- `MacroDef(MacroDef)`: Macro definitions
- `EffectDef(EffectDef)`: Effect definitions
- `Error`: Error nodes

## Expression Types

The `ExprKind` enum represents various expression types in Vial:

### Literals
- `Int(i64)`: Integer literals
- `Float(f64)`: Floating-point literals
- `Bool(bool)`: Boolean literals
- `String(String)`: String literals

### Variables and Operations
- `Variable(String)`: Variable references
- `BinOp(Box<Expr>, BinOp, Box<Expr>)`: Binary operations
- `UnOp(UnOp, Box<Expr>)`: Unary operations
- `Assign { l_val, r_val, op }`: Assignment operations

### Control Flow
- `IfElse { condition, then, else_ }`: Conditional expressions
- `Match(Box<Expr>, Vec<MatchArm>)`: Pattern matching
- `Block(Vec<Expr>)`: Expression blocks
- `Loop { label, body }`: Loop expressions
- `While(Box<Expr>, Box<Expr>)`: While loops
- `For { iterator, value, expression }`: For loops

### Functions and Calls
- `Lambda { args, expression }`: Anonymous functions
- `Call(Box<Expr>, Vec<Expr>)`: Function calls
- `Let { var, type_annot, value }`: Variable bindings

### Data Structures
- `Array(Vec<Expr>)`: Array literals
- `Tuple(Vec<Expr>)`: Tuple literals
- `Map(Vec<(Expr, Expr)>)`: Map literals
- `StructConstruct { name, fields }`: Struct construction
- `EnumConstruct { name, variant, args }`: Enum variant construction

### Effects
- `Perform { effect, args }`: Effect operations
- `Handle { body, handlers }`: Effect handlers

### Advanced Constructs
- `FieldAccess(Box<Expr>, String)`: Field access (e.g., `obj.field`)
- `Index(Box<Expr>, Box<Expr>)`: Indexing (e.g., `arr[index]`)
- `Cast { expr, target_type }`: Type casting
- `Return(Option<Box<Expr>>)`: Return statements
- `Break(Option<Box<Expr>>)`: Break statements
- `Continue`: Continue statements

## Declaration Types

### Function Declarations

```rust
pub struct Function {
    pub span: Range<usize>,
    pub file: String,
    pub vis: Visibility,           // Public/Private
    pub name: String,              // Function name
    pub type_params: Vec<TypeParam>, // Generic type parameters
    pub args: Vec<FnArg>,          // Function arguments
    pub return_type: Option<TypeAnnot>, // Return type annotation
    pub where_constraints: Vec<TypeConstraint>, // Where clauses
    pub effects: EffectAnnot,      // Effect annotations
    pub body: Option<Expr>,        // Function body
}
```

### Struct Declarations

```rust
pub struct Struct {
    pub span: Range<usize>,
    pub file: String,
    pub name: String,
    pub type_params: Vec<TypeParam>, // Generic type parameters
    pub fields: Vec<(FnArg, Visibility)>, // Fields with visibility
    pub methods: Vec<Function>,    // Associated methods
    pub vis: Visibility,           // Struct visibility
}
```

### Enum Declarations

```rust
pub struct Enum {
    pub span: Range<usize>,
    pub file: String,
    pub name: String,
    pub type_params: Vec<TypeParam>, // Generic type parameters
    pub variants: Vec<EnumVariant>, // Enum variants
    pub methods: Vec<Function>,    // Associated methods
    pub vis: Visibility,           // Enum visibility
}
```

## Type Annotations

Vial supports rich type annotations:

### TypeAnnotKind

- `Bool`, `Int`, `Float`, `String`, `Unit`, `Never`: Basic types
- `Named(String)`: Named types
- `Generic { name, args, kind }`: Generic types like `List<T>`
- `Function { params, return_type, effects }`: Function types with effects
- `Tuple(Vec<TypeAnnot>)`: Tuple types
- `Union(Vec<TypeAnnot>)`: Union types
- `Trait(Vec<String>)`: Trait bounds
- `Constructor { name, kind }`: Type constructors for HKTs
- `Variable { name, kind }`: Type variables

### Effect Annotations

Effects in Vial are tracked in the type system:

```rust
pub struct EffectAnnot {
    pub effects: Vec<String>,  // Specific effects (IO, State, etc.)
    pub rest: Option<String>,  // Effect variable for open effect sets
}
```

## Pattern Matching

Pattern matching is a core feature of Vial:

### Pattern Types

- `Wildcard`: `_` pattern
- `Bind(String)`: Variable binding patterns
- `Literal(Literal)`: Literal patterns
- `Array(Vec<Pattern>)`: Array patterns
- `Tuple(Vec<Pattern>)`: Tuple patterns
- `Or(Vec<Pattern>)`: Or patterns
- `As { name, pattern }`: As patterns
- `Struct { name, fields }`: Struct destructuring
- `Enum { name, variant, params }`: Enum variant patterns
- `Range(Box<Expr>, Box<Expr>)`: Range patterns
- `Rest(String)`: Rest patterns

## Attributes

AST nodes can have attributes for metadata:

```rust
pub struct Attribute {
    pub span: Range<usize>,
    pub name: String,              // Attribute name
    pub args: Vec<AttributeArg>,   // Attribute arguments
}
```

## Identifier Generation

Vial uses unique identifiers for various AST components:

- `Symbol`: String interning for efficient string handling
- `BindingId`: Identifiers for variable bindings
- `TypeId`: Identifiers for types
- `FunctionId`: Identifiers for functions
- `StructId`: Identifiers for structs
- `EnumId`: Identifiers for enums
- `TraitId`: Identifiers for traits
- `EffectId`: Identifiers for effects
- `VariantId`: Identifiers for enum variants
- `FieldId`: Identifiers for struct fields
- `MacroId`: Identifiers for macros
- `TypeAliasId`: Identifiers for type aliases
- `ScopeId`: Identifiers for scopes

## Typed AST

After type checking, Vial generates a Typed AST with additional type information:

- `TypedASTNode`: Typed version of AST nodes
- `TypedExpr`: Typed expressions with type annotations
- `TypedFunction`: Typed functions with resolved types
- `TypedStruct`: Typed structs with resolved types
- And so on for all other constructs

## Key Design Principles

1. **Span Information**: Every AST node contains span information for accurate error reporting
2. **Type Safety**: The AST structure enforces type safety at the syntax level
3. **Extensibility**: The design allows for easy addition of new language constructs
4. **Performance**: Efficient representation using enums and structs
5. **Error Handling**: Special error nodes for graceful error recovery

## Usage in the Compiler Pipeline

The AST is used throughout the compiler pipeline:

1. **Parsing**: Generated from source code tokens
2. **Validation**: Checked for structural correctness
3. **Type Checking**: Annotated with type information
4. **Desugaring**: Transformed to simpler constructs
5. **Monomorphization**: Generic instances generated
6. **Code Generation**: Used to generate target code

This structure allows Vial to maintain a clean separation between syntax and semantics while supporting advanced type system features.