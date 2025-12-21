Type System:

Algebraic Data Types (sum types, product types)
Pattern matching (exhaustive)
Higher-kinded types (System Fω)
Type classes/traits
Type inference
Structural types for records/JSON
Row polymorphism
Generics with constraints
Type aliases
Newtype wrappers
Constants

Memory & Resources:

Defer for automatic cleanup
Move semantics by default
Explicit borrowing (&T)
Mutable vs immutable (let vs let mut)
GenImmix Garbage collection
Compiler-enforced resource safety (minimal linear/affine types)

Concurrency:

Green threads
Actors with message passing
Supervision trees
Channels
Parallel collections
Select/receive for multi-channel
No async/await (no function coloring)

Error Handling:

Result type
Option type
? operator for propagation
No exceptions
Exhaustive match on errors

Metaprogramming:

Comptime code execution
Comptime expressions (comptime keyword)
Hygienic macros
Type reflection at comptime
Code generation via quote/unquote
Custom derive macros
Conditional compilation
Mixins
Attributes (eg: @doc("aaaaaaa"), @serde(name="myField")

Standard Library:

Immutable collections (List, Map, Set)
String, Int, Float, Bool primitives
Json first-class support
Result and Option utilities
Iterator protocol
Ranges

Web-Specific:

HTTP client/server built-in
WebSocket support
Database connection pooling
SQL query builder
JSON serialization/deserialization
Routing DSL
Template/view DSL (like LiveView)
Hot reload for development

Language Features:

Pipeline operator |>
String interpolation
Multiline strings
Unicode support
Module system
Visibility modifiers (pub/private)
Documentation comments
Unit tests integrated

Performance:

Compiles to native or WASM
Monomorphization
Zero-cost abstractions :joy_cat:
SIMD support
Tail call optimization

Developer Experience:

Fast compile times
Incremental compilation
Clear error messages with suggestions
LSP support (Language Server Protocol)
Formatter
Linter
Package manager

Type-Level Programming:

Type-level functions (limited, atleast)
Associated types
Type families (basic)
Phantom types
GADTs (maybe v2)

