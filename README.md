# Vial Programming Language

Vial is a modern, functional programming language with advanced type system features, including algebraic effects, higher-kinded types, and dependent types. It's designed to provide powerful abstraction capabilities while maintaining type safety and performance.

## Features

- **Algebraic Effects**: First-class support for algebraic effects and handlers for structured control flow
- **Higher-Kinded Types (HKT)**: Advanced type system with higher-kinded type support
- **Dependent Types**: Experimental support for dependent types
- **Advanced Type System**: Rich type system with generics, traits, and type constraints
- **Macros**: Extensible syntax with hygienic macro system
- **Pattern Matching**: Comprehensive pattern matching with guards
- **Effectful Programming**: Built-in effect system for handling side effects in a pure way

## Installation

### Prerequisites

- Rust 1.70+ (with Cargo)
- Git

### Building from Source

```bash
# Clone the repository
git clone https://github.com/your-username/vial.git
cd vial

# Build the project
cargo build --release

# Run the project
cargo run -- examples/basic.ni
```

## Usage

### Running Vial Programs

```bash
# Basic usage
cargo run -- <file.ni>

# Examples
cargo run -- examples/basic.ni
cargo run -- examples/example.ni
cargo run -- examples/fancy.ni
```

### Debug Mode

```bash
# With debug output
cargo run -- --debug examples/basic.ni
# or
cargo run -- -d examples/basic.ni
```

## Language Syntax

Vial supports a rich syntax for functional programming with the following constructs:

- Functions with type annotations
- Algebraic data types (structs, enums)
- Pattern matching
- Higher-order functions
- Algebraic effects and handlers
- Type classes (traits)
- Macros

For detailed syntax documentation, see [docs/syntax.md](docs/syntax.md).

## Example Program

Here's a simple example of a Vial program:

```vial
// Generic identity function
def id<T>(x: T) -> T {
    x
}

// Function application
id(1)
id(true)

// Generic function with type parameter
def identity<T>(x: T) -> T {
    x
}

identity(0)
identity("hi")
identity(false)

// Simple arithmetic function
def add(a: int, b: int) -> int {
    a + b
}

let result = add(1, 2)
```

## Project Structure

```
vial/
├── src/
│   ├── ast/          # Abstract Syntax Tree definitions
│   ├── lexer/        # Lexical analysis
│   ├── parser/       # Syntax parsing
│   ├── typechecker/  # Type checking and inference
│   ├── desugar/      # Desugaring transformations
│   ├── validation/   # AST validation
│   ├── hir/          # High-level Intermediate Representation
│   ├── main.rs       # Main entry point
│   └── lib.rs        # Library exports
├── examples/         # Example Vial programs
├── grammar/          # Grammar definition files
├── docs/             # Documentation (to be created)
├── Cargo.toml        # Project manifest
└── README.md         # This file
```

## Compilation Pipeline

The Vial compiler processes source code through several stages:

1. **Lexical Analysis**: Convert source code to tokens
2. **Parsing**: Build Abstract Syntax Tree (AST)
3. **Validation**: Check AST structure and imports
4. **Type Checking**: Verify type safety and infer types
5. **Desugaring**: Transform complex constructs to simpler forms
6. **Monomorphization**: Generate specialized code for generic functions
7. **Typed Validation**: Final validation of typed AST
8. **Code Generation**: (TODO) Generate intermediate representation
9. **Optimization**: (TODO) Optimize generated code
10. **Code Generation**: (TODO) Generate target code

For more details on the compilation pipeline, see [docs/pipeline.md](docs/pipeline.md).

## AST Structure

The Abstract Syntax Tree represents the syntactic structure of Vial programs. Key components include:

- Expressions (literals, variables, operations, function calls, etc.)
- Declarations (functions, structs, enums, traits)
- Type annotations and constraints
- Pattern matching constructs
- Effect definitions and handlers

For detailed AST documentation, see [docs/ast.md](docs/ast.md).

## Type System

Vial features a sophisticated type system with:

- Algebraic data types
- Higher-kinded types
- Type classes (traits)
- Effect types
- Dependent types (experimental)

For more information about the type system, see [docs/types.md](docs/types.md).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on contributing to the Vial project.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

Vial draws inspiration from several functional programming languages, including:

- ML family languages (OCaml, F#)
- Haskell (effects, type classes)
- Rust (ownership concepts)
- Modern research languages with algebraic effects