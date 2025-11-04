# Compilation Pipeline

The Vial compiler processes source code through several distinct phases to transform human-readable Vial code into executable form. This document details each stage of the compilation pipeline.

## Overview

The Vial compilation pipeline consists of the following stages:

1. **Lexical Analysis** - Tokenization of source code
2. **Parsing** - Construction of Abstract Syntax Tree (AST)
3. **Validation** - Structural and import validation
4. **Type Checking** - Type safety verification and inference
5. **Desugaring** - Transformation of complex constructs
6. **Monomorphization** - Generation of specialized code
7. **Typed Validation** - Final validation of typed AST
8. **Code Generation** - (Planned) Generation of intermediate representation
9. **Optimization** - (Planned) Code optimization
10. **Final Code Generation** - (Planned) Target code generation

## Stage 1: Lexical Analysis

The lexer, implemented in `src/lexer/`, converts source code characters into tokens. It uses the `logos` crate for efficient tokenization.

### Components

- **Token definitions**: Defined in `src/lexer/` using the `logos` derive macro
- **Lexer function**: Tokenizes input source code string
- **Span tracking**: Maintains position information for error reporting

### Token Types

The lexer recognizes various token types including:
- Keywords (`fn`, `let`, `if`, `else`, `match`, `effect`, `handle`, etc.)
- Identifiers
- Literals (integers, floats, strings, booleans)
- Operators (`+`, `-`, `*`, `/`, `=`, `==`, `!=`, etc.)
- Delimiters (`()`, `[]`, `{}`, ``, ``, ``, etc.)

### Error Handling

Lexical errors are reported with precise location information to aid debugging.

## Stage 2: Parsing

The parser, implemented in `src/parser/`, uses the `chumsky` parsing library to build an Abstract Syntax Tree (AST) from the token stream.

### Parsing Strategy

- **Recursive descent with Pratt parsing**: For expression parsing
- **Combinator-based parsing**: Using chumsky's combinator approach
- **Error recovery**: Designed to continue parsing after errors to provide multiple error messages

### AST Construction

The parser builds `ASTNode` structures as defined in `src/ast/mod.rs`, creating a hierarchical representation of the program structure.

### Error Reporting

Parse errors are reported using the `ariadne` crate, providing colorful and informative error messages with source code context.

## Stage 3: Validation

The validation stage, implemented in `src/validation/`, performs structural and semantic checks on the untyped AST.

### Untyped Validation

- **Import validation**: Checks that imported modules exist and are accessible
- **Reference validation**: Ensures variables are declared before use
- **Cycle detection**: Prevents circular dependencies
- **Scope validation**: Verifies variable scoping rules

### File Resolution

The validator resolves file paths and manages module dependencies across the project.

## Stage 4: Type Checking

The type checker, implemented in `src/typechecker/`, verifies type safety and performs type inference.

### Type Inference

- **Hindley-Milner style inference**: With extensions for effects
- **Constraint generation**: Creates constraints between types
- **Unification**: Solves type constraints
- **Effect tracking**: Tracks which effects functions may perform

### Type Safety

- **Type checking**: Verifies expressions match expected types
- **Effect checking**: Ensures effect safety
- **Trait resolution**: Finds appropriate trait implementations
- **Kind checking**: Verifies type constructor kinds

### Error Reporting

Provides detailed type error messages with source context using the `ariadne` crate.

## Stage 5: Desugaring

The desugaring stage, implemented in `src/desugar/`, transforms complex language constructs into simpler, more fundamental forms.

### Desugaring Transformations

- **Lambda lifting**: Moves nested functions to top level
- **Pattern compilation**: Converts pattern matching to case trees
- **Syntactic sugar removal**: Converts syntactic sugar to core constructs
- **Macro expansion**: Expands macro definitions

### Lambda Desugaring

The lambda desugaring specifically handles closure conversion and function representation.

## Stage 6: Monomorphization

The monomorphization stage, implemented in `src/typechecker/monomorphizer.rs`, generates specialized versions of generic functions.

### Monomorphization Process

- **Generic instantiation**: Creates specialized copies of generic functions
- **Type specialization**: Replaces generic types with concrete types
- **Code duplication**: Generates separate code for each type instantiation

### Benefits

- **Performance**: Eliminates generic overhead
- **Optimization**: Enables type-specific optimizations
- **Type safety**: Ensures type safety at runtime

## Stage 7: Typed Validation

The typed validation stage performs final checks on the fully typed AST.

### Validation Checks

- **Typed reference validation**: Ensures typed references are valid
- **Effect consistency**: Verifies effect annotations are consistent
- **Type consistency**: Checks for type system violations

## Stage 8: Code Generation (Planned)

Future stages will include:

### High-Level IR Generation

- **Vial IR**: An intermediate representation for Vial-specific optimizations
- **Closure conversion**: Proper handling of closures
- **Tail call optimization**: Optimization of tail recursive calls

### Optimization Passes

- **Common subexpression elimination**
- **Dead code elimination**
- **Constant folding and propagation**
- **Loop optimizations**

### Target Code Generation

- **Bytecode generation**: For a Vial virtual machine
- **Native code generation**: Direct compilation to machine code
- **LLVM integration**: Using LLVM for optimization and code generation

## Error Handling Throughout the Pipeline

### Error Accumulation

Each stage accumulates errors rather than stopping at the first error, providing users with comprehensive feedback.

### Error Recovery

The pipeline is designed to continue processing after errors to provide as much feedback as possible.

### Diagnostic Reporting

Errors and warnings are formatted using the `ariadne` crate for clear, contextual error messages.

## Performance Considerations

### Compilation Speed

- **Incremental compilation**: Planned for faster rebuilds
- **Parallel processing**: Potential for parallel compilation of modules
- **Caching**: Planned caching of compilation results

### Memory Usage

- **AST representation**: Designed for efficient memory usage
- **Type checking**: Optimized for memory efficiency during constraint solving

## Module System

### Import Resolution

- **Path resolution**: Resolves module paths relative to project structure
- **Symbol resolution**: Resolves identifiers across module boundaries
- **Cyclic dependency detection**: Prevents circular imports

### Compilation Units

- **File-based modules**: Each `.vi` file is a compilation unit
- **Dependency tracking**: Tracks dependencies between modules
- **Separate compilation**: Planned support for separate compilation

## Debugging and Testing

### Debug Output

The compiler supports debug flags (`--debug` or `-d`) to output internal AST representations for debugging.

### Testing Strategy

- **Unit tests**: For individual compiler components
- **Integration tests**: For complete pipeline testing
- **Regression tests**: To prevent reintroduction of fixed bugs

## Future Enhancements

### Planned Features

- **Better optimization**: More sophisticated optimization passes
- **Incremental compilation**: Faster compilation for large projects
- **Better error recovery**: More robust error handling
- **IDE integration**: Language server protocol support
- **Cross-compilation**: Support for multiple target platforms

### Performance Improvements

- **Parallel compilation**: Multi-threaded compilation pipeline
- **Better algorithms**: More efficient type checking and constraint solving
- **Memory optimization**: Reduced memory usage during compilation

## Architecture Considerations

### Modularity

The compiler is designed with a modular architecture where each stage is relatively independent, allowing for:

- **Easy testing**: Individual stages can be tested in isolation
- **Flexibility**: Stages can be modified without affecting others
- **Extensibility**: New stages or modifications can be added easily

### Extensibility

The design allows for:

- **New language features**: Can be added by extending appropriate stages
- **New backends**: Different code generation targets
- **New optimizations**: Additional optimization passes

This compilation pipeline enables Vial to provide both the expressiveness of a high-level functional language and the safety guarantees of a strongly-typed system.