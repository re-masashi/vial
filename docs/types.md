# Type System

Vial features a sophisticated type system with advanced features including algebraic effects, higher-kinded types, and experimental dependent types. This document provides a comprehensive overview of the type system design and implementation.

## Overview

The type system in Vial is designed to provide:

- Strong static typing with type inference
- Algebraic effects with effect tracking
- Higher-kinded types for advanced abstraction
- Algebraic data types (ADTs)
- Type classes (traits) with associated types
- Generic programming with parametric polymorphism
- Experimental dependent types

## Core Type Concepts

### Basic Types

Vial supports the following basic types:

- `bool`: Boolean values (`true`, `false`)
- `int`: Integer values
- `float`: Floating-point values
- `string`: String values
- `()`: The unit type representing no value

### Composite Types

#### Functions

Function types in Vial include effect information:

```
fn(param1: Type1, param2: Type2) -> ReturnType effects EffectSet
```

Where `EffectSet` represents the effects that the function may perform.

#### Tuples

Tuple types allow grouping multiple values:

```
(int, string, bool)  // A tuple of an integer, string, and boolean
```

#### Arrays and Lists

Generic types for collections:

```
[int]    // Array of integers
[string] // Array of strings
```

## Higher-Kinded Types (HKT)

Vial supports higher-kinded types, allowing abstraction over type constructors:

```vial
// Functor trait with higher-kinded type parameter
trait Functor {
  def map<A, B>(self: Self<A>, f: fn(A) -> B) -> Self<B>
}
```

### Kind System

The kind system in Vial includes:

- `*` (Star): The kind of proper types (like `int`, `string`)
- `* -> *`: The kind of type constructors that take one type (like `List`)
- `(* -> *) -> *`: The kind of type constructors that operate on other type constructors

## Algebraic Data Types

### Structs

Structs define product types:

```vial
struct Person {
    name: string,
    age: int,
}
```

### Enums

Enums define sum types with multiple variants:

```vial
enum Option<T> {
    Some(T),
    None,
}

enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

## Type Classes (Traits)

Vial supports type classes similar to Haskell:

```vial
trait Display {
  def show(self) -> string
}

trait Eq<T> {
  def eq(self, other: T) -> bool
}

// Usage in function
def compare<T>(x: T, y: T) -> string where T: Display + Eq<T> { 
  if x.eq(y) { "equal" } else { x.show() }
}
```

## Generic Types and Parametric Polymorphism

Vial supports generic programming with type parameters:

```vial
// Generic function
def identity<T>(x: T) -> T { x }

// Generic struct
struct Box<T> {
    value: T,
}

// Generic enum
enum List<T> {
    Cons(T, List<T>),
    Nil,
}
```

## Effect System

One of Vial's key features is its algebraic effect system with effect tracking:

### Effect Types

Effects are tracked in the type system:

```vial
// Effect declaration
effect State {
  get() -> int,
  put(int) -> ()
}

effect IO {
  read_line() -> string,
  write_line(string) -> ()
}

effect Exception {
  throw(string) -> !
}
```

### Effect Handlers

Effects are handled using handlers:

```vial
// Handler for State effect
def run_state<A>(f: fn() -> A effects State, initial: int) -> (A, int) { 
  handle f() with {
    get(k) => k(initial) (run_state(k, initial)),
    put(s, k) => run_state(k, s)
  }
}
```

## Type Inference

Vial performs Hindley-Milner style type inference with extensions for effects:

```vial
// The type fn() -> int effects IO will be inferred
def read_number() effects IO { 
  perform read_line().parse_int().unwrap_or(0) 
}

// The type fn() -> int effects State will be inferred
// based on the use of get and put
def increment_state() effects State { 
  let x = perform get();
  perform put(x + 1);
  perform get()
}
```

## Type Constraints and Where Clauses

Complex type relationships can be expressed using constraints:

```vial
def sort<T>(xs: [T]) -> [T] where T: Ord { 
  // implementation requiring ordering
}

def transform<F, A, B>(container: F<A>, f: fn(A) -> B) -> F<B] 
  where F: Functor { 
  // fmap equivalent using Functor constraint
}
```

## Type Checking Process

The type checker in Vial performs:

1. **Type Inference**: Determine types for expressions
2. **Effect Inference**: Track effects performed by expressions
3. **Constraint Generation**: Generate constraints for type variables
4. **Constraint Solving**: Solve type and effect constraints
5. **Kind Checking**: Verify kinds of type expressions
6. **Trait Resolution**: Find trait implementations

## Advanced Type Features

### GADTs (Generalized Algebraic Data Types)

Vial supports GADTs for more precise typing:

```vial
enum Expr (t: Type) : Type = {
  IntLit : Int -> Expr Int,
  BoolLit : Bool -> Expr Bool,
  Add : Expr Int -> Expr Int -> Expr Int,
  Eq : Expr a -> Expr a -> Expr Bool  -- Note: same type required
}
```

### Type Families

Experimental support for type-level functions:

```vial
// Type family for type-level addition
type family (+) (n: Nat) (m: Nat) : Nat

type instance 0 + m = m
type instance S n + m = S (n + m)
```

## Error Reporting

The type checker provides detailed error messages:

- **Type mismatches**: Clear indication of expected vs actual types
- **Effect mismatches**: Showing expected vs actual effects
- **Constraint failures**: Identifying missing trait implementations
- **Kind errors**: For incorrectly kinds type expressions

## Implementation Details

### Type Representation

Types in Vial are represented as:

```rust
// Simplified representation
pub enum Type {
    Bool,
    Int,
    Float,
    String,
    Unit,
    Never,
    Named(Symbol, Vec<Rc<Type>>),  // Type name with type args
    Function {
        params: Vec<Rc<Type>>,
        return_type: Rc<Type>,
        effects: EffectSet,
    },
    Tuple(Vec<Rc<Type>>),
    Variable(TypeId),  // Type variables during inference
    Forall {
        var: TypeId,
        kind: Kind,
        body: Rc<Type>,
    },  // For polymorphic types
}
```

### Effect Sets

Effects are tracked using effect sets:

```rust
pub struct EffectSet {
    pub effects: Vec<EffectId>,  // Specific effects
    pub rest: Option<EffectId>,  // Effect variable for open sets
}
```

## Type System Extensions

### Linear Types (Planned)

Future versions may include linear types for resource management:

```vial
// Hypothetical syntax for linear types
fn consume_resource (r: Linear Resource): () = 
  // r must be used exactly once
```

### Session Types (Planned)

For communication protocol verification:

```vial
// Hypothetical session type
type ServerSession = Send String (Recv String (End))
```

## Relationship to Other Type Systems

Vial's type system draws inspiration from:

- **Haskell**: Type classes, higher-kinded types, algebraic data types
- **ML family**: Hindley-Milner type inference
- **Effect Systems**: Algebraic effects from Eff, Koka, and Frank
- **Dependent Types**: Idris, Agda for dependent type features

## Performance Considerations

- Type checking is designed to be efficient with good error recovery
- Monomorphization eliminates generic overhead in the generated code
- Effect tracking is optimized to avoid runtime overhead where possible

This type system enables Vial to provide strong safety guarantees while maintaining expressiveness for functional programming patterns.