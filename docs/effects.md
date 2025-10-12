# Effect System

Vial features an advanced algebraic effect system that allows for structured handling of side effects in a pure functional programming context. This document details the effect system's design, syntax, and implementation.

## Overview

Algebraic effects in Vial provide a way to:

- Define and use computational effects in a structured manner
- Handle effects using composable handlers
- Track effects in the type system for safety
- Separate effectful computations from their interpretation

## Effect Definitions

Effects are defined using the `effect` keyword, specifying operations that can be performed:

```vial
effect State {
  get() -> int,
  put(int) -> ()
}
```

This defines a `State` effect with two operations:
- `get()`: retrieves the current state, returning an int
- `put(int)`: updates the state, taking an int and returning unit

### Multiple Effects

You can define effects with multiple operations:

```vial
effect Console {
  print(string) -> (),
  read_line() -> string
}

effect Exception {
  throw(string) -> !
}
```

## Performing Effects

Effects are performed using the `perform` keyword:

```vial
def increment() -> int effects State { 
  let current = perform get();
  perform put(current + 1);
  perform get()
}
```

The `effects State` in the function signature indicates that this function may perform `State` effects.

## Effect Handlers

Effects are handled using the `handle` expression:

```vial
def run_state<A>(comp: fn() -> A effects State, initial: int) -> (A, int) { 
  handle comp() with {
    get(k) => k(initial) (run_state(k, initial)),
    put(s, k) => run_state(k, s)
  }
}
```

In this handler:
- `get(k)` matches the `get` operation, where `k` is the continuation
- `k(initial) (run_state(k, initial))` resumes the computation with `initial` and recursively handles the rest
- `put(s, k)` matches the `put` operation with state `s` and continuation `k`

## Syntactic Sugar: The `with` Expression

Vial provides syntactic sugar for common effect handling patterns:

```vial
def example() -> int { 
  with run_state(0) {
    let x = perform get();
    perform put(x + 1);
    perform get()
  }
}
```

This is equivalent to:

```vial
def example() -> int { 
  let (result, _) = run_state(fn() -> {
    let x = perform get();
    perform put(x + 1);
    perform get()
  }, 0);
  result
}
```

## Common Effect Patterns

### State Effect

```vial
// Define the effect
effect State {
  get() -> int,
  put(int) -> ()
}

// Handler implementation
def run_state<A>(comp: fn() -> A effects State, initial: int) -> (A, int) { 
  handle comp() with {
    get(k) => k(initial) (run_state(k, initial)),
    put(s, k) => run_state(k, s)
  }
}

// Usage
def counter() -> int effects State { 
  let x = perform get();
  perform put(x + 1);
  perform get()
}

// Running with initial state
def result() -> int { 
  let (value, _) = run_state(counter, 0);
  value
}
```

### IO Effect

```vial
effect IO {
  read_line() -> string,
  write_line(string) -> ()
}

// Handler would implement actual file operations
def pure_io_handler<A>(comp: fn() -> A effects IO) -> A { 
  handle comp() with {
    read_line(k) => k("default input") (pure_io_handler(k)),
    write_line(content, k) => {
      print(content);  // Simulate writing
      pure_io_handler(k)
    }
  }
}
```

### Exception Effect

```vial
effect Exception {
  throw(string) -> !
}

def run_except<A>(comp: fn() -> A effects Exception) -> Result<A, string> { 
  handle comp() with {
    throw(msg, k) => Result::Err(msg)  // Note: k never continues
  }
  // If no exception, return Ok value (implementation would need to handle this properly)
}
```

## Effect Composition

Multiple effects can be composed. In Vial, function types can specify multiple effects:

```vial
// Function that may perform both State and IO effects
def complex_op() -> string effects State, IO { 
  let counter = perform get();
  perform put(counter + 1);
  let msg = "Counter: " + (counter + 1).to_string();
  perform write_line(msg);
  msg
}
```

## Effect Typing

### Effect Tracking in Types

Function types in Vial include effect information:

```vial
// Pure function
def pure_fn(x: int) -> int { x }

// Function with State effect
def stateful_fn(x: int) -> int effects State { x }

// Function with multiple effects
def complex_fn(x: int) -> string effects State, IO, Exception { x.to_string() }
```

## Implementation Details

### AST Representation

Effects are represented in the AST with:

- `EffectDef`: Effect declarations with operations
- `Perform`: Effect performance expressions
- `Handle`: Effect handler expressions
- `EffectAnnot`: Effect annotations in types

### Type System Integration

The type checker tracks effects using:

- `EffectSet`: Represents a set of possible effects
- Effect constraints and unification
- Subeffecting relationships

### Runtime Representation

Effects are handled through:

- Continuation-passing style transformation
- Handler composition
- Dynamic effect interpretation

## Advanced Effect Patterns

### Scoped Effects

Effects can be lexically scoped:

```vial
def with_file<A>(path: string, op: fn(File) -> A effects Resource) -> A effects IO { 
  handle {
    let file = perform open(path);
    let result = op(file);
    perform close(file);
    result
  } with {
    open(path, k) => native_open(path) >>= fn(file) => k(file) (native_close(file); k(())),
    close(file, k) => native_close(file); k(())
  }
}
```

## Error Handling with Effects

Effects provide structured error handling:

```vial
effect Error {
  throw(string) -> !
}

def safe_divide(x: int, y: int) -> int effects Error { 
  if y == 0 { 
    perform throw("Division by zero")
  } else {
    x / y
  }
}

def handle_division(x: int, y: int) -> string { 
  handle safe_divide(x, y) with {
    throw(msg, k) => "Error: " + msg
  }
}
```

## Performance Considerations

### Zero-Cost Abstractions

When possible, effect handlers should have zero runtime cost:

```vial
// This can potentially be optimized away completely
fn pure_computation: Int = 
  with runState 0 {
    42
  }
```

### Compilation Strategies

- **Direct style**: For pure computations
- **CPS conversion**: For effectful computations
- **Handler fusion**: Combining multiple handlers
- **Effect specialization**: Optimizing specific effect patterns

## Relationship to Other Systems

### Comparison to Exceptions

Unlike imperative exceptions, algebraic effects:
- Are statically tracked in types
- Allow resumption after handling
- Support multiple handlers
- Are more composable

### Comparison to Monads

Compared to monadic effect systems, algebraic effects:
- Have simpler syntax
- Allow effect handlers to be written separately
- Support effect composition more naturally
- Enable more flexible effect interpretation

## Best Practices

### Effect Modularity

- Define small, focused effects
- Combine effects as needed
- Keep effect definitions close to their usage

### Handler Design

- Write handlers that preserve expected semantics
- Consider the interaction between different effects
- Use composable handler patterns

### Type Safety

- Use effect types to document side effects
- Leverage the type system to prevent effect misuse
- Consider effect polymorphism for reusability

## Future Extensions

### Linear Effects

Planned extensions may include linear effects for resource management:

```vial
// Hypothetical syntax
effect LinearResource r = {
  acquire: () -> r,
  release: r -> ()  // Must be called exactly once
}
```

### Session Effects

For communication protocol verification:

```vial
// Hypothetical syntax
effect Session protocol = {
  send: Message -> (),
  receive: () -> Message
}
```

The effect system in Vial provides a powerful and flexible approach to handling side effects while maintaining the benefits of functional programming and static type safety.