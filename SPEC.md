# Vial Language Specification (SPEC)

Vial is a high-performance, statically-typed language designed for modern web backends.

---

## 1. Syntax & Philosophy

### 1.1 Core Principles
- **Explicit over Implicit**: Braces `{}` are used for blocks (not whitespace sensitive). Semicolons `;` are optional.
- **Boilerplate Free**: Extensive type inference and structural records.
- **Expression Oriented**: Every construct (if, match, block) returns a value.

### 1.2 Comments
- Single-line: `# comment`
- Documentation: `## doc comment` or `@doc("...")` attribute.

### 1.3 Variables & Mutability
```vial
let x = 10;         # Immutable
let mut y = 20;     # Mutable
y = 30;             # OK
```
Shadowing is permitted within the same or nested scopes.

---

## 2. Type System

### 2.1 Algebraic Data Types (ADTs)
Vial supports nominal Sum and Product types.
```vial
struct User {
    id: Int,
    name: String,
    email: String? # Option<String> shorthand
}

enum Status {
    Pending,
    Active(User),
    Inactive(reason: String)
}
```

### 2.2 Generalized Algebraic Data Types (GADTs)
GADTs allow for more precise type-level constraints on enum variants.
```vial
enum Expr<T> {
    Int(Int) : Expr<Int>,
    Bool(Bool) : Expr<Bool>,
    Add(Expr<Int>, Expr<Int>) : Expr<Int>,
    Eq<A>(Expr<A>, Expr<A>) : Expr<Bool>
}
```

### 2.3 Pattern Matching
Exhaustiveness is strictly enforced by the compiler.
```vial
match status {
    Status::Pending => print("Waiting..."),
    Status::Active(user) => print("Hello, #{user.name}"),
    Status::Inactive(reason) => print("Offline: #{reason}")
}
```

### 2.4 Generics & Type Classes (Traits)
Generics use `< >`. Traits define shared behavior.
```vial
trait Show<A> {
    def show(self: A) -> String;
    # Default implementation
    def log(self: A) { print(self.show()); }
}

impl Show<Int> {
    def show(self: Int) { self.to_string(); }
}
```

### 2.5 Associated Types
Traits can define associated types for flexible polymorphism.
```vial
trait Collection<C> {
    type Item;
    def add(self: mut C, item: Item);
}
```

### 2.6 Higher-Kinded Types (HKTs)
Vial supports System Fω style HKTs with explicit kinding.
```vial
trait Functor<F<* -> *>> {
    def map<A, B>(fa: F<A>, f: (A) -> B) -> F<B>;
}
```

### 2.7 Structural Records & Row Polymorphism
```vial
def greet<R>(obj: { name: String, .. R }) {
    print("Hello, #{obj.name}");
}
```

---

## 3. Memory & Resource Management

### 3.1 Borrowing & Move Semantics
- **Borrowing is Default**.
- **Explicit Move**: Use `move`.
- **Mutable Borrowing**: Use `&mut`.

### 3.2 Resource Safety (RAII & Defer)
- **Drop Trait**: Automatic destructor.
- **Defer**: Scheduled execution.

---

## 4. Concurrency (Actor Model)

### 4.1 Actors & Behaviors
- `be`: Asynchronous behavior (async message handler).
- `send`: Asynchronous message delivery.
```vial
actor Server {
    be request(req: Req) { ... }
}

send server, Event::Query;
```

### 4.2 Hot Code Swapping
Actors can update their logic at runtime while preserving state.
```vial
actor Counter {
    let mut count = 0;
    
    ## Called when actor is upgraded
    be @upgrade(old_state: { count: Int }) {
        count = old_state.count;
    }
}
```

### 4.3 Supervision Trees
Supervisors manage actor failure via restart strategies.

---

## 5. Metaprogramming

### 5.1 Macros
Macros are invoked with `::`.
```vial
macro debug(expr) { ... }
debug::(x);
```

### 5.2 Comptime Reflection API
The `reflect` module provides compile-time access to the AST.
```vial
comptime {
    let ty = reflect::type_of<User>();
    for field in ty.fields { ... }
}
```

---

## 6. Performance

### 6.1 SIMD Intrinsics
Native support for vectorized operations.
```vial
@simd
def vec_add(a: f32x4, b: f32x4) -> f32x4 { a + b }
```

### 6.2 Tail Call Optimization (TCO)
The compiler guarantees TCO for all self-recursive or cross-recursive calls marked with `@tco` or where inferred.

---

## 7. Web & First-Class JSON
Structural records map directly to JSON objects.
```vial
let json = { id: 1, type: "login" };
let id = json.id;
```
Built-in `sql::` macros for safe, compile-time verified queries.
