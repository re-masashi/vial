# Vial Syntax Reference (Exhaustive)

This document covers every possible syntactic construct in Vial.

---

## 1. Declarations

### 1.1 Variables
```vial
let x = 10;
let mut y = 20;
let z: Float = 3.14;
```

### 1.2 Functions
```vial
def add(a: Int, b: Int) -> Int {
    a + b
}

# Generic function
def id<T>(x: T) -> T { x }
```

### 1.3 Structs & Fields
```vial
struct User {
    id: Int,
    @serde(name: "full_name")
    name: String,
}

# Field access
user.name
# Method call
user.login()
```

### 1.4 Enums & Variants
```vial
enum Message {
    Quit,
    Move { x: Int, y: Int },
    Write(String),
}

# Variant access
Message::Quit
```

### 1.5 Actors & Behaviors
```vial
actor Clock {
    let mut ticks = 0;
    
    # Asynchronous behavior
    be tick() {
        ticks += 1;
    }
    
    # Message handler
    receive {
        Event::Reset => ticks = 0
    }
    
    # Hot swap handler
    be @upgrade(old: { ticks: Int }) {
        ticks = old.ticks;
    }
}
```

### 1.6 Traits & Implementations
```vial
trait Container<T> {
    type Item;
    def add(self: mut T, item: Item);
}

impl Container<List<Int>> {
    type Item = Int;
    def add(self: mut List<Int>, item: Int) { ... }
}
```

### 1.7 GADTs
```vial
enum Expr<T> {
    Int(Int) : Expr<Int>,
    Bool(Bool) : Expr<Bool>,
}
```

---

## 2. Expressions

### 2.1 Operators
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logic: `&&`, `||`, `!`
- Bitwise: `&`, `|`, `^`, `<<`, `>>`
- Pipeline: `|>`

### 2.2 Macros
Macros are invoked with `::`.
```vial
debug::(x);
sql::"SELECT * FROM users";
html::{ div { "Hello" } }
```

### 2.3 Attributes
```vial
@attr
let x = @optimized compute();

@doc("Field level")
struct S { @meta f: Int }
```

### 2.4 Control Flow
```vial
# If
let x = if cond { 1 } else { 2 };

# Match
match val {
    1 => "One",
    _ => "Other"
}

# For
for i in 1..10 { print(i); }

# Lambda expressions
let add = |x, y| -> x + y;
let identity = |x| -> x;
```

---

## 3. Concurrency

```vial
# Spawn actor
let p = spawn MyActor();

# Async Behavior Call
p.do_work();

# Explicit Send
send p, Message::Data(42);
```

---

## 4. Ownership & Borrowing

```vial
# Borrow (Default)
func(val);

# Move (Explicit)
func(move val);

# Mutable Borrow
func(&mut val);
```

---

## 5. Modules

```vial
import std::io;
import data::{Map, Set};
import network as net;

pub def start() { ... }
```
