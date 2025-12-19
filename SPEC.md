# Vial "Specification"

---

## 1. Lexical Structure

### 1.1 Keywords

```
fn let mut once uniq const type struct enum trait impl
pub use as self Self
if else match for while break continue return defer
do end
spawn select after runtime
comptime macro
true false and or not
```

### 1.2 Operators

```
+  -  *  /  %                    # arithmetic
==  !=  <  >  <=  >=             # comparison
and  or  not                     # logical
&  |  ^  ~  <<  >>               # bitwise
=  +=  -=  *=  /=                # assignment
..  ..=                          # range
|>                               # pipe
->  =>  ::  .  ?  &  &mut        # special
```

### 1.3 Delimiters

```
(  )    # grouping, fn args
<  >    # generics
{  }    # struct/map construction
[  ]    # arrays, indexing
```

### 1.4 Comments

```ruby
# Single line

#* Multi-line *#
```

### 1.5 Literals

```ruby
42  42i32  0xFF  0b1010          # integers
3.14  3.14f32  1e10              # floats
"hello #{name}"                  # strings
'a'  '\n'                        # chars
true  false                      # bools
()                               # unit
```

### 1.6 Semicolons

Optional. Terminates expression early when needed for clarity.

---

## 2. Types

### 2.1 Primitives

```
i8 i16 i32 i64 int    u8 u16 u32 u64 uint    f32 f64
bool char string ()  !
```

### 2.2 "Other Types"

```ruby
[int]                           # array
Map<string, int>                # map
0..10                           # range (exclusive), desugars to [0,1,2,3,4,5,6,7,8,9]
0..=10                          # range (inclusive), desugars to [0,1,2,3,4,5,6,7,8,9,10]
fn(int, int) -> int                  # function
```

### 2.3 Structs

```ruby
struct Point
  x: f64
  y: f64
end

# Construction (always {})
let p = Point { x: 1.0, y: 2.0 }
let p = Point { 1.0, 2.0 }

# Update
let p2 = Point { ..p, y: 3.0 }
```

### 2.4 Enums

```ruby
enum Option<T>
  Some(T)
  None
end

# Construction (always ::)
let x = Option::Some(42)
let n = Option::None
```

### 2.5 Maps

```ruby
let m = { "a" => 1, "b" => 2 }
m["a"]        # access
m["c"] = 3    # insert
```

### 2.6 Type Aliases

```ruby
type Name = string
type Callback<T> = fn(T) -> ()
```

### 2.7 GADTs

```ruby
enum Expr<T>
  IntLit(int) -> Expr<int>
  BoolLit(bool) -> Expr<bool>
  Add(Expr<int>, Expr<int>) -> Expr<int>
end
```

---

## 3. Bindings

| Mode | Keyword | Reassign | Move | Semantics |
|------|---------|----------|------|-----------|
| Immutable | `let` | No | No | Normal value |
| Mutable | `let mut` | Yes | No | Can reassign |
| Once | `let once` | No | Yes | Must use exactly once |
| Unique | `let uniq` | No | Yes | Can move, has drop |

### 3.1 Once (Linear)

Must be consumed exactly once. Compile error if unused or used twice.

```ruby
let once token = get_auth_token()
# must use token exactly once
authenticate(token)
```

### 3.2 Unique (Affine with Drop)

Can be moved at most once. If not used, `drop` is called automatically.

```ruby
let uniq conn = connect()
if needed do
  use(conn)  # moved, consumed
# if not moved, conn.drop() called at scope end
```

### 3.3 Drop Trait

```ruby
trait Drop
  fn drop(mut self)
end

impl Drop for Connection
  fn drop(mut self)
    self.close_internal()
end
```

### 3.4 Resource API Pattern

```ruby
# open_file returns handle and a linear close token
fn open_file(path: string) -> (FileHandle, once CloseToken)

let (file, close) = open_file("x.txt")
let content = file.read()
close()  # MUST call, enforced by compiler
```

### 3.5 Destructuring

```ruby
let [a, b] = arr
let Point { x, y } = point
```

### 3.6 Refutable Patterns

Refutable patterns in `let` are **compilation errors**:

```ruby
let Option::Some(v) = maybe  # COMPILATION ERROR: pattern may not match
```

Use `match` instead:

```ruby
match maybe
  Option::Some(v) => use(v)
  Option::None => handle()
end
```

---

## 4. Expressions

### 4.1 Block

```ruby
do
  expr1
  expr2  # returned
end
```

### 4.2 If

```ruby
if cond expr else expr

if cond do
  a
  b
end else do
  c
end
```

### 4.3 Match

```ruby
match value
  Pattern => expr
  Pattern => expr
end
```

**Patterns:**
```ruby
42                              # literal
x  _                            # binding, wildcard
Option::Some(x)                 # enum
Point { x, y }                  # struct
[a, b, ..rest]                  # array
P1 or P2                        # or-pattern
P if cond                       # guard
```

### 4.4 Loops

```ruby
for x in 0..10 body
while cond body
break value
continue
```

### 4.5 Operators

```ruby
a + b   a - b   a * b   a / b   a % b
a == b  a != b  a < b   a > b
a and b   a or b   not a
```

### 4.6 Pipe Expression

```ruby
# x |> f  desugars to  f(x)
# x |> f(y)  desugars to  f(x, y)

"hello" |> upcase() |> reverse()
# desugars to: reverse(upcase("hello"))

user
  |> validate()
  |> save()
  |> notify()
# desugars to: notify(save(validate(user)))
```

---

## 5. Functions

```ruby
fn add(a: int, b: int) -> int
  a + b

fn complex(x: int) -> int do # functions do not need 'do', do - end is just the expression
  let y = x * 2
  y + 1
end

fn greet(name: string)
  puts("Hello #{name}")
```

### 5.1 Generics

```ruby
fn identity<T>(x: T) -> T
  x

fn show<T: Show>(x: T) -> string
  x.show()
```

### 5.2 Lambdas

```ruby
|x| x + 1
|x, y| x + y
```

---

## 6. Traits

```ruby
trait Show
  fn show(self) -> string
end

impl Show for Point
  fn show(self) -> string
    "(#{self.x}, #{self.y})"
end
```

### 6.1 Associated Types

```ruby
trait Iterator
  type Item
  fn next(mut self) -> Option<Self::Item>
end
```

---

## 7. Higher-Kinded Types

```ruby
trait MapFunctor<F<_>>
  fn map<A, B>(self: F<A>, f: fn(A) -> B) -> F<B>
end

impl MapFunctor<Option>
  fn map<A, B>(self: Option<A>, f: fn(A) -> B) -> Option<B>
    match self
      Option::Some(a) => Option::Some(f(a))
      Option::None => Option::None
    end
end
```

---

## 8. Modules & Imports

File = module. String-based imports:

```ruby
use "std/io"                    # stdlib
use "@serde/json"               # package
use "./helper"                  # relative

use "std/io".puts               # specific
use "std/io".*                  # all
use "std/io" as io              # alias
```

### Visibility

```ruby
pub fn public_fn()
fn private_fn()
```

---

## 9. Comptime

```ruby
const PI = 3.14159

comptime fn factorial(n: int) -> int
  if n <= 1 1 else n * factorial(n - 1)

let x = comptime factorial(10)
```

---

## 10. Macros

```ruby
macro println(arg) # stupid example but pretty sure we can find better use cases
  print(arg)
  print("\n")
end
```

---

## 11. Attributes

```ruby
@derive(Show, Eq)
struct Struct
  # fields
end

@inline
@test
@cfg(os = "linux")
fn linux_only()
  # ...
```

### 11.3 Field Attributes

Applied to struct fields:

```ruby
struct User
  @serde(rename = "userName")
  name: string

  @serde(skip)
  password_hash: string

  @deprecated("use email instead")
  username: string
end
```

### 11.4 Variant Attributes

Applied to enum variants:

```ruby
enum Command
  @serde(rename = "GREET")
  Greet(User)

  Shutdown

  @deprecated("use Shutdown instead")
  Quit

  @default
  Unknown
end
```

### 11.5 Expression Attributes

Applied to expressions (including blocks, conditionals, loops):

```ruby
fn process(x: int) -> int
  # suppress unused result warning
  @allow(unused_result)
  compute_side_effect()

  # mark block for tracing
  @trace
  do
    complex_operation()
  end
```

### 11.6 Common Attributes

| Attribute | Applies To | Description |
|-----------|------------|-------------|
| `@inline` | Functions | Hint to inline function |
| `@test` | Functions | Mark as test function |
| `@derive(...)` | Structs, Enums | Auto-generate trait impls |
| `@cfg(...)` | Any | Conditional compilation |
| `@deprecated(...)` | Any | Mark as deprecated |
| `@doc(...)` | Any | Documentation metadata |
| `@serde(...)` | Fields, Variants | Serialization control |
| `@allow(...)` | Any | Suppress warnings |
| `@trace` | Expressions | Enable tracing/debugging |

---

## 12. Concurrency

```ruby
spawn expr
spawn do body end

let task = spawn compute()
let result = task.join()

let ch = Channel<int>.new()
ch.send(42)
let v = ch.recv()

select
  ch1.recv() as x => handle(x)
  ch2.recv() as y => handle(y)
  after 1.seconds() => timeout()
end
```

---

## 13. Entry Point

```ruby
fn main()
  puts("Hello")
```

Or, 
```ruby
fn main(args: [string])
  puts("Hello #{args[0]}")
```

No top-level expressions.

---

## 14. Example

```ruby
use "std/io".puts

@derive(Show)
struct User
  name: string
  age: int
end

enum Command
  Greet(User)
  Quit
end

fn main() do
  let user = User { name: "Alice", age: 30 }
  let cmd = Command::Greet(user)

  match cmd
    Command::Greet(u) => puts("Hello #{u.name}")
    Command::Quit => puts("Bye")
  end
end
```
