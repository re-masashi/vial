# Language Syntax

This document describes the syntax of the Vial programming language. Vial is a functional programming language with advanced type system features including algebraic effects, higher-kinded types, and dependent types.

## Lexical Structure

### Comments

Vial supports single-line comments:

```vial
// This is a single-line comment
```

### Identifiers

- **Variables and functions**: Start with lowercase letter or underscore, followed by letters, digits, or underscores
- **Types and modules**: Start with uppercase letter, followed by letters, digits, or underscores

Examples:
```vial
// Valid identifiers
variable_name
x
_list
TypeConstructor
MyModule
```

### Literals

#### Numeric Literals

- **Integers**: `42`, `0`, `-17`
- **Floats**: `3.14`, `0.5`, `-2.0`

#### String Literals

String literals are enclosed in double quotes:
```vial
"Hello, World!"
"This is a string with \"quotes\" inside"
```

#### Boolean Literals

```vial
true
false
```

#### Unit Literal

```vial
()
```

## Expressions

### Literals

Basic values:
```vial
42        // integer
3.14      // float
true      // boolean
"hello"   // string
()        // unit
```

### Variables

Variable references:
```vial
x
my_variable
value
```

### Functions

#### Function Definition

```vial
def name(param: Type) -> ReturnType { expression }
def pub name(param: Type) -> ReturnType { expression }  // public function
```

Example:
```vial
def add(x: int, y: int) -> int { x + y }
def pub square(x: int) -> int { x * x }
```

#### Function Application

```vial
function_name(arguments)
```

Example:
```vial
add(5, 3)
```

#### Anonymous Functions (Lambdas)

```vial
fn(x: Type) expression
fn(x) expression  // Type inferred
```

Example:
```vial
fn(x) x + 1
fn(x: int) x * 2
```

### Operators

#### Arithmetic Operators

```vial
+   // addition
-   // subtraction
*   // multiplication
/   // division
%   // modulo
**  // exponentiation
```

#### Comparison Operators

```vial
==  // equality
!=  // inequality
<   // less than
>   // greater than
<=  // less than or equal
>=  // greater than or equal
```

#### Logical Operators

```vial
&&  // logical AND
||  // logical OR
!   // logical NOT
```

#### Other Operators

```vial
|>  // pipe operator
```

### Conditional Expressions

```vial
if condition { then_expression } else { else_expression }
```

Example:
```vial
if x > 0 { "positive" } else { "non-positive" }
```

### Let Bindings

```vial
let variable = expression;
```

Example:
```vial
let x = 5;
x * 2
```

### Blocks

Multiple expressions in a block:
```vial
{
  expression1;
  expression2;
  expression3
}
```

### Pattern Matching

#### Basic Match

```vial
match expression {
  pattern1 => result1,
  pattern2 => result2,
  _ => default_result
}
```

Example:
```vial
match xs {
  [] => 0,
  [head, ...tail] => head,
  _ => -1
}
```

#### Match with Guards

```vial
match x {
  pattern if guard => result,
  ...
}
```

Example:
```vial
match value {
  x if x > 0 => "positive",
  x if x < 0 => "negative", 
  _ => "zero"
}
```

#### Struct Patterns in Match

```vial
match point {
  Point { x: 0, y: 0 } => "origin",        // specific values
  Point { x, y } => "any other point",     // shorthand binding (x binds to point.x, y to point.y)
  Point { x: val, y: val } => "diagonal",  // same variable for both fields
}
```

### Data Structures

#### Tuples

```vial
(1, "hello", true)
```

#### Arrays

```vial
[1, 2, 3, 4]
[]
```

#### Records (Structs)

```vial
// Definition (in declaration section)
struct Person {
    name: string,
    age: int,
}

// Construction - New syntax
Person { name: "Alice", age: 30 }

// Old syntax is not supported
// Person("Alice", 30)  // This syntax is not valid
```

#### Variants (Enums)

```vial
// Definition (in declaration section)
enum Option<T> {
    Some(T),
    None,
}

// Construction
Option::Some(42)
Option::None
```

### Control Flow

#### Loops

```vial
loop {
  body_expression
}
```

With break:
```vial
loop {
  if condition then break value else continue
}
```

#### While Loops

```vial
while condition expression
```

#### For Loops

```vial
for item in collection expression
```

#### Break and Continue

```vial
break value    // exit loop with value
continue       // continue to next iteration
```

### Effects and Handlers

#### Effect Operations

```vial
perform effect_name(arguments)
```

#### Effect Handlers

```vial
handle expression with {
  effect_pattern => handler_expression,
  ...
}
```

Example:
```vial
effect State {
  get() -> int,
  put(int) -> ()
}

handle computation() with {
  get(k) => k(0) (handle k()),
  put(s, k) => handle k()
}
```

#### Effectful Computations

```vial
with handler(arguments) expression
```

Example:
```vial
with run_state(0) {
  let x = perform get();
  perform put(x + 1);
  perform get()
}
```

## Declarations

### Function Declarations

```vial
def name(arg1: Type1, arg2: Type2) -> ReturnType { body }
def pub name(arg1: Type1, arg2: Type2) -> ReturnType { body }  // public function
def name<T>(arg: T) -> T { body }  // generic function
```

Examples:
```vial
// Simple function
def square(x: int) -> int { x * x }

// Public function
def pub double(x: int) -> int { x * 2 }

// Generic function
def identity<T>(x: T) -> T { x }
```

### Struct Declarations

```vial
struct Name {
    field1: Type1,
    field2: Type2,
}
```

Example:
```vial
struct Point {
    x: float,
    y: float,
}
```

### Enum Declarations

```vial
enum Name<T> {
    Variant1,
    Variant2(T),
    Variant3(T, U),
}
```

Example:
```vial
enum Option<T> {
    Some(T),
    None,
}

enum List<T> {
    Cons(T, List<T>),
    Nil,
}
```

### Type Aliases

```vial
type Name = ConcreteType
```

Example:
```vial
type StringList = [string]
type IntMap<V> = Map<int, V>
```

### Trait Declarations

```vial
trait Name {
    def method_name(self) -> ReturnType
    def method_with_params(self, param: Type) -> ReturnType
}
```

Example:
```vial
trait Display {
    def show(self) -> string
}

trait Iterator<T> {
    def next(self) -> Option<T>
}
```

### Implementation Blocks

```vial
impl Type {
    def method_name(self) -> ReturnType { body }
}

impl Trait for Type {
    def method_name(self) -> ReturnType { body }
}
```

Example:
```vial
impl Point {
    def distance_from_origin(self) -> float { 
        (self.x * self.x + self.y * self.y).sqrt() 
    }
}

impl Display for Point {
    def show(self) -> string { 
        "Point(" + self.x.to_string() + ", " + self.y.to_string() + ")" 
    }
}
```

### Effect Declarations

```vial
effect Name {
    operation_name(param: Type) -> ReturnType
}
```

Example:
```vial
effect State {
    get() -> int,
    put(int) -> ()
}

effect IO {
    read_line() -> string,
    write_line(string) -> ()
}
```

### Module Imports

```vial
import "path/to/module"
import "path/to/module" { item1, item2 }
```

Examples:
```vial
import "std/io"
import "./mymodule" { add, subtract }
```

## Patterns

### Pattern Types

#### Variable Patterns

```vial
x          // binds the matched value to x
_          // wildcard, matches anything but doesn't bind
```

#### Literal Patterns

```vial
42         // matches integer 42
"hello"    // matches string "hello"
true       // matches boolean true
```

#### Constructor Patterns

```vial
Option::None            // matches None variant
Option::Some(x)         // matches Some and binds value to x
Point(x, y)             // struct destructuring
(1, x, 3)              // tuple pattern
[head, ...tail]         // array/list pattern
```

#### Or Patterns

```vial
pattern1 | pattern2    // matches either pattern
```

#### As Patterns

```vial
name @ pattern         // binds name to the whole matched value while matching pattern
```

## Type Annotations

### Basic Types

```vial
int          // 64-bit integers
float        // 64-bit floating point
bool         // boolean values
string       // string values
()           // unit type
```

### Generic Types

```vial
List<int>           // List of integers
Option<string>      // Optional string
Map<int, string>    // Map from integers to strings
```

### Function Types

```vial
fn(int) -> string                    // Function from int to string
fn(int) -> fn(int) -> int           // Function from int to (int -> int function)
fn(int, string) -> bool             // Function from tuple to bool
fn() -> int effects IO              // Function with IO effect
```

### Higher-Kinded Types

```vial
// Higher-kinded types (if supported)
```

### Type Variables

```vial
T                             // Generic type parameter
<T, U>                        // Multiple type parameters
```

### Effects

```vial
effects IO, Exception         // Effect constraints
```

## Attributes

Functions and other declarations can have attributes:

```vial
@attribute_name
def function_name() { body }
```

Examples:
```vial
@inline
def function_name() { body }

@export
def public_function() { body }
```

## Examples

### Simple Function

```vial
def factorial(n: int) -> int { 
  if n <= 1 { 1 } else { n * factorial(n - 1) }
}
```

### Pattern Matching

```vial
def length<T>(xs: [T]) -> int { 
  match xs {
    [] => 0,
    [_head, ...tail] => 1 + length(tail)
  }
}
```

### Algebraic Data Type

```vial
enum Tree<T> {
  Leaf(T),
  Node(Tree<T>, Tree<T>)
}

def tree_sum(t: Tree<int>) -> int { 
  match t {
    Tree::Leaf(n) => n,
    Tree::Node(left, right) => tree_sum(left) + tree_sum(right)
  }
}
```

### Effectful Computation

```vial
effect State {
  get() -> int,
  put(int) -> ()
}

def increment() -> int effects State { 
  let current = perform get();
  perform put(current + 1);
  perform get()
}
```

### Generic Function

```vial
def find<T>(predicate: fn(T) -> bool, list: [T]) -> Option<T> { 
  match list {
    [] => Option::None,
    [head, ...tail] => 
      if predicate(head) { Option::Some(head) } else { find(predicate, tail) }
  }
}
```

This syntax provides a rich set of constructs for functional programming while maintaining type safety and supporting advanced features like algebraic effects and higher-kinded types.