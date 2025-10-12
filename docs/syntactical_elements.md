# Vial Programming Language - Complete Syntactical Elements Reference

This document provides a comprehensive reference for all syntactical elements in the Vial programming language, including their syntax, usage, and examples.

## Table of Contents
1. [Lexical Structure](#lexical-structure)
2. [Literals](#literals)
3. [Variables and Identifiers](#variables-and-identifiers)
4. [Types and Type Annotations](#types-and-type-annotations)
5. [Expressions](#expressions)
6. [Patterns](#patterns)
7. [Declarations](#declarations)
8. [Control Flow](#control-flow)
9. [Effects and Handlers](#effects-and-handlers)
10. [Modules and Imports](#modules-and-imports)
11. [Attributes](#attributes)
12. [Macros](#macros)

## Lexical Structure

### Comments
Vial supports single-line and multi-line comments:
```vial
// This is a single-line comment

/*
This is a multi-line comment
that spans multiple lines
*/
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

## Literals

### Numeric Literals
- **Integers**: `42`, `0`, `-17`
- **Floats**: `3.14`, `0.5`, `-2.0`

### String Literals
String literals are enclosed in double quotes:
```vial
"Hello, World!"
"This is a string with \"quotes\" inside"
r#"Raw string literal"#
```

### Boolean Literals
```vial
true
false
```

### Unit Literal
```vial
()
```

### Never Type
```vial
!
```

## Variables and Identifiers

Variables and functions follow the pattern: `[a-z_][a-zA-Z0-9_]*`
Types and modules follow the pattern: `[A-Z][a-zA-Z0-9_]*`

## Types and Type Annotations

### Basic Types
```vial
int          // 64-bit integers
float        // 64-bit floating point
bool         // boolean values
string       // string values
()           // unit type
!            // never type
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

### Tuple Types
```vial
(int, string)       // Tuple of int and string
(int, bool, float)  // Triple of int, bool, and float
```

### Union Types
```vial
int | string        // Union of int and string
int | string | bool // Union of int, string, and bool
```

### Pointer Types
```vial
*int               // Pointer to int
*List<string>      // Pointer to list of strings
```

## Expressions

### Literals
```vial
42        // integer
3.14      // float
true      // boolean
"hello"   // string
()        // unit
```

### Variables
```vial
x
my_variable
value
```

### Binary Operations
```vial
x + y     // addition
x - y     // subtraction
x * y     // multiplication
x / y     // division
x % y     // modulo
x ** y    // exponentiation
x == y    // equality
x != y    // inequality
x < y     // less than
x > y     // greater than
x <= y    // less than or equal
x >= y    // greater than or equal
x and y   // logical AND
x or y    // logical OR
x xor y   // logical XOR
x nor y   // logical NOR
x |> y    // pipe operator
```

### Unary Operations
```vial
-x        // negation
+x        // positive
not x     // logical NOT
?x        // unwrap operator
```

### Assignment Operations
```vial
x = y      // assignment
x += y     // add and assign
x -= y     // subtract and assign
x *= y     // multiply and assign
x /= y     // divide and assign
x %= y     // modulo and assign
```

### Function Application
```vial
function_name(arguments)
add(5, 3)
```

### Anonymous Functions (Lambdas)
```vial
fn(x: Type) expression
fn(x) expression  // Type inferred
fn(x: int, y: int) x + y
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
let variable: Type = expression;
```

Example:
```vial
let x = 5;
let y: int = 10;
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

#### Maps
```vial
{
  "key1": "value1",
  "key2": "value2"
}
```

#### Field Access
```vial
obj.field
obj?.field  // optional chaining
```

#### Indexing
```vial
arr[index]
```

### Type Casting
```vial
expression as Type
42 as float
```

### Control Flow Expressions

#### Return
```vial
return value
return        // return unit
```

#### Break
```vial
break value
break         // break with unit
```

#### Continue
```vial
continue
```

## Patterns

### Pattern Types

#### Wildcard Pattern
```vial
_          // matches anything but doesn't bind
```

#### Variable Binding Pattern
```vial
x          // binds the matched value to x
```

#### Literal Patterns
```vial
42         // matches integer 42
"hello"    // matches string "hello"
true       // matches boolean true
```

#### Array Patterns
```vial
[]         // empty array
[head, ...tail]  // head and tail decomposition
[1, 2, 3]  // matches array with exactly these elements
```

#### Tuple Patterns
```vial
(1, x, 3)  // tuple pattern with binding
(x, y)     // tuple with two bindings
```

#### Or Patterns
```vial
pattern1 | pattern2    // matches either pattern
```

#### As Patterns
```vial
name @ pattern         // binds name to the whole matched value while matching pattern
```

#### Struct Patterns
```vial
Point { x: 0, y: val }  // field matching with specific value and binding
Point { x, y }          // shorthand: equivalent to Point { x: x, y: y }
Point { x: a, y: b }    // field matching with different variable names
```

#### Enum Patterns
```vial
Option::None            // matches None variant
Option::Some(x)         // matches Some and binds value to x
```

#### Range Patterns
```vial
1..10    // range pattern (not yet implemented)
```

## Declarations

### Function Declarations
```vial
def name(arg1: Type1, arg2: Type2) -> ReturnType { body }
def pub name(arg1: Type1, arg2: Type2) -> ReturnType { body }  // public function
def name<T>(arg: T) -> T { body }  // generic function
def name(arg: Type) -> ReturnType effects EffectName { body }  // effectful function
def name(arg: Type) -> ReturnType where T: Trait { body }  // with constraints
```

Examples:
```vial
// Simple function
def square(x: int) -> int { x * x }

// Public function
def pub double(x: int) -> int { x * 2 }

// Generic function
def identity<T>(x: T) -> T { x }

// Function with effects
def read_line() -> string effects IO { perform read_line() }

// Function with constraints
def sort<T>(arr: [T]) -> [T] where T: Ord { ... }
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
    pub x: float,    // public field
    pub y: float,    // public field
}

struct Box<T> {
    value: T,        // private field
}
```

Methods are defined in separate implementation blocks (methods inside are allowed to but dont work rn because of a parser bug):
```vial
impl Point {
    def new(x: float, y: float) -> Point {
        Point { x: x, y: y }
    }
    
    def distance_from_origin(self) -> float {
        (self.x * self.x + self.y * self.y).sqrt()
    }
}
```

### Struct Construction
Structs are constructed using field initialization syntax:
```vial
Point { x: 3.0, y: 4.0 }
Person { name: "Alice", age: 30 }
```

Each field must be specified with its name followed by a colon and the value. All required fields must be provided, and duplicate fields are not allowed.
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

// GADT-style enum with constraints
enum Vec<T> {
    Empty,
    Cons(T, Vec<T>) where T: Clone,
}
```

Methods for enums are defined in separate implementation blocks (methods inside dont work rn because of a parser bug):
```vial
enum Option<T> {
    Some(T),
    None,
}

impl<T> Option<T> {
    def unwrap_or(self, default: T) -> T {
        match self {
            Some(val) => val,
            None => default,
        }
    }
}
```

### Type Aliases
```vial
type Name = ConcreteType
type Name<T> = ConcreteType<T>
type Name<T> = ConcreteType<T> where T: Trait
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

impl<T> Type<T> {
    def method_name(self) -> ReturnType { body }
}

impl<T> Trait for Type<T> where T: Constraint {
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

// Generic effect
effect State<S> {
    get() -> S,
    put(S) -> ()
}
```

## Control Flow

### Match Expressions
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

### Match with Guards
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

### If-let Expressions
```vial
if let pattern = expression {
    then_block
} else {
    else_block  // optional
}
```

Example:
```vial
if let Some(value) = option {
    println("Got: " + value.to_string())
} else {
    println("Got nothing")
}
```

### While-let Expressions
```vial
while let pattern = expression {
    body
}
```

Example:
```vial
let mut items = [1, 2, 3];
while let Some(item) = items.pop() {
    println(item)
}
```

### Loops
```vial
loop {
  body_expression
}
```

With break:
```vial
loop {
  if condition { break value } else { continue }
}
```

### While Loops
```vial
while condition expression
```

Example:
```vial
let i = 0;
while i < 10 {
    println(i);
    i += 1
}
```

### For Loops
```vial
for item in collection expression
```

Example:
```vial
for num in [1, 2, 3, 4, 5] {
    println(num)
}
```

## Effects and Handlers

### Effect Operations
```vial
perform effect_name(arguments)
```

### Effect Handlers
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
  get(k) => k(0),
  put(s, k) => handle k()
}
```

### Effectful Computations with `with`
```vial
with handler(arguments) {
  body_expression
}
```

Example:
```vial
with run_state(0) {
  let x = perform get();
  perform put(x + 1);
  perform get()
}
```

## Modules and Imports

### Module Imports
```vial
import "path/to/module"
import "path/to/module" { item1, item2 }
import "path/to/module" { item1 as alias1, item2 as alias2 }
import "path/to/module" as alias
```

Examples:
```vial
import "std/io"
import "std/collections" { List, Map }
import "std/result" { Result, Ok as Success, Err as Error }
import "std/option" as opt
```

## Attributes

Functions and other declarations can have attributes:
```vial
@attribute_name
def function_name() { body }

@attribute_name(arg1, arg2)
def function_name() { body }
```

Examples:
```vial
@inline
def function_name() { body }

@export
def public_function() { body }

@test
def test_function() { body }
```

## Macros

### Macro Definitions (Conceptual)
```vial
macro name { pattern => expansion }
```

### Macro Invocations
```vial
macro_name!(arguments)
println!("Hello")
vec![1, 2, 3]
```

## Advanced Features

### Higher-Kinded Types (Conceptual)
```vial
// Higher-kinded type parameters
def map<F<_>, A, B>(fa: F<A>, f: fn(A) -> B) -> F<B> { ... }
```

### Where Clauses
```vial
def function<T>(arg: T) -> T where T: Trait1 + Trait2 { ... }
```

### Type Constraints (GADT-style)
```vial
enum Vec<T> {
    Empty,
    Cons(T, Vec<T>) where T: Clone,
}
```

### Dependent Types (Conceptual)
```vial
// Future feature: dependent types
def vector_add<N>(v1: Vec<N, int>, v2: Vec<N, int>) -> Vec<N, int> { ... }
```

## Complete Example

Here's a comprehensive example using multiple syntactical elements:

```vial
// Import statements
import "std/io" { print, println }
import "std/collections" as col

// Type alias
type StringMap = Map<string, string>

// Struct with methods
struct Point {
    pub x: int,
    pub y: int,
    
    def new(x: int, y: int) -> Point {
        Point { x: x, y: y }
    }
    
    def distance(self) -> float {
        ((self.x * self.x + self.y * self.y) as float).sqrt()
    }
}

// Enum with methods
enum Option<T> {
    Some(T),
    None,
    
    def unwrap_or(self, default: T) -> T {
        match self {
            Some(val) => val,
            None => default,
        }
    }
}

// Trait definition
trait Display {
    def show(self) -> string
}

// Implementation
impl Display for Point {
    def show(self) -> string {
        "Point(" + self.x.to_string() + ", " + self.y.to_string() + ")"
    }
}

// Effect definition
effect IO {
    def read_line() -> string
    def write_line(s: string) -> ()
}

// Generic function with effects
def read_number() -> int effects IO {
    let line = perform read_line();
    line.parse_int().unwrap_or(0)
}

// Main function
def pub main() -> int effects IO {
    // Variables
    let point1 = Point { x: 3, y: 4 };
    let point2 = Point { x: 5, y: 12 };
    
    // Method calls
    println("Point 1: " + point1.show());
    println("Distance: " + point1.distance().to_string());
    
    // Pattern matching
    let opt = Some(42);
    match opt {
        Some(n) => println("Got: " + n.to_string()),
        None => println("Nothing"),
    };
    
    // Effects
    let name = perform read_line();
    perform write_line("Hello, " + name);
    
    0
}
```

This comprehensive reference covers all syntactical elements of the Vial programming language as implemented in the current codebase.