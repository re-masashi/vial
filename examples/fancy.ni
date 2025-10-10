// ==================== IMPORTS ====================
import "std/io" { print, println }
import "std/collections" as col
import "std/result" { Result, Ok, Err as Error }

// ==================== TYPE ALIASES ====================
type StringMap = Map<string, string>
type Result<T> = Result<T, string>
type IntOrString = int | string

// ==================== STRUCTS ====================
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

struct Box<T> {
    value: T,
    
    def unwrap(self) -> T {
        self.value
    }
}

// ==================== ENUMS ====================
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

enum Result<T, E> {
    Ok(T),
    Err(E),
    
    def is_ok(self) -> bool {
        match self {
            Ok(_) => true,
            Err(_) => false,
        }
    }
}

// GADT-style enum with constraints
enum Vec<T> {
    Empty,
    Cons(T, Vec<T>) where T: Clone,
}

// ==================== TRAITS ====================
trait Display {
    def show(self) -> string
}

trait Iterator<T> {
    def next(self) -> Option<T>
    def has_next(self) -> bool
}

trait Clone {
    def clone(self) -> Self
}

trait Functor<F> {
    def map<A, B>(self: F<A>, f: fn(A) -> B) -> F<B>
}

// ==================== IMPL BLOCKS ====================
impl Point {
    def add(self, other: Point) -> Point {
        Point { x: self.x + other.x, y: self.y + other.y }
    }
}

impl<T> Box<T> {
    def map<U>(self, f: fn(T) -> U) -> Box<U> {
        Box { value: f(self.value) }
    }
}

impl Display for Point {
    def show(self) -> string {
        "Point(" + self.x.to_string() + ", " + self.y.to_string() + ")"
    }
}

impl<T> Clone for Box<T> where T: Clone {
    def clone(self) -> Box<T> {
        Box { value: self.value.clone() }
    }
}

// ==================== EFFECTS ====================
effect IO {
    def read_line() -> string
    def write_line(s: string) -> ()
}

effect State<S> {
    def get() -> S
    def put(s: S) -> ()
}

effect Error {
    def throw(msg: string) -> !
}

// ==================== FUNCTIONS ====================

// Simple function
def add(x: int, y: int) -> int {
    x + y
}

// Generic function
def identity<T>(x: T) -> T {
    x
}

// Function with effects
def read_number() -> int effects IO {
    let line = perform read_line();
    line.parse_int().unwrap_or(0)
}

// Function with where clause
def sort<T>(arr: [T]) -> [T] where T: Ord {
    // Bubble sort implementation
    let n = arr.len();
    for i in 0..n {
        for j in 0..(n - i - 1) {
            if arr[j] > arr[j + 1] {
                let temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp
            }
        }
    };
    arr
}

// Higher-order function
def map<T, U>(arr: [T], f: fn(T) -> U) -> [U] {
    let result = [];
    for item in arr {
        result.push(f(item))
    };
    result
}

// Function with pattern matching
def factorial(n: int) -> int {
    match n {
        0 => 1,
        1 => 1,
        n => n * factorial(n - 1),
    }
}

// ==================== EXPRESSIONS ====================

def showcase_expressions() {
    // Let bindings
    let x = 42;
    let y: int = 100;
    
    // Binary operations
    let sum = x + y;
    let product = x * y;
    let power = x ** 2;
    
    // Comparison
    let is_equal = x == y;
    let is_greater = x > y;
    
    // Logical operations
    let and_result = true and false;
    let or_result = true or false;
    let xor_result = true xor false;
    
    // Unary operations
    let neg = -x;
    let not = not true;
    
    // Assignment operators
    x += 10;
    y *= 2;
    
    // Type cast
    let float_val = x as float;
    
    // Arrays
    let numbers = [1, 2, 3, 4, 5];
    let first = numbers[0];
    
    // Tuples
    let pair = (42, "hello");
    let triple = (1, 2.0, "three");
    
    // Maps
    let config = {
        "host": "localhost",
        "port": "8080",
    };
    
    // Blocks
    let result = {
        let a = 10;
        let b = 20;
        a + b
    };
    
    // If-else
    let max = if x > y { x } else { y };
    
    // If-let
    let opt = Some(42);
    if let Some(val) = opt {
        println(val)
    };
    
    // Match
    let message = match opt {
        Some(n) if n > 10 => "large number",
        Some(n) => "small number",
        None => "no value",
    };
    
    // While loop
    let i = 0;
    while i < 10 {
        println(i);
        i += 1
    };
    
    // While-let
    let items = [1, 2, 3];
    while let Some(item) = items.pop() {
        println(item)
    };
    
    // For loop
    for num in [1, 2, 3, 4, 5] {
        println(num)
    };
    
    // Loop
    let counter = 0;
    loop {
        if counter >= 10 {
            break counter
        };
        counter += 1
    };
    
    // Lambda
    let double = fn(x) x * 2;
    let add_lambda = fn(a, b) a + b;
    
    // Function calls
    let result = add(10, 20);
    let doubled = map([1, 2, 3], double);
    
    // Method calls
    let point = Point::new(3, 4);
    let dist = point.distance();
    
    // Field access
    let x_coord = point.x;
    
    // Optional chaining
    let opt_point = Some(point);
    let opt_x = opt_point?.x;
    
    // Enum construction
    let some_val = Option::Some(42);
    let none_val = Option::None;
    
    // Return
    return result;
}

// ==================== PATTERN MATCHING ====================

def showcase_patterns(value: Option<int>) {
    match value {
        // Wildcard
        _ => println("any value"),
    };
    
    match value {
        // Bind
        x => println(x),
    };
    
    match value {
        // Literal
        Some(0) => println("zero"),
        Some(1) => println("one"),
        Some(n) => println(n),
        None => println("none"),
    };
    
    match [1, 2, 3] {
        // Array pattern
        [a, b, c] => println(a + b + c),
        _ => println("other"),
    };
    
    match point {
        // Struct pattern
        Point { x: 0, y: 0 } => println("origin"),
        Point { x, y } => println(x + y),
    };
    
    match value {
        // Or pattern
        Some(1) | Some(2) | Some(3) => println("small"),
        Some(n) => println("large"),
        None => println("none"),
    };
    
    match value {
        // As pattern
        val as Some(n) => println(val),
        _ => println("none"),
    };
    
    match value {
        // Guard
        Some(n) if n > 10 => println("large"),
        Some(n) => println("small"),
        None => println("none"),
    }
}

// ==================== EFFECT HANDLERS ====================

def run_with_io() {
    handle {
        let name = perform read_line();
        perform write_line("Hello, " + name + "!")
    } with {
        read_line(k) => k("World"),
        write_line(s, k) => {
            print(s);
            k(())
        },
    }
}

def run_with_state() -> int {
    handle {
        let current = perform get();
        perform put(current + 1);
        perform get()
    } with {
        get(k) => k(0),
        put(new_state, k) => k(()),
    }
}

def run_with_error() -> Result<int, string> {
    handle {
        let x = 10;
        if x < 0 {
            perform throw("negative number")
        };
        Ok(x)
    } with {
        throw(msg, k) => Err(msg),
    }
}

// ==================== WITH EXPRESSIONS ====================

def with_resource() {
    with open_file("test.txt") as f {
        let contents = f.read();
        println(contents)
    }
}

// ==================== MACROS ====================

def showcase_macros() {
    // Macro invocation
    println!("Hello, world!");
    vec![1, 2, 3, 4, 5];
    assert!(x > 0);
    dbg!(x);
}

// ==================== COMPLEX TYPES ====================

// Function types
type BinaryOp = fn(int, int) -> int

// Tuple types
type Coordinate = (int, int, int)

// Union types
type JsonValue = int | float | string | bool | [JsonValue] | Map<string, JsonValue>

// Nested generics
type NestedResult = Result<Option<int>, string>

// HKT-style (if supported)
type ListFunctor = Functor<List>

// Trait bounds
type Comparable<T> = T where T: Ord + Display

// ==================== MAIN FUNCTION ====================

pub def main() -> int effects IO {
    println("=== Language Feature Showcase ===");
    
    // Create some data
    let point1 = Point::new(3, 4);
    let point2 = Point::new(5, 12);
    
    // Use methods
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
    
    // Higher-order functions
    let numbers = [1, 2, 3, 4, 5];
    let doubled = map(numbers, fn(x) x * 2);
    
    println("Done!");
    0
}
