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
}

impl Point {
    def new(x: int, y: int) -> Point {
        Point { x: x, y: y }
    }
    
    def distance(self) -> float {
        ((self.x * self.x + self.y * self.y) as float).sqrt()
    }
}

struct Box<T> {
    value: T,
}

impl<T> Box<T> {
    def unwrap(self) -> T {
        self.value
    }
}

// ==================== ENUMS ====================
enum Option<T> {
    Some(T),
    None,
}

impl<T> Option<T> {
    def unwrap_or(self, default: T) -> T {
        match self {
            Option::Some(val) => val,
            Option::None => default,
        }
    }
}

enum Result<T, E> {
    Ok(T),
    Err(E),
}

impl<T, E> Result<T, E> {
    def is_ok(self) -> bool {
        match self {
            Option::Ok(_) => true,
            Option::Err(_) => false,
        }
    }
}

// GADT-style enum with constraints
enum Vec<T> {
    Empty,
    Cons(T, Vec<T>) where T ~ Clone,
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

impl Point: Display {
    def show(self) -> string {
        "Point(" + self.x.to_string() + ", " + self.y.to_string() + ")"
    }
}

impl<T> Box<T>: Clone where T ~ Clone {
    def clone(self) -> Box<T> {
        Box { value: self.value.clone() }
    }
}

// ==================== EFFECTS ====================
effect IO {
    read_line() -> string,
    write_line(string) -> ()
}

effect State<S> {
    get() -> S,
    put(S) -> ()
}

effect Error {
    throw(string) -> !
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
def sort<T>(arr: Array<T>) -> Array<T> where T ~ Ord {
    // Bubble sort implementation
    let n = arr.len();
    for i in [0,1,2,3] {
        for j in [0,1,2] {
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
def map<T, U>(arr: List<T>, f: fn(T) -> U) -> Array<U> {
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
    let not_ = not true;
    
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
    let opt = Option::Some(42);
    if let Option::Some(val) = opt {
        println(val)
    };
    
    // Match
    let message = match opt {
        Option::Some(n) if n > 10 => "large number",
        Option::Some(n) => "small number",
        Option::None => "no value",
    };
    
    // While loop
    let i = 0;
    while i < 10 {
        println(i);
        i += 1
    };
    
    // While-let
    let items = [1, 2, 3];
    while let Option::Some(item) = items.pop() {
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
    
    // Field access and methods
    let point = Point { x: 3, y: 4 };
    let dist = point.distance();
    
    // Field access
    let x_coord = point.x;
    
    // Optional chaining
    let opt_point = Some(point);
    let opt_x = opt_point?.x;
    
    // Enum construction
    let some_val = Option::Some(42);
    let none_val = Option::None()
    
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
        Option::Some(0) => println("zero"),
        Option::Some(1) => println("one"),
        Option::Some(n) => println(n),
        Option::None => println("none"),
    };
    
    match [1, 2, 3] {
        // Array pattern
        [a, b, c] => println(a + b + c),
        _ => println("other"),
    };
    
    match point {
        // Struct pattern
        Point { x: 0, y: 0 } => println("origin"),
        // Point { x, y } => println(x + y),
    };
    
    match value {
        // Or pattern
        Option::Some(1) | Option::Some(2) | Option::Some(3) => println("small"),
        Option::Some(n) => println("large"),
        Option::None => println("none"),
    };
    
    //match value {
    //    // As pattern
    //    val as Option::Some(n) => println(val),
    //    _ => println("none"),
    //};
    
    match value {
        // Guard
        Option::Some(n) if n > 10 => println("large"),
        Option::Some(n) => println("small"),
        Option::None => println("none"),
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
            k()
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
        put(new_state, k) => k(),
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
    // with open_file("test.txt") as f {
    //    let contents = f.read();
    //    println(contents)
    //}
    0
}

// ==================== MACROS ====================

def showcase_macros() {
    // Macro invocation
    println!("Hello, world!");
    // vec![1, 2, 3, 4, 5];
    assert!(x > 0);
    dbg!(x);
}

// ==================== COMPLEX TYPES ====================

// Function types
type BinaryOp = fn(int, int) -> int

// Tuple types
type Coordinate = (int, int, int)

// Union types
// type JsonValue = int | float | string | bool | [JsonValue] | Map<string, JsonValue>

// Nested generics
type NestedResult = Result<Option<int>, string>

// HKT-style (if supported)
type ListFunctor = Functor<List>

// Trait bounds
// type Comparable<T> = T where T ~ Ord + Display

// ==================== MAIN FUNCTION ====================

def pub main() -> int effects IO {
    println("=== Language Feature Showcase ===");
    
    // Create some data
    let point1 = Point { x: 3, y: 4 };
    let point2 = Point { x: 5, y: 12 };
    
    // Use methods
    println("Point 1: " + point1.show());
    println("Distance: " + point1.distance().to_string());
    
    // Pattern matching
    let opt = Option::Some(42);
    match opt {
        Option::Some(n) => println("Got: " + n.to_string()),
        Option::None => println("Nothing"),
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
