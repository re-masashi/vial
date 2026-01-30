Top Level Items
===============

- [ ] Functions
- [ ] Extern Functions
- [ ] Types (aliases)
- [ ] Imports
- [ ] Traits
- [ ] Comptime 
- [ ] Impls
- [ ] Attributes

Functions
=========

- [ ] `name arg1, arg2, ... = expr`
- [ ] `name[generic1, generic2] arg1: type1, arg2: type2, ... -> return_type = expr`

(... does not represent variadic arguments. variadic arguments are not supported)

Extern Functions
================

- [ ] `extern name(type1, type2, ...) -> return_type`

(... does not represent variadic arguments. variadic arguments are not supported)

Types
=====

- [ ] `type name = { field1: type1, field2: type2, ... }` (struct)
- [ ] `type name = type1` (alias)
- [ ] `type name = A (arguments) | B (arguments) | C | D {some: typ1e}` (enum)

(type is a keyword)

types can also have generics

- [ ] `type name[generic1, generic2] = { field1: generic1, field2: generic2 }` (generic alias)

Imports
=======

File = Module
Everything is pub

- [ ] `use "some/file" ` (import all)
- [ ] `use "some/file" { name1, name2, ... }`
- [ ] `use "some/file" {name1 as alias1, name2, name3 as alias ...}`

Traits
======

- [ ] `trait name[generic1, generic2] { type name = type1, myfun[generic3] arg1: type1, arg2: type2, ... -> return_type, myfun2 arg1: type1, arg2: type2, ... -> return_type, ...}`

Comptime Functions
======

TODO

Impls
=====
- [ ] `impl name<generic1, generic2> { type name = type1, myfun (arg1: type1, arg2: type2, ...) -> return_type = expr, myfun2 (arg1: type1, arg2: type2, ...) -> return_type = expr, ...}`

Attributes
==========

Anything can have attributes. structs, enums, functions, traits, impls, externs, expressions, struct fields, etc

- [ ] `@name(attr1, attr2, ...) item`
- [ ] `@name(attr1=val1, ...) item`
- [ ] `@name(attr1=val1, 1, "hello", ...) item`

Expressions 
===========

Expressions arent allowed in the top level of a file. Only inside functions.

- [ ] `binops`
    - [ ] `+`
    - [ ] `-`
    - [ ] `*`
    - [ ] `/`
    - [ ] `**` (exponentiation)
    - [ ] `%` (mod)
    - [ ] `^` (xor)
    - [ ] `&` (bit and)
    - [ ] `|` (bit or)
    - [ ] `<<` (shl)
    - [ ] `>>` (shr)
    - [ ] `<` (lt)
    - [ ] `>` (gt)
    - [ ] `<=` (leq)
    - [ ] `>=` (geq)
    - [ ] `==` (eq)
    - [ ] `!=` (neq)
    - [ ] `&&` (and)
    - [ ] `||` (or)
    - [ ] `..` (range)
- [ ] `a = b` (assignment)
    - a can be `a.b` or `a#[b]` or any combination of these (as long as it's just these)
- [ ] `let a = b` or `let a: type = b` (everything is mut)
- [ ] `if expr then expr else expr` or `if expr then expr`
- [ ] `while expr expr`
- [ ] `for x in iterator expr`
- [ ] `match expr { pat1 => expr, pat2 => expr, pat3 => expr }`
- [ ] `{ expr1; expr2; expr3; expr4 }` (last semicolon is optional. merely syntactical. no semantic significance)
- [ ] `(a, b)` (tuple)
- [ ] `[a, b, c]` (array)
- [ ] `(expr)` (parenthesized expression)
- [ ] `fn (arg1, arg2) expr` or `fn (arg1: type, arg2: type) -> return_type expr` (lambda)
- [ ] `expr(a, b, c)` (call)
- [ ] `expr |> expr` or `expr |> expr(a) |> expr.b` (pipe)
    - pipes can be chained. eg: `expr |> expr |> expr`
    - expr1 |> expr2(a) is equivalent to expr2(expr1, a). just syntactical sugar
    - expr1 |> expr2.a is equivalent to expr2.a(expr1).
    - expr1 |> expr2 is equivalent to expr2(expr1)
    - expr1 |> expr2.a(b) is equivalent to expr2.a(expr1, b)
- [ ] `expr.a` (field access)
    - if expr has a method `.a(something)`, `expr.a` returns a function (that method). expr.a(b) becomes `Call{fun: expr.a, args: [b]}`
- [ ] `expr#[a]` (indexing)
- [ ] `{a: b, c: d, f, ..e}` (struct literal)
- [ ] `a::b(something)` or `a::b` or `a::b{a: b, c: d, ..e}` (enum variant)
- [ ] `static Type method(a, b)` (static method call)

(there's no way to call static methods because they dont exist)

Patterns
========

- [ ] A{a: b, c: d} (struct pattern)
- [ ] A::B (enum variant pattern)
- [ ] A::B{a: b, c: d, _} (enum variant pattern)
- [ ] A(pat1, pat1) (enum variant pattern)
- [ ] (pat1, pat2, pat3) (tuple pattern)
- [ ] pat1 | pat2 | pat3 (union pattern)
- [ ] literals (int, float, char, string, bool)
- [ ] _ (wildcard pattern)
- [ ] [pat1, pat2, pat3] (array pattern)
- [ ] [pat1, ..pat2] (array pattern)
- [x] [pat1, pat2, ..pat3] (array pattern)
- [x] a..b (range pattern)
- [x] break, continue, return

Comments
========

- [ ] `// comment`
- [ ] `/* comment */`

Type Annotations
=================

Present in arg types, return types, struct fields, enum variants, and let expression Annnotations

- [ ] `TypeName`
- [ ] `TypeName[generic1, generic2: * -> * and Trait1 + Trait2 ]` (Kind annot)
- [ ] `fn (typeannotation1, typeannotation2) -> return_typeannotation` (function type)
    - eg: `fn (a: Type1[T], b: Type2) -> Type3`
- [ ] `(a, b)` (tuple type).
    - a tuple with no types is a unit type
- [ ] `trait Trait1` or `trait Trait1 + Trait2 + Trait3[T]` (condition: must impl all traits)
- [ ] `{a: Type1, b: Type2}` (struct type)
- [ ] `Variant1 | Variant2 | Variant3 {a: Type1, b: Type2}` (enum type)

Compilation Pipeline
========
Lexing and parsing -> AST -> Desugared Untyped AST -> Typed AST -> Comptime Evaluated TypedAST (NO CHANGES FROM TYPEDAST, BECUSE COMPTIME WILL BE IMPLEMENTED LATER) -> AST Validation -> (maybe Monormorphized AST ->) Monomorphization Validation (no type vars left) -> IR -> Opt -> Bytecode 

Miscellaneous
=============

- Ranges are exclusive.
- pipe desugaring happens right after parsing.
- a.some_method becomes `fn(arg) { static Type some_method(a, arg) }`
- if a method has `self` as the first argument, it is a method. if not, it is a static method.
    - however, if we do `static Type method(a, b)`. it will be equivalent to a.method(b)
    - thus, doing a.static_method(b) will result in an error, but static a_stype method(a, b) will work.
- trailing commas are allowed everywhere.
- typed ast allows "deferred" types (to be determined by comptime).
- a self arg doesnt allow any annotations.
- `Self` is a valid return type in impls and traits.


Example
=======

```vial
type Person = {name: str, age: int}

type Color = White | Black | RGB (int, int, int)

trait Show {
    fn show(self) -> str
}

impl Show for Person {
    fn show(self) -> str {
        "Person {name: " + self.name + ", age: " + str(self.age) + "}";
    }
}

impl Show for Color {
    fn show(self) -> str {
        match self {
            White => return "White",
            Black => return "Black",
            RGB(r, g, b) => return "RGB(" + str(r) + ", " + str(g) + ", " + str(b) + ")",
        }
    }
}

main _ = {
    println("hi");
    let x = 1;
    let person = {name: "nafi", age: 21};
    let color = Color::RGB(255, 0, 0);
}
```
