use crate::ast::*;
use crate::lexer::Token;
use logos::Logos;

use chumsky::Parser;

// helper functions
fn parse_expr(source: &str) -> Expr {
    let tokens: Vec<_> = Token::lexer(source).filter_map(|t| t.ok()).collect();

    println!("Tokens: {:?}", tokens);

    let (ast, errors) = crate::parser::parser()
        .parse(tokens.as_slice())
        .into_output_errors();

    if !errors.is_empty() {
        for err in &errors {
            eprintln!("Parse error: {:?}", err);
        }
        panic!("Parse errors occurred");
    }

    assert!(ast.is_some(), "No AST produced");

    let nodes = ast.unwrap();
    assert_eq!(
        nodes.len(),
        1,
        "Expected exactly 1 node, got {}",
        nodes.len()
    );

    match &nodes[0].node {
        ASTNodeKind::Expr(expr) => expr.clone(),
        other => panic!("Expected Expr, got {:?}", other),
    }
}

fn parse_toplevel(source: &str) -> Vec<ASTNode> {
    let tokens: Vec<_> = Token::lexer(source).filter_map(|t| t.ok()).collect();

    let (ast, errors) = super::parser()
        .parse(tokens.as_slice())
        .into_output_errors();

    if !errors.is_empty() {
        for err in &errors {
            eprintln!("Parse error: {:?}", err);
        }
        panic!("Parse errors occurred");
    }

    ast.expect("No AST produced")
}

#[test]
#[allow(clippy::approx_constant)]
fn test_literals() {
    let expr = parse_expr("42");
    assert!(matches!(expr.expr, ExprKind::Int(42)));

    let expr = parse_expr("3.14");
    assert!(matches!(expr.expr, ExprKind::Float(f) if (f - 3.14).abs() < 0.001));

    let expr = parse_expr("true");
    assert!(matches!(expr.expr, ExprKind::Bool(true)));

    let expr = parse_expr("false");
    assert!(matches!(expr.expr, ExprKind::Bool(false)));

    let expr = parse_expr(r#""hello""#);
    match &expr.expr {
        ExprKind::String(s) => assert_eq!(s, "hello"),
        _ => panic!("Expected string literal"),
    }
}

#[test]
fn test_variables() {
    let expr = parse_expr("x");
    match &expr.expr {
        ExprKind::Variable(name) => assert_eq!(name, "x"),
        _ => panic!("Expected variable"),
    }
}

#[test]
fn test_unary_operators() {
    let expr = parse_expr("-42");
    match &expr.expr {
        ExprKind::UnOp(UnOp::Minus, inner) => {
            assert!(matches!(inner.expr, ExprKind::Int(42)));
        }
        _ => panic!("Expected unary minus"),
    }

    let expr = parse_expr("not true");
    match &expr.expr {
        ExprKind::UnOp(UnOp::Not, inner) => {
            assert!(matches!(inner.expr, ExprKind::Bool(true)));
        }
        _ => panic!("Expected NOT"),
    }
}

#[test]
fn test_binary_operators() {
    let expr = parse_expr("1 + 2");
    match &expr.expr {
        ExprKind::BinOp(left, op, right) => {
            assert!(matches!(left.expr, ExprKind::Int(1)));
            assert!(matches!(op, BinOp::Add));
            assert!(matches!(right.expr, ExprKind::Int(2)));
        }
        _ => panic!("Expected binary op"),
    }

    // Precedence: 1 + 2 * 3 = 1 + (2 * 3)
    let expr = parse_expr("1 + 2 * 3");
    match &expr.expr {
        ExprKind::BinOp(left, BinOp::Add, right) => {
            assert!(matches!(left.expr, ExprKind::Int(1)));
            match &right.expr {
                ExprKind::BinOp(l, BinOp::Mul, r) => {
                    assert!(matches!(l.expr, ExprKind::Int(2)));
                    assert!(matches!(r.expr, ExprKind::Int(3)));
                }
                _ => panic!("Expected multiplication on right"),
            }
        }
        _ => panic!("Expected addition at top level"),
    }
}

#[test]
fn test_pipe_operator() {
    // Simple pipe
    let expr = parse_expr("x |> double");
    match &expr.expr {
        ExprKind::BinOp(left, BinOp::Pipe, right) => {
            assert!(matches!(left.expr, ExprKind::Variable(_)));
            assert!(matches!(right.expr, ExprKind::Variable(_)));
        }
        _ => panic!("Expected pipe"),
    }

    // Chained pipes (Right-associative): x |> (double |> square)
    let expr = parse_expr("x |> double |> square");
    println!("{:#?}", expr.expr);
    match &expr.expr {
        ExprKind::BinOp(left, BinOp::Pipe, right) => {
            // Left is x
            match &left.expr {
                ExprKind::Variable(_) => {}
                _ => panic!("Expected `x` on the left side"),
            }
            match &right.expr {
                // Right is: double |> square
                ExprKind::BinOp(l, BinOp::Pipe, r) => {
                    assert!(matches!(l.expr, ExprKind::Variable(_))); // double
                    assert!(matches!(r.expr, ExprKind::Variable(_))); // square
                }
                _ => panic!("Expected `double |> square` on the right side"),
            }
        }
        _ => panic!("Expected pipe at top level"),
    }
}

#[test]
fn test_logical_operators() {
    let expr = parse_expr("true and false");
    assert!(matches!(expr.expr, ExprKind::BinOp(_, BinOp::And, _)));

    let expr = parse_expr("true or false");
    assert!(matches!(expr.expr, ExprKind::BinOp(_, BinOp::Or, _)));

    let expr = parse_expr("true xor false");
    assert!(matches!(expr.expr, ExprKind::BinOp(_, BinOp::Xor, _)));

    // Precedence: and binds tighter than or => (true and false) or true
    let expr = parse_expr("true and false or true");
    match &expr.expr {
        ExprKind::BinOp(left, BinOp::Or, _) => {
            assert!(matches!(left.expr, ExprKind::BinOp(_, BinOp::And, _)));
        }
        _ => panic!("Expected OR at top level"),
    }
}

#[test]
fn test_comparison() {
    let expr = parse_expr("x < 10");
    assert!(matches!(expr.expr, ExprKind::BinOp(_, BinOp::Less, _)));

    let expr = parse_expr("x >= 5");
    assert!(matches!(expr.expr, ExprKind::BinOp(_, BinOp::GreaterEq, _)));

    let expr = parse_expr("a == b");
    assert!(matches!(expr.expr, ExprKind::BinOp(_, BinOp::Eq, _)));
}

#[test]
fn test_assignment() {
    let expr = parse_expr("x = 42");
    match &expr.expr {
        ExprKind::Assign { l_val, r_val, op } => {
            assert!(matches!(l_val.expr, ExprKind::Variable(_)));
            assert!(matches!(r_val.expr, ExprKind::Int(42)));
            assert!(matches!(op, AssignOp::Assign));
        }
        _ => panic!("Expected assignment"),
    }

    let expr = parse_expr("x += 5");
    assert!(matches!(
        expr.expr,
        ExprKind::Assign {
            op: AssignOp::AddAssign,
            ..
        }
    ));
}

#[test]
fn test_arrays() {
    let expr = parse_expr("[1, 2, 3]");
    match &expr.expr {
        ExprKind::Array(elements) => {
            assert_eq!(elements.len(), 3);
        }
        _ => panic!("Expected array"),
    }

    let expr = parse_expr("[]");
    match &expr.expr {
        ExprKind::Array(elements) => assert_eq!(elements.len(), 0),
        _ => panic!("Expected empty array"),
    }
}

#[test]
fn test_tuples() {
    let expr = parse_expr("(1, 2, 3)");
    match &expr.expr {
        ExprKind::Tuple(elements) => {
            assert_eq!(elements.len(), 3);
        }
        _ => panic!("Expected tuple"),
    }
}

#[test]
fn test_blocks() {
    let expr = parse_expr("{ 1; 2; 3 }");
    match &expr.expr {
        ExprKind::Block(exprs) => {
            assert_eq!(exprs.len(), 3);
        }
        _ => panic!("Expected block"),
    }
}

#[test]
fn test_let_binding() {
    let expr = parse_expr("let x = 42");
    match &expr.expr {
        ExprKind::Let {
            var,
            type_annot,
            value,
        } => {
            assert_eq!(var, "x");
            assert!(type_annot.is_none());
            assert!(matches!(value.expr, ExprKind::Int(42)));
        }
        _ => panic!("Expected let binding"),
    }

    let expr = parse_expr("let x: int = 42");
    match &expr.expr {
        ExprKind::Let {
            var,
            type_annot,
            value,
        } => {
            assert_eq!(var, "x");
            assert!(type_annot.is_some());
            assert!(matches!(value.expr, ExprKind::Int(42)));
        }
        _ => panic!("Expected let with type"),
    }
}

#[test]
fn test_if_else() {
    let expr = parse_expr("if x { 1 } else { 2 }");
    match &expr.expr {
        ExprKind::IfElse {
            condition,
            then,
            else_,
        } => {
            assert!(matches!(condition.expr, ExprKind::Variable(_)));
            assert!(matches!(then.expr, ExprKind::Block(_)));
            assert!(else_.is_some());
        }
        _ => panic!("Expected if-else"),
    }

    let expr = parse_expr("if x { 1 }");
    match &expr.expr {
        ExprKind::IfElse { else_, .. } => {
            assert!(else_.is_none());
        }
        _ => panic!("Expected if without else"),
    }
}

#[test]
fn test_match_expression() {
    let expr = parse_expr("match x { y => 1, z => 2 }");
    match &expr.expr {
        ExprKind::Match(scrutinee, arms) => {
            assert!(matches!(scrutinee.expr, ExprKind::Variable(_)));
            assert_eq!(arms.len(), 2);
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_while_loop() {
    let expr = parse_expr("while x { 1 }");
    match &expr.expr {
        ExprKind::While(condition, body) => {
            assert!(matches!(condition.expr, ExprKind::Variable(_)));
            assert!(matches!(body.expr, ExprKind::Block(_)));
        }
        _ => panic!("Expected while loop"),
    }
}

#[test]
fn test_for_loop() {
    let expr = parse_expr("for x in list { 1 }");
    match &expr.expr {
        ExprKind::For {
            iterator,
            value,
            expression,
        } => {
            assert!(matches!(value.as_str(), "x"));
            assert!(matches!(iterator.expr, ExprKind::Variable(_)));
            assert!(matches!(expression.expr, ExprKind::Block(_)));
        }
        _ => panic!("Expected for loop"),
    }
}

#[test]
fn test_loop() {
    let expr = parse_expr("loop { break }");
    match &expr.expr {
        ExprKind::Loop { label, body } => {
            assert!(label.is_none());
            assert!(matches!(body.expr, ExprKind::Block(_)));
        }
        _ => panic!("Expected loop"),
    }
}

#[test]
fn test_lambda() {
    let expr = parse_expr("fn() 42");
    match &expr.expr {
        ExprKind::Lambda { args, expression } => {
            assert_eq!(args.len(), 0);
            assert!(matches!(expression.expr, ExprKind::Int(42)));
        }
        _ => panic!("Expected lambda"),
    }
}

#[test]
fn test_function_call() {
    let expr = parse_expr("foo(1, 2)");
    match &expr.expr {
        ExprKind::Call(func, args) => {
            assert!(matches!(func.expr, ExprKind::Variable(_)));
            assert_eq!(args.len(), 2);
        }
        _ => panic!("Expected function call"),
    }
}

#[test]
fn test_index_access() {
    let expr = parse_expr("arr[0]");
    match &expr.expr {
        ExprKind::Index(array, index) => {
            assert!(matches!(array.expr, ExprKind::Variable(_)));
            assert!(matches!(index.expr, ExprKind::Int(0)));
        }
        _ => panic!("Expected index access"),
    }
}

#[test]
fn test_field_access() {
    let expr = parse_expr("obj.field");
    match &expr.expr {
        ExprKind::FieldAccess(object, field) => {
            assert!(matches!(object.expr, ExprKind::Variable(_)));
            assert_eq!(field, "field");
        }
        _ => panic!("Expected field access"),
    }
}

#[test]
fn test_cast() {
    let expr = parse_expr("42 as float");
    match &expr.expr {
        ExprKind::Cast {
            expr: value,
            target_type,
        } => {
            assert!(matches!(value.expr, ExprKind::Int(42)));
            assert!(matches!(target_type.type_, TypeAnnotKind::Float));
        }
        _ => panic!("Expected cast"),
    }
}

#[test]
fn test_return() {
    let expr = parse_expr("return 42");
    assert!(matches!(expr.expr, ExprKind::Return(Some(_))));

    let expr = parse_expr("return");
    assert!(matches!(expr.expr, ExprKind::Return(None)));
}

#[test]
fn test_break_continue() {
    let expr = parse_expr("break");
    assert!(matches!(expr.expr, ExprKind::Break(None)));

    let expr = parse_expr("continue");
    assert!(matches!(expr.expr, ExprKind::Continue));
}

#[test]
fn test_enum_construct() {
    let expr = parse_expr("Option::Some(42)");
    match &expr.expr {
        ExprKind::EnumConstruct {
            name,
            variant,
            args,
        } => {
            assert_eq!(name, "Option");
            assert_eq!(variant, "Some");
            assert_eq!(args.len(), 1);
        }
        _ => panic!("Expected enum construction"),
    }
}

#[test]
fn test_complex_expression() {
    // This correctly parses as: x |> (double |> (square + 1)) due to right-associativity of |>
    let expr = parse_expr("x |> double |> square + 1");
    match &expr.expr {
        ExprKind::BinOp(left, BinOp::Pipe, right) => {
            // Left is x
            assert!(matches!(left.expr, ExprKind::Variable(_)));
            // Right is: double |> (square + 1)
            match &right.expr {
                ExprKind::BinOp(l, BinOp::Pipe, r) => {
                    assert!(matches!(l.expr, ExprKind::Variable(_))); // double
                    // r is: square + 1
                    assert!(matches!(r.expr, ExprKind::BinOp(_, BinOp::Add, _)));
                }
                _ => panic!("Expected nested pipe"),
            }
        }
        _ => panic!("Expected pipe at top"),
    }
}

#[test]
fn test_function_with_attribute() {
    let nodes = parse_toplevel("@inline def fast() { 42 }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Function(func) => {
            assert_eq!(func.name, "fast");
            assert_eq!(nodes[0].attributes.len(), 1);
            assert_eq!(nodes[0].attributes[0].name, "inline");
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_multiple_attributes() {
    let nodes = parse_toplevel("@test @ignore def my_test() { 1 }");
    assert_eq!(nodes.len(), 1);

    assert_eq!(nodes[0].attributes.len(), 2);
    assert_eq!(nodes[0].attributes[0].name, "test");
    assert_eq!(nodes[0].attributes[1].name, "ignore");
}

#[test]
fn test_attribute_with_args() {
    let nodes = parse_toplevel("@deprecated(since, version) def old_fn() { 1 }");
    assert_eq!(nodes.len(), 1);

    assert_eq!(nodes[0].attributes.len(), 1);
    assert_eq!(nodes[0].attributes[0].name, "deprecated");
}

#[test]
fn test_simple_function() {
    let nodes = parse_toplevel("def add(x: int, y: int) -> int { x + y }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Function(func) => {
            assert_eq!(func.name, "add");
            assert_eq!(func.args.len(), 2);
            assert_eq!(func.args[0].name, "x");
            assert_eq!(func.args[1].name, "y");
            assert!(func.return_type.is_some());
            assert!(func.body.is_some());
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_function_no_params() {
    let nodes = parse_toplevel("def greet() -> string { \"hello\" }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Function(func) => {
            assert_eq!(func.name, "greet");
            assert_eq!(func.args.len(), 0);
            assert!(func.return_type.is_some());
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_function_no_return_type() {
    let nodes = parse_toplevel("def print_hello() { 42 }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Function(func) => {
            assert_eq!(func.name, "print_hello");
            assert!(func.return_type.is_none());
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_pub_function() {
    let nodes = parse_toplevel("def pub public_fn() { 1 }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Function(func) => {
            assert_eq!(func.name, "public_fn");
            assert!(matches!(func.vis, Visibility::Public));
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_function_with_type_params() {
    let nodes = parse_toplevel("def identity<T>(x: T) -> T { x }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Function(func) => {
            assert_eq!(func.name, "identity");
            assert_eq!(func.type_params.len(), 1);
            assert_eq!(func.type_params[0].name, "T");
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_function_multiple_type_params() {
    let nodes = parse_toplevel("def map<T, U>(x: T, f: fn(T) -> U) -> U { f(x) }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Function(func) => {
            assert_eq!(func.name, "map");
            assert_eq!(func.type_params.len(), 2);
            assert_eq!(func.type_params[0].name, "T");
            assert_eq!(func.type_params[1].name, "U");
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_function_with_where_clause() {
    let nodes = parse_toplevel("def clone<T>(x: T) -> T where T ~ Clone { x }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Function(func) => {
            assert_eq!(func.name, "clone");
            assert_eq!(func.where_constraints.len(), 1);
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_generic_with_multiple_bounds() {
    let nodes = parse_toplevel(
        r#"
        def complex<T, U>(x: T, y: U) -> T 
        where T ~ Clone, U ~ Display {
            x
        }
    "#,
    );
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Function(func) => {
            assert_eq!(func.where_constraints.len(), 2);
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_function_with_effects() {
    let nodes =
        parse_toplevel("def read_file(path: string) -> string effects IO { perform Read(path) }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Function(func) => {
            assert_eq!(func.name, "read_file");
            // Check that effects field is populated
            assert!(!func.effects.effects.is_empty() || func.effects.rest.is_some());
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_simple_struct() {
    let nodes = parse_toplevel("struct Point { x: int, y: int }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Struct(s) => {
            assert_eq!(s.name, "Point");
            assert_eq!(s.fields.len(), 2);
            assert_eq!(s.fields[0].0.name, "x");
            assert_eq!(s.fields[1].0.name, "y");
        }
        _ => panic!("Expected struct"),
    }
}

#[test]
fn test_empty_struct() {
    let nodes = parse_toplevel("struct Empty {}");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Struct(s) => {
            assert_eq!(s.name, "Empty");
            assert_eq!(s.fields.len(), 0);
        }
        _ => panic!("Expected struct"),
    }
}

#[test]
fn test_struct_with_pub_fields() {
    let nodes = parse_toplevel("struct Person { pub name: string, age: int }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Struct(s) => {
            assert_eq!(s.name, "Person");
            assert_eq!(s.fields.len(), 2);
        }
        _ => panic!("Expected struct"),
    }
}

#[test]
fn test_generic_struct() {
    let nodes = parse_toplevel("struct Box<T> { value: T }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Struct(s) => {
            assert_eq!(s.name, "Box");
            assert_eq!(s.type_params.len(), 1);
            assert_eq!(s.type_params[0].name, "T");
            assert_eq!(s.fields.len(), 1);
        }
        _ => panic!("Expected struct"),
    }
}

#[test]
fn test_struct_multiple_generics() {
    let nodes = parse_toplevel("struct Pair<T, U> { first: T, second: U }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Struct(s) => {
            assert_eq!(s.name, "Pair");
            assert_eq!(s.type_params.len(), 2);
            assert_eq!(s.fields.len(), 2);
        }
        _ => panic!("Expected struct"),
    }
}

#[test]
fn test_pub_struct() {
    let nodes = parse_toplevel("struct pub PublicStruct { data: int }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Struct(s) => {
            assert_eq!(s.name, "PublicStruct");
            assert!(matches!(s.vis, Visibility::Public));
        }
        _ => panic!("Expected struct"),
    }
}

#[test]
fn test_simple_enum() {
    let nodes = parse_toplevel("enum Color { Red, Green, Blue }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Enum(e) => {
            assert_eq!(e.name, "Color");
            assert_eq!(e.variants.len(), 3);
            assert_eq!(e.variants[0].name, "Red");
            assert_eq!(e.variants[1].name, "Green");
            assert_eq!(e.variants[2].name, "Blue");
        }
        _ => panic!("Expected enum"),
    }
}

#[test]
fn test_enum_with_data() {
    let nodes = parse_toplevel("enum Option { Some(int), None }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Enum(e) => {
            assert_eq!(e.name, "Option");
            assert_eq!(e.variants.len(), 2);
            assert_eq!(e.variants[0].name, "Some");
            assert_eq!(e.variants[0].types.len(), 1);
            assert_eq!(e.variants[1].name, "None");
            assert_eq!(e.variants[1].types.len(), 0);
        }
        _ => panic!("Expected enum"),
    }
}

#[test]
fn test_generic_enum() {
    let nodes = parse_toplevel("enum Result<T, E> { Ok(T), Err(E) }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Enum(e) => {
            assert_eq!(e.name, "Result");
            assert_eq!(e.type_params.len(), 2);
            assert_eq!(e.variants.len(), 2);
        }
        _ => panic!("Expected enum"),
    }
}

#[test]
fn test_enum_variant_with_multiple_fields() {
    let nodes = parse_toplevel("enum Message { Text(string), Image(string, int, int) }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Enum(e) => {
            assert_eq!(e.name, "Message");
            assert_eq!(e.variants.len(), 2);
            assert_eq!(e.variants[0].types.len(), 1);
            assert_eq!(e.variants[1].types.len(), 3);
        }
        _ => panic!("Expected enum"),
    }
}

#[test]
fn test_enum_variant_with_where_clause() {
    let nodes = parse_toplevel("enum Expr { Int(int) where T ~ int, Bool(bool) where T ~ bool }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Enum(e) => {
            assert_eq!(e.name, "Expr");
            assert_eq!(e.variants.len(), 2);
            // Check that variants have constraints
            assert!(!e.variants[0].constraints.is_empty());
            assert!(!e.variants[1].constraints.is_empty());
        }
        _ => panic!("Expected enum"),
    }
}

#[test]
fn test_pub_enum() {
    let nodes = parse_toplevel("enum pub Status { Active, Inactive }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Enum(e) => {
            assert_eq!(e.name, "Status");
            assert!(matches!(e.vis, Visibility::Public));
        }
        _ => panic!("Expected enum"),
    }
}

#[test]
fn test_simple_type_alias() {
    let nodes = parse_toplevel("type Int32 = int");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::TypeAlias(alias) => {
            assert_eq!(alias.name, "Int32");
            assert!(matches!(alias.target_type.type_, TypeAnnotKind::Int));
            assert_eq!(alias.type_params.len(), 0);
        }
        _ => panic!("Expected type alias"),
    }
}

#[test]
fn test_generic_type_alias() {
    let nodes = parse_toplevel("type Result<T, E> = Either<T, E>");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::TypeAlias(alias) => {
            assert_eq!(alias.name, "Result");
            assert_eq!(alias.type_params.len(), 2);
            assert_eq!(alias.type_params[0].name, "T");
            assert_eq!(alias.type_params[1].name, "E");
        }
        _ => panic!("Expected type alias"),
    }
}

#[test]
fn test_type_alias_with_where() {
    let nodes = parse_toplevel("type Cloneable<T> where T ~ Clone = T");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::TypeAlias(alias) => {
            assert_eq!(alias.name, "Cloneable");
            assert_eq!(alias.where_constraints.len(), 1);
        }
        _ => panic!("Expected type alias"),
    }
}

#[test]
fn test_function_type_alias() {
    let nodes = parse_toplevel("type Callback = fn() -> ()");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::TypeAlias(alias) => {
            assert_eq!(alias.name, "Callback");
            assert!(matches!(
                alias.target_type.type_,
                TypeAnnotKind::Function { .. }
            ));
        }
        _ => panic!("Expected type alias"),
    }
}

#[test]
fn test_nested_generics() {
    let nodes = parse_toplevel("type Matrix<T> = Vec<Vec<T>>");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::TypeAlias(alias) => {
            assert_eq!(alias.name, "Matrix");
            // Check the outermost Generic wrapper
            match &alias.target_type.type_ {
                TypeAnnotKind::Generic { name, args, .. } => {
                    assert_eq!(name, "Vec");
                    assert_eq!(args.len(), 1);
                }
                _ => panic!("Expected generic type"),
            }
        }
        _ => panic!("Expected type alias"),
    }
}

#[test]
fn test_simple_trait() {
    let nodes = parse_toplevel("trait Display { def show(self) -> string }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Trait(trait_def) => {
            assert_eq!(trait_def.name, "Display");
            assert_eq!(trait_def.methods.len(), 1);
            assert_eq!(trait_def.methods[0].name, "show");
        }
        _ => panic!("Expected trait"),
    }
}

#[test]
fn test_trait_multiple_methods() {
    let nodes = parse_toplevel(
        r#"
        trait Iterator {
            def next(self) -> Option<T>
            def has_next(self) -> bool
        }
    "#,
    );
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Trait(trait_def) => {
            assert_eq!(trait_def.name, "Iterator");
            assert_eq!(trait_def.methods.len(), 2);
        }
        _ => panic!("Expected trait"),
    }
}

#[test]
fn test_generic_trait() {
    let nodes = parse_toplevel("trait Container<T> { def get(self) -> T }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Trait(trait_def) => {
            assert_eq!(trait_def.name, "Container");
            assert_eq!(trait_def.type_params.len(), 1);
            assert_eq!(trait_def.type_params[0].name, "T");
        }
        _ => panic!("Expected trait"),
    }
}

#[test]
fn test_empty_trait() {
    let nodes = parse_toplevel("trait Marker {}");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Trait(trait_def) => {
            assert_eq!(trait_def.name, "Marker");
            assert_eq!(trait_def.methods.len(), 0);
        }
        _ => panic!("Expected trait"),
    }
}

#[test]
fn test_simple_impl() {
    let nodes = parse_toplevel(
        r#"
        impl Point {
            def distance(self) -> float { 0.0 }
        }
    "#,
    );
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Impl(impl_block) => {
            assert_eq!(impl_block.type_name, "Point");
            assert!(impl_block.trait_.is_none());
            assert_eq!(impl_block.methods.len(), 1);
        }
        _ => panic!("Expected impl"),
    }
}

#[test]
fn test_impl_trait_for_type() {
    let nodes = parse_toplevel(
        r#"
        impl Point: Display {
            def show(self) -> string { "point" }
        }
    "#,
    );
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Impl(impl_block) => {
            assert_eq!(impl_block.type_name, "Point");
            assert!(impl_block.trait_.is_some());
            assert_eq!(impl_block.trait_.as_ref().unwrap(), "Display");
        }
        _ => panic!("Expected impl"),
    }
}

#[test]
fn test_generic_impl() {
    let nodes = parse_toplevel(
        r#"
        impl<T> Box<T> {
            def unwrap(self) -> T { self.value }
        }
    "#,
    );
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Impl(impl_block) => {
            assert_eq!(impl_block.type_name, "Box");
            assert_eq!(impl_block.type_params.len(), 1);
        }
        _ => panic!("Expected impl"),
    }
}

#[test]
fn test_impl_with_where_clause() {
    let nodes = parse_toplevel(
        r#"
        impl<T> Container<T> where T ~ Clone {
            def get(self) -> T { self.value }
        }
    "#,
    );
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Impl(impl_block) => {
            assert_eq!(impl_block.type_name, "Container");
            assert_eq!(impl_block.where_constraints.len(), 1);
        }
        _ => panic!("Expected impl"),
    }
}

#[test]
fn test_impl_multiple_methods() {
    let nodes = parse_toplevel(
        r#"
        impl Counter {
            def increment(self) -> () { self.count = self.count + 1 }
            def get(self) -> int { self.count }
            def reset(self) -> () { self.count = 0 }
        }
    "#,
    );
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Impl(impl_block) => {
            assert_eq!(impl_block.methods.len(), 3);
            assert_eq!(impl_block.methods[0].name, "increment");
            assert_eq!(impl_block.methods[1].name, "get");
            assert_eq!(impl_block.methods[2].name, "reset");
        }
        _ => panic!("Expected impl"),
    }
}

#[test]
fn test_simple_effect() {
    let nodes = parse_toplevel(
        r#"
        effect State {
            get() -> int;
            set(int) -> ();
        }
    "#,
    );
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::EffectDef(effect) => {
            assert_eq!(effect.name, "State");
            assert_eq!(effect.operations.len(), 2);
            assert_eq!(effect.operations[0].name, "get");
            assert_eq!(effect.operations[1].name, "set");
        }
        _ => panic!("Expected effect"),
    }
}

#[test]
fn test_empty_effect() {
    let nodes = parse_toplevel("effect NoOp {}");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::EffectDef(effect) => {
            assert_eq!(effect.name, "NoOp");
            assert_eq!(effect.operations.len(), 0);
        }
        _ => panic!("Expected effect"),
    }
}

#[test]
fn test_generic_effect() {
    let nodes = parse_toplevel(
        r#"
        effect State<T> {
            get() -> T;
            set(T) -> ();
        }
    "#,
    );
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::EffectDef(effect) => {
            assert_eq!(effect.name, "State");
            assert_eq!(effect.type_params.len(), 1);
            assert_eq!(effect.type_params[0].name, "T");
        }
        _ => panic!("Expected effect"),
    }
}

#[test]
fn test_effect_with_where() {
    let nodes = parse_toplevel(
        r#"
        effect Container<T> where T ~ Clone {
            get() -> T;
        }
    "#,
    );
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::EffectDef(effect) => {
            assert_eq!(effect.name, "Container");
            assert_eq!(effect.where_constraints.len(), 1);
        }
        _ => panic!("Expected effect"),
    }
}

#[test]
fn test_effect_complex_operations() {
    let nodes = parse_toplevel(
        r#"
        effect FileSystem {
            read(string) -> string;
            write(string, string) -> ();
            delete(string) -> bool;
        }
    "#,
    );
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::EffectDef(effect) => {
            assert_eq!(effect.operations.len(), 3);
            // Check parameter counts
            assert_eq!(effect.operations[0].params.len(), 1);
            assert_eq!(effect.operations[1].params.len(), 2);
            assert_eq!(effect.operations[2].params.len(), 1);
        }
        _ => panic!("Expected effect"),
    }
}

#[test]
fn test_pub_effect() {
    let nodes = parse_toplevel(
        r#"
        effect pub IO {
            read(string) -> string;
            write(string) -> ();
        }
    "#,
    );
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::EffectDef(effect) => {
            assert_eq!(effect.name, "IO");
            assert!(matches!(effect.vis, Visibility::Public));
            assert_eq!(effect.operations.len(), 2);
        }
        _ => panic!("Expected effect"),
    }
}

#[test]
fn test_function_with_struct_param() {
    let nodes = parse_toplevel("def distance(p: Point) -> float { 0.0 }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Function(func) => {
            assert_eq!(func.name, "distance");
            assert_eq!(func.args.len(), 1);
            // Check that arg type is Named("Point")
            match &func.args[0].type_ {
                Some(ty) => assert!(matches!(ty.type_, TypeAnnotKind::Named(_))),
                None => panic!("Expected type annotation"),
            }
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_function_returning_generic() {
    let nodes = parse_toplevel("def wrap<T>(x: T) -> Box<T> { Box(x) }");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Function(func) => {
            assert_eq!(func.name, "wrap");
            match &func.return_type {
                Some(ty) => assert!(matches!(ty.type_, TypeAnnotKind::Generic { .. })),
                None => panic!("Expected return type"),
            }
        }
        _ => panic!("Expected function"),
    }
}

#[test]
fn test_multiple_top_level_items() {
    let source = r#"
        def add(x: int, y: int) -> int { x + y }
        struct Point { x: int, y: int }
        enum Color { Red, Green, Blue }
    "#;
    let nodes = parse_toplevel(source);
    assert_eq!(nodes.len(), 3);

    assert!(matches!(nodes[0].node, ASTNodeKind::Function(_)));
    assert!(matches!(nodes[1].node, ASTNodeKind::Struct(_)));
    assert!(matches!(nodes[2].node, ASTNodeKind::Enum(_)));
}

#[test]
fn test_full_program() {
    let source = r#"
        type ID = int
        
        struct User {
            id: ID,
            name: string,
        }
        
        impl User: Show {
            def show(self) -> string {
                self.name
            }
        }
        
        effect Logger {
            log(string) -> ();
        }
        
        @inline
        def main() {
            let user = User(1, "Alice");
            perform log(user.show())
        }
    "#;

    let nodes = parse_toplevel(source);
    assert_eq!(nodes.len(), 5); // TypeAlias, Struct, Impl, EffectDef, Function

    // Verify types
    assert!(matches!(nodes[0].node, ASTNodeKind::TypeAlias(_)));
    assert!(matches!(nodes[1].node, ASTNodeKind::Struct(_)));
    assert!(matches!(nodes[2].node, ASTNodeKind::Impl(_)));
    assert!(matches!(nodes[3].node, ASTNodeKind::EffectDef(_)));
    assert!(matches!(nodes[4].node, ASTNodeKind::Function(_)));
}

#[test]
fn test_comprehensive_expressions() {
    // Test complex expressions with multiple operators and types
    let expr = parse_expr("42 + 3.14 * 2 - true as float");
    match &expr.expr {
        ExprKind::BinOp(_, BinOp::Sub, _) => {
            // Should parse as (42 + (3.14 * 2)) - (true as float) due to precedence
        }
        _ => panic!("Expected subtraction operation at top level"),
    }

    // Test nested field access and indexing
    let expr = parse_expr("obj.field[0].subfield");
    match &expr.expr {
        ExprKind::FieldAccess(_, _) => { // obj.field[0].subfield
            // The expression should be parsed as ((obj.field)[0]).subfield
        }
        _ => panic!("Expected field access"),
    }

    // Test complex function call with nested expressions
    let expr = parse_expr("func(x + y, [1, 2, 3], if cond { a } else { b })");
    match &expr.expr {
        ExprKind::Call(_, args) => {
            assert_eq!(args.len(), 3);
        }
        _ => panic!("Expected function call"),
    }
}

#[test]
fn test_destructuring_in_let_bindings() {
    // For now, test that a simple let binding still works
    let nodes = parse_toplevel("let x = (1, 2)");
    assert_eq!(nodes.len(), 1);

    match &nodes[0].node {
        ASTNodeKind::Expr(expr) => match &expr.expr {
            ExprKind::Let { var, .. } => {
                assert_eq!(var, "x");
            }
            _ => panic!("Expected let binding"),
        },
        _ => panic!("Expected expression node"),
    }
}

#[test]
fn test_tuple_patterns_in_match_expressions() {
    // Test tuple pattern in match arm
    let expr = parse_expr("match (1, 2) { (a, b) => a + b }");
    match &expr.expr {
        ExprKind::Match(_scrutinee, arms) => {
            assert_eq!(arms.len(), 1);
            match &arms[0].pattern.pat {
                PatKind::Tuple(patterns) => {
                    assert_eq!(patterns.len(), 2);
                    match &patterns[0].pat {
                        PatKind::Bind(name) => assert_eq!(name, "a"),
                        _ => panic!("Expected bind pattern for first element"),
                    }
                    match &patterns[1].pat {
                        PatKind::Bind(name) => assert_eq!(name, "b"),
                        _ => panic!("Expected bind pattern for second element"),
                    }
                }
                _ => panic!("Expected tuple pattern"),
            }
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_nested_tuple_patterns() {
    // Test nested tuple patterns
    let expr = parse_expr("match ((1, 2), (3, 4)) { ((a, b), (c, d)) => a + b + c + d }");
    match &expr.expr {
        ExprKind::Match(_scrutinee, arms) => {
            assert_eq!(arms.len(), 1);
            match &arms[0].pattern.pat {
                PatKind::Tuple(patterns) => {
                    assert_eq!(patterns.len(), 2); // Two nested tuples
                    // The first element is itself a tuple pattern
                    match &patterns[0].pat {
                        PatKind::Tuple(inner_patterns) => {
                            assert_eq!(inner_patterns.len(), 2);
                            match &inner_patterns[0].pat {
                                PatKind::Bind(name) => assert_eq!(name, "a"),
                                _ => panic!("Expected bind pattern"),
                            }
                        }
                        _ => panic!("Expected nested tuple pattern"),
                    }
                }
                _ => panic!("Expected tuple pattern"),
            }
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_complex_tuple_patterns() {
    // Test complex tuple patterns with other patterns mixed in
    let expr = parse_expr(
        "match (1, [2, 3], Option::Some(4)) { (a, [b, c], Option::Some(d)) => a + b + c + d }",
    );
    match &expr.expr {
        ExprKind::Match(_scrutinee, arms) => {
            assert_eq!(arms.len(), 1);
            match &arms[0].pattern.pat {
                PatKind::Tuple(patterns) => {
                    assert_eq!(patterns.len(), 3); // Three different types of patterns

                    // First element: bind to 'a'
                    match &patterns[0].pat {
                        PatKind::Bind(name) => assert_eq!(name, "a"),
                        _ => panic!("Expected bind pattern for first element"),
                    }

                    // Second element: array pattern
                    match &patterns[1].pat {
                        PatKind::Array(array_patterns) => {
                            assert_eq!(array_patterns.len(), 2);
                        }
                        _ => panic!("Expected array pattern for second element"),
                    }

                    // Third element: enum pattern
                    match &patterns[2].pat {
                        PatKind::Enum {
                            name,
                            variant,
                            params,
                            ..
                        } => {
                            assert_eq!(name, "Option");
                            assert_eq!(variant, "Some");
                            assert_eq!(params.len(), 1);
                            match &params[0].pat {
                                PatKind::Bind(name) => assert_eq!(name, "d"),
                                _ => panic!("Expected bind pattern in enum"),
                            }
                        }
                        _ => panic!("Expected enum pattern for third element"),
                    }
                }
                _ => panic!("Expected tuple pattern"),
            }
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_if_let_with_tuple_pattern() {
    // Test if-let with tuple pattern
    let expr = parse_expr("if let (x, y) = (1, 2) { x + y } else { 0 }");
    match &expr.expr {
        ExprKind::IfLet {
            pattern,
            expr: _,
            then: _,
            else_: _,
        } => match &pattern.pat {
            PatKind::Tuple(patterns) => {
                assert_eq!(patterns.len(), 2);
                match &patterns[0].pat {
                    PatKind::Bind(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected bind pattern for 'x'"),
                }
                match &patterns[1].pat {
                    PatKind::Bind(name) => assert_eq!(name, "y"),
                    _ => panic!("Expected bind pattern for 'y'"),
                }
            }
            _ => panic!("Expected tuple pattern in if-let"),
        },
        _ => panic!("Expected if-let expression"),
    }
}

#[test]
fn test_while_let_with_tuple_pattern() {
    // Test while-let with tuple pattern
    let expr = parse_expr("while let (x, y) = (1, 2) { break }");
    match &expr.expr {
        ExprKind::WhileLet {
            pattern,
            expr: _,
            body: _,
        } => match &pattern.pat {
            PatKind::Tuple(patterns) => {
                assert_eq!(patterns.len(), 2);
                match &patterns[0].pat {
                    PatKind::Bind(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected bind pattern for 'x'"),
                }
                match &patterns[1].pat {
                    PatKind::Bind(name) => assert_eq!(name, "y"),
                    _ => panic!("Expected bind pattern for 'y'"),
                }
            }
            _ => panic!("Expected tuple pattern in while-let"),
        },
        _ => panic!("Expected while-let expression"),
    }
}

#[test]
fn test_struct_construct() {
    let expr = parse_expr("Point { x: 1, y: 2 }");
    match &expr.expr {
        ExprKind::StructConstruct { name, fields } => {
            assert_eq!(name, "Point");
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].0, "x");
            assert!(matches!(fields[0].1.expr, ExprKind::Int(1)));
            assert_eq!(fields[1].0, "y");
            assert!(matches!(fields[1].1.expr, ExprKind::Int(2)));
        }
        _ => panic!("Expected struct construction"),
    }

    // Test struct with string fields
    let expr = parse_expr("Person { name: \"Alice\", age: 30 }");
    match &expr.expr {
        ExprKind::StructConstruct { name, fields } => {
            assert_eq!(name, "Person");
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].0, "name");
            match &fields[0].1.expr {
                ExprKind::String(s) => assert_eq!(s, "Alice"),
                _ => panic!("Expected string literal"),
            }
            assert_eq!(fields[1].0, "age");
            assert!(matches!(fields[1].1.expr, ExprKind::Int(30)));
        }
        _ => panic!("Expected struct construction"),
    }

    // Test empty struct
    let expr = parse_expr("Empty {}");
    match &expr.expr {
        ExprKind::StructConstruct { name, fields } => {
            assert_eq!(name, "Empty");
            assert_eq!(fields.len(), 0);
        }
        _ => panic!("Expected struct construction"),
    }

    // Test struct with nested expressions
    let expr = parse_expr("Point { x: a + b, y: c * d }");
    match &expr.expr {
        ExprKind::StructConstruct { name, fields } => {
            assert_eq!(name, "Point");
            assert_eq!(fields.len(), 2);
            match &fields[0].1.expr {
                ExprKind::BinOp(_, BinOp::Add, _) => {}
                _ => panic!("Expected addition expression for x field"),
            }
            match &fields[1].1.expr {
                ExprKind::BinOp(_, BinOp::Mul, _) => {}
                _ => panic!("Expected multiplication expression for y field"),
            }
        }
        _ => panic!("Expected struct construction"),
    }
}

#[test]
fn test_nested_expressions() {
    // Test deeply nested block expressions
    let expr = parse_expr("{ { { 42 } } }");
    match &expr.expr {
        ExprKind::Block(blocks) => {
            assert_eq!(blocks.len(), 1);
            match &blocks[0].expr {
                ExprKind::Block(inner_blocks) => {
                    assert_eq!(inner_blocks.len(), 1);
                    match &inner_blocks[0].expr {
                        ExprKind::Block(final_block) => {
                            assert_eq!(final_block.len(), 1);
                            assert!(matches!(final_block[0].expr, ExprKind::Int(42)));
                        }
                        _ => panic!("Expected nested block"),
                    }
                }
                _ => panic!("Expected nested block"),
            }
        }
        _ => panic!("Expected block expression"),
    }

    // Test nested lambda expressions
    let expr = parse_expr("fn(x) fn(y) x + y");
    match &expr.expr {
        ExprKind::Lambda { args, expression } => {
            assert_eq!(args.len(), 1);
            match &expression.expr {
                ExprKind::Lambda {
                    args: inner_args,
                    expression: inner_expr,
                } => {
                    assert_eq!(inner_args.len(), 1);
                    assert!(matches!(inner_expr.expr, ExprKind::BinOp(_, BinOp::Add, _)));
                }
                _ => panic!("Expected nested lambda"),
            }
        }
        _ => panic!("Expected lambda expression"),
    }

    // Test deeply nested if-else expressions
    let expr = parse_expr("if true { if false { 1 } else { 2 } } else { 3 }");
    match &expr.expr {
        ExprKind::IfElse {
            condition: _,
            then,
            else_: Some(else_expr),
        } => {
            // Check the 'then' branch contains another if-else
            match &then.expr {
                ExprKind::Block(block_exprs) => {
                    assert_eq!(block_exprs.len(), 1);
                    assert!(matches!(block_exprs[0].expr, ExprKind::IfElse { .. }));
                }
                _ => panic!("Expected block in then branch"),
            }

            // Check the 'else' branch
            match &else_expr.expr {
                ExprKind::Block(block_exprs) => {
                    assert_eq!(block_exprs.len(), 1);
                    assert!(matches!(block_exprs[0].expr, ExprKind::Int(3)));
                }
                _ => panic!("Expected block in else branch"),
            }
        }
        _ => panic!("Expected if-else expression"),
    }
}

#[test]
fn test_struct_construct_edge_cases() {
    // Test struct with single field
    let expr = parse_expr("Single { value: 42 }");
    match &expr.expr {
        ExprKind::StructConstruct { name, fields } => {
            assert_eq!(name, "Single");
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].0, "value");
            assert!(matches!(fields[0].1.expr, ExprKind::Int(42)));
        }
        _ => panic!("Expected struct construction"),
    }

    // Test struct with many fields
    let expr = parse_expr("Many { a: 1, b: 2, c: 3, d: 4, e: 5 }");
    match &expr.expr {
        ExprKind::StructConstruct { name, fields } => {
            assert_eq!(name, "Many");
            assert_eq!(fields.len(), 5);
            for (i, (field_name, field_expr)) in fields.iter().enumerate() {
                let expected_name = (b'a' + i as u8) as char;
                assert_eq!(field_name, &expected_name.to_string());
                match &field_expr.expr {
                    ExprKind::Int(val) => assert_eq!(*val, (i + 1) as i64),
                    _ => panic!("Expected integer"),
                }
            }
        }
        _ => panic!("Expected struct construction"),
    }

    // Test struct with complex field expressions
    let expr = parse_expr("Complex { sum: 1 + 2, cond: if true { 5 } else { 6 } }");
    match &expr.expr {
        ExprKind::StructConstruct { name, fields } => {
            assert_eq!(name, "Complex");
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].0, "sum");
            assert!(matches!(
                fields[0].1.expr,
                ExprKind::BinOp(_, BinOp::Add, _)
            ));
            assert_eq!(fields[1].0, "cond");
            assert!(matches!(fields[1].1.expr, ExprKind::IfElse { .. }));
        }
        _ => panic!("Expected struct construction"),
    }

    // Test struct with trailing comma (should work)
    let expr = parse_expr("Trailing { x: 1, y: 2, }");
    match &expr.expr {
        ExprKind::StructConstruct { name, fields } => {
            assert_eq!(name, "Trailing");
            assert_eq!(fields.len(), 2);
        }
        _ => panic!("Expected struct construction"),
    }
}

#[test]
fn test_struct_pattern_parsing() {
    // Test struct pattern with explicit bindings
    let expr = parse_expr("match p { Point { x: a, y: b } => a + b }");
    match &expr.expr {
        ExprKind::Match(_, arms) => {
            assert_eq!(arms.len(), 1);
            match &arms[0].pattern.pat {
                PatKind::Struct { name, fields } => {
                    assert_eq!(name, "Point");
                    assert_eq!(fields.len(), 2);
                    assert_eq!(fields[0].0, "x");
                    match &fields[0].1.pat {
                        PatKind::Bind(bind_name) => assert_eq!(bind_name, "a"),
                        _ => panic!("Expected bind pattern"),
                    }
                    assert_eq!(fields[1].0, "y");
                    match &fields[1].1.pat {
                        PatKind::Bind(bind_name) => assert_eq!(bind_name, "b"),
                        _ => panic!("Expected bind pattern"),
                    }
                }
                _ => panic!("Expected struct pattern"),
            }
        }
        _ => panic!("Expected match expression"),
    }

    // Test struct pattern with shorthand bindings
    let expr = parse_expr("match p { Point { x, y } => x + y }");
    match &expr.expr {
        ExprKind::Match(_, arms) => {
            assert_eq!(arms.len(), 1);
            match &arms[0].pattern.pat {
                PatKind::Struct { name, fields } => {
                    assert_eq!(name, "Point");
                    assert_eq!(fields.len(), 2);
                    assert_eq!(fields[0].0, "x");
                    match &fields[0].1.pat {
                        PatKind::Bind(bind_name) => assert_eq!(bind_name, "x"), // Should bind to same name
                        _ => panic!("Expected bind pattern"),
                    }
                    assert_eq!(fields[1].0, "y");
                    match &fields[1].1.pat {
                        PatKind::Bind(bind_name) => assert_eq!(bind_name, "y"), // Should bind to same name
                        _ => panic!("Expected bind pattern"),
                    }
                }
                _ => panic!("Expected struct pattern"),
            }
        }
        _ => panic!("Expected match expression"),
    }

    // Test mixed struct pattern (some explicit, some shorthand)
    let expr = parse_expr("match p { Point { x: val, y } => val + y }");
    match &expr.expr {
        ExprKind::Match(_, arms) => {
            assert_eq!(arms.len(), 1);
            match &arms[0].pattern.pat {
                PatKind::Struct { name, fields } => {
                    assert_eq!(name, "Point");
                    assert_eq!(fields.len(), 2);
                    assert_eq!(fields[0].0, "x");
                    match &fields[0].1.pat {
                        PatKind::Bind(bind_name) => assert_eq!(bind_name, "val"),
                        _ => panic!("Expected bind pattern"),
                    }
                    assert_eq!(fields[1].0, "y");
                    match &fields[1].1.pat {
                        PatKind::Bind(bind_name) => assert_eq!(bind_name, "y"),
                        _ => panic!("Expected bind pattern"),
                    }
                }
                _ => panic!("Expected struct pattern"),
            }
        }
        _ => panic!("Expected match expression"),
    }
}
#[test]
fn test_extern_function_parsing_basic() {
    let code = r#"extern def hello_world() -> Unit = "libc""#;

    let tokens: Vec<_> = Token::lexer(code).filter_map(|t| t.ok()).collect();
    let ast = super::parser().parse(tokens.as_slice()).unwrap();

    assert_eq!(ast.len(), 1);
    match &ast[0].node {
        ASTNodeKind::ExternFunction(extern_fn) => {
            assert_eq!(extern_fn.library, "libc");
        }
        _ => panic!("Expected extern function"),
    }
}

#[test]
fn test_extern_function_with_parameters() {
    let code = r#"extern def add(x: Int, y: Int) -> Int = "mymath""#;

    let tokens: Vec<_> = Token::lexer(code).filter_map(|t| t.ok()).collect();
    let ast = super::parser().parse(tokens.as_slice()).unwrap();

    assert_eq!(ast.len(), 1);
    match &ast[0].node {
        ASTNodeKind::ExternFunction(extern_fn) => {
            assert_eq!(extern_fn.args.len(), 2);
            assert_eq!(extern_fn.library, "mymath");
        }
        _ => panic!("Expected extern function"),
    }
}

#[test]
fn test_extern_function_with_return_type() {
    let code = r#"extern def strlen(s: String) -> Int = "libc""#;

    let tokens: Vec<_> = Token::lexer(code).filter_map(|t| t.ok()).collect();
    let ast = super::parser().parse(tokens.as_slice()).unwrap();

    assert_eq!(ast.len(), 1);
    match &ast[0].node {
        ASTNodeKind::ExternFunction(extern_fn) => {
            assert_eq!(extern_fn.args.len(), 1);
            assert!(extern_fn.return_type.is_some());
            assert_eq!(extern_fn.library, "libc");
        }
        _ => panic!("Expected extern function"),
    }
}

#[test]
fn test_extern_function_with_effects() {
    let code = r#"extern def read_file(path: String) -> String effects IO = "filelib""#;

    let tokens: Vec<_> = Token::lexer(code).filter_map(|t| t.ok()).collect();
    let ast = super::parser().parse(tokens.as_slice()).unwrap();

    assert_eq!(ast.len(), 1);
    match &ast[0].node {
        ASTNodeKind::ExternFunction(extern_fn) => {
            assert_eq!(extern_fn.library, "filelib");
            assert!(!extern_fn.effects.effects.is_empty());
        }
        _ => panic!("Expected extern function"),
    }
}

#[test]
fn test_extern_function_with_visibility() {
    let code = r#"extern def pub public_func() -> Unit = "publib""#;

    let tokens: Vec<_> = Token::lexer(code).filter_map(|t| t.ok()).collect();
    let ast = super::parser().parse(tokens.as_slice()).unwrap();

    assert_eq!(ast.len(), 1);
    match &ast[0].node {
        ASTNodeKind::ExternFunction(extern_fn) => {
            assert_eq!(extern_fn.vis, Visibility::Public);
            assert_eq!(extern_fn.library, "publib");
        }
        _ => panic!("Expected extern function"),
    }
}

#[test]
fn test_multiple_extern_functions() {
    let code = r#"
        extern def func1() -> Unit = "lib1"
        extern def func2(x: Int) -> Int = "lib2"
        extern def pub func3(a: Int, b: Int) -> Int = "lib3"
    "#;

    let tokens: Vec<_> = Token::lexer(code).filter_map(|t| t.ok()).collect();
    let ast = super::parser().parse(tokens.as_slice()).unwrap();

    // Filter to only extern functions (some tokens might be whitespace/comments)
    let extern_fns: Vec<_> = ast
        .iter()
        .filter(|node| matches!(node.node, ASTNodeKind::ExternFunction(_)))
        .collect();

    assert_eq!(extern_fns.len(), 3);

    // Check first function
    if let ASTNodeKind::ExternFunction(extern_fn) = &extern_fns[0].node {
        assert_eq!(extern_fn.library, "lib1");
    } else {
        panic!("Expected extern function");
    }

    // Check second function
    if let ASTNodeKind::ExternFunction(extern_fn) = &extern_fns[1].node {
        assert_eq!(extern_fn.library, "lib2");
    } else {
        panic!("Expected extern function");
    }

    // Check third function
    if let ASTNodeKind::ExternFunction(extern_fn) = &extern_fns[2].node {
        assert_eq!(extern_fn.vis, Visibility::Public);
        assert_eq!(extern_fn.library, "lib3");
    } else {
        panic!("Expected extern function");
    }
}

#[test]
fn test_extern_function_with_generics() {
    let code = r#"extern def generic_func<T>(x: T) -> T = "genlib""#;

    let tokens: Vec<_> = Token::lexer(code).filter_map(|t| t.ok()).collect();
    let ast = super::parser().parse(tokens.as_slice()).unwrap();

    assert_eq!(ast.len(), 1);
    match &ast[0].node {
        ASTNodeKind::ExternFunction(extern_fn) => {
            assert_eq!(extern_fn.type_params.len(), 1);
            assert_eq!(extern_fn.library, "genlib");
        }
        _ => panic!("Expected extern function"),
    }
}
