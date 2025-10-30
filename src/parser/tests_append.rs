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
    let code = r#"pub extern def public_func() -> Unit = "publib""#;
    
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
        pub extern def func3(a: Int, b: Int) -> Int = "lib3"
    "#;
    
    let tokens: Vec<_> = Token::lexer(code).filter_map(|t| t.ok()).collect();
    let ast = super::parser().parse(tokens.as_slice()).unwrap();
    
    // Filter to only extern functions (some tokens might be whitespace/comments)
    let extern_fns: Vec<_> = ast.iter()
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