use crate::lexer::Token;
use logos::Logos;

fn lex(input: &str) -> Vec<Token> {
    Token::lexer(input)
        .filter_map(|result| result.ok())
        .collect()
}

#[test]
fn test_keywords() {
    let tokens = lex("let if else def fn struct enum type match return");
    assert_eq!(
        tokens,
        vec![
            Token::KeywordLet,
            Token::KeywordIf,
            Token::KeywordElse,
            Token::KeywordDef,
            Token::KeywordFn,
            Token::KeywordStruct,
            Token::KeywordEnum,
            Token::KeywordType,
            Token::KeywordMatch,
            Token::KeywordReturn,
        ]
    );
}

#[test]
fn test_literals() {
    let tokens = lex("42 3.14 true false \"hello\" r#\"raw\"");
    assert_eq!(
        tokens,
        vec![
            Token::Int(42),
            Token::Float(3.14),
            Token::Bool(true),
            Token::Bool(false),
            Token::String("hello".to_string()),
            Token::RawString("raw".to_string()),
        ]
    );
}

#[test]
fn test_operators() {
    let tokens = lex("+ - * / % ** == != < > <= >= and or");
    assert_eq!(
        tokens,
        vec![
            Token::Plus,
            Token::Minus,
            Token::Mul,
            Token::Div,
            Token::Mod,
            Token::Power,
            Token::Eq,
            Token::NotEq,
            Token::Less,
            Token::Greater,
            Token::LessEq,
            Token::GreaterEq,
            Token::And,
            Token::Or,
        ]
    );
}

#[test]
fn test_identifiers() {
    let tokens = lex("foo bar_baz _internal MyType");
    assert_eq!(
        tokens,
        vec![
            Token::Variable("foo".to_string()),
            Token::Variable("bar_baz".to_string()),
            Token::Variable("_internal".to_string()),
            Token::Variable("MyType".to_string()),
        ]
    );
}

#[test]
fn test_delimiters() {
    let tokens = lex("( ) [ ] { } , ; : . :: -> =>");
    assert_eq!(
        tokens,
        vec![
            Token::LParen,
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Colon,
            Token::Dot,
            Token::Access,
            Token::Arrow,
            Token::FatArrow,
        ]
    );
}

#[test]
fn test_numbers_with_underscores() {
    let tokens = lex("1_000_000 3.141_592 1_234.567_89");
    assert_eq!(
        tokens,
        vec![
            Token::Int(1_000_000),
            Token::Float(3.141_592),
            Token::Float(1_234.567_89),
        ]
    );
}

#[test]
fn test_string_escapes() {
    let tokens = lex(r#""hello\nworld" "tab\there""#);
    assert_eq!(
        tokens,
        vec![
            Token::String("hello\nworld".to_string()),
            Token::String("tab\there".to_string()),
        ]
    );
}

#[test]
fn test_comments_ignored() {
    let tokens = lex("x // comment\ny");
    assert_eq!(
        tokens,
        vec![
            Token::Variable("x".to_string()),
            Token::Variable("y".to_string()),
        ]
    );
}

#[test]
fn test_macro_invocation() {
    let tokens = lex("print! assert!");
    assert_eq!(
        tokens,
        vec![
            Token::MacroInvocation("print".to_string()),
            Token::MacroInvocation("assert".to_string()),
        ]
    );
}

#[test]
fn test_token_display() {
    // Test literals
    assert_eq!(format!("{}", Token::Bool(true)), "true");
    assert_eq!(format!("{}", Token::Int(42)), "42");
    assert_eq!(format!("{}", Token::Float(3.14)), "3.14");
    assert_eq!(
        format!("{}", Token::String("hello".to_string())),
        "\"hello\""
    );
    assert_eq!(
        format!("{}", Token::RawString("raw".to_string())),
        "r#\"raw\""
    );
    assert_eq!(format!("{}", Token::Variable("x".to_string())), "x");

    // Test keywords
    assert_eq!(format!("{}", Token::KeywordLet), "let");
    assert_eq!(format!("{}", Token::KeywordIf), "if");
    assert_eq!(format!("{}", Token::KeywordElse), "else");
    assert_eq!(format!("{}", Token::KeywordFn), "fn");
    assert_eq!(format!("{}", Token::KeywordStruct), "struct");
    assert_eq!(format!("{}", Token::KeywordEnum), "enum");
    assert_eq!(format!("{}", Token::KeywordString), "str"); // Note: KeywordString displays as "str"

    // Test operators
    assert_eq!(format!("{}", Token::Plus), "+");
    assert_eq!(format!("{}", Token::Minus), "-");
    assert_eq!(format!("{}", Token::Mul), "*");
    assert_eq!(format!("{}", Token::Div), "/");
    assert_eq!(format!("{}", Token::Power), "**");
    assert_eq!(format!("{}", Token::Eq), "==");
    assert_eq!(format!("{}", Token::NotEq), "!=");
    assert_eq!(format!("{}", Token::Less), "<");
    assert_eq!(format!("{}", Token::Greater), ">");
    assert_eq!(format!("{}", Token::LessEq), "<=");
    assert_eq!(format!("{}", Token::GreaterEq), ">=");
    assert_eq!(format!("{}", Token::Pipe), "|>");
    assert_eq!(format!("{}", Token::Assign), "=");
    assert_eq!(format!("{}", Token::AddAssign), "+=");

    // Test punctuation
    assert_eq!(format!("{}", Token::LParen), "(");
    assert_eq!(format!("{}", Token::RParen), ")");
    assert_eq!(format!("{}", Token::LBracket), "[");
    assert_eq!(format!("{}", Token::RBracket), "]");
    assert_eq!(format!("{}", Token::LBrace), "{");
    assert_eq!(format!("{}", Token::RBrace), "}");
    assert_eq!(format!("{}", Token::Comma), ",");
    assert_eq!(format!("{}", Token::Semicolon), ";");
    assert_eq!(format!("{}", Token::Colon), ":");
    assert_eq!(format!("{}", Token::Dot), ".");
    assert_eq!(format!("{}", Token::Access), "::");
    assert_eq!(format!("{}", Token::Arrow), "->");
    assert_eq!(format!("{}", Token::FatArrow), "=>");
    assert_eq!(format!("{}", Token::Union), "|");
    assert_eq!(format!("{}", Token::Not), "not");
    assert_eq!(format!("{}", Token::And), "and");
    assert_eq!(format!("{}", Token::Or), "or");
    assert_eq!(format!("{}", Token::Xor), "xor");
    assert_eq!(format!("{}", Token::Nor), "nor");
}
