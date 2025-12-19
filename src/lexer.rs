use logos::Logos;

/// Unescape a string literal by processing escape sequences.
/// Handles: \n, \r, \t, \\, \", \', \x41, and preserves #{...} interpolations.
fn unescape_string(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(&next_ch) = chars.peek() {
                match next_ch {
                    'n' => {
                        chars.next();
                        result.push('\n');
                    }
                    'r' => {
                        chars.next();
                        result.push('\r');
                    }
                    't' => {
                        chars.next();
                        result.push('\t');
                    }
                    '\\' => {
                        chars.next();
                        result.push('\\');
                    }
                    '"' => {
                        chars.next();
                        result.push('"');
                    }
                    '\'' => {
                        chars.next();
                        result.push('\'');
                    }
                    'x' => {
                        chars.next();
                        // Collect up to 2 hex digits
                        let mut hex = String::new();
                        for _ in 0..2 {
                            if let Some(&c) = chars.peek() {
                                if c.is_ascii_hexdigit() {
                                    hex.push(c);
                                    chars.next();
                                } else {
                                    break;
                                }
                            }
                        }
                        if !hex.is_empty() {
                            if let Ok(code) = u8::from_str_radix(&hex, 16) {
                                result.push(code as char);
                            }
                        }
                    }
                    _ => {
                        result.push('\\');
                        result.push(chars.next().unwrap());
                    }
                }
            } else {
                result.push(ch);
            }
        } else {
            result.push(ch);
        }
    }

    result
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token {
    #[regex(r"#[^\*\n][^\n]*", priority = 2, allow_greedy = true)]
    #[regex(r"#\n")]
    #[regex(r"#")]
    LineComment,

    #[regex(r"#\*([^*]|\*[^#])*\*#")]
    BlockComment,

    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,
    #[token("once")]
    Once,
    #[token("uniq")]
    Uniq,
    #[token("const")]
    Const,
    #[token("type")]
    Type,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("trait")]
    Trait,
    #[token("impl")]
    Impl,

    #[token("pub")]
    Pub,
    #[token("use")]
    Use,
    #[token("as")]
    As,
    #[token("self")]
    LowerSelf,
    #[token("Self")]
    UpperSelf,

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("match")]
    Match,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,
    #[token("defer")]
    Defer,

    #[token("do")]
    Do,
    #[token("end")]
    End,

    #[token("spawn")]
    Spawn,
    #[token("select")]
    Select,
    #[token("after")]
    After,

    #[token("comptime")]
    Comptime,
    #[token("macro")]
    Macro,

    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,

    #[token("in")]
    In,

    // Assignment operators
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,

    // Comparison operators
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,

    // Bitwise shift
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,

    // Range operators
    #[token("..=")]
    DotDotEq,
    #[token("..")]
    DotDot,

    // Special operators
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("::")]
    ColonColon,
    #[token("|>")]
    Pipe,
    #[token("&mut")]
    AmpMut,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,

    #[token("<")]
    Lt,
    #[token(">")]
    Gt,

    #[token("&")]
    Amp,
    #[token("|")]
    BitOr,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,

    #[token("=")]
    Eq,
    #[token(".")]
    Dot,
    #[token("?")]
    Question,
    #[token("@")]
    At,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("_")]
    Underscore,

    // Hex integers: 0xFF, 0XFF
    #[regex(r"0[xX][0-9a-fA-F][0-9a-fA-F_]*", |lex| lex.slice().to_string())]
    HexInt(String),

    // Binary integers: 0b1010, 0B1010
    #[regex(r"0[bB][01][01_]*", |lex| lex.slice().to_string())]
    BinInt(String),

    // Octal integers: 0o777, 0O777
    #[regex(r"0[oO][0-7][0-7_]*", |lex| lex.slice().to_string())]
    OctInt(String),

    // Floats: 3.14, 3.14f32, 1e10, 1.5e-10
    #[regex(r"[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9]+)?[fF]?(32|64)?", |lex| lex.slice().to_string())]
    #[regex(r"[0-9][0-9_]*[eE][+-]?[0-9]+[fF]?(32|64)?", |lex| lex.slice().to_string())]
    Float(String),

    // Decimal integers with optional suffix: 42, 42i32, 42u64
    #[regex(r"[0-9][0-9_]*([iu](8|16|32|64|int))?", |lex| lex.slice().to_string())]
    Int(String),

    // Triple-quoted string literals: """hello world"""
    // No escape processing in triple-quoted strings (raw strings)
    // Pattern: starts with """, contains anything (non-greedy), ends with """
    #[regex(r#""""[\s\S]*?""""#, |lex| lex.slice().to_string(), priority = 2)]
    TripleQuotedString(String),

    // String literals with interpolation support and escape sequence processing: "hello #{name}"
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        // Remove quotes and unescape
        let unquoted = &s[1..s.len()-1];
        let unescaped = unescape_string(unquoted);
        format!("\"{}\"", unescaped)
    })]
    String(String),

    // Character literals: 'a', '\n', '\x41'
    #[regex(r"'([^'\\]|\\.)'", |lex| {
        let s = lex.slice();
        // Remove quotes and unescape
        let unquoted = &s[1..s.len()-1];
        let unescaped = unescape_string(unquoted);
        format!("'{}'", unescaped)
    })]
    Char(String),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Comments
            Token::LineComment => write!(f, "LineComment"),
            Token::BlockComment => write!(f, "BlockComment"),

            // Keywords
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Mut => write!(f, "mut"),
            Token::Once => write!(f, "once"),
            Token::Uniq => write!(f, "uniq"),
            Token::Const => write!(f, "const"),
            Token::Type => write!(f, "type"),
            Token::Struct => write!(f, "struct"),
            Token::Enum => write!(f, "enum"),
            Token::Trait => write!(f, "trait"),
            Token::Impl => write!(f, "impl"),
            Token::Pub => write!(f, "pub"),
            Token::Use => write!(f, "use"),
            Token::As => write!(f, "as"),
            Token::LowerSelf => write!(f, "self"),
            Token::UpperSelf => write!(f, "Self"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Match => write!(f, "match"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Return => write!(f, "return"),
            Token::Defer => write!(f, "defer"),
            Token::Do => write!(f, "do"),
            Token::End => write!(f, "end"),
            Token::Spawn => write!(f, "spawn"),
            Token::Select => write!(f, "select"),
            Token::After => write!(f, "after"),
            Token::Comptime => write!(f, "comptime"),
            Token::Macro => write!(f, "macro"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::Not => write!(f, "not"),
            Token::In => write!(f, "in"),

            // Operators
            Token::PlusEq => write!(f, "+="),
            Token::MinusEq => write!(f, "-="),
            Token::StarEq => write!(f, "*="),
            Token::SlashEq => write!(f, "/="),
            Token::EqEq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::LtEq => write!(f, "<="),
            Token::GtEq => write!(f, ">="),
            Token::Shl => write!(f, "<<"),
            Token::Shr => write!(f, ">>"),
            Token::DotDotEq => write!(f, "..="),
            Token::DotDot => write!(f, ".."),
            Token::Arrow => write!(f, "->"),
            Token::FatArrow => write!(f, "=>"),
            Token::ColonColon => write!(f, "::"),
            Token::Pipe => write!(f, "|>"),
            Token::AmpMut => write!(f, "&mut"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Amp => write!(f, "&"),
            Token::BitOr => write!(f, "|"),
            Token::Caret => write!(f, "^"),
            Token::Tilde => write!(f, "~"),
            Token::Eq => write!(f, "="),
            Token::Dot => write!(f, "."),
            Token::Question => write!(f, "?"),
            Token::At => write!(f, "@"),

            // Delimiters
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::Underscore => write!(f, "_"),

            // Literals
            Token::HexInt(s) => write!(f, "HexInt({s})"),
            Token::BinInt(s) => write!(f, "BinInt({s})"),
            Token::OctInt(s) => write!(f, "OctInt({s})"),
            Token::Float(s) => write!(f, "Float({s})"),
            Token::Int(s) => write!(f, "Int({s})"),
            Token::TripleQuotedString(s) => write!(f, "TripleQuotedString({s})"),
            Token::String(s) => write!(f, "String({s})"),
            Token::Char(s) => write!(f, "Char({s})"),

            // Identifiers
            Token::Ident(s) => write!(f, "Ident({s})"),
        }
    }
}

/// A token with its span information
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub token: T,
    pub span: std::ops::Range<usize>,
}

/// Lex the input source code into a vector of spanned tokens.
/// Returns an error with the position of the first unrecognized character.
pub fn lex(source: &str) -> Result<Vec<Spanned<Token>>, usize> {
    let mut lexer = Token::lexer(source);
    let mut tokens = Vec::new();

    while let Some(result) = lexer.next() {
        match result {
            Ok(token) => {
                tokens.push(Spanned {
                    token,
                    span: lexer.span(),
                });
            }
            Err(()) => {
                return Err(lexer.span().start);
            }
        }
    }

    Ok(tokens)
}

/// Convenience function to lex and filter out comments
pub fn lex_without_comments(source: &str) -> Result<Vec<Spanned<Token>>, usize> {
    lex(source).map(|tokens| {
        tokens
            .into_iter()
            .filter(|t| !matches!(t.token, Token::LineComment | Token::BlockComment))
            .collect()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_tokens(source: &str) -> Vec<Token> {
        lex_without_comments(source)
            .unwrap()
            .into_iter()
            .map(|s| s.token)
            .collect()
    }

    #[test]
    fn test_keywords() {
        assert_eq!(
            lex_tokens("fn let mut"),
            vec![Token::Fn, Token::Let, Token::Mut]
        );
        assert_eq!(
            lex_tokens("struct enum trait impl"),
            vec![Token::Struct, Token::Enum, Token::Trait, Token::Impl]
        );
        assert_eq!(
            lex_tokens("if else match for while"),
            vec![
                Token::If,
                Token::Else,
                Token::Match,
                Token::For,
                Token::While
            ]
        );
        assert_eq!(lex_tokens("do end"), vec![Token::Do, Token::End]);
        assert_eq!(
            lex_tokens("true false and or not"),
            vec![Token::True, Token::False, Token::And, Token::Or, Token::Not]
        );
    }

    #[test]
    fn test_remaining_keywords() {
        assert_eq!(
            lex_tokens("once uniq const type"),
            vec![Token::Once, Token::Uniq, Token::Const, Token::Type]
        );
        assert_eq!(
            lex_tokens("pub use as self Self"),
            vec![
                Token::Pub,
                Token::Use,
                Token::As,
                Token::LowerSelf,
                Token::UpperSelf
            ]
        );
        assert_eq!(
            lex_tokens("break continue return defer"),
            vec![Token::Break, Token::Continue, Token::Return, Token::Defer]
        );
        assert_eq!(
            lex_tokens("spawn select after"),
            vec![Token::Spawn, Token::Select, Token::After]
        );
        assert_eq!(
            lex_tokens("comptime macro"),
            vec![Token::Comptime, Token::Macro]
        );
    }

    #[test]
    fn test_operators() {
        assert_eq!(
            lex_tokens("+ - * / %"),
            vec![
                Token::Plus,
                Token::Minus,
                Token::Star,
                Token::Slash,
                Token::Percent
            ]
        );
        assert_eq!(
            lex_tokens("== != < > <= >="),
            vec![
                Token::EqEq,
                Token::NotEq,
                Token::Lt,
                Token::Gt,
                Token::LtEq,
                Token::GtEq
            ]
        );
        assert_eq!(lex_tokens(".. ..="), vec![Token::DotDot, Token::DotDotEq]);
        assert_eq!(
            lex_tokens("-> => :: |>"),
            vec![
                Token::Arrow,
                Token::FatArrow,
                Token::ColonColon,
                Token::Pipe
            ]
        );
    }

    #[test]
    fn test_remaining_operators() {
        assert_eq!(
            lex_tokens("& | ^ ~"),
            vec![Token::Amp, Token::BitOr, Token::Caret, Token::Tilde]
        );
        assert_eq!(
            lex_tokens("= += -= *= /="),
            vec![
                Token::Eq,
                Token::PlusEq,
                Token::MinusEq,
                Token::StarEq,
                Token::SlashEq
            ]
        );
        assert_eq!(lex_tokens("&mut"), vec![Token::AmpMut]);
        assert_eq!(lex_tokens("<< >>"), vec![Token::Shl, Token::Shr]);
    }

    #[test]
    fn test_integers() {
        assert_eq!(lex_tokens("42"), vec![Token::Int("42".into())]);
        assert_eq!(lex_tokens("42i32"), vec![Token::Int("42i32".into())]);
        assert_eq!(lex_tokens("0xFF"), vec![Token::HexInt("0xFF".into())]);
        assert_eq!(lex_tokens("0b1010"), vec![Token::BinInt("0b1010".into())]);
        assert_eq!(lex_tokens("0o777"), vec![Token::OctInt("0o777".into())]);
        assert_eq!(lex_tokens("123_456"), vec![Token::Int("123_456".into())]);
        assert_eq!(
            lex_tokens("0xDEAD_BEEF"),
            vec![Token::HexInt("0xDEAD_BEEF".into())]
        );
        assert_eq!(
            lex_tokens("42u8 42i16 42u32 42i64 42uint"),
            vec![
                Token::Int("42u8".into()),
                Token::Int("42i16".into()),
                Token::Int("42u32".into()),
                Token::Int("42i64".into()),
                Token::Int("42uint".into()),
            ]
        );
    }

    #[test]
    fn test_floats() {
        assert_eq!(lex_tokens("3.14"), vec![Token::Float("3.14".into())]);
        assert_eq!(lex_tokens("3.14f32"), vec![Token::Float("3.14f32".into())]);
        assert_eq!(lex_tokens("1e10"), vec![Token::Float("1e10".into())]);
        assert_eq!(lex_tokens("1.5e-10"), vec![Token::Float("1.5e-10".into())]);
        assert_eq!(lex_tokens("2.5f64"), vec![Token::Float("2.5f64".into())]);
        assert_eq!(lex_tokens("123.456"), vec![Token::Float("123.456".into())]);
    }

    #[test]
    fn test_strings_and_chars() {
        // Basic strings are unescaped
        assert_eq!(
            lex_tokens(r#""hello""#),
            vec![Token::String(r#""hello""#.into())]
        );
        assert_eq!(lex_tokens(r#"'a'"#), vec![Token::Char("'a'".into())]);
        // Escape sequences are processed
        assert_eq!(lex_tokens(r#"'\n'"#), vec![Token::Char("'\n'".into())]);
        assert_eq!(
            lex_tokens(r#""hello #{name}""#),
            vec![Token::String(r#""hello #{name}""#.into())]
        );
        assert_eq!(lex_tokens("()"), vec![Token::LParen, Token::RParen]);
    }

    #[test]
    fn test_escape_sequences() {
        // Test common escape sequences
        assert_eq!(
            lex_tokens(r#""hello\nworld""#),
            vec![Token::String(format!(r#""hello{}world""#, "\n"))]
        );
        assert_eq!(
            lex_tokens(r#""tab\there""#),
            vec![Token::String(format!(r#""tab{}here""#, "\t"))]
        );
        assert_eq!(
            lex_tokens(r#""quote\"here""#),
            vec![Token::String(r#""quote"here""#.into())]
        );
        assert_eq!(
            lex_tokens(r#""backslash\\path""#),
            vec![Token::String(r#""backslash\path""#.into())]
        );
        // Hex escape
        assert_eq!(
            lex_tokens(r#""hex\x41test""#),
            vec![Token::String(r#""hexAtest""#.into())]
        );
    }

    #[test]
    fn test_triple_quoted_strings() {
        // Triple-quoted strings preserve content as-is (no escape processing)
        assert_eq!(
            lex_tokens(r#""""hello world""""#),
            vec![Token::TripleQuotedString(r#""""hello world""""#.into())]
        );
        // Triple-quoted strings can contain quotes
        assert_eq!(
            lex_tokens(r#""""he said "hi""""#),
            vec![Token::TripleQuotedString(r#""""he said "hi""""#.into())]
        );
        // Triple-quoted strings can contain newlines
        let multiline = "\"\"\"line 1\nline 2\"\"\"";
        assert_eq!(
            lex_tokens(multiline),
            vec![Token::TripleQuotedString(multiline.into())]
        );
        // Escape sequences are NOT processed in triple-quoted strings
        assert_eq!(
            lex_tokens(r#""""backslash\n""""#),
            vec![Token::TripleQuotedString(r#""""backslash\n""""#.into())]
        );
    }

    #[test]
    fn test_identifiers() {
        assert_eq!(
            lex_tokens("foo bar_baz _private"),
            vec![
                Token::Ident("foo".into()),
                Token::Ident("bar_baz".into()),
                Token::Ident("_private".into()),
            ]
        );
    }

    #[test]
    fn test_identifiers_edge_cases() {
        assert_eq!(
            lex_tokens("a b2 c_3 _ d_e_f"),
            vec![
                Token::Ident("a".into()),
                Token::Ident("b2".into()),
                Token::Ident("c_3".into()),
                Token::Underscore,
                Token::Ident("d_e_f".into()),
            ]
        );
        assert_eq!(
            lex_tokens("Self Option None"),
            vec![
                Token::UpperSelf,
                Token::Ident("Option".into()),
                Token::Ident("None".into()),
            ]
        );
    }

    #[test]
    fn test_delimiters() {
        assert_eq!(
            lex_tokens("( ) { } [ ]"),
            vec![
                Token::LParen,
                Token::RParen,
                Token::LBrace,
                Token::RBrace,
                Token::LBracket,
                Token::RBracket
            ]
        );
        assert_eq!(
            lex_tokens(", : ; _"),
            vec![
                Token::Comma,
                Token::Colon,
                Token::Semicolon,
                Token::Underscore
            ]
        );
    }

    #[test]
    fn test_comments() {
        let tokens = lex("# comment\nfoo").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::LineComment));

        let tokens = lex("#* multi\nline *# bar").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::BlockComment));
    }

    #[test]
    fn test_comments_edge_cases() {
        // Empty comment
        let tokens = lex("#\nfoo").unwrap();
        assert!(matches!(tokens[0].token, Token::LineComment));

        // Block comment with * inside
        let tokens = lex("#* comment * with * inside *# foo").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::BlockComment));
    }

    #[test]
    fn test_comments_comprehensive() {
        // Single line comments
        let tokens = lex("let x = 1 # inline comment\nfoo").unwrap();
        assert_eq!(tokens.len(), 6);
        assert!(matches!(tokens[4].token, Token::LineComment));

        let tokens = lex("# comment with numbers 123\n# another").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::LineComment));
        assert!(matches!(tokens[1].token, Token::LineComment));

        let tokens = lex("# comment with special chars !@#$%^&*()\nfoo").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::LineComment));

        // Block comments
        let tokens = lex("#* multi\nline\ncomment *# foo").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::BlockComment));

        let tokens = lex("#* comment with # inside *# foo").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::BlockComment));

        let tokens = lex("#* nested * not really *# foo").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::BlockComment));

        // Empty block comment
        let tokens = lex("#**# foo").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::BlockComment));

        // Block comment with code-like content
        let tokens = lex("#* let x = fn() -> int { 42 } *# foo").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::BlockComment));

        // Comments at EOF
        let tokens = lex("foo # comment").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[1].token, Token::LineComment));

        let tokens = lex("foo #* block comment *#").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[1].token, Token::BlockComment));

        // Multiple consecutive comments
        let tokens = lex("# first\n#* second *#\n# third").unwrap();
        assert_eq!(tokens.len(), 3);
        assert!(matches!(tokens[0].token, Token::LineComment));
        assert!(matches!(tokens[1].token, Token::BlockComment));
        assert!(matches!(tokens[2].token, Token::LineComment));

        // Comments with quotes
        let tokens = lex("#* comment with \"quotes\" and 'chars' *# foo").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::BlockComment));

        // Line comment with trailing content (should not be included)
        let tokens = lex_without_comments("# comment\nfoo # another").unwrap();
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0].token, Token::Ident(s) if s == "foo"));

        // Block comment spanning multiple lines with various content
        let tokens = lex("#* \nline1\nline2\n123\n!@#\n *# foo").unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::BlockComment));
    }

    #[test]
    fn test_struct_definition() {
        let source = r#"
struct Point
  x: f64
  y: f64
end
        "#;
        let tokens = lex_tokens(source);
        assert_eq!(
            tokens,
            vec![
                Token::Struct,
                Token::Ident("Point".into()),
                Token::Ident("x".into()),
                Token::Colon,
                Token::Ident("f64".into()),
                Token::Ident("y".into()),
                Token::Colon,
                Token::Ident("f64".into()),
                Token::End,
            ]
        );
    }

    #[test]
    fn test_function_definition() {
        let source = r#"
fn add(a: int, b: int) -> int
  a + b
        "#;
        let tokens = lex_tokens(source);
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Ident("add".into()),
                Token::LParen,
                Token::Ident("a".into()),
                Token::Colon,
                Token::Ident("int".into()),
                Token::Comma,
                Token::Ident("b".into()),
                Token::Colon,
                Token::Ident("int".into()),
                Token::RParen,
                Token::Arrow,
                Token::Ident("int".into()),
                Token::Ident("a".into()),
                Token::Plus,
                Token::Ident("b".into()),
            ]
        );
    }

    #[test]
    fn test_attributes() {
        let source = "@derive(Show, Eq)";
        let tokens = lex_tokens(source);
        assert_eq!(
            tokens,
            vec![
                Token::At,
                Token::Ident("derive".into()),
                Token::LParen,
                Token::Ident("Show".into()),
                Token::Comma,
                Token::Ident("Eq".into()),
                Token::RParen,
            ]
        );
    }

    #[test]
    fn test_attributes_usage() {
        let source = "@inline @test @cfg(os = \"linux\")";
        let tokens = lex_tokens(source);
        assert_eq!(
            tokens,
            vec![
                Token::At,
                Token::Ident("inline".into()),
                Token::At,
                Token::Ident("test".into()),
                Token::At,
                Token::Ident("cfg".into()),
                Token::LParen,
                Token::Ident("os".into()),
                Token::Eq,
                Token::String("\"linux\"".into()),
                Token::RParen,
            ]
        );
    }

    #[test]
    fn test_pipe_operator() {
        let source = r#""hello" |> upcase() |> reverse()"#;
        let tokens = lex_tokens(source);
        assert_eq!(
            tokens,
            vec![
                Token::String(r#""hello""#.into()),
                Token::Pipe,
                Token::Ident("upcase".into()),
                Token::LParen,
                Token::RParen,
                Token::Pipe,
                Token::Ident("reverse".into()),
                Token::LParen,
                Token::RParen,
            ]
        );
    }

    #[test]
    fn test_lambda() {
        let source = "|x, y| x + y";
        let tokens = lex_tokens(source);
        assert_eq!(
            tokens,
            vec![
                Token::BitOr,
                Token::Ident("x".into()),
                Token::Comma,
                Token::Ident("y".into()),
                Token::BitOr,
                Token::Ident("x".into()),
                Token::Plus,
                Token::Ident("y".into()),
            ]
        );
    }

    #[test]
    fn test_match_expression() {
        let source = r#"
match value
  Option::Some(x) => x
  Option::None => 0
end
        "#;
        let tokens = lex_tokens(source);
        assert_eq!(
            tokens,
            vec![
                Token::Match,
                Token::Ident("value".into()),
                Token::Ident("Option".into()),
                Token::ColonColon,
                Token::Ident("Some".into()),
                Token::LParen,
                Token::Ident("x".into()),
                Token::RParen,
                Token::FatArrow,
                Token::Ident("x".into()),
                Token::Ident("Option".into()),
                Token::ColonColon,
                Token::Ident("None".into()),
                Token::FatArrow,
                Token::Int("0".into()),
                Token::End,
            ]
        );
    }

    #[test]
    fn test_complex_expressions() {
        // Enum definition from spec
        let source = r#"
enum Option<T>
  Some(T)
  None
end
        "#;
        let tokens = lex_tokens(source);
        assert!(tokens.contains(&Token::Enum));
        assert!(tokens.contains(&Token::Lt));
        assert!(tokens.contains(&Token::Gt));

        // Type alias
        let source = "type Name = string";
        let tokens = lex_tokens(source);
        assert_eq!(
            tokens,
            vec![
                Token::Type,
                Token::Ident("Name".into()),
                Token::Eq,
                Token::Ident("string".into())
            ]
        );

        // Destructuring
        let source = "let [a, b] = arr";
        let tokens = lex_tokens(source);
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::LBracket,
                Token::Ident("a".into()),
                Token::Comma,
                Token::Ident("b".into()),
                Token::RBracket,
                Token::Eq,
                Token::Ident("arr".into())
            ]
        );

        // Range literals
        assert_eq!(
            lex_tokens("0..10 0..=10"),
            vec![
                Token::Int("0".into()),
                Token::DotDot,
                Token::Int("10".into()),
                Token::Int("0".into()),
                Token::DotDotEq,
                Token::Int("10".into())
            ]
        );
    }

    #[test]
    fn test_invalid_tokens() {
        let source = "let x = $";
        let tokens = lex(source);
        assert!(tokens.is_err());
    }

    #[test]
    fn test_empty_source() {
        let tokens = lex_tokens("");
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_complex_spec_example() {
        // Example from SPEC.md
        let source = r#"
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

fn main()
  let user = User { name: "Alice", age: 30 }
  let cmd = Command::Greet(user)

  match cmd
    Command::Greet(u) => puts("Hello #{u.name}")
    Command::Quit => puts("Bye")
  end
end
        "#;

        let tokens = lex_tokens(source);
        assert!(!tokens.is_empty());

        // Check for specific sequence starts
        assert!(tokens.contains(&Token::Use));
        assert!(tokens.contains(&Token::Struct));
        assert!(tokens.contains(&Token::Enum));
        assert!(tokens.contains(&Token::Fn));
        assert!(tokens.contains(&Token::Match));

        // Verify string interpolation token format
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::String(s) if s.contains("#{u.name}")))
        );
    }
}
