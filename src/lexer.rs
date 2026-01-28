use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error = LexError)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"//.*")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
pub enum Token {
    #[token("extern")]
    Extern,
    #[token("type")]
    Type,
    #[token("use")]
    Use,
    #[token("as")]
    As,
    #[token("trait")]
    Trait,
    #[token("impl")]
    Impl,
    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("match")]
    Match,
    #[token("fn")]
    Fn,
    #[token("static")]
    Static,
    #[token("Self")]
    SelfType,
    #[token("self")]
    SelfValue,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("**")]
    Power,
    #[token("%")]
    Percent,
    #[token("^")]
    Caret,
    #[token("&")]
    Ampersand,
    #[token("|")]
    Pipe,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,
    #[token("==")]
    EqEq,
    #[token("!=")]
    Neq,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("..")]
    DotDot,
    #[token("=")]
    Eq,
    #[token("|>")]
    PipeRight,
    #[token(".")]
    Dot,
    #[token("::")]
    PathSep,
    #[token("#[")]
    LabelOpen,
    #[token("=>")]
    FatArrow,
    #[token("->")]
    Arrow,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("@")]
    At,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    #[regex(r"0x[0-9a-fA-F]+", |lex| i64::from_str_radix(&lex.slice()[2..], 16).ok())]
    #[regex(r"0b[01]+", |lex| i64::from_str_radix(&lex.slice()[2..], 2).ok())]
    #[regex(r"0o[0-7]+", |lex| i64::from_str_radix(&lex.slice()[2..], 8).ok())]
    Integer(i64),

    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),

    #[regex(r#""([^"\\]|\\.)*""#, parse_string)]
    String(String),

    #[regex(r#"'([^'\\]|\\.)'"#, parse_char)]
    Char(char),
}

fn parse_string(lex: &mut logos::Lexer<Token>) -> Option<String> {
    let s = lex.slice();
    let content = &s[1..s.len() - 1];
    let mut result = String::with_capacity(content.len());
    let mut chars = content.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('\'') => result.push('\''),
                Some('\"') => result.push('\"'),
                Some('0') => result.push('\0'),
                _ => return None,
            }
        } else {
            result.push(c);
        }
    }
    Some(result)
}

fn parse_char(lex: &mut logos::Lexer<Token>) -> Option<char> {
    let s = lex.slice();
    if s.len() < 3 {
        return None;
    }
    let content = &s[1..s.len() - 1];
    if content.starts_with('\\') {
        match content.chars().nth(1) {
            Some('n') => Some('\n'),
            Some('r') => Some('\r'),
            Some('t') => Some('\t'),
            Some('\\') => Some('\\'),
            Some('\'') => Some('\''),
            Some('\"') => Some('\"'),
            Some('0') => Some('\0'),
            _ => None,
        }
    } else {
        content.chars().next()
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexError {
    #[default]
    InvalidToken,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> Vec<Token> {
        Token::lexer(input)
            .map(|t| t.unwrap_or_else(|_| panic!("Lex error at {}", input)))
            .collect()
    }

    #[test]
    fn test_keywords() {
        let input = "extern type use as trait impl let if then else while for in match fn static Self self true false break continue return";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Extern,
                Token::Type,
                Token::Use,
                Token::As,
                Token::Trait,
                Token::Impl,
                Token::Let,
                Token::If,
                Token::Then,
                Token::Else,
                Token::While,
                Token::For,
                Token::In,
                Token::Match,
                Token::Fn,
                Token::Static,
                Token::SelfType,
                Token::SelfValue,
                Token::True,
                Token::False,
                Token::Break,
                Token::Continue,
                Token::Return,
            ]
        );
    }

    #[test]
    fn test_operators() {
        let input = "+ - * / ** % ^ & | << >> < > <= >= == != && || .. = |> . :: #[ => ->";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Plus,
                Token::Minus,
                Token::Star,
                Token::Slash,
                Token::Power,
                Token::Percent,
                Token::Caret,
                Token::Ampersand,
                Token::Pipe,
                Token::Shl,
                Token::Shr,
                Token::Lt,
                Token::Gt,
                Token::Leq,
                Token::Geq,
                Token::EqEq,
                Token::Neq,
                Token::AndAnd,
                Token::OrOr,
                Token::DotDot,
                Token::Eq,
                Token::PipeRight,
                Token::Dot,
                Token::PathSep,
                Token::LabelOpen,
                Token::FatArrow,
                Token::Arrow,
            ]
        );
    }

    #[test]
    fn test_delimiters() {
        let input = "( ) [ ] { } , ; : @";
        let tokens = lex(input);
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
                Token::At,
            ]
        );
    }

    #[test]
    fn test_literals() {
        let input = "my_var 123 0x123 0b101 0o123 123.456 1.2e3 \"hello world\" '\\n' 'a'";
        let tokens = lex(input);
        assert_eq!(tokens[0], Token::Identifier("my_var".to_string()));
        assert_eq!(tokens[1], Token::Integer(123));
        assert_eq!(tokens[2], Token::Integer(0x123));
        assert_eq!(tokens[3], Token::Integer(0b101));
        assert_eq!(tokens[4], Token::Integer(0o123));
        assert_eq!(tokens[5], Token::Float(123.456));
        assert_eq!(tokens[6], Token::Float(1200.0));
        assert_eq!(tokens[7], Token::String("hello world".to_string()));
        assert_eq!(tokens[8], Token::Char('\n'));
        assert_eq!(tokens[9], Token::Char('a'));
    }

    #[test]
    fn test_comments() {
        let input = "let x = 5 // some comment\n /* block\ncomment */ let y = 10";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Identifier("x".to_string()),
                Token::Eq,
                Token::Integer(5),
                Token::Let,
                Token::Identifier("y".to_string()),
                Token::Eq,
                Token::Integer(10),
            ]
        );
    }

    #[test]
    fn test_complex_snippet() {
        let input = "fn main() { let x = 5; if x > 0 then { print(\"hello\") } }";
        let tokens = lex(input);
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("main".to_string()),
                Token::LParen,
                Token::RParen,
                Token::LBrace,
                Token::Let,
                Token::Identifier("x".to_string()),
                Token::Eq,
                Token::Integer(5),
                Token::Semicolon,
                Token::If,
                Token::Identifier("x".to_string()),
                Token::Gt,
                Token::Integer(0),
                Token::Then,
                Token::LBrace,
                Token::Identifier("print".to_string()),
                Token::LParen,
                Token::String("hello".to_string()),
                Token::RParen,
                Token::RBrace,
                Token::RBrace,
            ]
        );
    }
}
