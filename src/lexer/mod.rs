use logos::Logos;
use std::fmt;

#[cfg(test)]
mod tests;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \n\r\t\f]+")] // Ignore this regex pattern between tokens
#[logos(skip r"//(.*)\n")] // Ignore this regex pattern between tokens
#[logos(skip r"/\*([^*]|\*[^/])*\*/")] // Ignore this regex pattern between tokens
#[derive(Clone)]
pub enum Token {
    #[regex(r"true|false", |lex| {
        lex.slice().parse::<bool>().unwrap()
    })]
    Bool(bool),

    #[regex(r"0|[1-9][0-9_]*", |lex| {
        let s = lex.slice().replace("_", "");
        // We parse to i64 for wider support.
        s.parse::<i64>().unwrap()
    }, priority = 4)]
    Int(i64),

    #[regex(r"(([0-9][0-9_]*\.[0-9_]+|[0-9]*\.[0-9_]+)([eE][+-]?[0-9_]+)?)", |lex| {
        let s = lex.slice().replace("_", "");
        s.parse::<f64>().unwrap()
    }, priority = 3)]
    Float(f64),

    #[regex(r#""([^"\\]*(\\.[^"\\]*)*)""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1]
            .replace("\\\"", "\"")
            .replace("\\\\", "\\")
            .replace("\\n", "\n")
            .replace("\\r", "\r")
            .replace("\\t", "\t")
    })]
    String(String),

    #[regex(r#"r#"([^"]*)""#, |lex| {
        let s = lex.slice();
        // Remove the outer r" and " (s[2..s.len() - 1])
        s[3..s.len() - 1].to_string()
    })]
    RawString(String),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex|{
        lex.slice().to_string()
    })]
    Variable(String),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*!", |lex|{
        let s = lex.slice();
        s[..s.len()-1].to_string()
    })]
    MacroInvocation(String),

    #[token("bool")]
    KeywordBool,

    #[token("int")]
    KeywordInt,

    #[token("float")]
    KeywordFloat,

    #[token("string")]
    KeywordString,

    #[token("let")]
    KeywordLet,

    #[token("pub")]
    KeywordPub,

    #[token("if")]
    KeywordIf,

    #[token("else")]
    KeywordElse,

    #[token("fn")]
    KeywordFn,

    #[token("def")]
    KeywordDef,

    #[token("macro")]
    KeywordMacro,

    #[token("with")]
    KeywordWith,

    #[token("as")]
    KeywordAs,

    #[token("in")]
    KeywordIn,

    #[token("for")]
    KeywordFor,

    #[token("while")]
    KeywordWhile,

    #[token("loop")]
    KeywordLoop,

    #[token("where")]
    KeywordWhere,

    #[token("extern")]
    KeywordExtern,

    #[token("effect")]
    KeywordEffect,

    #[token("perform")]
    KeywordPerform,

    #[token("handle")]
    KeywordHandle,

    #[token("forall")]
    KeywordForall,

    #[token("exists")]
    KeywordExists,

    #[token("import")]
    KeywordImport,

    #[token("struct")]
    KeywordStruct,

    #[token("enum")]
    KeywordEnum,

    #[token("impl")]
    KeywordImpl,

    #[token("trait")]
    KeywordTrait,

    #[token("type")]
    KeywordType,

    #[token("match")]
    KeywordMatch,

    #[token("return")]
    KeywordReturn,

    #[token("break")]
    KeywordBreak,

    #[token("continue")]
    KeywordContinue,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Mul,

    #[token("/")]
    Div,

    #[token("%")]
    Mod,

    #[token("**", priority = 3)]
    Power,

    #[token("$")]
    Dollar,

    #[token("@")]
    At,

    #[token("==")]
    Eq,

    #[token("!=")]
    NotEq,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

    #[token("<=")]
    LessEq,

    #[token(">=")]
    GreaterEq,

    #[token("and")]
    And,

    #[token("or")]
    Or,

    #[token("xor")]
    Xor,

    #[token("nor")]
    Nor,

    #[token("not")]
    Not,

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

    #[token("#[")]
    HashLBracket, // Attribute start (#[test], #[macro])

    #[token(",")]
    Comma,

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[token(".")]
    Dot,

    #[token("...")]
    Spread,

    #[token("..")]
    DotDot,

    #[token("::")]
    Access,

    #[token("->")]
    Arrow,

    #[token("~")]
    Tilde,

    #[token("!")]
    Bang,

    // New tokens for pattern matching
    #[token("=>")]
    FatArrow, // For match arms

    #[token("|")]
    Union,

    #[token("|>")]
    Pipe,

    #[token("?.")]
    OptionalChain,

    #[token("?")]
    Unwrap,

    #[token("=")]
    Assign,

    #[token("+=")]
    AddAssign,

    #[token("-=")]
    SubAssign,

    #[token("*=")]
    MulAssign,

    #[token("/=")]
    DivAssign,

    #[token("%=")]
    ModAssign,
}

/// Display trait for nice printing and error messages.
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Literals
            Token::Bool(b) => write!(f, "{}", b),
            Token::Int(i) => write!(f, "{}", i),
            Token::Float(fl) => write!(f, "{}", fl),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::RawString(s) => write!(f, "r#\"{}\"", s),
            Token::Variable(s) => write!(f, "{}", s),

            // Macro
            Token::MacroInvocation(s) => write!(f, "{}!", s),
            Token::HashLBracket => write!(f, "#["),

            // Keywords
            Token::KeywordBool => write!(f, "bool"),
            Token::KeywordInt => write!(f, "int"),
            Token::KeywordFloat => write!(f, "float"),
            Token::KeywordString => write!(f, "str"),
            Token::KeywordImport => write!(f, "import"),
            Token::KeywordDef => write!(f, "def"),
            Token::KeywordMacro => write!(f, "macro"),
            Token::KeywordLet => write!(f, "let"),
            Token::KeywordPub => write!(f, "pub"),
            Token::KeywordIf => write!(f, "if"),
            Token::KeywordIn => write!(f, "in"),
            Token::KeywordElse => write!(f, "else"),
            Token::KeywordFn => write!(f, "fn"),
            Token::KeywordFor => write!(f, "for"),
            Token::KeywordWhile => write!(f, "while"),
            Token::KeywordLoop => write!(f, "loop"),
            Token::KeywordWhere => write!(f, "where"),
            Token::KeywordReturn => write!(f, "return"),
            Token::KeywordBreak => write!(f, "break"),
            Token::KeywordContinue => write!(f, "continue"),
            Token::KeywordExtern => write!(f, "extern"),
            Token::KeywordEffect => write!(f, "effect"),
            Token::KeywordPerform => write!(f, "perform"),
            Token::KeywordHandle => write!(f, "handle"),
            Token::KeywordForall => write!(f, "forall"),
            Token::KeywordExists => write!(f, "exists"),
            Token::KeywordWith => write!(f, "with"),
            Token::KeywordAs => write!(f, "as"),
            Token::KeywordStruct => write!(f, "struct"),
            Token::KeywordEnum => write!(f, "enum"),
            Token::KeywordImpl => write!(f, "impl"),
            Token::KeywordTrait => write!(f, "trait"),
            Token::KeywordType => write!(f, "type"),
            Token::KeywordMatch => write!(f, "match"),

            // Logical Operators
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::Xor => write!(f, "xor"),
            Token::Nor => write!(f, "nor"),
            Token::Not => write!(f, "not"),

            // Operators
            Token::Plus => write!(f, "+"),
            Token::Dollar => write!(f, "$"),
            Token::At => write!(f, "@"),
            Token::Minus => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Mod => write!(f, "%"),
            Token::Power => write!(f, "**"),
            Token::Eq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::Less => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::LessEq => write!(f, "<="),
            Token::GreaterEq => write!(f, ">="),
            Token::Pipe => write!(f, "|>"),
            Token::OptionalChain => write!(f, "?."),
            Token::Unwrap => write!(f, "?"),
            Token::Assign => write!(f, "="),
            Token::AddAssign => write!(f, "+="),
            Token::SubAssign => write!(f, "-="),
            Token::MulAssign => write!(f, "*="),
            Token::DivAssign => write!(f, "/="),
            Token::ModAssign => write!(f, "%="),

            // Punctuation & Delimiters
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::Dot => write!(f, "."),
            Token::DotDot => write!(f, ".."),
            Token::Spread => write!(f, "..."),
            Token::Access => write!(f, "::"),
            Token::Arrow => write!(f, "->"),
            Token::Tilde => write!(f, "~"),
            Token::Bang => write!(f, "!"),
            Token::FatArrow => write!(f, "=>"),
            // Token::Underscore => write!(f, "_"),
            Token::Union => write!(f, "|"),
        }
    }
}
