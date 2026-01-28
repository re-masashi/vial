use std::ops::Range;

#[derive(Debug, Clone, Default)]
pub struct Location {
    pub span: Range<usize>,
    pub file: String,
}

#[derive(Debug, Clone)]
pub struct Meta {
    pub location: Location,
    pub attributes: Vec<Attribute>,
}

impl Default for Meta {
    fn default() -> Self {
        Self {
            location: Location {
                span: 0..0,
                file: "".to_string(),
            },
            attributes: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: String,
    pub args: Vec<AttributeArg>,
}

#[derive(Debug, Clone)]
pub enum AttributeArg {
    Identifier(String),
    KeyValue { key: String, value: String },
    Literal(AttrLiteral),
}

#[derive(Debug, Clone)]
pub enum AttrLiteral {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
}
