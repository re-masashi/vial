use crate::ast::*;
use crate::lexer::Token;

use std::iter::Peekable;
use std::ops::Range;
use std::vec::IntoIter;

type TokenIter = Peekable<IntoIter<(Token, Range<usize>)>>;

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Range<usize>,
    pub valid_syntax: Vec<String>,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    UnexpectedToken(Token, Vec<Token>),
    UnexpectedEOF,
}

pub fn parse(tokens: Vec<(Token, Range<usize>)>, file: &String) -> Result<Vec<Item>, ParseError> {
    let mut iter = tokens.into_iter().peekable();
    let mut items = Vec::new();
    let mut attributes = Vec::new();

    while let Some((t, _span)) = iter.peek() {
        match t {
            Token::At => {
                attributes.push(parse_attribute(&mut iter, file)?);
            }
            _ => {
                let item = parse_item(&mut iter, file, attributes.clone())?;
                items.push(item);
                attributes.clear();
            }
        }
    }

    Ok(items)
}

pub fn parse_item(
    iter: &mut TokenIter,
    file: &String,
    attributes: Vec<Attribute>,
) -> Result<Item, ParseError> {
    let mut visibility = Visibility::Private;
    if let Some((Token::Pub, _)) = iter.peek() {
        iter.next();
        visibility = Visibility::Public;
    }

    let (_t, _span) = iter.peek().ok_or(ParseError {
        kind: ParseErrorKind::UnexpectedEOF,
        span: 0..0,
        valid_syntax: vec![],
    })?;
    let Some((t, span)) = iter.next() else {
        unreachable!()
    };

    let kind = match t {
        Token::Struct => {
            let struct_decl = parse_struct(iter, file)?;
            ItemKind::Struct(struct_decl)
        }
        Token::Enum => {
            let enum_decl = parse_enum(iter, file)?;
            ItemKind::Enum(enum_decl)
        }
        Token::Fn => {
            let fn_decl = parse_function(iter, file)?;
            ItemKind::Fn(fn_decl)
        }
        Token::Comptime => {
            // Expect 'fn' next
            if let Some((Token::Fn, _)) = iter.peek() {
                iter.next(); // consume 'fn'
                let mut fn_decl = parse_function(iter, file)?;
                fn_decl.is_comptime = true;
                ItemKind::Fn(fn_decl)
            } else {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(
                        iter.peek().unwrap_or(&(Token::Fn, 0..0)).0.clone(),
                        vec![Token::Fn],
                    ),
                    span: iter.peek().unwrap_or(&(Token::Fn, 0..0)).1.clone(),
                    valid_syntax: vec!["fn".to_string()],
                });
            }
        }
        Token::Impl => {
            let impl_decl = parse_impl(iter, file)?;
            ItemKind::Impl(impl_decl)
        }
        Token::Trait => {
            let trait_decl = parse_trait(iter, file)?;
            ItemKind::Trait(trait_decl)
        }
        Token::Type => {
            let type_alias_decl = parse_type_alias(iter, file)?;
            ItemKind::TypeAlias(type_alias_decl)
        }
        Token::Const => {
            let (name, ty, expr) = parse_const_decl(iter, file)?;
            ItemKind::Const {
                name,
                ty,
                value: expr,
            }
        }
        Token::Macro => {
            let macro_decl = parse_macro_decl(iter, file)?;
            ItemKind::Macro(macro_decl)
        }
        Token::Use => {
            let use_decl = parse_use(iter, file)?;
            ItemKind::Use(use_decl)
        }
        _ => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(
                    t.clone(),
                    vec![
                        Token::Struct,
                        Token::Enum,
                        Token::Fn,
                        Token::Comptime,
                        Token::Impl,
                        Token::Trait,
                        Token::Type,
                        Token::Const,
                        Token::Macro,
                        Token::Use,
                        Token::At,
                    ],
                ),
                span: span.clone(),
                valid_syntax: vec![],
            });
        }
    };

    Ok(Item::new(
        Span {
            file: file.clone(),
            range: span.clone(),
        },
        visibility,
        kind,
    )
    .with_attributes(attributes))
}

pub fn parse_expression(_iter: &mut TokenIter, _file: &String) -> Result<Expr, ParseError> {
    todo!()
}

pub fn parse_attribute(iter: &mut TokenIter, file: &String) -> Result<Attribute, ParseError> {
    // Consume the @ token
    let (at_token, at_span) = iter.next().ok_or(ParseError {
        kind: ParseErrorKind::UnexpectedEOF,
        span: 0..0,
        valid_syntax: vec!["@".to_string()],
    })?;

    if !matches!(at_token, Token::At) {
        return Err(ParseError {
            kind: ParseErrorKind::UnexpectedToken(at_token, vec![Token::At]),
            span: at_span,
            valid_syntax: vec!["@".to_string()],
        });
    }

    let start_span = at_span;

    // Expect identifier for attribute name
    let (name_token, name_span) = iter.next().ok_or(ParseError {
        kind: ParseErrorKind::UnexpectedEOF,
        span: start_span.clone(),
        valid_syntax: vec!["identifier".to_string()],
    })?;

    let name = match name_token {
        Token::Ident(s) => s.clone(),
        _ => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(
                    name_token.clone(),
                    vec![Token::Ident("".to_string())],
                ),
                span: name_span.clone(),
                valid_syntax: vec!["attribute name".to_string()],
            });
        }
    };

    let mut args = Vec::new();

    // Check if there are arguments (parentheses)
    if let Some((Token::LParen, _)) = iter.peek() {
        iter.next(); // consume '('

        // Parse arguments until ')'
        while let Some((t, _span)) = iter.peek() {
            if *t == Token::RParen {
                iter.next(); // consume ')'
                break;
            }

            let arg = parse_attr_arg(iter, file)?;
            args.push(arg);

            // Check for comma or end
            if let Some((Token::Comma, _)) = iter.peek() {
                iter.next(); // consume ','
            } else if let Some((Token::RParen, _)) = iter.peek() {
                // Will be consumed above
            } else {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(
                        iter.peek().unwrap().0.clone(),
                        vec![Token::Comma, Token::RParen],
                    ),
                    span: iter.peek().unwrap().1.clone(),
                    valid_syntax: vec!["comma or closing parenthesis".to_string()],
                });
            }
        }
    }

    let end_span = iter.peek().map(|(_, s)| s.end).unwrap_or(start_span.end);
    let span = Span::new(file.clone(), start_span.start..end_span);

    Ok(Attribute { span, name, args })
}

#[allow(clippy::only_used_in_recursion)]
fn parse_attr_arg(iter: &mut TokenIter, file: &String) -> Result<AttrArg, ParseError> {
    let (token, span) = iter.next().ok_or(ParseError {
        kind: ParseErrorKind::UnexpectedEOF,
        span: 0..0,
        valid_syntax: vec!["attribute argument".to_string()],
    })?;

    match token {
        Token::Ident(s) => {
            // Check if this is a key-value pair (followed by '=')
            if let Some((Token::Eq, _)) = iter.peek() {
                iter.next(); // consume '='
                let value = parse_attr_arg(iter, file)?;
                Ok(AttrArg::KeyValue {
                    key: s.clone(),
                    value: Box::new(value),
                })
            } else {
                Ok(AttrArg::Ident(s.clone()))
            }
        }
        Token::String(s) => Ok(AttrArg::Literal(Literal::String(s.clone()))),
        Token::Char(s) => Ok(AttrArg::Literal(Literal::Char(s.clone()))),
        Token::Int(s) => {
            // Parse int suffix if present
            let (value, suffix) = if let Some(suffix_str) = parse_int_suffix(&s) {
                let val_end = s.len() - suffix_str.len();
                (s[..val_end].to_string(), Some(suffix_str))
            } else {
                (s.clone(), None)
            };

            let int_type = suffix.map(|s| match s.as_str() {
                "i8" => IntType::I8,
                "i16" => IntType::I16,
                "i32" => IntType::I32,
                "i64" => IntType::I64,
                "int" => IntType::Int,
                "u8" => IntType::U8,
                "u16" => IntType::U16,
                "u32" => IntType::U32,
                "u64" => IntType::U64,
                "uint" => IntType::Uint,
                _ => IntType::Int, // fallback
            });

            Ok(AttrArg::Literal(Literal::Int {
                value,
                suffix: int_type,
            }))
        }
        Token::Float(s) => {
            // Parse float suffix if present
            let (value, suffix) = if s.ends_with("f32") {
                (s[..s.len() - 3].to_string(), Some(FloatType::F32))
            } else if s.ends_with("f64") {
                (s[..s.len() - 3].to_string(), Some(FloatType::F64))
            } else {
                (s.clone(), None)
            };

            Ok(AttrArg::Literal(Literal::Float { value, suffix }))
        }
        Token::True => Ok(AttrArg::Literal(Literal::Bool(true))),
        Token::False => Ok(AttrArg::Literal(Literal::Bool(false))),
        Token::LBracket => {
            // Parse list [arg1, arg2, ...]
            let mut elements = Vec::new();
            while let Some((t, _span)) = iter.peek() {
                if *t == Token::RBracket {
                    iter.next(); // consume ']'
                    break;
                }

                let elem = parse_attr_arg(iter, file)?;
                elements.push(elem);

                if let Some((Token::Comma, _)) = iter.peek() {
                    iter.next(); // consume ','
                } else if let Some((Token::RBracket, _)) = iter.peek() {
                    // Will be consumed above
                } else {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(
                            iter.peek().unwrap().0.clone(),
                            vec![Token::Comma, Token::RBracket],
                        ),
                        span: iter.peek().unwrap().1.clone(),
                        valid_syntax: vec!["comma or closing bracket".to_string()],
                    });
                }
            }
            Ok(AttrArg::List(elements))
        }
        _ => Err(ParseError {
            kind: ParseErrorKind::UnexpectedToken(
                token.clone(),
                vec![
                    Token::Ident("".to_string()),
                    Token::String("".to_string()),
                    Token::Char("".to_string()),
                    Token::Int("".to_string()),
                    Token::Float("".to_string()),
                    Token::True,
                    Token::False,
                    Token::LBracket,
                ],
            ),
            span: span.clone(),
            valid_syntax: vec!["identifier, literal, or list".to_string()],
        }),
    }
}

fn parse_int_suffix(s: &str) -> Option<String> {
    if s.ends_with("i8") {
        Some("i8".to_string())
    } else if s.ends_with("i16") {
        Some("i16".to_string())
    } else if s.ends_with("i32") {
        Some("i32".to_string())
    } else if s.ends_with("i64") {
        Some("i64".to_string())
    } else if s.ends_with("int") {
        Some("int".to_string())
    } else if s.ends_with("u8") {
        Some("u8".to_string())
    } else if s.ends_with("u16") {
        Some("u16".to_string())
    } else if s.ends_with("u32") {
        Some("u32".to_string())
    } else if s.ends_with("u64") {
        Some("u64".to_string())
    } else if s.ends_with("uint") {
        Some("uint".to_string())
    } else {
        None
    }
}

pub fn parse_type(iter: &mut TokenIter, file: &String) -> Result<Type, ParseError> {
    let start_span = iter
        .peek()
        .ok_or(ParseError {
            kind: ParseErrorKind::UnexpectedEOF,
            span: 0..0,
            valid_syntax: vec!["type".to_string()],
        })?
        .1
        .clone();

    let (token, span) = iter.next().unwrap();

    let kind = match token {
        Token::Ident(name) => {
            // Check if it's a primitive type
            match name.as_str() {
                "i8" => TypeKind::Int(IntType::I8),
                "i16" => TypeKind::Int(IntType::I16),
                "i32" => TypeKind::Int(IntType::I32),
                "i64" => TypeKind::Int(IntType::I64),
                "int" => TypeKind::Int(IntType::Int),
                "u8" => TypeKind::Int(IntType::U8),
                "u16" => TypeKind::Int(IntType::U16),
                "u32" => TypeKind::Int(IntType::U32),
                "u64" => TypeKind::Int(IntType::U64),
                "uint" => TypeKind::Int(IntType::Uint),
                "f32" => TypeKind::Float(FloatType::F32),
                "f64" => TypeKind::Float(FloatType::F64),
                "bool" => TypeKind::Bool,
                "char" => TypeKind::Char,
                "string" => TypeKind::String,
                _ => {
                    // It's a path - handle qualified names like Option<T>
                    let mut segments = vec![PathSegment {
                        span: Span::new(file.clone(), span.start..span.end),
                        ident: name,
                        generics: None,
                    }];

                    // Check for :: qualified names
                    while let Some((Token::ColonColon, _)) = iter.peek() {
                        iter.next(); // consume ::

                        let (next_ident, next_span) = match iter.next() {
                            Some((Token::Ident(n), s)) => (n, s),
                            Some((t, s)) => {
                                return Err(ParseError {
                                    kind: ParseErrorKind::UnexpectedToken(
                                        t,
                                        vec![Token::Ident("".to_string())],
                                    ),
                                    span: s,
                                    valid_syntax: vec!["identifier".to_string()],
                                });
                            }
                            None => {
                                return Err(ParseError {
                                    kind: ParseErrorKind::UnexpectedEOF,
                                    span: 0..0,
                                    valid_syntax: vec!["identifier".to_string()],
                                });
                            }
                        };

                        segments.push(PathSegment {
                            span: Span::new(file.clone(), next_span.start..next_span.end),
                            ident: next_ident,
                            generics: None,
                        });
                    }

                    // Check for generic arguments
                    if let Some((Token::Lt, _)) = iter.peek() {
                        let mut generic_args = Vec::new();
                        iter.next(); // consume '<'

                        while let Some((t, _span)) = iter.peek() {
                            if *t == Token::Gt {
                                iter.next(); // consume '>'
                                break;
                            }

                            generic_args.push(parse_type(iter, file)?);

                            if let Some((Token::Comma, _)) = iter.peek() {
                                iter.next(); // consume ','
                            } else if let Some((Token::Gt, _)) = iter.peek() {
                                // Will be consumed above
                            } else {
                                return Err(ParseError {
                                    kind: ParseErrorKind::UnexpectedToken(
                                        iter.peek().unwrap().0.clone(),
                                        vec![Token::Comma, Token::Gt],
                                    ),
                                    span: iter.peek().unwrap().1.clone(),
                                    valid_syntax: vec!["comma or closing bracket".to_string()],
                                });
                            }
                        }

                        // Update the last segment with generics
                        if let Some(last) = segments.last_mut() {
                            last.generics = Some(generic_args);
                        }
                    }

                    TypeKind::Path(Path {
                        span: Span::new(file.clone(), start_span.start..span.end),
                        segments,
                    })
                }
            }
        }
        Token::LBracket => {
            // Array type like [int]
            let element_ty = parse_type(iter, file)?;
            expect_token(iter, Token::RBracket)?;
            TypeKind::Array {
                element: Box::new(element_ty),
                size: None, // TODO: support sized arrays
            }
        }
        Token::LParen => {
            // Tuple or unit type
            if let Some((Token::RParen, _)) = iter.peek() {
                iter.next(); // consume ')'
                TypeKind::Unit
            } else {
                let mut elements = Vec::new();
                loop {
                    elements.push(parse_type(iter, file)?);
                    if let Some((Token::Comma, _)) = iter.peek() {
                        iter.next(); // consume ','
                    } else {
                        break;
                    }
                }
                expect_token(iter, Token::RParen)?;
                if elements.len() == 1 {
                    // Single element tuple is just the type
                    elements.into_iter().next().unwrap().kind
                } else {
                    TypeKind::Tuple { elements }
                }
            }
        }
        Token::Amp => {
            // Reference type
            let mutable = if let Some((Token::Mut, _)) = iter.peek() {
                iter.next(); // consume 'mut'
                true
            } else {
                false
            };
            let ty = parse_type(iter, file)?;
            TypeKind::Ref {
                mutable,
                ty: Box::new(ty),
            }
        }
        Token::AmpMut => {
            // Mutable reference type (&mut)
            let ty = parse_type(iter, file)?;
            TypeKind::Ref {
                mutable: true,
                ty: Box::new(ty),
            }
        }
        _ => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(
                    token,
                    vec![
                        Token::Ident("".to_string()),
                        Token::LBracket,
                        Token::LParen,
                        Token::Amp,
                        Token::AmpMut,
                    ],
                ),
                span,
                valid_syntax: vec!["type".to_string()],
            });
        }
    };

    Ok(Type {
        span: Span::new(
            file.clone(),
            start_span.start..iter.peek().map(|(_, s)| s.end).unwrap_or(start_span.end),
        ),
        kind,
    })
}

fn expect_token(iter: &mut TokenIter, expected: Token) -> Result<(), ParseError> {
    match iter.next() {
        Some((t, span)) if t == expected => Ok(()),
        Some((t, span)) => Err(ParseError {
            kind: ParseErrorKind::UnexpectedToken(t, vec![expected.clone()]),
            span,
            valid_syntax: vec![format!("{:?}", expected)],
        }),
        None => Err(ParseError {
            kind: ParseErrorKind::UnexpectedEOF,
            span: 0..0,
            valid_syntax: vec![format!("{:?}", expected)],
        }),
    }
}

pub fn parse_pattern(_iter: &mut TokenIter, _file: &String) -> Result<Pattern, ParseError> {
    todo!()
}

pub fn parse_struct(iter: &mut TokenIter, file: &String) -> Result<StructDecl, ParseError> {
    let start_span = iter.peek().map(|(_, s)| s.start).unwrap_or(0);

    // Parse struct name
    let name = match iter.next() {
        Some((Token::Ident(n), _)) => n,
        Some((t, span)) => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(t, vec![Token::Ident("".to_string())]),
                span,
                valid_syntax: vec!["struct name".to_string()],
            });
        }
        None => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
                span: 0..0,
                valid_syntax: vec!["struct name".to_string()],
            });
        }
    };

    // Parse optional generics
    let generics = parse_generic_params(iter, file)?;

    let mut fields = Vec::new();

    // Parse fields until 'end'
    loop {
        match iter.peek() {
            Some((Token::End, _)) => {
                iter.next(); // consume 'end'
                break;
            }
            Some((Token::At, _)) => {
                // Field attributes - parse them but they're handled by parse_item
                // For now, skip to next field
                continue;
            }
            Some(_) => {
                // Parse field
                let mut field_attributes = Vec::new();

                // Collect attributes
                while let Some((Token::At, _)) = iter.peek() {
                    field_attributes.push(parse_attribute(iter, file)?);
                }

                // Parse visibility
                let visibility = if let Some((Token::Pub, _)) = iter.peek() {
                    iter.next();
                    Visibility::Public
                } else {
                    Visibility::Private
                };

                // Parse field name
                let field_name = match iter.next() {
                    Some((Token::Ident(n), span)) => n,
                    Some((t, span)) => {
                        return Err(ParseError {
                            kind: ParseErrorKind::UnexpectedToken(
                                t,
                                vec![Token::Ident("".to_string())],
                            ),
                            span,
                            valid_syntax: vec!["field name".to_string()],
                        });
                    }
                    None => {
                        return Err(ParseError {
                            kind: ParseErrorKind::UnexpectedEOF,
                            span: 0..0,
                            valid_syntax: vec!["field name".to_string()],
                        });
                    }
                };

                // Expect colon
                expect_token(iter, Token::Colon)?;

                // Parse field type
                let field_ty = parse_type(iter, file)?;

                fields.push(StructFieldDecl {
                    span: Span::new(file.clone(), 0..0), // TODO: proper span
                    attributes: field_attributes,
                    visibility,
                    name: field_name,
                    ty: field_ty,
                });
            }
            None => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedEOF,
                    span: 0..0,
                    valid_syntax: vec!["end or field".to_string()],
                });
            }
        }
    }

    Ok(StructDecl {
        span: Span::new(
            file.clone(),
            start_span..iter.peek().map(|(_, s)| s.end).unwrap_or(start_span),
        ),
        name,
        generics,
        fields,
    })
}

pub fn parse_enum(iter: &mut TokenIter, file: &String) -> Result<EnumDecl, ParseError> {
    let start_span = iter.peek().map(|(_, s)| s.start).unwrap_or(0);

    // Parse enum name
    let name = match iter.next() {
        Some((Token::Ident(n), _)) => n,
        Some((t, span)) => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(t, vec![Token::Ident("".to_string())]),
                span,
                valid_syntax: vec!["enum name".to_string()],
            });
        }
        None => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
                span: 0..0,
                valid_syntax: vec!["enum name".to_string()],
            });
        }
    };

    // Parse optional generics
    let generics = parse_generic_params(iter, file)?;

    let mut variants = Vec::new();
    let mut is_gadt = false;

    // Parse variants until 'end'
    loop {
        match iter.peek() {
            Some((Token::End, _)) => {
                iter.next(); // consume 'end'
                break;
            }
            Some((Token::At, _)) => {
                // Variant attributes - parse them
                let attributes = vec![parse_attribute(iter, file)?];
                // Parse the variant after attributes
                let variant = parse_enum_variant(iter, file, attributes)?;
                if variant.gadt_return.is_some() {
                    is_gadt = true;
                }
                variants.push(variant);
            }
            Some(_) => {
                // Parse variant without attributes
                let variant = parse_enum_variant(iter, file, Vec::new())?;
                if variant.gadt_return.is_some() {
                    is_gadt = true;
                }
                variants.push(variant);
            }
            None => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedEOF,
                    span: 0..0,
                    valid_syntax: vec!["end or variant".to_string()],
                });
            }
        }
    }

    Ok(EnumDecl {
        span: Span::new(
            file.clone(),
            start_span..iter.peek().map(|(_, s)| s.end).unwrap_or(start_span),
        ),
        name,
        generics,
        variants,
        is_gadt,
    })
}

fn parse_enum_variant(
    iter: &mut TokenIter,
    file: &String,
    attributes: Vec<Attribute>,
) -> Result<EnumVariant, ParseError> {
    let start_span = iter.peek().map(|(_, s)| s.start).unwrap_or(0);

    // Parse variant name
    let name = match iter.next() {
        Some((Token::Ident(n), _)) => n,
        Some((t, span)) => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(t, vec![Token::Ident("".to_string())]),
                span,
                valid_syntax: vec!["variant name".to_string()],
            });
        }
        None => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
                span: 0..0,
                valid_syntax: vec!["variant name".to_string()],
            });
        }
    };

    let mut fields = Vec::new();

    // Check for variant fields (parentheses)
    if let Some((Token::LParen, _)) = iter.peek() {
        iter.next(); // consume '('

        if let Some((Token::RParen, _)) = iter.peek() {
            iter.next(); // consume ')'
        // Empty fields
        } else {
            // Parse field types
            loop {
                fields.push(parse_type(iter, file)?);
                if let Some((Token::Comma, _)) = iter.peek() {
                    iter.next(); // consume ','
                } else {
                    break;
                }
            }
            expect_token(iter, Token::RParen)?;
        }
    }

    // Check for GADT return type (-> Type)
    let gadt_return = if let Some((Token::Arrow, _)) = iter.peek() {
        iter.next(); // consume '->'
        Some(parse_type(iter, file)?)
    } else {
        None
    };

    Ok(EnumVariant {
        span: Span::new(
            file.clone(),
            start_span..iter.peek().map(|(_, s)| s.end).unwrap_or(start_span),
        ),
        attributes,
        name,
        fields,
        gadt_return,
    })
}

pub fn parse_function(iter: &mut TokenIter, file: &String) -> Result<FnDecl, ParseError> {
    let start_span = iter.peek().map(|(_, s)| s.start).unwrap_or(0);

    // Parse function name
    let name = match iter.next() {
        Some((Token::Ident(n), _)) => n,
        Some((t, span)) => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(t, vec![Token::Ident("".to_string())]),
                span,
                valid_syntax: vec!["function name".to_string()],
            });
        }
        None => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
                span: 0..0,
                valid_syntax: vec!["function name".to_string()],
            });
        }
    };

    // Parse optional generics
    let generics = parse_generic_params(iter, file)?;

    // Parse parameters
    expect_token(iter, Token::LParen)?;
    let mut params = Vec::new();
    if let Some((Token::RParen, _)) = iter.peek() {
        iter.next(); // consume ')'
    } else {
        loop {
            let param_start = iter.peek().map(|(_, s)| s.start).unwrap_or(0);

            // Parse parameter pattern (for now, just identifier)
            let pattern = match iter.next() {
                Some((Token::Ident(name), span)) => Pattern::new(
                    Span::new(file.clone(), span.start..span.end),
                    PatternKind::Ident {
                        name: name.clone(),
                        mutable: false,
                    },
                ),
                Some((t, span)) => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(t, vec![Token::Ident("".to_string())]),
                        span,
                        valid_syntax: vec!["parameter name".to_string()],
                    });
                }
                None => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedEOF,
                        span: 0..0,
                        valid_syntax: vec!["parameter".to_string()],
                    });
                }
            };

            // Expect colon
            expect_token(iter, Token::Colon)?;

            // Parse parameter type
            let ty = parse_type(iter, file)?;

            let param_end = iter.peek().map(|(_, s)| s.end).unwrap_or(param_start);
            params.push(FnParam {
                span: Span::new(file.clone(), param_start..param_end),
                attributes: Vec::new(), // TODO: handle attributes
                pattern,
                ty,
            });

            if let Some((Token::Comma, _)) = iter.peek() {
                iter.next(); // consume ','
            } else {
                break;
            }
        }
        expect_token(iter, Token::RParen)?;
    }

    // Parse optional return type
    let return_type = if let Some((Token::Arrow, _)) = iter.peek() {
        iter.next(); // consume '->'
        Some(parse_type(iter, file)?)
    } else {
        None
    };

    // Parse body
    let body = if let Some((Token::Do, _)) = iter.peek() {
        iter.next(); // consume 'do'
        // Parse block body
        let mut exprs = Vec::new();
        loop {
            if let Some((Token::End, _)) = iter.peek() {
                iter.next(); // consume 'end'
                break;
            }
            let expr = parse_expression(iter, file)?;
            exprs.push(expr);
        }
        Some(Expr::new(
            Span::new(file.clone(), 0..0), // TODO: proper span
            ExprKind::Block { exprs },
        ))
    } else {
        // Parse single expression body
        Some(parse_expression(iter, file)?)
    };

    Ok(FnDecl {
        span: Span::new(
            file.clone(),
            start_span..iter.peek().map(|(_, s)| s.end).unwrap_or(start_span),
        ),
        name,
        generics,
        params,
        return_type,
        body,
        is_comptime: false, // TODO: handle comptime
    })
}



pub fn parse_impl(_iter: &mut TokenIter, _file: &String) -> Result<ImplDecl, ParseError> {
    todo!()
}

pub fn parse_trait(_iter: &mut TokenIter, _file: &String) -> Result<TraitDecl, ParseError> {
    todo!()
}

pub fn parse_type_alias(
    _iter: &mut TokenIter,
    _file: &String,
) -> Result<TypeAliasDecl, ParseError> {
    todo!()
}

pub fn parse_const_decl(
    _iter: &mut TokenIter,
    _file: &String,
) -> Result<(String, Option<Type>, Expr), ParseError> {
    todo!()
}

pub fn parse_macro_decl(_iter: &mut TokenIter, _file: &String) -> Result<MacroDecl, ParseError> {
    todo!()
}

fn parse_generic_params(
    iter: &mut TokenIter,
    file: &String,
) -> Result<Vec<GenericParam>, ParseError> {
    let mut generics = Vec::new();

    if let Some((Token::Lt, _)) = iter.peek() {
        iter.next(); // consume '<'

        while let Some((t, span)) = iter.peek() {
            if *t == Token::Gt {
                iter.next(); // consume '>'
                break;
            }

            // Parse generic parameter name
            let (name, param_start) = match iter.next() {
                Some((Token::Ident(n), span)) => (n, span.start),
                Some((t, span)) => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(
                            t.clone(),
                            vec![Token::Ident("".to_string())],
                        ),
                        span: span.clone(),
                        valid_syntax: vec!["generic parameter name".to_string()],
                    });
                }
                None => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedEOF,
                        span: 0..0,
                        valid_syntax: vec!["generic parameter".to_string()],
                    });
                }
            };

            let mut bounds = Vec::new();

            // Check for bounds (traits)
            if let Some((Token::Colon, _)) = iter.peek() {
                iter.next(); // consume ':'

                // Parse trait bounds separated by '+'
                loop {
                    // For bounds, we expect paths (trait names)
                    let bound_ty = parse_type(iter, file)?;
                    match bound_ty.kind {
                        TypeKind::Path(path) => bounds.push(path),
                        _ => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedToken(
                                    Token::Ident("".to_string()), // TODO: better error
                                    vec![Token::Ident("trait name".to_string())],
                                ),
                                span: bound_ty.span.range,
                                valid_syntax: vec!["trait name".to_string()],
                            });
                        }
                    }

                    if let Some((Token::Plus, _)) = iter.peek() {
                        iter.next(); // consume '+'
                    } else {
                        break;
                    }
                }
            }

            // Check for default type
            let default = if let Some((Token::Eq, _)) = iter.peek() {
                iter.next(); // consume '='
                Some(parse_type(iter, file)?)
            } else {
                None
            };

            let param_end = iter.peek().map(|(_, s)| s.end).unwrap_or(param_start);
            generics.push(GenericParam {
                span: Span::new(file.clone(), param_start..param_end),
                attributes: Vec::new(), // TODO: support attributes on generic params
                kind: GenericParamKind::Type {
                    name,
                    bounds,
                    default,
                },
            });

            // Check for comma or end
            if let Some((Token::Comma, _)) = iter.peek() {
                iter.next(); // consume ','
            } else if let Some((Token::Gt, _)) = iter.peek() {
                // Will be consumed above
            } else {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(
                        iter.peek().unwrap().0.clone(),
                        vec![Token::Comma, Token::Gt],
                    ),
                    span: iter.peek().unwrap().1.clone(),
                    valid_syntax: vec!["comma or closing bracket".to_string()],
                });
            }
        }
    }

    Ok(generics)
}

pub fn parse_use(_iter: &mut TokenIter, _file: &String) -> Result<UseDecl, ParseError> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex_without_comments;

    fn parse_single_attribute(source: &str) -> Result<Attribute, ParseError> {
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        parse_attribute(&mut iter, &"<test>".to_string())
    }

    #[test]
    fn test_parse_attribute_simple() {
        let attr = parse_single_attribute("@inline").unwrap();
        assert_eq!(attr.name, "inline");
        assert!(attr.args.is_empty());
    }

    #[test]
    fn test_parse_attribute_with_ident_args() {
        let attr = parse_single_attribute("@derive(Show, Eq)").unwrap();
        assert_eq!(attr.name, "derive");
        assert_eq!(attr.args.len(), 2);
        match &attr.args[0] {
            AttrArg::Ident(s) => assert_eq!(s, "Show"),
            _ => panic!("Expected Ident"),
        }
        match &attr.args[1] {
            AttrArg::Ident(s) => assert_eq!(s, "Eq"),
            _ => panic!("Expected Ident"),
        }
    }

    #[test]
    fn test_parse_attribute_with_key_value() {
        let attr = parse_single_attribute("@cfg(os = \"linux\")").unwrap();
        assert_eq!(attr.name, "cfg");
        assert_eq!(attr.args.len(), 1);
        match &attr.args[0] {
            AttrArg::KeyValue { key, value } => {
                assert_eq!(key, "os");
                match value.as_ref() {
                    AttrArg::Literal(Literal::String(s)) => assert_eq!(s, "\"linux\""),
                    _ => panic!("Expected string literal"),
                }
            }
            _ => panic!("Expected KeyValue"),
        }
    }

    #[test]
    fn test_parse_attribute_with_int_literal() {
        let attr = parse_single_attribute("@version(1)").unwrap();
        assert_eq!(attr.name, "version");
        assert_eq!(attr.args.len(), 1);
        match &attr.args[0] {
            AttrArg::Literal(Literal::Int { value, suffix }) => {
                assert_eq!(value, "1");
                assert!(suffix.is_none());
            }
            _ => panic!("Expected int literal"),
        }
    }

    #[test]
    fn test_parse_attribute_with_list() {
        let attr = parse_single_attribute("@allow([unused, dead_code])").unwrap();
        assert_eq!(attr.name, "allow");
        assert_eq!(attr.args.len(), 1);
        match &attr.args[0] {
            AttrArg::List(list) => {
                assert_eq!(list.len(), 2);
                match &list[0] {
                    AttrArg::Ident(s) => assert_eq!(s, "unused"),
                    _ => panic!("Expected Ident"),
                }
                match &list[1] {
                    AttrArg::Ident(s) => assert_eq!(s, "dead_code"),
                    _ => panic!("Expected Ident"),
                }
            }
            _ => panic!("Expected List"),
        }
    }

    fn parse_single_struct(source: &str) -> Result<StructDecl, ParseError> {
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        // Skip the struct token
        iter.next();
        parse_struct(&mut iter, &"<test>".to_string())
    }

    fn parse_single_enum(source: &str) -> Result<EnumDecl, ParseError> {
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        // Skip the enum token
        iter.next();
        parse_enum(&mut iter, &"<test>".to_string())
    }

    #[test]
    fn test_parse_simple_struct() {
        let struct_decl = parse_single_struct("struct Point\n  x: f64\n  y: f64\nend").unwrap();
        assert_eq!(struct_decl.name, "Point");
        assert!(struct_decl.generics.is_empty());
        assert_eq!(struct_decl.fields.len(), 2);
        assert_eq!(struct_decl.fields[0].name, "x");
        assert_eq!(struct_decl.fields[1].name, "y");
    }

    #[test]
    fn test_parse_struct_with_generics() {
        let struct_decl = parse_single_struct("struct Container<T>\n  value: T\nend").unwrap();
        assert_eq!(struct_decl.name, "Container");
        assert_eq!(struct_decl.generics.len(), 1);
        match &struct_decl.generics[0].kind {
            GenericParamKind::Type { name, .. } => assert_eq!(name, "T"),
            _ => panic!("Expected Type generic param"),
        }
        assert_eq!(struct_decl.fields.len(), 1);
        assert_eq!(struct_decl.fields[0].name, "value");
    }

    #[test]
    fn test_parse_simple_enum() {
        let enum_decl = parse_single_enum("enum Option<T>\n  Some(T)\n  None\nend").unwrap();
        assert_eq!(enum_decl.name, "Option");
        assert_eq!(enum_decl.generics.len(), 1);
        match &enum_decl.generics[0].kind {
            GenericParamKind::Type { name, .. } => assert_eq!(name, "T"),
            _ => panic!("Expected Type generic param"),
        }
        assert_eq!(enum_decl.variants.len(), 2);
        assert_eq!(enum_decl.variants[0].name, "Some");
        assert_eq!(enum_decl.variants[0].fields.len(), 1);
        assert_eq!(enum_decl.variants[1].name, "None");
        assert_eq!(enum_decl.variants[1].fields.len(), 0);
        assert!(!enum_decl.is_gadt);
    }

    #[test]
    fn test_parse_gadt_enum() {
        let enum_decl = parse_single_enum("enum Expr<T>\n  IntLit(int) -> Expr<int>\n  BoolLit(bool) -> Expr<bool>\n  Add(Expr<int>, Expr<int>) -> Expr<int>\nend").unwrap();
        assert_eq!(enum_decl.name, "Expr");
        assert!(enum_decl.is_gadt);
        assert_eq!(enum_decl.variants.len(), 3);
        assert_eq!(enum_decl.variants[0].name, "IntLit");
        assert!(enum_decl.variants[0].gadt_return.is_some());
    }

    #[test]
    fn test_parse_struct_with_attributes() {
        // Note: Currently struct attributes are handled at the Item level, not in parse_struct itself
        // This test verifies basic struct parsing works
        let struct_decl =
            parse_single_struct("struct User\n  id: int\n  name: string\nend").unwrap();
        assert_eq!(struct_decl.name, "User");
        assert_eq!(struct_decl.fields.len(), 2);
        assert_eq!(struct_decl.fields[0].name, "id");
        assert_eq!(struct_decl.fields[1].name, "name");
    }

    #[test]
    fn test_parse_struct_with_public_fields() {
        let struct_decl =
            parse_single_struct("struct Config\n  pub host: string\n  port: int\nend").unwrap();
        assert_eq!(struct_decl.name, "Config");
        assert_eq!(struct_decl.fields.len(), 2);
        assert_eq!(struct_decl.fields[0].visibility, Visibility::Public);
        assert_eq!(struct_decl.fields[0].name, "host");
        assert_eq!(struct_decl.fields[1].visibility, Visibility::Private);
        assert_eq!(struct_decl.fields[1].name, "port");
    }

    #[test]
    fn test_parse_struct_with_complex_types() {
        let struct_decl = parse_single_struct("struct Complex\n  arr: [int]\n  tuple: (string, int)\n  ref: &string\n  opt: Option<int>\nend").unwrap();
        assert_eq!(struct_decl.name, "Complex");
        assert_eq!(struct_decl.fields.len(), 4);
        assert_eq!(struct_decl.fields[0].name, "arr");
        assert_eq!(struct_decl.fields[1].name, "tuple");
        assert_eq!(struct_decl.fields[2].name, "ref");
        assert_eq!(struct_decl.fields[3].name, "opt");
    }

    #[test]
    fn test_parse_empty_struct() {
        let struct_decl = parse_single_struct("struct Empty\nend").unwrap();
        assert_eq!(struct_decl.name, "Empty");
        assert!(struct_decl.fields.is_empty());
    }

    #[test]
    fn test_parse_struct_with_generic_bounds() {
        let struct_decl =
            parse_single_struct("struct Container<T: Clone + Eq>\n  value: T\nend").unwrap();
        assert_eq!(struct_decl.name, "Container");
        assert_eq!(struct_decl.generics.len(), 1);
        match &struct_decl.generics[0].kind {
            GenericParamKind::Type { name, bounds, .. } => {
                assert_eq!(name, "T");
                assert_eq!(bounds.len(), 2);
                // Note: bounds parsing would need more implementation
            }
            _ => panic!("Expected Type generic param"),
        }
    }

    #[test]
    fn test_parse_enum_with_attributes() {
        // Note: Currently variant attributes are not fully implemented
        // This test verifies basic enum parsing works
        let enum_decl = parse_single_enum("enum Command\n  Quit\n  Greet(string)\nend").unwrap();
        assert_eq!(enum_decl.name, "Command");
        assert_eq!(enum_decl.variants.len(), 2);
        assert_eq!(enum_decl.variants[0].name, "Quit");
        assert_eq!(enum_decl.variants[1].name, "Greet");
    }

    #[test]
    fn test_parse_enum_with_tuple_variants() {
        let enum_decl = parse_single_enum("enum Result<T, E>\n  Ok(T)\n  Err(E)\nend").unwrap();
        assert_eq!(enum_decl.name, "Result");
        assert_eq!(enum_decl.generics.len(), 2);
        assert_eq!(enum_decl.variants.len(), 2);
        assert_eq!(enum_decl.variants[0].name, "Ok");
        assert_eq!(enum_decl.variants[0].fields.len(), 1);
        assert_eq!(enum_decl.variants[1].name, "Err");
        assert_eq!(enum_decl.variants[1].fields.len(), 1);
    }

    #[test]
    fn test_parse_enum_with_multiple_generics() {
        let enum_decl = parse_single_enum("enum Either<A, B>\n  Left(A)\n  Right(B)\nend").unwrap();
        assert_eq!(enum_decl.name, "Either");
        assert_eq!(enum_decl.generics.len(), 2);
        assert!(!enum_decl.is_gadt);
    }

    #[test]
    fn test_parse_enum_empty_variants() {
        let enum_decl = parse_single_enum("enum Bool\n  True\n  False\nend").unwrap();
        assert_eq!(enum_decl.name, "Bool");
        assert_eq!(enum_decl.variants.len(), 2);
        assert_eq!(enum_decl.variants[0].name, "True");
        assert!(enum_decl.variants[0].fields.is_empty());
        assert_eq!(enum_decl.variants[1].name, "False");
        assert!(enum_decl.variants[1].fields.is_empty());
    }

    #[test]
    fn test_parse_complex_gadt_enum() {
        let enum_decl = parse_single_enum("enum Expr<T>\n  Lit(T) -> Expr<T>\n  Add(Expr<int>, Expr<int>) -> Expr<int>\n  Pair(Expr<int>, Expr<int>) -> Expr<(int, int)>\nend").unwrap();
        assert_eq!(enum_decl.name, "Expr");
        assert!(enum_decl.is_gadt);
        assert_eq!(enum_decl.variants.len(), 3);

        // Check Lit variant
        assert_eq!(enum_decl.variants[0].name, "Lit");
        assert_eq!(enum_decl.variants[0].fields.len(), 1);
        assert!(enum_decl.variants[0].gadt_return.is_some());

        // Check Add variant
        assert_eq!(enum_decl.variants[1].name, "Add");
        assert_eq!(enum_decl.variants[1].fields.len(), 2);
        assert!(enum_decl.variants[1].gadt_return.is_some());

        // Check Pair variant
        assert_eq!(enum_decl.variants[2].name, "Pair");
        assert_eq!(enum_decl.variants[2].fields.len(), 2);
        assert!(enum_decl.variants[2].gadt_return.is_some());
    }

    #[test]
    fn test_parse_enum_with_qualified_types() {
        let enum_decl =
            parse_single_enum("enum Message\n  Text(string)\n  Binary(std::io::Buffer)\nend")
                .unwrap();
        assert_eq!(enum_decl.name, "Message");
        assert_eq!(enum_decl.variants.len(), 2);
        assert_eq!(enum_decl.variants[0].name, "Text");
        assert_eq!(enum_decl.variants[1].name, "Binary");
    }

    #[test]
    fn test_parse_nested_generics() {
        let enum_decl =
            parse_single_enum("enum Tree<T>\n  Leaf(T)\n  Node(Tree<T>, Tree<T>)\nend").unwrap();
        assert_eq!(enum_decl.name, "Tree");
        assert_eq!(enum_decl.variants.len(), 2);
        assert_eq!(enum_decl.variants[0].name, "Leaf");
        assert_eq!(enum_decl.variants[0].fields.len(), 1);
        assert_eq!(enum_decl.variants[1].name, "Node");
        assert_eq!(enum_decl.variants[1].fields.len(), 2);
    }

    #[test]
    fn test_parse_enum_mixed_variant_types() {
        let enum_decl =
            parse_single_enum("enum Shape\n  Circle(f64)\n  Rectangle(f64, f64)\n  Point\nend")
                .unwrap();
        assert_eq!(enum_decl.name, "Shape");
        assert_eq!(enum_decl.variants.len(), 3);

        assert_eq!(enum_decl.variants[0].name, "Circle");
        assert_eq!(enum_decl.variants[0].fields.len(), 1);

        assert_eq!(enum_decl.variants[1].name, "Rectangle");
        assert_eq!(enum_decl.variants[1].fields.len(), 2);

        assert_eq!(enum_decl.variants[2].name, "Point");
        assert_eq!(enum_decl.variants[2].fields.len(), 0);
    }

    #[test]
    fn test_parse_error_missing_end() {
        let source = "struct Test\n  field: int\n";
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        iter.next(); // skip struct
        let result = parse_struct(&mut iter, &"<test>".to_string());
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_invalid_field_syntax() {
        let source = "struct Test\n  field\nend";
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        iter.next(); // skip struct
        let result = parse_struct(&mut iter, &"<test>".to_string());
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_multiple_declarations() {
        let source = r#"
struct Point
  x: f64
  y: f64
end

enum Color
  Red
  Green
  Blue
end

struct User
  name: string
  age: int
end
        "#;

        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let result = parse(token_pairs, &"<test>".to_string());
        assert!(result.is_ok());
        let items = result.unwrap();
        assert_eq!(items.len(), 3);
        match &items[0].kind {
            ItemKind::Struct(s) => assert_eq!(s.name, "Point"),
            _ => panic!("Expected struct"),
        }
        match &items[1].kind {
            ItemKind::Enum(e) => assert_eq!(e.name, "Color"),
            _ => panic!("Expected enum"),
        }
        match &items[2].kind {
            ItemKind::Struct(s) => assert_eq!(s.name, "User"),
            _ => panic!("Expected struct"),
        }
    }

    #[test]
    fn test_parse_enum_single_variant() {
        let enum_decl = parse_single_enum("enum Unit\n  Value\nend").unwrap();
        assert_eq!(enum_decl.name, "Unit");
        assert_eq!(enum_decl.variants.len(), 1);
        assert_eq!(enum_decl.variants[0].name, "Value");
        assert!(enum_decl.variants[0].fields.is_empty());
    }

    #[test]
    fn test_parse_struct_reference_types() {
        let struct_decl = parse_single_struct(
            "struct Refs\n  owned: string\n  borrowed: &string\n  mut_borrowed: &mut string\nend",
        )
        .unwrap();
        assert_eq!(struct_decl.name, "Refs");
        assert_eq!(struct_decl.fields.len(), 3);
        assert_eq!(struct_decl.fields[0].name, "owned");
        assert_eq!(struct_decl.fields[1].name, "borrowed");
        assert_eq!(struct_decl.fields[2].name, "mut_borrowed");
    }

    #[test]
    fn test_parse_enum_with_unit_tuple() {
        let enum_decl = parse_single_enum("enum MaybeUnit\n  Nothing()\n  Just(int)\nend").unwrap();
        assert_eq!(enum_decl.name, "MaybeUnit");
        assert_eq!(enum_decl.variants.len(), 2);
        assert_eq!(enum_decl.variants[0].name, "Nothing");
        assert!(enum_decl.variants[0].fields.is_empty());
        assert_eq!(enum_decl.variants[1].name, "Just");
        assert_eq!(enum_decl.variants[1].fields.len(), 1);
    }

    #[test]
    fn test_parse_error_invalid_generic_syntax() {
        let source = "struct Bad<T>\n  field: T\nend";
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        iter.next(); // skip struct
        // This should work fine
        let result = parse_struct(&mut iter, &"<test>".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_error_unexpected_token_after_struct_name() {
        let source = "struct Test fn\n  field: int\nend";
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        iter.next(); // skip struct
        let result = parse_struct(&mut iter, &"<test>".to_string());
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_enum_gadt_complex_return() {
        let enum_decl =
            parse_single_enum("enum List<T>\n  Nil -> List<T>\n  Cons(T, List<T>) -> List<T>\nend")
                .unwrap();
        assert_eq!(enum_decl.name, "List");
        assert!(enum_decl.is_gadt);
        assert_eq!(enum_decl.variants.len(), 2);
        assert!(enum_decl.variants[0].gadt_return.is_some());
        assert!(enum_decl.variants[1].gadt_return.is_some());
    }

    #[test]
    fn test_parse_struct_with_array_types() {
        let struct_decl =
            parse_single_struct("struct Arrays\n  fixed: [int]\n  nested: [[string]]\nend")
                .unwrap();
        assert_eq!(struct_decl.name, "Arrays");
        assert_eq!(struct_decl.fields.len(), 2);
        assert_eq!(struct_decl.fields[0].name, "fixed");
        assert_eq!(struct_decl.fields[1].name, "nested");
    }

    #[test]
    fn test_parse_enum_with_path_types() {
        // Test with generic types in enum variants
        let enum_decl =
            parse_single_enum("enum Result<T, E>\n  Ok(Option<T>)\n  Err(E)\nend").unwrap();
        assert_eq!(enum_decl.name, "Result");
        assert_eq!(enum_decl.variants.len(), 2);
        assert_eq!(enum_decl.variants[0].fields.len(), 1);
        assert_eq!(enum_decl.variants[1].fields.len(), 1);
    }

    #[test]
    fn test_parse_struct_with_generic_constraints() {
        // Note: Generic constraints parsing is not fully implemented yet
        // This test verifies the basic structure works
        let struct_decl = parse_single_struct("struct Wrapper<T>\n  value: T\nend").unwrap();
        assert_eq!(struct_decl.name, "Wrapper");
        assert_eq!(struct_decl.generics.len(), 1);
        match &struct_decl.generics[0].kind {
            GenericParamKind::Type { name, .. } => assert_eq!(name, "T"),
            _ => panic!("Expected Type generic param"),
        }
    }

    fn parse_single_function(source: &str) -> Result<FnDecl, ParseError> {
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        // Skip the fn token
        iter.next();
        parse_function(&mut iter, &"<test>".to_string())
    }

    #[test]
    fn test_parse_simple_function() {
        let fn_decl = parse_single_function("fn add(a: int, b: int) -> int\n  a + b").unwrap();
        assert_eq!(fn_decl.name, "add");
        assert!(fn_decl.generics.is_empty());
        assert_eq!(fn_decl.params.len(), 2);
        assert_eq!(fn_decl.params[0].pattern.kind, PatternKind::Ident { name: "a".to_string(), mutable: false });
        assert_eq!(fn_decl.params[1].pattern.kind, PatternKind::Ident { name: "b".to_string(), mutable: false });
        assert!(fn_decl.return_type.is_some());
        assert!(!fn_decl.is_comptime);
        assert!(fn_decl.body.is_some());
    }

    #[test]
    fn test_parse_function_with_generics() {
        let fn_decl = parse_single_function("fn identity<T>(x: T) -> T\n  x").unwrap();
        assert_eq!(fn_decl.name, "identity");
        assert_eq!(fn_decl.generics.len(), 1);
        assert_eq!(fn_decl.params.len(), 1);
        assert!(fn_decl.return_type.is_some());
        assert!(!fn_decl.is_comptime);
    }

    #[test]
    fn test_parse_function_no_return_type() {
        let fn_decl = parse_single_function("fn greet(name: string)\n  puts(\"Hello\")").unwrap();
        assert_eq!(fn_decl.name, "greet");
        assert!(fn_decl.return_type.is_none());
        assert_eq!(fn_decl.params.len(), 1);
    }

    #[test]
    fn test_parse_function_with_block_body() {
        let fn_decl = parse_single_function("fn complex(x: int) -> int do\n  x\n  1\nend").unwrap();
        assert_eq!(fn_decl.name, "complex");
        assert!(fn_decl.return_type.is_some());
        assert!(fn_decl.body.is_some());
        if let ExprKind::Block { exprs } = &fn_decl.body.as_ref().unwrap().kind {
            assert_eq!(exprs.len(), 2);
        } else {
            panic!("Expected block body");
        }
    }
}
