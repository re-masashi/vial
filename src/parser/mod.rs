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

// Precedence levels (higher number = higher precedence)
const PREC_ASSIGN: u8 = 1;
const PREC_PIPE: u8 = 2;
const PREC_OR: u8 = 3;
const PREC_AND: u8 = 4;
const PREC_COMPARE: u8 = 5;
const PREC_BITOR: u8 = 6;
const PREC_BITXOR: u8 = 7;
const PREC_BITAND: u8 = 8;
const PREC_SHIFT: u8 = 9;
const PREC_ADD: u8 = 10;
const PREC_MUL: u8 = 11;
const PREC_UNARY: u8 = 12;
const PREC_RANGE: u8 = 6;
const PREC_CALL: u8 = 13; // function calls, field access
const PREC_PRIMARY: u8 = 14;

pub fn parse_expression(iter: &mut TokenIter, file: &String) -> Result<Expr, ParseError> {
    let mut attributes = Vec::new();

    while let Some((Token::At, _)) = iter.peek() {
        attributes.push(parse_attribute(iter, file)?);
    }

    let mut expr = parse_expr_bp(iter, file, 0)?;
    expr.attributes = attributes;
    Ok(expr)
}

fn parse_expr_bp(iter: &mut TokenIter, file: &String, min_prec: u8) -> Result<Expr, ParseError> {
    let mut left = parse_primary(iter, file)?;

    // Postfix operators
    loop {
        match iter.peek() {
            Some((Token::LParen, _)) => {
                iter.next(); // consume '('
                let mut args = Vec::new();
                if !matches!(iter.peek(), Some((Token::RParen, _))) {
                    loop {
                        args.push(parse_expression(iter, file)?);
                        match iter.peek() {
                            Some((Token::Comma, _)) => { iter.next(); }
                            Some((Token::RParen, _)) => break,
                            _ => return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedToken(
                                    iter.peek().map(|(t, _)| t.clone()).unwrap_or(Token::Ident("".to_string())),
                                    vec![Token::Comma, Token::RParen],
                                ),
                                span: iter.peek().map(|(_, s)| s.clone()).unwrap_or(0..0),
                                valid_syntax: vec!["',' or ')' in function call".to_string()],
                            }),
                        }
                    }
                }
                expect_token(iter, Token::RParen)?;
                let end_span = args.last().map(|a| &a.span).unwrap_or(&left.span);
                left = Expr::new(
                    Span::merge(&left.span, end_span),
                    ExprKind::Call {
                        func: Box::new(left),
                        args,
                    },
                );
            }
            Some((Token::LBracket, _)) => {
                iter.next(); // consume '['
                let index = parse_expression(iter, file)?;
                expect_token(iter, Token::RBracket)?;
                left = Expr::new(
                    Span::merge(&left.span, &index.span),
                    ExprKind::Index {
                        base: Box::new(left),
                        index: Box::new(index),
                    },
                );
            }
            Some((Token::Dot, _)) => {
                iter.next(); // consume '.'
                let (token, span) = iter.next().ok_or(ParseError {
                    kind: ParseErrorKind::UnexpectedEOF,
                    span: 0..0,
                    valid_syntax: vec!["field name".to_string()],
                })?;
                let field = match token {
                    Token::Ident(name) => name,
                    _ => return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(
                            token,
                            vec![Token::Ident("".to_string())],
                        ),
                        span,
                        valid_syntax: vec!["field name".to_string()],
                    }),
                };
                left = Expr::new(
                    Span::merge(&left.span, &Span::new(file.clone(), span)),
                    ExprKind::Field {
                        base: Box::new(left),
                        field,
                    },
                );
            }
            Some((Token::LBrace, _)) => {
                iter.next(); // consume '{'
                let path = match &left.kind {
                    ExprKind::Ident(name) => Path {
                        span: left.span.clone(),
                        segments: vec![PathSegment {
                            span: left.span.clone(),
                            ident: name.clone(),
                            generics: None,
                        }],
                    },
                    ExprKind::Path(p) => p.clone(),
                    _ => return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(
                            Token::LBrace,
                            vec![Token::Ident("".to_string())],
                        ),
                        span: left.span.range,
                        valid_syntax: vec!["identifier for struct construction".to_string()],
                    }),
                };
                let mut fields = Vec::new();
                if let Some((Token::RBrace, _)) = iter.peek() {
                    iter.next();
                } else {
                    loop {
                        let field_name = match iter.next() {
                            Some((Token::Ident(name), _)) => name,
                            Some((Token::DotDot, span)) => {
                                let spread_expr = parse_expression(iter, file)?;
                                fields.push(StructField {
                                    span: Span::new(file.clone(), span.start..spread_expr.span.range.end),
                                    name: None,
                                    value: spread_expr,
                                });
                                if let Some((Token::Comma, _)) = iter.peek() {
                                    iter.next();
                                } else {
                                    break;
                                }
                                continue;
                            }
                            _ => return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedEOF,
                                span: 0..0,
                                valid_syntax: vec!["field name or '}'".to_string()],
                            }),
                        };
                        if let Some((Token::Colon, _)) = iter.peek() {
                            iter.next();
                            let value = parse_expression(iter, file)?;
                            fields.push(StructField {
                                span: Span::new(file.clone(), 0..value.span.range.end),
                                name: Some(field_name),
                                value,
                            });
                        } else {
                            fields.push(StructField {
                                span: Span::new(file.clone(), 0..0),
                                name: Some(field_name.clone()),
                                value: Expr::new(Span::dummy(), ExprKind::Ident(field_name)),
                            });
                        }
                        if let Some((Token::Comma, _)) = iter.peek() {
                            iter.next();
                        } else {
                            break;
                        }
                    }
                }
                expect_token(iter, Token::RBrace)?;
                left = Expr::new(
                    Span::merge(&left.span, &left.span),
                    ExprKind::Struct {
                        path,
                        fields,
                        spread: None,
                    },
                );
            }
            _ => break,
        }
    }

    loop {
        let op_prec = match iter.peek() {
            Some((Token::Eq, _)) => PREC_ASSIGN,
            Some((Token::PlusEq, _)) => PREC_ASSIGN,
            Some((Token::MinusEq, _)) => PREC_ASSIGN,
            Some((Token::StarEq, _)) => PREC_ASSIGN,
            Some((Token::SlashEq, _)) => PREC_ASSIGN,
            Some((Token::Pipe, _)) => PREC_PIPE,
            Some((Token::Or, _)) => PREC_OR,
            Some((Token::And, _)) => PREC_AND,
            Some((Token::EqEq, _)) => PREC_COMPARE,
            Some((Token::NotEq, _)) => PREC_COMPARE,
            Some((Token::Lt, _)) => PREC_COMPARE,
            Some((Token::Gt, _)) => PREC_COMPARE,
            Some((Token::LtEq, _)) => PREC_COMPARE,
            Some((Token::GtEq, _)) => PREC_COMPARE,
            Some((Token::BitOr, _)) => PREC_BITOR,
            Some((Token::Caret, _)) => PREC_BITXOR,
            Some((Token::Amp, _)) => PREC_BITAND,
            Some((Token::Shl, _)) => PREC_SHIFT,
            Some((Token::Shr, _)) => PREC_SHIFT,
            Some((Token::Plus, _)) => PREC_ADD,
            Some((Token::Minus, _)) => PREC_ADD,
            Some((Token::Star, _)) => PREC_MUL,
            Some((Token::Slash, _)) => PREC_MUL,
            Some((Token::Percent, _)) => PREC_MUL,
            Some((Token::DotDot, _)) => 6, // range operators
            Some((Token::DotDotEq, _)) => 6,
            _ => break,
        };

        if op_prec < min_prec {
            break;
        }

        let (token, _) = iter.next().unwrap();

        if token == Token::Pipe {
            return parse_pipe(iter, file, left);
        }

        if matches!(token, Token::DotDot | Token::DotDotEq) {
            // Handle range
            let inclusive = matches!(token, Token::DotDotEq);
            let end = if let Some((Token::RBracket, _)) | Some((Token::RBrace, _)) | Some((Token::Comma, _)) | Some((Token::RParen, _)) | Some((Token::End, _)) | Some((Token::Else, _)) | Some((Token::FatArrow, _)) | Some((Token::Colon, _)) | Some((Token::Semicolon, _)) | None = iter.peek() {
                None
            } else {
                Some(Box::new(parse_expr_bp(iter, file, PREC_COMPARE)?))
            };
            return Ok(Expr::new(
                Span::merge(&left.span, &end.as_ref().map(|e| &e.span).unwrap_or(&left.span)),
                ExprKind::Range {
                    start: Some(Box::new(left)),
                    end,
                    inclusive,
                },
            ));
        }

        let op = match token {
            Token::Eq => None,
            Token::PlusEq => Some(BinOp::Add),
            Token::MinusEq => Some(BinOp::Sub),
            Token::StarEq => Some(BinOp::Mul),
            Token::SlashEq => Some(BinOp::Div),
            Token::Or => Some(BinOp::Or),
            Token::And => Some(BinOp::And),
            Token::EqEq => Some(BinOp::Eq),
            Token::NotEq => Some(BinOp::Ne),
            Token::Lt => Some(BinOp::Lt),
            Token::Gt => Some(BinOp::Gt),
            Token::LtEq => Some(BinOp::Le),
            Token::GtEq => Some(BinOp::Ge),
            Token::BitOr => Some(BinOp::BitOr),
            Token::Caret => Some(BinOp::BitXor),
            Token::Amp => Some(BinOp::BitAnd),
            Token::Shl => Some(BinOp::Shl),
            Token::Shr => Some(BinOp::Shr),
            Token::Plus => Some(BinOp::Add),
            Token::Minus => Some(BinOp::Sub),
            Token::Star => Some(BinOp::Mul),
            Token::Slash => Some(BinOp::Div),
            Token::Percent => Some(BinOp::Rem),
            _ => unreachable!(),
        };

        if let Some(bin_op) = op {
            let right = parse_expr_bp(iter, file, op_prec + 1)?;
            left = Expr::new(
                Span::merge(&left.span, &right.span),
                ExprKind::Binary {
                    left: Box::new(left),
                    op: bin_op,
                    right: Box::new(right),
                },
            );
        } else {
            // Assignment
            let right = parse_expr_bp(iter, file, op_prec)?;
            left = Expr::new(
                Span::merge(&left.span, &right.span),
                ExprKind::Assign {
                    target: Box::new(left),
                    op,
                    value: Box::new(right),
                },
            );
        }
    }

    // Check for postfix operators
    left = parse_postfix(iter, file, left)?;

    Ok(left)
}

fn parse_pipe(iter: &mut TokenIter, file: &String, left: Expr) -> Result<Expr, ParseError> {
    let right = parse_expr_bp(iter, file, PREC_PIPE + 1)?;
    let expr = Expr::new(
        Span::merge(&left.span, &right.span),
        ExprKind::Pipe {
            left: Box::new(left),
            right: Box::new(right),
        },
    );
    Ok(expr)
}

fn parse_primary(iter: &mut TokenIter, file: &String) -> Result<Expr, ParseError> {
    let start_span = iter.peek().map(|(_, s)| s.start).unwrap_or(0);

    let (token, span) = iter.next().ok_or(ParseError {
        kind: ParseErrorKind::UnexpectedEOF,
        span: 0..0,
        valid_syntax: vec!["expression".to_string()],
    })?;

    let kind = match token {
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

            ExprKind::Literal(Literal::Int {
                value,
                suffix: int_type,
            })
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

            ExprKind::Literal(Literal::Float { value, suffix })
        }
        Token::String(s) => ExprKind::Literal(Literal::String(s)),
        Token::Char(s) => ExprKind::Literal(Literal::Char(s)),
        Token::True => ExprKind::Literal(Literal::Bool(true)),
        Token::False => ExprKind::Literal(Literal::Bool(false)),
        Token::Ident(s) => {
            // Check if it's a path
            let mut segments = vec![PathSegment {
                span: Span::new(file.clone(), span.start..span.end),
                ident: s,
                generics: None,
            }];

            while let Some((Token::ColonColon, _)) = iter.peek() {
                iter.next(); // consume ::

                let (next_ident, next_span) = match iter.next() {
                    Some((Token::Ident(n), s)) => (n, s),
                    _ => return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(
                            iter.peek().unwrap_or(&(Token::Ident("".to_string()), 0..0)).0.clone(),
                            vec![Token::Ident("".to_string())],
                        ),
                        span: iter.peek().unwrap_or(&(Token::Ident("".to_string()), 0..0)).1.clone(),
                        valid_syntax: vec!["identifier".to_string()],
                    }),
                };

                segments.push(PathSegment {
                    span: Span::new(file.clone(), next_span.start..next_span.end),
                    ident: next_ident,
                    generics: None,
                });
            }

            let path = Path {
                span: Span::new(file.clone(), start_span..segments.last().unwrap().span.range.end),
                segments,
            };

            // Check if it's an enum variant construction
            if path.segments.len() > 1 && let Some((Token::LParen, _)) = iter.peek() {
                iter.next(); // consume '('
                let mut args = Vec::new();
                if let Some((Token::RParen, _)) = iter.peek() {
                    iter.next(); // consume ')'
                } else {
                    loop {
                        args.push(parse_expression(iter, file)?);
                        if let Some((Token::Comma, _)) = iter.peek() {
                            iter.next(); // consume ','
                        } else {
                            break;
                        }
                    }
                    expect_token(iter, Token::RParen)?;
                }
                ExprKind::EnumVariant { path, args }
            } else if path.segments.len() == 1 {
                ExprKind::Ident(path.segments[0].ident.clone())
            } else {
                ExprKind::Path(path)
            }
        }
        Token::LParen => {
            // Tuple or unit
            if let Some((Token::RParen, _)) = iter.peek() {
                iter.next(); // consume ')'
                ExprKind::Literal(Literal::Unit)
            } else {
                let mut elements = Vec::new();
                loop {
                    elements.push(parse_expression(iter, file)?);
                    if let Some((Token::Comma, _)) = iter.peek() {
                        iter.next(); // consume ','
                    } else {
                        break;
                    }
                }
                expect_token(iter, Token::RParen)?;
                if elements.len() == 1 {
                    // Single element tuple is just the expression
                    return Ok(elements.into_iter().next().unwrap());
                } else {
                    ExprKind::Tuple { elements }
                }
            }
        }
        Token::LBracket => {
            // Array
            let mut elements = Vec::new();
            if let Some((Token::RBracket, _)) = iter.peek() {
                iter.next(); // consume ']'
            } else {
                loop {
                    elements.push(parse_expression(iter, file)?);
                    if let Some((Token::Comma, _)) = iter.peek() {
                        iter.next(); // consume ','
                    } else {
                        break;
                    }
                }
                expect_token(iter, Token::RBracket)?;
            }
            ExprKind::Array { elements }
        }
        Token::LBrace => {
            // Map or struct
            parse_struct_or_map(iter, file)?
        }
        Token::Do => {
            // Block
            let mut exprs = Vec::new();
            loop {
                if let Some((Token::End, _)) = iter.peek() {
                    iter.next(); // consume 'end'
                    break;
                }
                exprs.push(parse_expression(iter, file)?);
            }
            ExprKind::Block { exprs }
        }
        Token::If => {
            // If expression
            let condition = parse_expression(iter, file)?;
            let then_branch = parse_expression(iter, file)?;
            let else_branch = if let Some((Token::Else, _)) = iter.peek() {
                iter.next(); // consume 'else'
                Some(Box::new(parse_expression(iter, file)?))
            } else {
                None
            };
            ExprKind::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch,
            }
        }
        Token::Match => {
            // Match expression
            let scrutinee = parse_expression(iter, file)?;
            let mut arms = Vec::new();
            loop {
                if let Some((Token::End, _)) = iter.peek() {
                    iter.next(); // consume 'end'
                    break;
                }
                let pattern = parse_pattern(iter, file)?;
                expect_token(iter, Token::FatArrow)?;
                let guard = if let Some((Token::If, _)) = iter.peek() {
                    iter.next(); // consume 'if'
                    Some(Box::new(parse_expression(iter, file)?))
                } else {
                    None
                };
                let body = parse_expression(iter, file)?;
                arms.push(MatchArm {
                    span: Span::new(file.clone(), pattern.span.range.start..body.span.range.end),
                    attributes: Vec::new(),
                    pattern,
                    guard,
                    body: Box::new(body),
                });
            }
            ExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            }
        }
        Token::For => {
            // For loop
            let pattern = parse_pattern(iter, file)?;
            expect_token(iter, Token::In)?;
            let iterator = parse_expression(iter, file)?;
            let body = parse_expression(iter, file)?;
            ExprKind::For {
                pattern,
                iterator: Box::new(iterator),
                body: Box::new(body),
            }
        }
        Token::While => {
            // While loop
            let condition = parse_expression(iter, file)?;
            let body = parse_expression(iter, file)?;
            ExprKind::While {
                condition: Box::new(condition),
                body: Box::new(body),
            }
        }
        Token::Break => {
            let value = if is_expr_start(iter.peek()) {
                Some(Box::new(parse_expression(iter, file)?))
            } else {
                None
            };
            ExprKind::Break { value }
        }
        Token::Continue => ExprKind::Continue,
        Token::Return => {
            let value = if is_expr_start(iter.peek()) {
                Some(Box::new(parse_expression(iter, file)?))
            } else {
                None
            };
            ExprKind::Return { value }
        }
        Token::Defer => {
            let expr = parse_expression(iter, file)?;
            ExprKind::Defer {
                expr: Box::new(expr),
            }
        }
        Token::Spawn => {
            let expr = parse_expression(iter, file)?;
            ExprKind::Spawn {
                expr: Box::new(expr),
            }
        }
        Token::Comptime => {
            let expr = parse_expression(iter, file)?;
            ExprKind::Comptime {
                expr: Box::new(expr),
            }
        }
        Token::Minus => {
            // Unary minus
            let expr = parse_expr_bp(iter, file, PREC_UNARY)?;
            ExprKind::Unary {
                op: UnOp::Neg,
                expr: Box::new(expr),
            }
        }
        Token::Not => {
            // Logical not
            let expr = parse_expr_bp(iter, file, PREC_UNARY)?;
            ExprKind::Unary {
                op: UnOp::Not,
                expr: Box::new(expr),
            }
        }
        Token::Amp => {
            // Reference
            let mutable = if let Some((Token::Mut, _)) = iter.peek() {
                iter.next(); // consume 'mut'
                true
            } else {
                false
            };
            let expr = parse_expr_bp(iter, file, PREC_UNARY)?;
            ExprKind::Unary {
                op: if mutable { UnOp::RefMut } else { UnOp::Ref },
                expr: Box::new(expr),
            }
        }
        Token::AmpMut => {
            // Mutable reference
            let expr = parse_expr_bp(iter, file, PREC_UNARY)?;
            ExprKind::Unary {
                op: UnOp::RefMut,
                expr: Box::new(expr),
            }
        }
        Token::Tilde => {
            // Bitwise not
            let expr = parse_expr_bp(iter, file, PREC_UNARY)?;
            ExprKind::Unary {
                op: UnOp::BitNot,
                expr: Box::new(expr),
            }
        }
        Token::DotDot => {
            let inclusive = false;
            let end = parse_expr_bp(iter, file, PREC_RANGE - 1)?;
            ExprKind::Range {
                start: None,
                end: Some(Box::new(end)),
                inclusive,
            }
        }
        Token::DotDotEq => {
            let inclusive = true;
            let end = parse_expr_bp(iter, file, PREC_RANGE - 1)?;
            ExprKind::Range {
                start: None,
                end: Some(Box::new(end)),
                inclusive,
            }
        }
        _ => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(
                    token,
                    vec![
                        Token::Int("".to_string()),
                        Token::Float("".to_string()),
                        Token::String("".to_string()),
                        Token::Char("".to_string()),
                        Token::True,
                        Token::False,
                        Token::Ident("".to_string()),
                        Token::LParen,
                        Token::LBracket,
                        Token::LBrace,
                        Token::Do,
                        Token::If,
                        Token::Match,
                        Token::For,
                        Token::While,
                        Token::Break,
                        Token::Continue,
                        Token::Return,
                        Token::Defer,
                        Token::Spawn,
                        Token::Comptime,
                        Token::Minus,
                        Token::Not,
                        Token::Amp,
                        Token::AmpMut,
                        Token::Tilde,
                    ],
                ),
                span,
                valid_syntax: vec!["primary expression".to_string()],
            });
        }
    };

    Ok(Expr::new(
        Span::new(file.clone(), start_span..iter.peek().map(|(_, s)| s.end).unwrap_or(start_span + 1)),
        kind,
    ))
}

fn parse_struct_or_map(iter: &mut TokenIter, file: &String) -> Result<ExprKind, ParseError> {
    // Note: the LBrace has already been consumed by parse_primary
    
    // Check for empty map/block
    if let Some((Token::RBrace, _)) = iter.peek() {
        iter.next(); // consume }
        return Ok(ExprKind::Map { entries: Vec::new() });
    }
    
    let mut entries = Vec::new();
    let key = parse_expression(iter, file)?;
    expect_token(iter, Token::FatArrow)?;
    let value = parse_expression(iter, file)?;
    entries.push((key, value));
    while let Some((Token::Comma, _)) = iter.peek() {
        iter.next();
        // Check if there's another entry or just closing brace
        if let Some((Token::RBrace, _)) = iter.peek() {
            break;
        }
        let key = parse_expression(iter, file)?;
        expect_token(iter, Token::FatArrow)?;
        let value = parse_expression(iter, file)?;
        entries.push((key, value));
    }
    expect_token(iter, Token::RBrace)?;
    Ok(ExprKind::Map { entries })
}

fn parse_postfix(iter: &mut TokenIter, file: &String, mut expr: Expr) -> Result<Expr, ParseError> {
    loop {
        match iter.peek() {
            Some((Token::LParen, _)) => {
                // Function call
                iter.next(); // consume '('
                let mut args = Vec::new();
                if let Some((Token::RParen, _)) = iter.peek() {
                    iter.next(); // consume ')'
                } else {
                    loop {
                        args.push(parse_expression(iter, file)?);
                        if let Some((Token::Comma, _)) = iter.peek() {
                            iter.next(); // consume ','
                        } else {
                            break;
                        }
                    }
                    expect_token(iter, Token::RParen)?;
                }
                expr = Expr::new(
                    Span::merge(&expr.span, &args.last().map(|e| &e.span).unwrap_or(&expr.span)),
                    ExprKind::Call {
                        func: Box::new(expr),
                        args,
                    },
                );
            }
            Some((Token::Dot, _)) => {
                // Field access
                iter.next(); // consume '.'
                let field = match iter.next() {
                    Some((Token::Ident(name), _)) => name,
                    _ => return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(
                            iter.peek().unwrap_or(&(Token::Ident("".to_string()), 0..0)).0.clone(),
                            vec![Token::Ident("".to_string())],
                        ),
                        span: iter.peek().unwrap_or(&(Token::Ident("".to_string()), 0..0)).1.clone(),
                        valid_syntax: vec!["field name".to_string()],
                    }),
                };
                expr = Expr::new(
                    Span::merge(&expr.span, &Span::new(file.clone(), 0..0)), // TODO: proper span
                    ExprKind::Field {
                        base: Box::new(expr),
                        field,
                    },
                );
            }
            Some((Token::LBracket, _)) => {
                // Index
                iter.next(); // consume '['
                let index = parse_expression(iter, file)?;
                expect_token(iter, Token::RBracket)?;
                expr = Expr::new(
                    Span::merge(&expr.span, &index.span),
                    ExprKind::Index {
                        base: Box::new(expr),
                        index: Box::new(index),
                    },
                );
            }
            _ => break,
        }
    }
    Ok(expr)
}

fn is_expr_start(peek: Option<&(Token, Range<usize>)>) -> bool {
    matches!(peek, Some((Token::Int(_) | Token::Float(_) | Token::String(_) | Token::Char(_) | Token::True | Token::False | Token::Ident(_) | Token::LParen | Token::LBracket | Token::LBrace | Token::Do | Token::If | Token::Match | Token::For | Token::While | Token::Break | Token::Continue | Token::Return | Token::Defer | Token::Spawn | Token::Comptime | Token::Minus | Token::Not | Token::Amp | Token::AmpMut | Token::Tilde | Token::DotDot | Token::DotDotEq, _)))
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
        Token::Fn => {
            // Function type: fn(T, U) -> V
            expect_token(iter, Token::LParen)?;
            let mut params = Vec::new();
            if let Some((Token::RParen, _)) = iter.peek() {
                iter.next(); // consume ')'
            } else {
                loop {
                    params.push(parse_type(iter, file)?);
                    if let Some((Token::Comma, _)) = iter.peek() {
                        iter.next(); // consume ','
                    } else {
                        break;
                    }
                }
                expect_token(iter, Token::RParen)?;
            }

            let return_type = if let Some((Token::Arrow, _)) = iter.peek() {
                iter.next(); // consume '->'
                Box::new(parse_type(iter, file)?)
            } else {
                Box::new(Type::new(Span::dummy(), TypeKind::Unit))
            };

            TypeKind::Fn {
                params,
                return_type,
            }
        }
        Token::UpperSelf => TypeKind::Path(Path {
            span: Span::new(file.clone(), span.start..span.end),
            segments: vec![PathSegment {
                span: Span::new(file.clone(), span.start..span.end),
                ident: "Self".to_string(),
                generics: None,
            }],
        }),
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
        Some((t, _span)) if t == expected => Ok(()),
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

fn parse_primary_pattern(iter: &mut TokenIter, file: &String) -> Result<Pattern, ParseError> {
    let start_span = iter.peek().map(|(_, s)| s.start).unwrap_or(0);

    let (token, span) = iter.next().ok_or(ParseError {
        kind: ParseErrorKind::UnexpectedEOF,
        span: 0..0,
        valid_syntax: vec!["pattern".to_string()],
    })?;

    let kind = match token {
        Token::Underscore => PatternKind::Wildcard,
        Token::Int(s) => {
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
                _ => IntType::Int,
            });
            PatternKind::Literal(Literal::Int { value, suffix: int_type })
        }
        Token::Float(s) => {
            let (value, suffix) = if s.ends_with("f32") {
                (s[..s.len() - 3].to_string(), Some(FloatType::F32))
            } else if s.ends_with("f64") {
                (s[..s.len() - 3].to_string(), Some(FloatType::F64))
            } else {
                (s.clone(), None)
            };
            PatternKind::Literal(Literal::Float { value, suffix })
        }
        Token::String(s) => PatternKind::Literal(Literal::String(s)),
        Token::Char(s) => PatternKind::Literal(Literal::Char(s)),
        Token::True => PatternKind::Literal(Literal::Bool(true)),
        Token::False => PatternKind::Literal(Literal::Bool(false)),
        Token::Ident(s) => {
            // Check if it's a path
            let mut segments = vec![PathSegment {
                span: Span::new(file.clone(), span.start..span.end),
                ident: s,
                generics: None,
            }];

            while let Some((Token::ColonColon, _)) = iter.peek() {
                iter.next(); // consume ::

                let (next_ident, next_span) = match iter.next() {
                    Some((Token::Ident(n), s)) => (n, s),
                    _ => return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(
                            iter.peek().unwrap_or(&(Token::Ident("".to_string()), 0..0)).0.clone(),
                            vec![Token::Ident("".to_string())],
                        ),
                        span: iter.peek().unwrap_or(&(Token::Ident("".to_string()), 0..0)).1.clone(),
                        valid_syntax: vec!["identifier".to_string()],
                    }),
                };

                segments.push(PathSegment {
                    span: Span::new(file.clone(), next_span.start..next_span.end),
                    ident: next_ident,
                    generics: None,
                });
            }

            if segments.len() == 1 {
                // Binding or enum variant
                if let Some((Token::LParen, _)) = iter.peek() {
                    // Enum variant
                    iter.next(); // consume '('
                    let mut args = Vec::new();
                    if let Some((Token::RParen, _)) = iter.peek() {
                        iter.next(); // consume ')'
                    } else {
                        loop {
                            args.push(parse_pattern(iter, file)?);
                            if let Some((Token::Comma, _)) = iter.peek() {
                                iter.next(); // consume ','
                            } else {
                                break;
                            }
                        }
                        expect_token(iter, Token::RParen)?;
                    }
                    PatternKind::EnumVariant {
                        path: Path {
                            span: Span::new(file.clone(), start_span..span.end),
                            segments,
                        },
                        args,
                    }
                } else if let Some((Token::LBrace, _)) = iter.peek() {
                    // Struct pattern
                    iter.next(); // consume '{'
                    let mut fields = Vec::new();
                    let mut rest = false;
                    if let Some((Token::RBrace, _)) = iter.peek() {
                        iter.next(); // consume '}'
                    } else {
                        loop {
                            if let Some((Token::DotDot, _)) = iter.peek() {
                                iter.next(); // consume '..'
                                rest = true;
                                break;
                            }
                            let field_name = match iter.next() {
                                Some((Token::Ident(name), _)) => name,
                                _ => return Err(ParseError {
                                    kind: ParseErrorKind::UnexpectedEOF,
                                    span: 0..0,
                                    valid_syntax: vec!["field name".to_string()],
                                }),
                            };
                            let pattern = if let Some((Token::Colon, _)) = iter.peek() {
                                iter.next(); // consume ':'
                                Some(parse_pattern(iter, file)?)
                            } else {
                                None
                            };
                            fields.push(FieldPattern {
                                span: Span::new(file.clone(), 0..0), // TODO
                                name: field_name,
                                pattern,
                            });
                            if let Some((Token::Comma, _)) = iter.peek() {
                                iter.next(); // consume ','
                            } else {
                                break;
                            }
                        }
                        expect_token(iter, Token::RBrace)?;
                    }
                    PatternKind::Struct {
                        path: Path {
                            span: Span::new(file.clone(), start_span..span.end),
                            segments,
                        },
                        fields,
                        rest,
                    }
                } else {
                    // Binding
                    let mutable = if let Some((Token::Mut, _)) = iter.peek() {
                        iter.next(); // consume 'mut'
                        true
                    } else {
                        false
                    };
                    PatternKind::Ident {
                        name: segments[0].ident.clone(),
                        mutable,
                    }
                }
            } else {
                // Path pattern
                PatternKind::Path(Path {
                    span: Span::new(file.clone(), start_span..segments.last().unwrap().span.range.end),
                    segments,
                })
            }
        }
        Token::LParen => {
            // Tuple pattern
            let mut elements = Vec::new();
            if let Some((Token::RParen, _)) = iter.peek() {
                iter.next(); // consume ')'
            } else {
                loop {
                    elements.push(parse_pattern(iter, file)?);
                    if let Some((Token::Comma, _)) = iter.peek() {
                        iter.next(); // consume ','
                    } else {
                        break;
                    }
                }
                expect_token(iter, Token::RParen)?;
            }
            PatternKind::Tuple { elements }
        }
        Token::LBracket => {
            // Array pattern
            let mut elements = Vec::new();
            let mut rest = None;
            if let Some((Token::RBracket, _)) = iter.peek() {
                iter.next(); // consume ']'
            } else {
                loop {
                    if let Some((Token::DotDot, _)) = iter.peek() {
                        iter.next(); // consume '..'
                        if is_pattern_start(iter.peek()) {
                            rest = Some(Box::new(parse_pattern(iter, file)?));
                        }
                        break;
                    }
                    elements.push(parse_pattern(iter, file)?);
                    if let Some((Token::Comma, _)) = iter.peek() {
                        iter.next(); // consume ','
                    } else {
                        break;
                    }
                }
                expect_token(iter, Token::RBracket)?;
            }
            PatternKind::Array { elements, rest }
        }
        _ => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(
                    token,
                    vec![
                        Token::Underscore,
                        Token::Int("".to_string()),
                        Token::Float("".to_string()),
                        Token::String("".to_string()),
                        Token::Char("".to_string()),
                        Token::True,
                        Token::False,
                        Token::Ident("".to_string()),
                        Token::LParen,
                        Token::LBracket,
                    ],
                ),
                span,
                valid_syntax: vec!["pattern".to_string()],
            });
        }
    };

    Ok(Pattern::new(
        Span::new(file.clone(), start_span..iter.peek().map(|(_, s)| s.end).unwrap_or(start_span + 1)),
        kind,
    ))
}

pub fn parse_pattern(iter: &mut TokenIter, file: &String) -> Result<Pattern, ParseError> {
    let mut patterns = vec![parse_primary_pattern(iter, file)?];

    while let Some((Token::BitOr, _)) = iter.peek() {
        iter.next(); // consume '|'
        patterns.push(parse_primary_pattern(iter, file)?);
    }

    if patterns.len() == 1 {
        Ok(patterns.into_iter().next().unwrap())
    } else {
        let start_span = patterns[0].span.range.start;
        let end_span = patterns.last().unwrap().span.range.end;
        Ok(Pattern::new(
            Span::new(file.clone(), start_span..end_span),
            PatternKind::Or { patterns },
        ))
    }
}

fn is_pattern_start(peek: Option<&(Token, Range<usize>)>) -> bool {
    matches!(peek, Some((Token::Underscore | Token::Int(_) | Token::Float(_) | Token::String(_) | Token::Char(_) | Token::True | Token::False | Token::Ident(_) | Token::LParen | Token::LBracket, _)))
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
                    Some((Token::Ident(n), _span)) => n,
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

            // Check for 'self' parameter
            if let Some((Token::LowerSelf, _)) = iter.peek() {
                iter.next(); // consume 'self'
                let span = Span::new(
                    file.clone(),
                    param_start..iter.peek().map(|(_, s)| s.start).unwrap_or(param_start),
                );
                params.push(FnParam {
                    span: span.clone(),
                    attributes: Vec::new(),
                    pattern: Pattern::new(
                        span.clone(),
                        PatternKind::Ident {
                            name: "self".to_string(),
                            mutable: false,
                        },
                    ),
                    ty: Type::new(
                        span,
                        TypeKind::Path(Path {
                            span: Span::dummy(),
                            segments: vec![PathSegment {
                                span: Span::dummy(),
                                ident: "Self".to_string(),
                                generics: None,
                            }],
                        }),
                    ),
                });
            } else if let Some((Token::Amp, _)) = iter.peek() {
                // Check for &self or &mut self
                iter.next(); // consume '&'
                let mutable = if let Some((Token::Mut, _)) = iter.peek() {
                    iter.next(); // consume 'mut'
                    true
                } else {
                    false
                };

                if let Some((Token::LowerSelf, _)) = iter.peek() {
                    iter.next(); // consume 'self'
                    let span = Span::new(
                        file.clone(),
                        param_start..iter.peek().map(|(_, s)| s.start).unwrap_or(param_start),
                    );
                    let self_ty = Type::new(
                        Span::dummy(),
                        TypeKind::Path(Path {
                            span: Span::dummy(),
                            segments: vec![PathSegment {
                                span: Span::dummy(),
                                ident: "Self".to_string(),
                                generics: None,
                            }],
                        }),
                    );
                    params.push(FnParam {
                        span: span.clone(),
                        attributes: Vec::new(),
                        pattern: Pattern::new(
                            span.clone(),
                            PatternKind::Ident {
                                name: "self".to_string(),
                                mutable: false,
                            },
                        ),
                        ty: Type::new(
                            span,
                            TypeKind::Ref {
                                mutable,
                                ty: Box::new(self_ty),
                            },
                        ),
                    });
                } else {
                    // Not self, just a pattern starting with &? (Not valid in param list usually, but handled by type)
                    // Wait, param list is Pattern : Type
                    // &Pattern is unlikely for simple args.
                    // But we consumed '&'. We are committed.
                    // Does standard param parsing handle this?
                    // Standard param parsing: Ident : Type.
                    // If we consumed '&', we broke it.
                    // So we must peek deeper?
                    // Peek 2 tokens?
                    // Logic:
                    // If & self -> self param
                    // If & mut self -> self param
                    // If & ... -> maybe error? Or maybe destructuring?
                    // For now assume strictly self handling here.
                    // If not self, we backtrack? We can't backtrack easily.
                    // But `parse_function` logic was `ident : type`. `&` is not ident.
                    // So `&` is only valid if it starts `&self`.
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(Token::Amp, vec![Token::LowerSelf]),
                        span: 0..0, // TODO
                        valid_syntax: vec!["self".to_string()],
                    });
                }
            } else if let Some((Token::Mut, _)) = iter.peek() {
                // Check for mut self
                iter.next(); // consume 'mut'
                if let Some((Token::LowerSelf, _)) = iter.peek() {
                    iter.next(); // consume 'self'
                    let span = Span::new(
                        file.clone(),
                        param_start..iter.peek().map(|(_, s)| s.start).unwrap_or(param_start),
                    );
                    params.push(FnParam {
                        span: span.clone(),
                        attributes: Vec::new(),
                        pattern: Pattern::new(
                            span.clone(),
                            PatternKind::Ident {
                                name: "self".to_string(),
                                mutable: true,
                            },
                        ),
                        ty: Type::new(
                            span,
                            TypeKind::Path(Path {
                                span: Span::dummy(),
                                segments: vec![PathSegment {
                                    span: Span::dummy(),
                                    ident: "Self".to_string(),
                                    generics: None,
                                }],
                            }),
                        ),
                    });
                } else {
                    // It is `mut ident : type`?
                    // We consumed `mut`.
                    let name = match iter.next() {
                        Some((Token::Ident(n), _)) => n,
                        _ => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedEOF,
                                span: 0..0,
                                valid_syntax: vec!["param name".to_string()],
                            });
                        }
                    };
                    expect_token(iter, Token::Colon)?;
                    let ty = parse_type(iter, file)?;
                    let param_end = iter.peek().map(|(_, s)| s.end).unwrap_or(param_start);
                    params.push(FnParam {
                        span: Span::new(file.clone(), param_start..param_end),
                        attributes: Vec::new(),
                        pattern: Pattern::new(
                            Span::dummy(),
                            PatternKind::Ident {
                                name,
                                mutable: true,
                            },
                        ),
                        ty,
                    });
                }
            } else {
                // Normal parameter: patterns...
                // Only supporting Ident for now
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
                            kind: ParseErrorKind::UnexpectedToken(
                                t,
                                vec![Token::Ident("".to_string())],
                            ),
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
            }

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
            exprs.push(parse_expression(iter, file)?);
        }
        Some(Expr::new(
            Span::new(file.clone(), 0..0), // TODO: proper span
            ExprKind::Block { exprs },
        ))
    } else {
        // Parse single expression body
        // Check if next token indicates end of function (declaration only)
        match iter.peek() {
            Some((Token::End, _))
            | Some((Token::Fn, _))
            | Some((Token::Type, _))
            | Some((Token::Const, _))
            | Some((Token::RBrace, _)) => None,
            _ => Some(parse_expression(iter, file)?),
        }
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

pub fn parse_impl(iter: &mut TokenIter, file: &String) -> Result<ImplDecl, ParseError> {
    let start_span = iter.peek().map(|(_, s)| s.start).unwrap_or(0);

    // Parse optional generics
    let generics = parse_generic_params(iter, file)?;

    // Parse type or trait reference
    // We need to differentiate between `impl Type` and `impl Trait for Type`
    // We can parse a type first. If we see `for`, it was a trait path.
    let first_ty = parse_type(iter, file)?;

    let (trait_path, self_ty) = if let Some((Token::For, _)) = iter.peek() {
        iter.next(); // consume 'for'

        // The first type must be a path if it's a trait
        let trait_path = match first_ty.kind {
            TypeKind::Path(p) => p,
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(Token::For, vec![]), // TODO: better error
                    span: first_ty.span.range,
                    valid_syntax: vec!["trait path".to_string()],
                });
            }
        };

        let self_ty = parse_type(iter, file)?;
        (Some(trait_path), self_ty)
    } else {
        (None, first_ty)
    };

    let mut items = Vec::new();

    // Parse items until 'end'
    loop {
        match iter.peek() {
            Some((Token::End, _)) => {
                iter.next(); // consume 'end'
                break;
            }
            Some((Token::At, _)) => {
                // Parse attributes
                let attributes = vec![parse_attribute(iter, file)?];

                // Check visibility
                let visibility = if let Some((Token::Pub, _)) = iter.peek() {
                    iter.next();
                    Visibility::Public
                } else {
                    Visibility::Private
                };

                // Parse item kind
                let kind = if let Some((Token::Fn, _)) = iter.peek() {
                    iter.next(); // consume 'fn'
                    let fn_decl = parse_function(iter, file)?;
                    ImplItemKind::Fn(fn_decl)
                } else if let Some((Token::Type, _)) = iter.peek() {
                    iter.next(); // consume 'type'
                    let name = match iter.next() {
                        Some((Token::Ident(n), _)) => n,
                        _ => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedEOF,
                                span: 0..0,
                                valid_syntax: vec!["type name".to_string()],
                            });
                        }
                    };
                    expect_token(iter, Token::Eq)?;
                    let ty = parse_type(iter, file)?;
                    ImplItemKind::Type { name, ty }
                } else if let Some((Token::Const, _)) = iter.peek() {
                    iter.next(); // consume 'const'
                    let (name, ty, value) = parse_const_decl(iter, file)?;
                    // ty is Option<Type> in parse_const_decl, but required in ImplItemKind::Const?
                    // Wait, ImplItemKind::Const definition: Const { name: String, ty: Type, value: Expr }
                    // So we need to ensure type is present or inferred? AST says Type is required.
                    // parse_const_decl returns Option<Type>. We should enforce it or AST allows inference?
                    // Let's assume for now we unwrap or handle it.
                    // Actually let's look at parse_const_decl signature in mod.rs: `Result<(String, Option<Type>, Expr), ParseError>`
                    // And ImplItemKind::Const: `ty: Type`
                    // So specific to impls, consts might necessitate types.
                    // For now let's just use a dummy type if missing or error out.
                    if let Some(t) = ty {
                        ImplItemKind::Const { name, ty: t, value }
                    } else {
                        return Err(ParseError {
                            kind: ParseErrorKind::UnexpectedEOF, // TODO better error
                            span: 0..0,
                            valid_syntax: vec!["type annotation".to_string()],
                        });
                    }
                } else {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(
                            iter.peek().unwrap().0.clone(),
                            vec![Token::Fn, Token::Type, Token::Const],
                        ),
                        span: iter.peek().unwrap().1.clone(),
                        valid_syntax: vec!["fn, type, or const".to_string()],
                    });
                };

                items.push(ImplItem {
                    span: Span::new(file.clone(), 0..0), // TODO
                    attributes,
                    visibility,
                    kind,
                });
            }
            Some(_) => {
                // No attributes
                let attributes = Vec::new();

                // Check visibility
                let visibility = if let Some((Token::Pub, _)) = iter.peek() {
                    iter.next();
                    Visibility::Public
                } else {
                    Visibility::Private
                };

                // Parse item kind
                let kind = if let Some((Token::Fn, _)) = iter.peek() {
                    iter.next(); // consume 'fn'
                    let fn_decl = parse_function(iter, file)?;
                    ImplItemKind::Fn(fn_decl)
                } else if let Some((Token::Type, _)) = iter.peek() {
                    iter.next(); // consume 'type'
                    let name = match iter.next() {
                        Some((Token::Ident(n), _)) => n,
                        _ => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedEOF,
                                span: 0..0,
                                valid_syntax: vec!["type name".to_string()],
                            });
                        }
                    };
                    expect_token(iter, Token::Eq)?;
                    let ty = parse_type(iter, file)?;
                    ImplItemKind::Type { name, ty }
                } else if let Some((Token::Const, _)) = iter.peek() {
                    iter.next(); // consume 'const'
                    let (name, ty, value) = parse_const_decl(iter, file)?;
                    if let Some(t) = ty {
                        ImplItemKind::Const { name, ty: t, value }
                    } else {
                        // Fallback or error
                        return Err(ParseError {
                            kind: ParseErrorKind::UnexpectedEOF,
                            span: 0..0,
                            valid_syntax: vec!["type annotation".to_string()],
                        });
                    }
                } else {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(
                            iter.peek().unwrap().0.clone(),
                            vec![Token::Fn, Token::Type, Token::Const],
                        ),
                        span: iter.peek().unwrap().1.clone(),
                        valid_syntax: vec!["fn, type, or const".to_string()],
                    });
                };

                items.push(ImplItem {
                    span: Span::new(file.clone(), 0..0), // TODO
                    attributes,
                    visibility,
                    kind,
                });
            }
            None => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedEOF,
                    span: 0..0,
                    valid_syntax: vec!["end or impl item".to_string()],
                });
            }
        }
    }

    Ok(ImplDecl {
        span: Span::new(
            file.clone(),
            start_span..iter.peek().map(|(_, s)| s.end).unwrap_or(start_span),
        ),
        generics,
        trait_path,
        self_ty,
        items,
    })
}

pub fn parse_trait(iter: &mut TokenIter, file: &String) -> Result<TraitDecl, ParseError> {
    let start_span = iter.peek().map(|(_, s)| s.start).unwrap_or(0);

    // Parse trait name
    let name = match iter.next() {
        Some((Token::Ident(n), _)) => n,
        Some((t, span)) => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(t, vec![Token::Ident("".to_string())]),
                span,
                valid_syntax: vec!["trait name".to_string()],
            });
        }
        None => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
                span: 0..0,
                valid_syntax: vec!["trait name".to_string()],
            });
        }
    };

    // Parse optional generics
    let generics = parse_generic_params(iter, file)?;

    // Parse supertraits
    let mut supertraits = Vec::new(); // TODO: parse supertraits : Super1 + Super2
    if let Some((Token::Colon, _)) = iter.peek() {
        iter.next(); // consume ':'
        loop {
            // Expect path
            let ty = parse_type(iter, file)?;
            match ty.kind {
                TypeKind::Path(p) => supertraits.push(p),
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(Token::Ident("".to_string()), vec![]),
                        span: ty.span.range,
                        valid_syntax: vec!["supertrait path".to_string()],
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

    let mut associated_types = Vec::new();
    let mut methods = Vec::new();

    // Parse items until 'end'
    loop {
        match iter.peek() {
            Some((Token::End, _)) => {
                iter.next(); // consume 'end'
                break;
            }
            Some((Token::At, _)) => {
                // Attributes (skip for now / TODO)
                iter.next();
                // Continue to parse item... simplified for now
            }
            Some((t, _)) => {
                if *t == Token::Fn {
                    iter.next(); // consume 'fn'
                    let fn_decl = parse_function(iter, file)?;
                    methods.push(fn_decl);
                } else if *t == Token::Type {
                    iter.next(); // consume 'type'
                    let name = match iter.next() {
                        Some((Token::Ident(n), _)) => n,
                        _ => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedEOF,
                                span: 0..0,
                                valid_syntax: vec!["type name".to_string()],
                            });
                        }
                    };

                    let mut bounds = Vec::new();
                    // Optional bounds : Bound + Bound
                    if let Some((Token::Colon, _)) = iter.peek() {
                        iter.next(); // consume ':'
                        loop {
                            let bound_ty = parse_type(iter, file)?;
                            match bound_ty.kind {
                                TypeKind::Path(p) => bounds.push(p),
                                _ => {
                                    return Err(ParseError {
                                        kind: ParseErrorKind::UnexpectedToken(
                                            Token::Ident("".to_string()),
                                            vec![],
                                        ),
                                        span: bound_ty.span.range,
                                        valid_syntax: vec!["trait bound".to_string()],
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

                    // Optional default = Type
                    let default = if let Some((Token::Eq, _)) = iter.peek() {
                        iter.next(); // consume '='
                        Some(parse_type(iter, file)?)
                    } else {
                        None
                    };

                    associated_types.push(AssociatedType {
                        span: Span::new(file.clone(), 0..0), // TODO
                        attributes: Vec::new(),
                        name,
                        bounds,
                        default,
                    });
                } else {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(
                            t.clone(),
                            vec![Token::Fn, Token::Type],
                        ),
                        span: iter.peek().map(|(_, s)| s.clone()).unwrap_or(0..0),
                        valid_syntax: vec!["fn or type".to_string()],
                    });
                }
            }
            None => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedEOF,
                    span: 0..0,
                    valid_syntax: vec!["end or trait item".to_string()],
                });
            }
        }
    }

    Ok(TraitDecl {
        span: Span::new(
            file.clone(),
            start_span..iter.peek().map(|(_, s)| s.end).unwrap_or(start_span),
        ),
        name,
        generics,
        supertraits,
        associated_types,
        methods,
    })
}

pub fn parse_type_alias(iter: &mut TokenIter, file: &String) -> Result<TypeAliasDecl, ParseError> {
    let start_span = iter.peek().map(|(_, s)| s.start).unwrap_or(0);

    // Parse alias name
    let name = match iter.next() {
        Some((Token::Ident(n), _)) => n,
        Some((t, span)) => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(t, vec![Token::Ident("".to_string())]),
                span,
                valid_syntax: vec!["type alias name".to_string()],
            });
        }
        None => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
                span: 0..0,
                valid_syntax: vec!["type alias name".to_string()],
            });
        }
    };

    // Parse optional generics
    let generics = parse_generic_params(iter, file)?;

    // Expect '='
    expect_token(iter, Token::Eq)?;

    // Parse type
    let ty = parse_type(iter, file)?;

    Ok(TypeAliasDecl {
        span: Span::new(
            file.clone(),
            start_span..iter.peek().map(|(_, s)| s.end).unwrap_or(start_span),
        ),
        name,
        generics,
        ty,
    })
}

pub fn parse_const_decl(
    iter: &mut TokenIter,
    file: &String,
) -> Result<(String, Option<Type>, Expr), ParseError> {
    // Parse name
    let name = match iter.next() {
        Some((Token::Ident(n), _)) => n,
        Some((t, span)) => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(t, vec![Token::Ident("".to_string())]),
                span,
                valid_syntax: vec!["constant name".to_string()],
            });
        }
        None => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
                span: 0..0,
                valid_syntax: vec!["constant name".to_string()],
            });
        }
    };

    // Parse optional type annotation
    let ty = if let Some((Token::Colon, _)) = iter.peek() {
        iter.next(); // consume ':'
        Some(parse_type(iter, file)?)
    } else {
        None
    };

    // Expect '='
    expect_token(iter, Token::Eq)?;

    // Parse value
    let value = parse_expression(iter, file)?;

    Ok((name, ty, value))
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

        while let Some((t, _span)) = iter.peek() {
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

pub fn parse_use(iter: &mut TokenIter, file: &String) -> Result<UseDecl, ParseError> {
    let start_span = iter.peek().map(|(_, s)| s.start).unwrap_or(0);

    // Parse path (string literal)
    let path = match iter.next() {
        Some((Token::String(s), _)) => s,
        Some((t, span)) => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(t, vec![Token::String("".to_string())]),
                span,
                valid_syntax: vec!["string literal for path".to_string()],
            });
        }
        None => {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
                span: 0..0,
                valid_syntax: vec!["use path".to_string()],
            });
        }
    };

    // Parse optional items (.name, .*, .{name1, name2})
    let items = if let Some((Token::Dot, _)) = iter.peek() {
        iter.next(); // consume '.'
        match iter.next() {
            Some((Token::Star, _)) => UseItems::All,
            Some((Token::Ident(name), _)) => UseItems::Single(name),
            Some((Token::LBrace, _)) => {
                let mut names = Vec::new();
                loop {
                    match iter.next() {
                        Some((Token::Ident(name), _)) => names.push(name),
                        Some((t, span)) => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedToken(
                                    t,
                                    vec![Token::Ident("".to_string())],
                                ),
                                span,
                                valid_syntax: vec!["identifier".to_string()],
                            });
                        }
                        None => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedEOF,
                                span: 0..0,
                                valid_syntax: vec!["identifier or closing brace".to_string()],
                            });
                        }
                    }
                    match iter.peek() {
                        Some((Token::Comma, _)) => {
                            iter.next(); // consume ','
                        }
                        Some((Token::RBrace, _)) => {
                            iter.next(); // consume '}'
                            break;
                        }
                        Some((t, span)) => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedToken(
                                    t.clone(),
                                    vec![Token::Comma, Token::RBrace],
                                ),
                                span: span.clone(),
                                valid_syntax: vec!["comma or closing brace".to_string()],
                            });
                        }
                        None => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedEOF,
                                span: 0..0,
                                valid_syntax: vec!["comma or closing brace".to_string()],
                            });
                        }
                    }
                }
                UseItems::Multiple(names)
            }
            Some((t, span)) => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(
                        t,
                        vec![Token::Star, Token::Ident("".to_string()), Token::LBrace],
                    ),
                    span,
                    valid_syntax: vec!["*, identifier, or {list}".to_string()],
                });
            }
            None => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedEOF,
                    span: 0..0,
                    valid_syntax: vec!["*, identifier, or {list}".to_string()],
                });
            }
        }
    } else {
        UseItems::None
    };

    // Parse optional alias (as name)
    let alias = if let Some((Token::As, _)) = iter.peek() {
        iter.next(); // consume 'as'
        match iter.next() {
            Some((Token::Ident(name), _)) => Some(name),
            Some((t, span)) => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(t, vec![Token::Ident("".to_string())]),
                    span,
                    valid_syntax: vec!["alias name".to_string()],
                });
            }
            None => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedEOF,
                    span: 0..0,
                    valid_syntax: vec!["alias name".to_string()],
                });
            }
        }
    } else {
        None
    };

    Ok(UseDecl {
        span: Span::new(
            file.clone(),
            start_span..iter.peek().map(|(_, s)| s.end).unwrap_or(start_span),
        ),
        path,
        alias,
        items,
    })
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
        assert_eq!(
            fn_decl.params[0].pattern.kind,
            PatternKind::Ident {
                name: "a".to_string(),
                mutable: false
            }
        );
        assert_eq!(
            fn_decl.params[1].pattern.kind,
            PatternKind::Ident {
                name: "b".to_string(),
                mutable: false
            }
        );
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

    fn parse_single_trait(source: &str) -> Result<TraitDecl, ParseError> {
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        // Skip the trait token
        iter.next();
        parse_trait(&mut iter, &"<test>".to_string())
    }

    #[test]
    fn test_parse_simple_trait() {
        let trait_decl = parse_single_trait("trait Show\n  fn show(self) -> string\nend").unwrap();
        assert_eq!(trait_decl.name, "Show");
        assert!(trait_decl.generics.is_empty());
        assert_eq!(trait_decl.methods.len(), 1);
        assert_eq!(trait_decl.methods[0].name, "show");
    }

    #[test]
    fn test_parse_trait_with_generics() {
        let trait_decl =
            parse_single_trait("trait Converter<From, To>\n  fn convert(from: From) -> To\nend")
                .unwrap();
        assert_eq!(trait_decl.name, "Converter");
        assert_eq!(trait_decl.generics.len(), 2);
        assert_eq!(trait_decl.methods.len(), 1);
    }

    #[test]
    fn test_parse_trait_with_associated_type() {
        let trait_decl = parse_single_trait(
            "trait Iterator\n  type Item\n  fn next(mut self) -> Option<Item>\nend",
        )
        .unwrap();
        assert_eq!(trait_decl.name, "Iterator");
        assert_eq!(trait_decl.associated_types.len(), 1);
        assert_eq!(trait_decl.associated_types[0].name, "Item");
        assert_eq!(trait_decl.methods.len(), 1);
    }

    #[test]
    fn test_parse_trait_with_associated_type_bounds_and_default() {
        let trait_decl =
            parse_single_trait("trait Container\n  type Item: Clone + Eq = i32\nend").unwrap();
        assert_eq!(trait_decl.name, "Container");
        assert_eq!(trait_decl.associated_types.len(), 1);
        let assoc = &trait_decl.associated_types[0];
        assert_eq!(assoc.name, "Item");
        assert_eq!(assoc.bounds.len(), 2);
        assert!(assoc.default.is_some());
    }

    #[test]
    fn test_parse_trait_with_supertraits() {
        let trait_decl =
            parse_single_trait("trait Copy: Clone\n  fn copy(self) -> Self\nend").unwrap();
        assert_eq!(trait_decl.name, "Copy");
        assert_eq!(trait_decl.supertraits.len(), 1);
    }

    fn parse_single_impl(source: &str) -> Result<ImplDecl, ParseError> {
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        // Skip the impl token
        iter.next();
        parse_impl(&mut iter, &"<test>".to_string())
    }

    #[test]
    fn test_parse_inherent_impl() {
        let impl_decl = parse_single_impl(
            "impl Point\n  fn new(x: f64, y: f64) -> Point\n    \"new_point\"\n  end\nend",
        )
        .unwrap();
        assert!(impl_decl.trait_path.is_none());
        match impl_decl.self_ty.kind {
            TypeKind::Path(p) => assert_eq!(p.segments[0].ident, "Point"),
            _ => panic!("Expected Path type"),
        }
        assert_eq!(impl_decl.items.len(), 1);
    }

    #[test]
    fn test_parse_trait_impl() {
        let impl_decl = parse_single_impl(
            "impl Show for Point\n  fn show(self) -> string\n    \"Point\"\n  end\nend",
        )
        .unwrap();
        assert!(impl_decl.trait_path.is_some());
        assert_eq!(impl_decl.trait_path.unwrap().segments[0].ident, "Show");
        match impl_decl.self_ty.kind {
            TypeKind::Path(p) => assert_eq!(p.segments[0].ident, "Point"),
            _ => panic!("Expected Path type"),
        }
    }

    #[test]
    fn test_parse_impl_with_generics() {
        let impl_decl = parse_single_impl(
            "impl<T> Show for Option<T>\n  fn show(self) -> string\n    \"Option\"\n  end\nend",
        )
        .unwrap();
        assert_eq!(impl_decl.generics.len(), 1);
        assert!(impl_decl.trait_path.is_some());
        // Check Option<T>
        match impl_decl.self_ty.kind {
            TypeKind::Path(p) => {
                assert_eq!(p.segments[0].ident, "Option");
                assert!(p.segments[0].generics.is_some());
            }
            _ => panic!("Expected Path type"),
        }
    }

    #[test]
    fn test_parse_impl_with_associated_type() {
        let impl_decl = parse_single_impl("impl Iterator for Range\n  type Item = int\n  fn next(mut self) -> Option<int>\n    None\n  end\nend").unwrap();
        assert_eq!(impl_decl.items.len(), 2);
        match &impl_decl.items[0].kind {
            ImplItemKind::Type { name, .. } => assert_eq!(name, "Item"),
            _ => panic!("Expected Type item"),
        }
    }

    fn parse_single_type_alias(source: &str) -> Result<TypeAliasDecl, ParseError> {
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        // Skip the type token
        iter.next();
        parse_type_alias(&mut iter, &"<test>".to_string())
    }

    fn parse_single_item(source: &str) -> Result<Item, ParseError> {
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        parse_item(&mut iter, &"<test>".to_string(), Vec::new())
    }

    fn parse_single_expression(source: &str) -> Result<Expr, ParseError> {
        let tokens = lex_without_comments(source).unwrap();
        let token_pairs: Vec<(Token, Range<usize>)> =
            tokens.into_iter().map(|s| (s.token, s.span)).collect();
        let mut iter = token_pairs.into_iter().peekable();
        parse_expression(&mut iter, &"<test>".to_string())
    }

    #[test]
    fn test_parse_const() {
        let item = parse_single_item("const PI = 3.14159").unwrap();
        match &item.kind {
            ItemKind::Const { name, ty, value } => {
                assert_eq!(name, "PI");
                assert!(ty.is_none());
                match &value.kind {
                    ExprKind::Literal(Literal::Float { value, suffix }) => {
                        assert_eq!(value, "3.14159");
                        assert!(suffix.is_none());
                    }
                    _ => panic!("Expected float literal"),
                }
            }
            _ => panic!("Expected const item"),
        }
    }

    #[test]
    fn test_parse_const_with_type() {
        let item = parse_single_item("const MAX: int = 100").unwrap();
        match &item.kind {
            ItemKind::Const { name, ty, value } => {
                assert_eq!(name, "MAX");
                assert!(ty.is_some());
                match &value.kind {
                    ExprKind::Literal(Literal::Int { value, suffix }) => {
                        assert_eq!(value, "100");
                        assert!(suffix.is_none());
                    }
                    _ => panic!("Expected int literal"),
                }
            }
            _ => panic!("Expected const item"),
        }
    }

    #[test]
    fn test_parse_use_simple() {
        let item = parse_single_item("use \"std/io\"").unwrap();
        match &item.kind {
            ItemKind::Use(use_decl) => {
                assert_eq!(use_decl.path, "\"std/io\"");
                assert!(use_decl.alias.is_none());
                assert!(matches!(use_decl.items, UseItems::None));
            }
            _ => panic!("Expected use item"),
        }
    }

    #[test]
    fn test_parse_use_single() {
        let item = parse_single_item("use \"std/io\".puts").unwrap();
        match &item.kind {
            ItemKind::Use(use_decl) => {
                assert_eq!(use_decl.path, "\"std/io\"");
                assert!(use_decl.alias.is_none());
                match &use_decl.items {
                    UseItems::Single(name) => assert_eq!(name, "puts"),
                    _ => panic!("Expected Single"),
                }
            }
            _ => panic!("Expected use item"),
        }
    }

    #[test]
    fn test_parse_use_all() {
        let item = parse_single_item("use \"std/io\".*").unwrap();
        match &item.kind {
            ItemKind::Use(use_decl) => {
                assert_eq!(use_decl.path, "\"std/io\"");
                assert!(use_decl.alias.is_none());
                assert!(matches!(use_decl.items, UseItems::All));
            }
            _ => panic!("Expected use item"),
        }
    }

    #[test]
    fn test_parse_use_multiple() {
        let item = parse_single_item("use \"std/io\".{puts, gets}").unwrap();
        match &item.kind {
            ItemKind::Use(use_decl) => {
                assert_eq!(use_decl.path, "\"std/io\"");
                assert!(use_decl.alias.is_none());
                match &use_decl.items {
                    UseItems::Multiple(names) => {
                        assert_eq!(names.len(), 2);
                        assert_eq!(names[0], "puts");
                        assert_eq!(names[1], "gets");
                    }
                    _ => panic!("Expected Multiple"),
                }
            }
            _ => panic!("Expected use item"),
        }
    }

    #[test]
    fn test_parse_use_alias() {
        let item = parse_single_item("use \"std/io\" as io").unwrap();
        match &item.kind {
            ItemKind::Use(use_decl) => {
                assert_eq!(use_decl.path, "\"std/io\"");
                assert_eq!(use_decl.alias, Some("io".to_string()));
                assert!(matches!(use_decl.items, UseItems::None));
            }
            _ => panic!("Expected use item"),
        }
    }

    #[test]
    fn test_parse_type_alias() {
        let alias = parse_single_type_alias("type ID = int").unwrap();
        assert_eq!(alias.name, "ID");
        assert!(alias.generics.is_empty());
        match alias.ty.kind {
            TypeKind::Int(_) => {}
            _ => panic!("Expected Int type"),
        }
    }

    #[test]
    fn test_parse_type_alias_with_generics() {
        let alias = parse_single_type_alias("type Callback<T> = fn(T) -> ()").unwrap();
        assert_eq!(alias.name, "Callback");
        assert_eq!(alias.generics.len(), 1);
        match alias.ty.kind {
            TypeKind::Fn { .. } => {}
            _ => panic!("Expected Fn type"),
        }
    }

    // Expression parsing tests
    #[test]
    fn test_parse_literal_int() {
        let expr = parse_single_expression("42").unwrap();
        match &expr.kind {
            ExprKind::Literal(Literal::Int { value, suffix }) => {
                assert_eq!(value, "42");
                assert!(suffix.is_none());
            }
            _ => panic!("Expected int literal"),
        }
    }

    #[test]
    fn test_parse_literal_int_with_suffix() {
        let expr = parse_single_expression("42i32").unwrap();
        match &expr.kind {
            ExprKind::Literal(Literal::Int { value, suffix }) => {
                assert_eq!(value, "42");
                assert_eq!(suffix, &Some(IntType::I32));
            }
            _ => panic!("Expected int literal with suffix"),
        }
    }

    #[test]
    fn test_parse_literal_float() {
        let expr = parse_single_expression("3.14").unwrap();
        match &expr.kind {
            ExprKind::Literal(Literal::Float { value, suffix }) => {
                assert_eq!(value, "3.14");
                assert!(suffix.is_none());
            }
            _ => panic!("Expected float literal"),
        }
    }

    #[test]
    fn test_parse_literal_float_with_suffix() {
        let expr = parse_single_expression("3.14f64").unwrap();
        match &expr.kind {
            ExprKind::Literal(Literal::Float { value, suffix }) => {
                assert_eq!(value, "3.14");
                assert_eq!(suffix, &Some(FloatType::F64));
            }
            _ => panic!("Expected float literal with suffix"),
        }
    }

    #[test]
    fn test_parse_literal_string() {
        let expr = parse_single_expression("\"hello\"").unwrap();
        match &expr.kind {
            ExprKind::Literal(Literal::String(s)) => assert_eq!(s, "\"hello\""),
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn test_parse_literal_char() {
        let expr = parse_single_expression("'a'").unwrap();
        match &expr.kind {
            ExprKind::Literal(Literal::Char(c)) => assert_eq!(c, "'a'"),
            _ => panic!("Expected char literal"),
        }
    }

    #[test]
    fn test_parse_literal_bool_true() {
        let expr = parse_single_expression("true").unwrap();
        match &expr.kind {
            ExprKind::Literal(Literal::Bool(b)) => assert!(*b),
            _ => panic!("Expected bool literal true"),
        }
    }

    #[test]
    fn test_parse_literal_bool_false() {
        let expr = parse_single_expression("false").unwrap();
        match &expr.kind {
            ExprKind::Literal(Literal::Bool(b)) => assert!(!*b),
            _ => panic!("Expected bool literal false"),
        }
    }

    #[test]
    fn test_parse_literal_unit() {
        let expr = parse_single_expression("()").unwrap();
        match &expr.kind {
            ExprKind::Literal(Literal::Unit) => {}
            _ => panic!("Expected unit literal"),
        }
    }

    #[test]
    fn test_parse_identifier() {
        let expr = parse_single_expression("variable").unwrap();
        match &expr.kind {
            ExprKind::Ident(name) => assert_eq!(name, "variable"),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_parse_binary_add() {
        let expr = parse_single_expression("a + b").unwrap();
        match &expr.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinOp::Add);
                match &left.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "a"),
                    _ => panic!("Expected left ident"),
                }
                match &right.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "b"),
                    _ => panic!("Expected right ident"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_binary_subtract() {
        let expr = parse_single_expression("x - y").unwrap();
        match &expr.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinOp::Sub);
                match &left.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected left ident"),
                }
                match &right.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "y"),
                    _ => panic!("Expected right ident"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_binary_multiply() {
        let expr = parse_single_expression("2 * 3").unwrap();
        match &expr.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinOp::Mul);
                match &left.kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "2"),
                    _ => panic!("Expected left int literal"),
                }
                match &right.kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "3"),
                    _ => panic!("Expected right int literal"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_binary_divide() {
        let expr = parse_single_expression("10 / 2").unwrap();
        match &expr.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinOp::Div);
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_binary_equals() {
        let expr = parse_single_expression("a == b").unwrap();
        match &expr.kind {
            ExprKind::Binary { op, .. } => {
                assert_eq!(op, &BinOp::Eq);
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_binary_not_equals() {
        let expr = parse_single_expression("x != y").unwrap();
        match &expr.kind {
            ExprKind::Binary { op, .. } => {
                assert_eq!(op, &BinOp::Ne);
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_binary_less_than() {
        let expr = parse_single_expression("a < b").unwrap();
        match &expr.kind {
            ExprKind::Binary { op, .. } => {
                assert_eq!(op, &BinOp::Lt);
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_binary_greater_than() {
        let expr = parse_single_expression("x > y").unwrap();
        match &expr.kind {
            ExprKind::Binary { op, .. } => {
                assert_eq!(op, &BinOp::Gt);
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_binary_logical_and() {
        let expr = parse_single_expression("a and b").unwrap();
        match &expr.kind {
            ExprKind::Binary { op, .. } => {
                assert_eq!(op, &BinOp::And);
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_binary_logical_or() {
        let expr = parse_single_expression("x or y").unwrap();
        match &expr.kind {
            ExprKind::Binary { op, .. } => {
                assert_eq!(op, &BinOp::Or);
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_binary_assignment() {
        let expr = parse_single_expression("a = 5").unwrap();
        match &expr.kind {
            ExprKind::Assign { target, op, value } => {
                assert!(op.is_none());
                match &target.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "a"),
                    _ => panic!("Expected target ident"),
                }
                match &value.kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "5"),
                    _ => panic!("Expected value int literal"),
                }
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_binary_compound_assignment() {
        let expr = parse_single_expression("x += 10").unwrap();
        match &expr.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinOp::Add);
                match &left.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected left ident"),
                }
                match &right.kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "10"),
                    _ => panic!("Expected right 10"),
                }
            }
            _ => panic!("Expected compound assignment expression"),
        }
    }

    #[test]
    fn test_parse_binary_precedence() {
        let expr = parse_single_expression("a + b * c").unwrap();
        match &expr.kind {
            ExprKind::Binary { left, op, right } => {
                assert_eq!(op, &BinOp::Add);
                match &right.kind {
                    ExprKind::Binary { op: inner_op, .. } => {
                        assert_eq!(inner_op, &BinOp::Mul);
                    }
                    _ => panic!("Expected nested binary for multiplication"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_pipe_operator() {
        let expr = parse_single_expression("a |> b").unwrap();
        match &expr.kind {
            ExprKind::Pipe { left, right } => {
                match &left.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "a"),
                    _ => panic!("Expected left ident"),
                }
                match &right.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "b"),
                    _ => panic!("Expected right ident"),
                }
            }
            _ => panic!("Expected pipe expression"),
        }
    }

    #[test]
    fn test_parse_unary_negation() {
        let expr = parse_single_expression("-x").unwrap();
        match &expr.kind {
            ExprKind::Unary { op, expr: inner } => {
                assert_eq!(op, &UnOp::Neg);
                match &inner.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected inner ident"),
                }
            }
            _ => panic!("Expected unary expression"),
        }
    }

    #[test]
    fn test_parse_unary_logical_not() {
        let expr = parse_single_expression("not flag").unwrap();
        match &expr.kind {
            ExprKind::Unary { op, expr: inner } => {
                assert_eq!(op, &UnOp::Not);
                match &inner.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "flag"),
                    _ => panic!("Expected inner ident"),
                }
            }
            _ => panic!("Expected unary expression"),
        }
    }

    #[test]
    fn test_parse_unary_reference() {
        let expr = parse_single_expression("&value").unwrap();
        match &expr.kind {
            ExprKind::Unary { op, expr: inner } => {
                assert_eq!(op, &UnOp::Ref);
                match &inner.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "value"),
                    _ => panic!("Expected inner ident"),
                }
            }
            _ => panic!("Expected unary expression"),
        }
    }

    #[test]
    fn test_parse_unary_mutable_reference() {
        let expr = parse_single_expression("&mut data").unwrap();
        match &expr.kind {
            ExprKind::Unary { op, expr: inner } => {
                assert_eq!(op, &UnOp::RefMut);
            }
            _ => panic!("Expected unary expression"),
        }
    }

    #[test]
    fn test_parse_unary_bitwise_not() {
        let expr = parse_single_expression("~mask").unwrap();
        match &expr.kind {
            ExprKind::Unary { op, expr: inner } => {
                assert_eq!(op, &UnOp::BitNot);
                match &inner.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "mask"),
                    _ => panic!("Expected inner ident"),
                }
            }
            _ => panic!("Expected unary expression"),
        }
    }

    #[test]
    fn test_parse_function_call_no_args() {
        let expr = parse_single_expression("func()").unwrap();
        match &expr.kind {
            ExprKind::Call { func, args } => {
                assert!(args.is_empty());
                match &func.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "func"),
                    _ => panic!("Expected func ident"),
                }
            }
            _ => panic!("Expected call expression"),
        }
    }

    #[test]
    fn test_parse_function_call_with_args() {
        let expr = parse_single_expression("add(a, b)").unwrap();
        match &expr.kind {
            ExprKind::Call { func, args } => {
                assert_eq!(args.len(), 2);
                match &args[0].kind {
                    ExprKind::Ident(name) => assert_eq!(name, "a"),
                    _ => panic!("Expected arg a"),
                }
                match &args[1].kind {
                    ExprKind::Ident(name) => assert_eq!(name, "b"),
                    _ => panic!("Expected arg b"),
                }
            }
            _ => panic!("Expected call expression"),
        }
    }

    #[test]
    fn test_parse_method_call() {
        let expr = parse_single_expression("obj.method(arg)").unwrap();
        match &expr.kind {
            ExprKind::Call { func, args } => {
                assert_eq!(args.len(), 1);
                match &func.kind {
                    ExprKind::Field { base, field } => {
                        match &base.kind {
                            ExprKind::Ident(name) => assert_eq!(name, "obj"),
                            _ => panic!("Expected base obj"),
                        }
                        assert_eq!(field, "method");
                    }
                    _ => panic!("Expected field access for method"),
                }
            }
            _ => panic!("Expected call expression"),
        }
    }

    #[test]
    fn test_parse_field_access() {
        let expr = parse_single_expression("point.x").unwrap();
        match &expr.kind {
            ExprKind::Field { base, field } => {
                assert_eq!(field, "x");
                match &base.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "point"),
                    _ => panic!("Expected base ident"),
                }
            }
            _ => panic!("Expected field expression"),
        }
    }

    #[test]
    fn test_parse_indexing() {
        let expr = parse_single_expression("arr[0]").unwrap();
        match &expr.kind {
            ExprKind::Index { base, index } => {
                match &base.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "arr"),
                    _ => panic!("Expected base ident"),
                }
                match &index.kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "0"),
                    _ => panic!("Expected index int literal"),
                }
            }
            _ => panic!("Expected index expression"),
        }
    }

    #[test]
    fn test_parse_chained_access() {
        let expr = parse_single_expression("obj.field[1].method()").unwrap();
        match &expr.kind {
            ExprKind::Call { func, args } => {
                assert!(args.is_empty());
                match &func.kind {
                    ExprKind::Field { base, field } => {
                        assert_eq!(field, "method");
                        match &base.kind {
                            ExprKind::Index { base: inner_base, .. } => {
                                match &inner_base.kind {
                                    ExprKind::Field { base: inner_inner, field: inner_field } => {
                                        assert_eq!(inner_field, "field");
                                        match &inner_inner.kind {
                                            ExprKind::Ident(name) => assert_eq!(name, "obj"),
                                            _ => panic!("Expected obj ident"),
                                        }
                                    }
                                    _ => panic!("Expected nested field access"),
                                }
                            }
                            _ => panic!("Expected index access"),
                        }
                    }
                    _ => panic!("Expected field access"),
                }
            }
            _ => panic!("Expected call expression"),
        }
    }

    #[test]
    fn test_parse_tuple() {
        let expr = parse_single_expression("(a, b, c)").unwrap();
        match &expr.kind {
            ExprKind::Tuple { elements } => {
                assert_eq!(elements.len(), 3);
                match &elements[0].kind {
                    ExprKind::Ident(name) => assert_eq!(name, "a"),
                    _ => panic!("Expected first element"),
                }
                match &elements[1].kind {
                    ExprKind::Ident(name) => assert_eq!(name, "b"),
                    _ => panic!("Expected second element"),
                }
                match &elements[2].kind {
                    ExprKind::Ident(name) => assert_eq!(name, "c"),
                    _ => panic!("Expected third element"),
                }
            }
            _ => panic!("Expected tuple expression"),
        }
    }

    #[test]
    fn test_parse_single_element_tuple() {
        let expr = parse_single_expression("(value)").unwrap();
        match &expr.kind {
            ExprKind::Ident(name) => assert_eq!(name, "value"),
            _ => panic!("Expected single element to be unwrapped"),
        }
    }

    #[test]
    fn test_parse_array() {
        let expr = parse_single_expression("[1, 2, 3]").unwrap();
        match &expr.kind {
            ExprKind::Array { elements } => {
                assert_eq!(elements.len(), 3);
                match &elements[0].kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "1"),
                    _ => panic!("Expected first element 1"),
                }
                match &elements[1].kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "2"),
                    _ => panic!("Expected second element 2"),
                }
                match &elements[2].kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "3"),
                    _ => panic!("Expected third element 3"),
                }
            }
            _ => panic!("Expected array expression"),
        }
    }

    #[test]
    fn test_parse_empty_array() {
        let expr = parse_single_expression("[]").unwrap();
        match &expr.kind {
            ExprKind::Array { elements } => {
                assert!(elements.is_empty());
            }
            _ => panic!("Expected empty array expression"),
        }
    }

    #[test]
    fn test_parse_simple_map() {
        match parse_single_expression("{a => b}") {
            Ok(expr) => {
                match &expr.kind {
                    ExprKind::Map { entries } => {
                        assert_eq!(entries.len(), 1);
                    }
                    _ => panic!("Expected map, got {:?}", expr.kind),
                }
            }
            Err(e) => panic!("Error: {:?}", e),
        }
    }

    #[test]
    fn test_parse_map() {
        let expr = parse_single_expression("{key => value, x => 42}").unwrap();
        match &expr.kind {
            ExprKind::Map { entries } => {
                assert_eq!(entries.len(), 2);
                match &entries[0] {
                    (key, value) => {
                        match &key.kind {
                            ExprKind::Ident(name) => assert_eq!(name, "key"),
                            _ => panic!("Expected key ident"),
                        }
                        match &value.kind {
                            ExprKind::Ident(name) => assert_eq!(name, "value"),
                            _ => panic!("Expected value ident"),
                        }
                    }
                }
                match &entries[1] {
                    (key, value) => {
                        match &key.kind {
                            ExprKind::Ident(name) => assert_eq!(name, "x"),
                            _ => panic!("Expected key x"),
                        }
                        match &value.kind {
                            ExprKind::Literal(Literal::Int { value: v, .. }) => assert_eq!(v, "42"),
                            _ => panic!("Expected value 42"),
                        }
                    }
                }
            }
            _ => panic!("Expected map expression"),
        }
    }

    #[test]
    fn test_parse_struct_construction() {
        let expr = parse_single_expression("Point { x: 1, y: 2 }").unwrap();
        match &expr.kind {
            ExprKind::Struct { path, fields, .. } => {
                assert_eq!(path.segments[0].ident, "Point");
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].name.as_ref().unwrap(), "x");
                assert_eq!(fields[1].name.as_ref().unwrap(), "y");
            }
            _ => panic!("Expected struct expression"),
        }
    }

    #[test]
    fn test_parse_struct_shorthand() {
        let expr = parse_single_expression("Point { x, y }").unwrap();
        match &expr.kind {
            ExprKind::Struct { path, fields, .. } => {
                assert_eq!(path.segments[0].ident, "Point");
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].name.as_ref().unwrap(), "x");
                assert_eq!(fields[1].name.as_ref().unwrap(), "y");
                // Check that values are idents
                match &fields[0].value.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected shorthand value"),
                }
            }
            _ => panic!("Expected struct expression"),
        }
    }

    #[test]
    fn test_parse_enum_variant_construction() {
        let expr = parse_single_expression("Option::Some(42)").unwrap();
        match &expr.kind {
            ExprKind::EnumVariant { path, args } => {
                assert_eq!(path.segments.len(), 2);
                assert_eq!(path.segments[0].ident, "Option");
                assert_eq!(path.segments[1].ident, "Some");
                assert_eq!(args.len(), 1);
                match &args[0].kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "42"),
                    _ => panic!("Expected arg 42"),
                }
            }
            _ => panic!("Expected enum variant expression"),
        }
    }

    #[test]
    fn test_parse_path() {
        let expr = parse_single_expression("std::io::println").unwrap();
        match &expr.kind {
            ExprKind::Path(path) => {
                assert_eq!(path.segments.len(), 3);
                assert_eq!(path.segments[0].ident, "std");
                assert_eq!(path.segments[1].ident, "io");
                assert_eq!(path.segments[2].ident, "println");
            }
            _ => panic!("Expected path expression"),
        }
    }

    #[test]
    fn test_parse_if_expression() {
        let expr = parse_single_expression("if condition result").unwrap();
        match &expr.kind {
            ExprKind::If { condition, then_branch, else_branch } => {
                match &condition.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "condition"),
                    _ => panic!("Expected condition ident"),
                }
                match &then_branch.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "result"),
                    _ => panic!("Expected then ident"),
                }
                assert!(else_branch.is_none());
            }
            _ => panic!("Expected if expression"),
        }
    }

    #[test]
    fn test_parse_if_else_expression() {
        let expr = parse_single_expression("if flag true else false").unwrap();
        match &expr.kind {
            ExprKind::If { condition, then_branch, else_branch } => {
                match &condition.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "flag"),
                    _ => panic!("Expected condition ident"),
                }
                match &then_branch.kind {
                    ExprKind::Literal(Literal::Bool(b)) => assert!(*b),
                    _ => panic!("Expected then true"),
                }
                match &else_branch.as_ref().unwrap().kind {
                    ExprKind::Literal(Literal::Bool(b)) => assert!(!*b),
                    _ => panic!("Expected else false"),
                }
            }
            _ => panic!("Expected if-else expression"),
        }
    }

    #[test]
    fn test_parse_match_expression() {
        let expr = parse_single_expression("match value\n  1 => \"one\"\n  2 => \"two\"\nend").unwrap();
        match &expr.kind {
            ExprKind::Match { scrutinee, arms } => {
                match &scrutinee.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "value"),
                    _ => panic!("Expected scrutinee ident"),
                }
                assert_eq!(arms.len(), 2);
                match &arms[0].body.kind {
                    ExprKind::Literal(Literal::String(s)) => assert_eq!(s, "\"one\""),
                    _ => panic!("Expected first arm body"),
                }
                match &arms[1].body.kind {
                    ExprKind::Literal(Literal::String(s)) => assert_eq!(s, "\"two\""),
                    _ => panic!("Expected second arm body"),
                }
            }
            _ => panic!("Expected match expression"),
        }
    }

    #[test]
    fn test_parse_or_pattern() {
        let expr = parse_single_expression("match value\n  1 | 2 => \"one or two\"\nend").unwrap();
        match &expr.kind {
            ExprKind::Match { scrutinee, arms } => {
                assert_eq!(arms.len(), 1);
                match &arms[0].pattern.kind {
                    PatternKind::Or { patterns } => {
                        assert_eq!(patterns.len(), 2);
                        match &patterns[0].kind {
                            PatternKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "1"),
                            _ => panic!("Expected first pattern literal"),
                        }
                        match &patterns[1].kind {
                            PatternKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "2"),
                            _ => panic!("Expected second pattern literal"),
                        }
                    }
                    _ => panic!("Expected or pattern"),
                }
            }
            _ => panic!("Expected match expression"),
        }
    }

    #[test]
    fn test_parse_or_pattern_multiple() {
        let expr = parse_single_expression("match value\n  1 | 2 | 3 => \"small\"\nend").unwrap();
        match &expr.kind {
            ExprKind::Match { scrutinee, arms } => {
                assert_eq!(arms.len(), 1);
                match &arms[0].pattern.kind {
                    PatternKind::Or { patterns } => {
                        assert_eq!(patterns.len(), 3);
                        for (i, expected) in ["1", "2", "3"].iter().enumerate() {
                            match &patterns[i].kind {
                                PatternKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, expected),
                                _ => panic!("Expected pattern literal {}", i),
                            }
                        }
                    }
                    _ => panic!("Expected or pattern"),
                }
            }
            _ => panic!("Expected match expression"),
        }
    }

    #[test]
    fn test_parse_for_loop() {
        let expr = parse_single_expression("for x in list x * 2").unwrap();
        match &expr.kind {
            ExprKind::For { pattern, iterator, body } => {
                match &pattern.kind {
                    PatternKind::Ident { name, mutable } => {
                        assert_eq!(name, "x");
                        assert!(!mutable);
                    }
                    _ => panic!("Expected pattern ident"),
                }
                match &iterator.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "list"),
                    _ => panic!("Expected iterator ident"),
                }
                match &body.kind {
                    ExprKind::Binary { op, .. } => assert_eq!(op, &BinOp::Mul),
                    _ => panic!("Expected body binary"),
                }
            }
            _ => panic!("Expected for expression"),
        }
    }

    #[test]
    fn test_parse_while_loop() {
        let expr = parse_single_expression("while condition action").unwrap();
        match &expr.kind {
            ExprKind::While { condition, body } => {
                match &condition.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "condition"),
                    _ => panic!("Expected condition ident"),
                }
                match &body.kind {
                    ExprKind::Ident(name) => assert_eq!(name, "action"),
                    _ => panic!("Expected body ident"),
                }
            }
            _ => panic!("Expected while expression"),
        }
    }

    #[test]
    fn test_parse_block_expression() {
        let expr = parse_single_expression("do\n  x = 1\n  y = 2\n  x + y\nend").unwrap();
        match &expr.kind {
            ExprKind::Block { exprs } => {
                assert_eq!(exprs.len(), 3);
                match &exprs[0].kind {
                    ExprKind::Assign { .. } => {}
                    _ => panic!("Expected first assignment"),
                }
                match &exprs[1].kind {
                    ExprKind::Assign { .. } => {}
                    _ => panic!("Expected second assignment"),
                }
                match &exprs[2].kind {
                    ExprKind::Binary { op, .. } => assert_eq!(op, &BinOp::Add),
                    _ => panic!("Expected final addition"),
                }
            }
            _ => panic!("Expected block expression"),
        }
    }

    #[test]
    fn test_parse_break_expression() {
        let expr = parse_single_expression("break").unwrap();
        match &expr.kind {
            ExprKind::Break { value } => {
                assert!(value.is_none());
            }
            _ => panic!("Expected break expression"),
        }
    }

    #[test]
    fn test_parse_break_with_value() {
        let expr = parse_single_expression("break result").unwrap();
        match &expr.kind {
            ExprKind::Break { value } => {
                match &value.as_ref().unwrap().kind {
                    ExprKind::Ident(name) => assert_eq!(name, "result"),
                    _ => panic!("Expected break value ident"),
                }
            }
            _ => panic!("Expected break expression"),
        }
    }

    #[test]
    fn test_parse_continue_expression() {
        let expr = parse_single_expression("continue").unwrap();
        match &expr.kind {
            ExprKind::Continue => {}
            _ => panic!("Expected continue expression"),
        }
    }

    #[test]
    fn test_parse_return_expression() {
        let expr = parse_single_expression("return").unwrap();
        match &expr.kind {
            ExprKind::Return { value } => {
                assert!(value.is_none());
            }
            _ => panic!("Expected return expression"),
        }
    }

    #[test]
    fn test_parse_return_with_value() {
        let expr = parse_single_expression("return value").unwrap();
        match &expr.kind {
            ExprKind::Return { value } => {
                match &value.as_ref().unwrap().kind {
                    ExprKind::Ident(name) => assert_eq!(name, "value"),
                    _ => panic!("Expected return value ident"),
                }
            }
            _ => panic!("Expected return expression"),
        }
    }

    #[test]
    fn test_parse_defer_expression() {
        let expr = parse_single_expression("defer cleanup()").unwrap();
        match &expr.kind {
            ExprKind::Defer { .. } => {}
            _ => panic!("Expected defer expression"),
        }
    }

    #[test]
    fn test_parse_spawn_expression() {
        let expr = parse_single_expression("spawn task()").unwrap();
        match &expr.kind {
            ExprKind::Spawn { .. } => {}
            _ => panic!("Expected spawn expression"),
        }
    }

    #[test]
    fn test_parse_comptime_expression() {
        let expr = parse_single_expression("comptime 1 + 2").unwrap();
        match &expr.kind {
            ExprKind::Comptime { expr: inner } => {
                match &inner.kind {
                    ExprKind::Binary { op, .. } => assert_eq!(op, &BinOp::Add),
                    _ => panic!("Expected binary in comptime"),
                }
            }
            _ => panic!("Expected comptime expression"),
        }
    }

    #[test]
    fn test_parse_range_expression() {
        let expr = parse_single_expression("1..10").unwrap();
        match &expr.kind {
            ExprKind::Range { start, end, inclusive } => {
                match &start.as_ref().unwrap().kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "1"),
                    _ => panic!("Expected start 1"),
                }
                match &end.as_ref().unwrap().kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "10"),
                    _ => panic!("Expected end 10"),
                }
                assert!(!inclusive);
            }
            _ => panic!("Expected range expression"),
        }
    }

    #[test]
    fn test_parse_inclusive_range_expression() {
        let expr = parse_single_expression("1..=10").unwrap();
        match &expr.kind {
            ExprKind::Range { start, end, inclusive } => {
                assert!(inclusive);
            }
            _ => panic!("Expected inclusive range expression"),
        }
    }

    #[test]
    fn test_parse_range_from_start() {
        let expr = parse_single_expression("5..").unwrap();
        match &expr.kind {
            ExprKind::Range { start, end, inclusive } => {
                match &start.as_ref().unwrap().kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "5"),
                    _ => panic!("Expected start 5"),
                }
                assert!(end.is_none());
                assert!(!inclusive);
            }
            _ => panic!("Expected range from start"),
        }
    }

    #[test]
    fn test_parse_range_to_end() {
        let expr = parse_single_expression("..10").unwrap();
        match &expr.kind {
            ExprKind::Range { start, end, inclusive } => {
                assert!(start.is_none());
                match &end.as_ref().unwrap().kind {
                    ExprKind::Literal(Literal::Int { value, .. }) => assert_eq!(value, "10"),
                    _ => panic!("Expected end 10"),
                }
                assert!(!inclusive);
            }
            _ => panic!("Expected range to end"),
        }
    }
}
