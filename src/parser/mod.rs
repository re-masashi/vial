use crate::ast::*;
use crate::lexer::Token;
use chumsky::pratt::{infix, left, prefix, right};
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;
use std::ops::Range;

type Span = SimpleSpan;

#[cfg(test)]
pub mod tests;

fn to_range(span: SimpleSpan) -> Range<usize> {
    span.start..span.end
}

fn ident<'src>()
-> impl Parser<'src, &'src [Token], String, extra::Err<Rich<'src, Token, Span>>> + Clone {
    select! {
        Token::Variable(name) => name,
    }
    .labelled("identifier")
}

fn type_annot<'src>()
-> impl Parser<'src, &'src [Token], TypeAnnot, extra::Err<Rich<'src, Token, Span>>> + Clone {
    recursive(|ty| {
        let primitive = choice((
            just(Token::KeywordInt).to(TypeAnnotKind::Int),
            just(Token::KeywordFloat).to(TypeAnnotKind::Float),
            just(Token::KeywordBool).to(TypeAnnotKind::Bool),
            just(Token::KeywordString).to(TypeAnnotKind::String),
        ))
        .labelled("primitive type");

        let unit = just(Token::LParen)
            .then(just(Token::RParen))
            .to(TypeAnnotKind::Unit)
            .labelled("unit type ()");

        let never = just(Token::Bang)
            .to(TypeAnnotKind::Never)
            .labelled("never type");

        let named = ident().map(TypeAnnotKind::Named);

        let generic = ident()
            .then(
                just(Token::Less)
                    .labelled("'<'")
                    .ignore_then(
                        ty.clone()
                            .separated_by(just(Token::Comma))
                            .at_least(1)
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::Greater).labelled("'>'"))
                    .recover_with(via_parser(nested_delimiters(
                        Token::Less,
                        Token::Greater,
                        [
                            (Token::LParen, Token::RParen),
                            (Token::LBrace, Token::RBrace),
                        ],
                        |_| vec![],
                    ))),
            )
            .map(|(name, args)| TypeAnnotKind::Generic {
                name,
                args,
                kind: None,
            })
            .labelled("generic type");

        let tuple = just(Token::LParen)
            .labelled("'(' for tuple type")
            .ignore_then(
                ty.clone()
                    .separated_by(just(Token::Comma))
                    .at_least(2)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::RParen).labelled("')' for tuple type"))
            .map(TypeAnnotKind::Tuple)
            .labelled("tuple type");

        let function = just(Token::KeywordFn)
            .ignore_then(
                just(Token::LParen)
                    .labelled("'(' for function type")
                    .ignore_then(
                        ty.clone()
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::RParen).labelled("')' for function type")),
            )
            .then_ignore(just(Token::Arrow).labelled("'->'"))
            .then(ty.clone())
            .then(
                select! { Token::Variable(s) if s == "effects" => () }
                    .ignore_then(ident().separated_by(just(Token::Comma)).collect::<Vec<_>>())
                    .map(EffectAnnot::closed)
                    .or_not(),
            )
            .map(|((params, ret), effects)| TypeAnnotKind::Function {
                params,
                return_type: Box::new(ret),
                effects: effects.unwrap_or_else(EffectAnnot::pure),
            })
            .labelled("function type");

        let base = choice((function, tuple, generic, primitive, unit, never, named)).map_with(
            |kind, e| TypeAnnot {
                span: to_range(e.span()),
                file: String::new(),
                type_: kind,
            },
        );

        base.clone()
            .then(
                just(Token::Union)
                    .ignore_then(base.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map_with(|(first, rest), e| {
                if rest.is_empty() {
                    first
                } else {
                    let mut all = vec![first];
                    all.extend(rest);
                    TypeAnnot {
                        span: to_range(e.span()),
                        file: String::new(),
                        type_: TypeAnnotKind::Union(all),
                    }
                }
            })
            .labelled("type")
    })
}

fn pattern<'src>()
-> impl Parser<'src, &'src [Token], Pattern, extra::Err<Rich<'src, Token, Span>>> + Clone {
    recursive(|pat| {
        let wildcard = select! { Token::Variable(s) if s == "_" => PatKind::Wildcard }
            .labelled("wildcard '_'");

        let literal = choice((
            select! { Token::Int(n) => Literal::Int(n) },
            select! { Token::Float(f) => Literal::Float(f) },
            select! { Token::String(s) => Literal::String(s) },
            select! { Token::Bool(b) => Literal::Bool(b) },
        ))
        .map(PatKind::Literal)
        .labelled("literal");

        let bind = ident().map(PatKind::Bind).labelled("binding");

        let array_pat = just(Token::LBracket)
            .labelled("'[' for array pattern")
            .ignore_then(
                pat.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::RBracket).labelled("']' for array pattern"))
            .map(PatKind::Array)
            .recover_with(via_parser(nested_delimiters(
                Token::LBracket,
                Token::RBracket,
                [
                    (Token::LParen, Token::RParen),
                    (Token::LBrace, Token::RBrace),
                ],
                |_| PatKind::Error,
            )))
            .labelled("array pattern");

        let enum_pat = ident()
            .then_ignore(just(Token::Access).labelled("'::'"))
            .then(ident())
            .then(
                just(Token::LParen)
                    .ignore_then(
                        pat.clone()
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::RParen))
                    .or_not(),
            )
            .map(|((name, variant), params)| PatKind::Enum {
                name,
                variant,
                params: params.unwrap_or_default(),
            })
            .labelled("enum pattern");

        let struct_pat = ident()
            .then(
                just(Token::LBrace)
                    .labelled("'{' for struct pattern")
                    .ignore_then(
                        ident()
                            .then_ignore(just(Token::Colon).labelled("':'"))
                            .then(pat.clone())
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::RBrace).labelled("'}' for struct pattern")),
            )
            .map(|(name, fields)| PatKind::Struct { name, fields })
            .labelled("struct pattern");

        let tuple_pat = just(Token::LParen)
            .labelled("'(' for tuple pattern")
            .ignore_then(
                pat.clone()
                    .separated_by(just(Token::Comma))
                    .at_least(1) // Allow single elements, will be handled below
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::RParen).labelled("')' for tuple pattern"))
            .map(|patterns: Vec<Pattern>| {
                if patterns.len() == 1 {
                    // For a single pattern in parentheses, we might want to just return the inner pattern
                    // But for consistency with tuple expressions, we'll still create a tuple with one element
                    PatKind::Tuple(patterns)
                } else {
                    PatKind::Tuple(patterns)
                }
            })
            .recover_with(via_parser(nested_delimiters(
                Token::LParen,
                Token::RParen,
                [
                    (Token::LBrace, Token::RBrace),
                    (Token::LBracket, Token::RBracket),
                ],
                |_| PatKind::Error,
            )))
            .labelled("tuple pattern");

        let as_pat = ident()
            .then_ignore(just(Token::KeywordAs).labelled("'as'"))
            .then(pat.clone())
            .map(|(name, pattern)| PatKind::As {
                name,
                pattern: Box::new(pattern),
            })
            .labelled("as pattern");

        let base_pat = choice((
            tuple_pat, array_pat, enum_pat, struct_pat, as_pat, literal, wildcard, bind,
        ))
        .map_with(|pat, e| Pattern {
            span: to_range(e.span()),
            file: String::new(),
            pat,
        });

        base_pat
            .clone()
            .then(
                just(Token::Union)
                    .ignore_then(base_pat.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map_with(|(first, rest), e| {
                if rest.is_empty() {
                    first
                } else {
                    let mut all = vec![first];
                    all.extend(rest);
                    Pattern {
                        span: to_range(e.span()),
                        file: String::new(),
                        pat: PatKind::Or(all),
                    }
                }
            })
            .labelled("pattern")
    })
}

fn expr<'src>()
-> impl Parser<'src, &'src [Token], Expr, extra::Err<Rich<'src, Token, Span>>> + Clone {
    recursive(|expr| {
        let literal = choice((
            select! { Token::Int(n) => ExprKind::Int(n) }.labelled("integer"),
            select! { Token::Float(f) => ExprKind::Float(f) }.labelled("float"),
            select! { Token::String(s) => ExprKind::String(s) }.labelled("string"),
            select! { Token::Bool(b) => ExprKind::Bool(b) }.labelled("boolean"),
        ));

        let variable = ident().map(ExprKind::Variable);

        let lambda = just(Token::KeywordFn)
            .ignore_then(
                just(Token::LParen)
                    .labelled("'(' for lambda")
                    .ignore_then(
                        ident()
                            .then(just(Token::Colon).ignore_then(type_annot()).or_not())
                            .map_with(|(name, ty), e| FnArg {
                                span: to_range(e.span()),
                                file: String::new(),
                                name,
                                type_: ty,
                            })
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::RParen).labelled("')' for lambda"))
                    .recover_with(via_parser(nested_delimiters(
                        Token::LParen,
                        Token::RParen,
                        [
                            (Token::LBrace, Token::RBrace),
                            (Token::LBracket, Token::RBracket),
                        ],
                        |_| vec![],
                    ))),
            )
            .then(expr.clone().labelled("lambda body"))
            .map(|(args, body)| ExprKind::Lambda {
                args,
                expression: Box::new(body),
            })
            .labelled("lambda");

        let block = just(Token::LBrace)
            .labelled("'{' for block")
            .ignore_then(
                expr.clone()
                    .then_ignore(just(Token::Semicolon).or_not())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::RBrace).labelled("'}' for block"))
            .recover_with(via_parser(nested_delimiters(
                Token::LBrace,
                Token::RBrace,
                [
                    (Token::LParen, Token::RParen),
                    (Token::LBracket, Token::RBracket),
                ],
                |_| vec![],
            )))
            .map(ExprKind::Block)
            .labelled("block");

        let let_expr = just(Token::KeywordLet)
            .ignore_then(ident().labelled("variable name"))
            .then(just(Token::Colon).ignore_then(type_annot()).or_not())
            .then_ignore(just(Token::Assign).labelled("'='"))
            .then(expr.clone().labelled("value"))
            .recover_with(skip_then_retry_until(
                any().ignored(),
                one_of([
                    Token::Semicolon,
                    Token::RBrace,
                    Token::KeywordLet,
                    Token::KeywordDef,
                ])
                .ignored(),
            ))
            .map(|((var, ty), value)| ExprKind::Let {
                var,
                type_annot: ty,
                value: Box::new(value),
            })
            .labelled("let binding");

        let if_let = just(Token::KeywordIf)
            .ignore_then(just(Token::KeywordLet))
            .ignore_then(pattern())
            .then_ignore(just(Token::Assign).labelled("'='"))
            .then(expr.clone())
            .then(expr.clone())
            .then(just(Token::KeywordElse).ignore_then(expr.clone()).or_not())
            .map(|(((pattern, expr_val), then), else_)| ExprKind::IfLet {
                pattern,
                expr: Box::new(expr_val),
                then: Box::new(then),
                else_: else_.map(Box::new),
            })
            .labelled("if-let");

        let if_expr = just(Token::KeywordIf)
            .ignore_then(expr.clone().labelled("condition"))
            .then(expr.clone().labelled("then branch"))
            .then(just(Token::KeywordElse).ignore_then(expr.clone()).or_not())
            .map(|((cond, then_block), else_block)| ExprKind::IfElse {
                condition: Box::new(cond),
                then: Box::new(then_block),
                else_: else_block.map(Box::new),
            })
            .labelled("if");

        let while_let = just(Token::KeywordWhile)
            .ignore_then(just(Token::KeywordLet))
            .ignore_then(pattern())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then(expr.clone())
            .map(|((pattern, expr_val), body)| ExprKind::WhileLet {
                pattern,
                expr: Box::new(expr_val),
                body: Box::new(body),
            })
            .labelled("while-let");

        let while_expr = just(Token::KeywordWhile)
            .ignore_then(expr.clone().labelled("condition"))
            .then(expr.clone().labelled("body"))
            .map(|(cond, body)| ExprKind::While(Box::new(cond), Box::new(body)))
            .labelled("while loop");

        let match_arm = pattern()
            .then(just(Token::KeywordIf).ignore_then(expr.clone()).or_not())
            .then_ignore(just(Token::FatArrow).labelled("'=>'"))
            .then(expr.clone().labelled("match arm body"))
            .map_with(|((pattern, guard), body), e| MatchArm {
                pattern,
                guard,
                body: Box::new(body),
                span: to_range(e.span()),
            })
            .recover_with(skip_then_retry_until(
                any().ignored(),
                one_of([Token::Comma, Token::RBrace]).ignored(),
            ));

        let match_expr = just(Token::KeywordMatch)
            .ignore_then(expr.clone().labelled("match value"))
            .then(
                just(Token::LBrace)
                    .labelled("'{' for match")
                    .ignore_then(
                        match_arm
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::RBrace).labelled("'}' for match"))
                    .recover_with(via_parser(nested_delimiters(
                        Token::LBrace,
                        Token::RBrace,
                        [
                            (Token::LParen, Token::RParen),
                            (Token::LBracket, Token::RBracket),
                        ],
                        |_| vec![],
                    ))),
            )
            .map(|(scrutinee, arms)| ExprKind::Match(Box::new(scrutinee), arms))
            .labelled("match");

        let for_expr = just(Token::KeywordFor)
            .ignore_then(ident().labelled("loop variable"))
            .then_ignore(just(Token::KeywordIn).labelled("'in'"))
            .then(expr.clone().labelled("iterator"))
            .then(expr.clone().labelled("loop body"))
            .map(|((value, iterator), body)| ExprKind::For {
                value,
                iterator: Box::new(iterator),
                expression: Box::new(body),
            })
            .labelled("for loop");

        let loop_expr = just(Token::KeywordLoop)
            .ignore_then(expr.clone())
            .map(|body| ExprKind::Loop {
                label: None,
                body: Box::new(body),
            })
            .labelled("loop");

        let with_expr = just(Token::KeywordWith)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::KeywordAs).labelled("'as'"))
            .then(ident())
            .then(expr.clone())
            .map(|((context, var), body)| ExprKind::With {
                context: Box::new(context),
                var,
                body: Box::new(body),
            })
            .labelled("with");

        let return_expr = just(Token::KeywordReturn)
            .then(expr.clone().or_not())
            .map(|(_, e)| ExprKind::Return(e.map(Box::new)))
            .labelled("return");

        let break_expr = just(Token::KeywordBreak)
            .then(expr.clone().or_not())
            .map(|(_, e)| ExprKind::Break(e.map(Box::new)))
            .labelled("break");

        let continue_expr = just(Token::KeywordContinue)
            .to(ExprKind::Continue)
            .labelled("continue");

        let perform_expr = just(Token::KeywordPerform)
            .ignore_then(ident())
            .then(
                just(Token::LParen)
                    .ignore_then(
                        expr.clone()
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::RParen)),
            )
            .map(|(effect, args)| ExprKind::Perform { effect, args })
            .labelled("perform");

        let effect_handler = ident()
            .then(
                just(Token::LParen)
                    .ignore_then(
                        ident()
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::RParen)),
            )
            .then_ignore(just(Token::FatArrow))
            .then(expr.clone())
            .map_with(|((effect, mut params), body), e| {
                let resume_param = params.pop().unwrap_or_else(|| "k".to_string());
                EffectHandler {
                    span: to_range(e.span()),
                    effect,
                    params,
                    resume_param,
                    body,
                }
            });

        let handle_expr = just(Token::KeywordHandle)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::KeywordWith))
            .then(
                just(Token::LBrace)
                    .ignore_then(
                        effect_handler
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::RBrace)),
            )
            .map(|(body, handlers)| ExprKind::Handle {
                body: Box::new(body),
                handlers,
            })
            .labelled("handle");

        let enum_construct = ident()
            .then_ignore(just(Token::Access))
            .then(ident())
            .then(
                just(Token::LParen)
                    .ignore_then(
                        expr.clone()
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::RParen)),
            )
            .map(|((name, variant), args)| ExprKind::EnumConstruct {
                name,
                variant,
                args,
            })
            .labelled("enum construction");

        let import = just(Token::KeywordImport)
            .ignore_then(select! { Token::String(s) => s })
            .then(
                choice((
                    just(Token::LBrace)
                        .ignore_then(
                            ident()
                                .then(just(Token::KeywordAs).ignore_then(ident()).or_not())
                                .separated_by(just(Token::Comma))
                                .allow_trailing()
                                .collect::<Vec<_>>(),
                        )
                        .then_ignore(just(Token::RBrace))
                        .map(|items| (items, None)),
                    just(Token::KeywordAs)
                        .ignore_then(ident())
                        .map(|alias| (vec![], Some(alias))),
                ))
                .or_not(),
            )
            .map_with(|(path, items), e| {
                let (items, alias) = items.unwrap_or((vec![], None));
                let path_parts: Vec<String> = path.split('/').map(|s| s.to_string()).collect();
                ExprKind::Import(Import {
                    span: to_range(e.span()),
                    file: String::new(),
                    path: path_parts,
                    items,
                    alias,
                })
            })
            .labelled("import");

        let macro_call = select! { Token::MacroInvocation(name) => name }
            .then(
                just(Token::LParen)
                    .ignore_then(
                        expr.clone()
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::RParen)),
            )
            .map(|(name, args)| ExprKind::MacroCall(name, args, Delimiter::Paren))
            .labelled("macro call");

        let paren_expr = just(Token::LParen)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::RParen))
            .map(|e| e.expr);

        let atom = choice((
            literal,
            if_let,
            if_expr,
            while_let,
            while_expr,
            match_expr,
            for_expr,
            loop_expr,
            with_expr,
            return_expr,
            break_expr,
            continue_expr,
            perform_expr,
            handle_expr,
            import,
            macro_call,
            enum_construct,
            lambda,
            block,
            variable,
            let_expr,
            paren_expr,
        ))
        .map_with(|kind, e| Expr {
            span: to_range(e.span()),
            file: String::new(),
            expr: kind,
        });

        let array_or_expr = just(Token::LBracket)
            .labelled("'[' for array")
            .ignore_then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::RBracket).labelled("']' for array"))
            .recover_with(via_parser(nested_delimiters(
                Token::LBracket,
                Token::RBracket,
                [
                    (Token::LParen, Token::RParen),
                    (Token::LBrace, Token::RBrace),
                ],
                |_| vec![],
            )))
            .map_with(|exprs, e| Expr {
                span: to_range(e.span()),
                file: String::new(),
                expr: ExprKind::Array(exprs),
            });

        let map_or_block = just(Token::LBrace)
            .then(choice((
                expr.clone()
                    .then_ignore(just(Token::Colon).labelled("':' for map"))
                    .then(expr.clone())
                    .separated_by(just(Token::Comma))
                    .at_least(1)
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .map(ExprKind::Map),
                expr.clone()
                    .then_ignore(just(Token::Semicolon).or_not())
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .map(ExprKind::Block),
            )))
            .then_ignore(just(Token::RBrace))
            .map_with(|(_, kind), e| Expr {
                span: to_range(e.span()),
                file: String::new(),
                expr: kind,
            })
            .recover_with(via_parser(nested_delimiters(
                Token::LBrace,
                Token::RBrace,
                [
                    (Token::LParen, Token::RParen),
                    (Token::LBracket, Token::RBracket),
                ],
                |_| Expr {
                    span: 0..0,
                    file: String::new(),
                    expr: ExprKind::Error,
                },
            )));

        let tuple_or_paren = just(Token::LParen)
            .ignore_then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .at_least(1)
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::RParen))
            .map_with(|exprs, e| {
                if exprs.len() == 1 {
                    exprs.into_iter().next().unwrap()
                } else {
                    Expr {
                        span: to_range(e.span()),
                        file: String::new(),
                        expr: ExprKind::Tuple(exprs),
                    }
                }
            });

        let base = choice((array_or_expr, map_or_block, tuple_or_paren, atom)).boxed();

        let call_args = just(Token::LParen)
            .ignore_then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::RParen))
            .recover_with(via_parser(nested_delimiters(
                Token::LParen,
                Token::RParen,
                [
                    (Token::LBrace, Token::RBrace),
                    (Token::LBracket, Token::RBracket),
                ],
                |_| vec![],
            )));

        let index = just(Token::LBracket)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::RBracket));

        let field = just(Token::Dot).ignore_then(ident().labelled("field name"));
        let optional_chain = just(Token::OptionalChain).ignore_then(ident());
        let cast = just(Token::KeywordAs).ignore_then(type_annot());

        let postfix_op = choice((
            call_args.map(PostOp::Call),
            index.map(|idx| PostOp::Index(Box::new(idx))),
            optional_chain.map(PostOp::OptionalChain),
            field.map(PostOp::Field),
            cast.map(PostOp::Cast),
        ));

        let postfix = base.foldl(postfix_op.repeated(), |lhs, op| {
            let span = lhs.span.clone();
            Expr {
                span,
                file: String::new(),
                expr: match op {
                    PostOp::Call(args) => ExprKind::Call(Box::new(lhs), args),
                    PostOp::Index(idx) => ExprKind::Index(Box::new(lhs), idx),
                    PostOp::Field(f) => ExprKind::FieldAccess(Box::new(lhs), f),
                    PostOp::OptionalChain(f) => ExprKind::OptionalChain(Box::new(lhs), f),
                    PostOp::Cast(ty) => ExprKind::Cast {
                        expr: Box::new(lhs),
                        target_type: ty,
                    },
                },
            }
        });

        postfix
            .pratt((
                infix(
                    right(1),
                    just(Token::Pipe),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::Pipe, Box::new(r)),
                        }
                    },
                ),
                infix(
                    right(2),
                    just(Token::Assign),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::Assign {
                                l_val: Box::new(l),
                                r_val: Box::new(r),
                                op: AssignOp::Assign,
                            },
                        }
                    },
                ),
                infix(
                    right(2),
                    just(Token::AddAssign),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::Assign {
                                l_val: Box::new(l),
                                r_val: Box::new(r),
                                op: AssignOp::AddAssign,
                            },
                        }
                    },
                ),
                infix(
                    right(2),
                    just(Token::SubAssign),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::Assign {
                                l_val: Box::new(l),
                                r_val: Box::new(r),
                                op: AssignOp::SubAssign,
                            },
                        }
                    },
                ),
                infix(
                    right(2),
                    just(Token::MulAssign),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::Assign {
                                l_val: Box::new(l),
                                r_val: Box::new(r),
                                op: AssignOp::MulAssign,
                            },
                        }
                    },
                ),
                infix(
                    right(2),
                    just(Token::DivAssign),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::Assign {
                                l_val: Box::new(l),
                                r_val: Box::new(r),
                                op: AssignOp::DivAssign,
                            },
                        }
                    },
                ),
                infix(
                    right(2),
                    just(Token::ModAssign),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::Assign {
                                l_val: Box::new(l),
                                r_val: Box::new(r),
                                op: AssignOp::ModAssign,
                            },
                        }
                    },
                ),
                infix(left(3), just(Token::Or), |l: Expr, _op, r: Expr, _extra| {
                    let span = l.span.start..r.span.end;
                    Expr {
                        span,
                        file: String::new(),
                        expr: ExprKind::BinOp(Box::new(l), BinOp::Or, Box::new(r)),
                    }
                }),
                infix(
                    left(3),
                    just(Token::Nor),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::Nor, Box::new(r)),
                        }
                    },
                ),
                infix(
                    left(5),
                    just(Token::Xor),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::Xor, Box::new(r)),
                        }
                    },
                ),
                infix(
                    left(7),
                    just(Token::And),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::And, Box::new(r)),
                        }
                    },
                ),
                infix(left(9), just(Token::Eq), |l: Expr, _op, r: Expr, _extra| {
                    let span = l.span.start..r.span.end;
                    Expr {
                        span,
                        file: String::new(),
                        expr: ExprKind::BinOp(Box::new(l), BinOp::Eq, Box::new(r)),
                    }
                }),
                infix(
                    left(9),
                    just(Token::NotEq),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::NotEq, Box::new(r)),
                        }
                    },
                ),
                infix(
                    left(9),
                    just(Token::Less),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::Less, Box::new(r)),
                        }
                    },
                ),
                infix(
                    left(9),
                    just(Token::Greater),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::Greater, Box::new(r)),
                        }
                    },
                ),
                infix(
                    left(9),
                    just(Token::LessEq),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::LessEq, Box::new(r)),
                        }
                    },
                ),
                infix(
                    left(9),
                    just(Token::GreaterEq),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::GreaterEq, Box::new(r)),
                        }
                    },
                ),
                infix(
                    left(11),
                    just(Token::Plus),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::Add, Box::new(r)),
                        }
                    },
                ),
                infix(
                    left(11),
                    just(Token::Minus),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::Sub, Box::new(r)),
                        }
                    },
                ),
                infix(
                    left(13),
                    just(Token::Mul),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::Mul, Box::new(r)),
                        }
                    },
                ),
                infix(
                    left(13),
                    just(Token::Div),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::Div, Box::new(r)),
                        }
                    },
                ),
                infix(
                    left(13),
                    just(Token::Mod),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::Mod, Box::new(r)),
                        }
                    },
                ),
                infix(
                    right(15),
                    just(Token::Power),
                    |l: Expr, _op, r: Expr, _extra| {
                        let span = l.span.start..r.span.end;
                        Expr {
                            span,
                            file: String::new(),
                            expr: ExprKind::BinOp(Box::new(l), BinOp::Pow, Box::new(r)),
                        }
                    },
                ),
                prefix(17, just(Token::Minus), |_op, e: Expr, _extra| Expr {
                    span: e.span.clone(),
                    file: String::new(),
                    expr: ExprKind::UnOp(UnOp::Minus, Box::new(e)),
                }),
                prefix(17, just(Token::Not), |_op, e: Expr, _extra| Expr {
                    span: e.span.clone(),
                    file: String::new(),
                    expr: ExprKind::UnOp(UnOp::Not, Box::new(e)),
                }),
                prefix(17, just(Token::Plus), |_op, e: Expr, _extra| Expr {
                    span: e.span.clone(),
                    file: String::new(),
                    expr: ExprKind::UnOp(UnOp::Plus, Box::new(e)),
                }),
            ))
            .boxed()
    })
}

#[derive(Debug)]
enum PostOp {
    Call(Vec<Expr>),
    Index(Box<Expr>),
    Field(String),
    OptionalChain(String),
    Cast(TypeAnnot),
}

fn type_param<'src>()
-> impl Parser<'src, &'src [Token], TypeParam, extra::Err<Rich<'src, Token, Span>>> + Clone {
    ident().map(|name| TypeParam {
        name,
        kind: None,
        bounds: vec![],
    })
}

fn where_clause<'src>()
-> impl Parser<'src, &'src [Token], Vec<TypeConstraint>, extra::Err<Rich<'src, Token, Span>>> + Clone
{
    just(Token::KeywordWhere).ignore_then(
        type_annot()
            .then_ignore(just(Token::Tilde))
            .then(type_annot())
            .map_with(|(left, right), e| TypeConstraint {
                span: to_range(e.span()),
                left,
                right,
            })
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>(),
    )
}

// Add attribute parser
fn attribute<'src>()
-> impl Parser<'src, &'src [Token], Attribute, extra::Err<Rich<'src, Token, Span>>> + Clone {
    just(Token::At)
        .ignore_then(ident())
        .then(
            just(Token::LParen)
                .ignore_then(
                    choice((
                        ident().map(AttributeArg::Ident),
                        select! {
                            Token::Int(n) => AttributeArg::Literal(Literal::Int(n)),
                            Token::Float(f) => AttributeArg::Literal(Literal::Float(f)),
                            Token::String(s) => AttributeArg::Literal(Literal::String(s)),
                            Token::Bool(b) => AttributeArg::Literal(Literal::Bool(b)),
                        },
                    ))
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::RParen))
                .or_not(),
        )
        .map_with(|(name, args), e| Attribute {
            span: to_range(e.span()),
            name,
            args: args.unwrap_or_default(),
        })
        .labelled("attribute")
}

fn function<'src>()
-> impl Parser<'src, &'src [Token], Function, extra::Err<Rich<'src, Token, Span>>> + Clone {
    let vis = just(Token::KeywordPub).to(Visibility::Public).or_not();

    just(Token::KeywordDef)
        .ignore_then(vis)
        .then(ident().labelled("function name"))
        .then(
            just(Token::Less)
                .ignore_then(
                    type_param()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Greater))
                .or_not(),
        )
        .then(
            just(Token::LParen)
                .labelled("'(' for parameters")
                .ignore_then(
                    ident()
                        .then(just(Token::Colon).ignore_then(type_annot()).or_not())
                        .map_with(|(name, ty), e| FnArg {
                            span: to_range(e.span()),
                            file: String::new(),
                            name,
                            type_: ty,
                        })
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::RParen).labelled("')' for parameters"))
                .recover_with(via_parser(nested_delimiters(
                    Token::LParen,
                    Token::RParen,
                    [
                        (Token::LBrace, Token::RBrace),
                        (Token::LBracket, Token::RBracket),
                    ],
                    |_| vec![],
                ))),
        )
        .then(just(Token::Arrow).ignore_then(type_annot()).or_not())
        .then(
            select! { Token::Variable(s) if s == "effects" => () }
                .ignore_then(ident().separated_by(just(Token::Comma)).collect::<Vec<_>>())
                .map(EffectAnnot::closed)
                .or_not(),
        )
        .then(where_clause().or_not())
        .then(expr())
        .recover_with(skip_then_retry_until(
            any().ignored(),
            one_of([
                Token::KeywordDef,
                Token::KeywordStruct,
                Token::KeywordEnum,
                Token::KeywordType,
                Token::KeywordLet,
                Token::KeywordIf,
                Token::KeywordFor,
                Token::KeywordWhile,
                Token::KeywordMatch,
                Token::KeywordReturn,
                Token::KeywordImport,
                Token::KeywordImpl,
                Token::KeywordTrait,
                Token::KeywordEffect,
                Token::KeywordWith,
                Token::KeywordLoop,
                Token::KeywordHandle,
            ])
            .ignored(),
        ))
        .map_with(
            |(((((((vis, name), type_params), args), ret_type), effects), where_), body), e| {
                Function {
                    span: to_range(e.span()),
                    file: String::new(),
                    vis: vis.unwrap_or(Visibility::Private),
                    name,
                    type_params: type_params.unwrap_or_default(),
                    args,
                    return_type: ret_type,
                    where_constraints: where_.unwrap_or_default(),
                    effects: effects.unwrap_or_else(EffectAnnot::pure),
                    body: Some(body),
                }
            },
        )
        .labelled("function")
}

fn struct_def<'src>()
-> impl Parser<'src, &'src [Token], Struct, extra::Err<Rich<'src, Token, Span>>> + Clone {
    let vis = just(Token::KeywordPub).to(Visibility::Public).or_not();

    // Field: (pub)? name: Type
    let field = just(Token::KeywordPub)
        .to(Visibility::Public)
        .or_not()
        .then(ident())
        .then_ignore(just(Token::Colon).labelled("':'"))
        .then(type_annot())
        .map_with(|((field_vis, name), ty), e| StructMember::Field {
            vis: field_vis.unwrap_or(Visibility::Private),
            arg: FnArg {
                span: to_range(e.span()),
                file: String::new(),
                name,
                type_: Some(ty),
            },
        });

    // Method: def name(...) -> Type { body }
    let method = function().map(|f| StructMember::Method(Box::new(f)));

    // Member: either field or method, but method is clearly indicated by "def"
    let member = choice((
        // Try method first if we see "def"
        just(Token::KeywordDef).rewind().ignore_then(method),
        // Otherwise try field
        field,
    ));

    just(Token::KeywordStruct)
        .ignore_then(vis)
        .then(ident().labelled("struct name"))
        .then(
            just(Token::Less)
                .ignore_then(
                    type_param()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Greater))
                .or_not(),
        )
        .then(
            just(Token::LBrace)
                .labelled("'{' for struct body")
                .ignore_then(
                    member
                        .separated_by(just(Token::Comma).or(just(Token::Semicolon)))
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::RBrace).labelled("'}' for struct body")),
        )
        .map_with(|(((vis, name), type_params), members), e| {
            let mut fields = vec![];
            let mut methods = vec![];

            for member in members {
                match member {
                    StructMember::Field { vis, arg } => fields.push((arg, vis)),
                    StructMember::Method(func) => methods.push(*func),
                }
            }

            Struct {
                span: to_range(e.span()),
                file: String::new(),
                name,
                type_params: type_params.unwrap_or_default(),
                fields,
                methods,
                vis: vis.unwrap_or(Visibility::Private),
            }
        })
        .labelled("struct definition")
}

#[derive(Debug)]
enum StructMember {
    Field { vis: Visibility, arg: FnArg },
    Method(Box<Function>),
}

fn enum_def<'src>()
-> impl Parser<'src, &'src [Token], Enum, extra::Err<Rich<'src, Token, Span>>> + Clone {
    let vis = just(Token::KeywordPub).to(Visibility::Public).or_not();

    let variant = ident()
        .then(
            just(Token::LParen)
                .ignore_then(
                    type_annot()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::RParen))
                .or_not(),
        )
        .then(where_clause().or_not())
        .map_with(|((name, types), constraints), e| EnumVariant {
            span: to_range(e.span()),
            file: String::new(),
            name,
            types: types.unwrap_or_default(),
            constraints: constraints.unwrap_or_default(),
        });

    let method = function();

    just(Token::KeywordEnum)
        .ignore_then(vis)
        .then(ident().labelled("enum name"))
        .then(
            just(Token::Less)
                .ignore_then(
                    type_param()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Greater))
                .or_not(),
        )
        .then(
            just(Token::LBrace)
                .labelled("'{' for enum body")
                .ignore_then(
                    choice((
                        variant.map(EnumMember::Variant),
                        method.map(|f| EnumMember::Method(Box::new(f))),
                    ))
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::RBrace).labelled("'}' for enum body"))
                .recover_with(via_parser(nested_delimiters(
                    Token::LBrace,
                    Token::RBrace,
                    [
                        (Token::LParen, Token::RParen),
                        (Token::LBracket, Token::RBracket),
                    ],
                    |_| vec![],
                ))),
        )
        .map_with(|(((vis, name), type_params), members), e| {
            let mut variants = vec![];
            let mut methods = vec![];

            for member in members {
                match member {
                    EnumMember::Variant(v) => variants.push(v),
                    EnumMember::Method(m) => methods.push(*m),
                }
            }

            Enum {
                span: to_range(e.span()),
                file: String::new(),
                name,
                type_params: type_params.unwrap_or_default(),
                variants,
                methods,
                vis: vis.unwrap_or(Visibility::Private),
            }
        })
        .labelled("enum")
}

#[derive(Debug)]
enum EnumMember {
    Variant(EnumVariant),
    Method(Box<Function>),
}

fn type_alias<'src>()
-> impl Parser<'src, &'src [Token], TypeAlias, extra::Err<Rich<'src, Token, Span>>> + Clone {
    just(Token::KeywordType)
        .ignore_then(ident().labelled("type name"))
        .then(
            just(Token::Less)
                .ignore_then(
                    type_param()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Greater))
                .or_not(),
        )
        .then(where_clause().or_not())
        .then_ignore(just(Token::Assign).labelled("'='"))
        .then(type_annot().labelled("type definition"))
        .map_with(
            |(((name, type_params), where_), target_type), e| TypeAlias {
                span: to_range(e.span()),
                file: String::new(),
                name,
                type_params: type_params.unwrap_or_default(),
                target_type,
                where_constraints: where_.unwrap_or_default(),
            },
        )
        .labelled("type alias")
}

fn impl_block<'src>()
-> impl Parser<'src, &'src [Token], Impl, extra::Err<Rich<'src, Token, Span>>> + Clone {
    just(Token::KeywordImpl)
        .ignore_then(
            // Type params: impl<T>
            just(Token::Less)
                .ignore_then(
                    type_param()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Greater))
                .or_not(),
        )
        .then(ident().labelled("type name")) // Type name FIRST
        .then(
            // Generic args after type: User<T>
            just(Token::Less)
                .ignore_then(
                    type_annot()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Greater))
                .or_not(),
        )
        .then(
            // Optional trait: : TraitName
            just(Token::Colon)
                .ignore_then(ident().labelled("trait name"))
                .or_not(),
        )
        .then(where_clause().or_not())
        .then(
            just(Token::LBrace)
                .labelled("'{' for impl body")
                .ignore_then(function_in_impl().repeated().collect::<Vec<_>>())
                .then_ignore(just(Token::RBrace).labelled("'}' for impl body"))
                .recover_with(via_parser(nested_delimiters(
                    Token::LBrace,
                    Token::RBrace,
                    [
                        (Token::LParen, Token::RParen),
                        (Token::LBracket, Token::RBracket),
                    ],
                    |_| vec![],
                ))),
        )
        .map_with(
            |(((((type_params, type_name), _generic_args), trait_), where_), methods), e| Impl {
                span: to_range(e.span()),
                file: String::new(),
                type_name,
                type_params: type_params.unwrap_or_default(),
                methods,
                trait_,
                where_constraints: where_.unwrap_or_default(),
            },
        )
        .labelled("impl block")
}

fn function_in_impl<'src>()
-> impl Parser<'src, &'src [Token], Function, extra::Err<Rich<'src, Token, Span>>> + Clone {
    let vis = just(Token::KeywordPub).to(Visibility::Public).or_not();

    just(Token::KeywordDef)
        .ignore_then(vis)
        .then(ident().labelled("function name"))
        .then(
            just(Token::Less)
                .ignore_then(
                    type_param()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Greater))
                .or_not(),
        )
        .then(
            just(Token::LParen)
                .labelled("'(' for parameters")
                .ignore_then(
                    ident()
                        .then(just(Token::Colon).ignore_then(type_annot()).or_not())
                        .map_with(|(name, ty), e| FnArg {
                            span: to_range(e.span()),
                            file: String::new(),
                            name,
                            type_: ty,
                        })
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::RParen).labelled("')' for parameters"))
                .recover_with(via_parser(nested_delimiters(
                    Token::LParen,
                    Token::RParen,
                    [
                        (Token::LBrace, Token::RBrace),
                        (Token::LBracket, Token::RBracket),
                    ],
                    |_| vec![],
                ))),
        )
        .then(just(Token::Arrow).ignore_then(type_annot()).or_not())
        .then(
            select! { Token::Variable(s) if s == "effects" => () }
                .ignore_then(ident().separated_by(just(Token::Comma)).collect::<Vec<_>>())
                .map(EffectAnnot::closed)
                .or_not(),
        )
        .then(where_clause().or_not())
        .then(
            // CRITICAL: Parse body as a simple block, not full expr()
            just(Token::LBrace)
                .ignore_then(
                    expr()
                        .then_ignore(just(Token::Semicolon).or_not())
                        .repeated()
                        .at_least(1)
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::RBrace))
                .map_with(|exprs, e| Expr {
                    span: to_range(e.span()),
                    file: String::new(),
                    expr: if exprs.len() == 1 {
                        exprs.into_iter().next().unwrap().expr
                    } else {
                        ExprKind::Block(exprs)
                    },
                }),
        )
        .map_with(
            |(((((((vis, name), type_params), args), ret_type), effects), where_), body), e| {
                Function {
                    span: to_range(e.span()),
                    file: String::new(),
                    vis: vis.unwrap_or(Visibility::Private),
                    name,
                    type_params: type_params.unwrap_or_default(),
                    args,
                    return_type: ret_type,
                    where_constraints: where_.unwrap_or_default(),
                    effects: effects.unwrap_or_else(EffectAnnot::pure),
                    body: Some(body),
                }
            },
        )
        .labelled("method")
}

fn function_decl<'src>()
-> impl Parser<'src, &'src [Token], Function, extra::Err<Rich<'src, Token, Span>>> + Clone {
    let vis = just(Token::KeywordPub).to(Visibility::Public).or_not();

    just(Token::KeywordDef)
        .ignore_then(vis)
        .then(ident())
        .then(
            just(Token::Less)
                .ignore_then(
                    type_param()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Greater))
                .or_not(),
        )
        .then(
            just(Token::LParen)
                .ignore_then(
                    ident()
                        .then(just(Token::Colon).ignore_then(type_annot()).or_not())
                        .map_with(|(name, ty), e| FnArg {
                            span: to_range(e.span()),
                            file: String::new(),
                            name,
                            type_: ty,
                        })
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::RParen)),
        )
        .then(just(Token::Arrow).ignore_then(type_annot()).or_not())
        .then(
            select! { Token::Variable(s) if s == "effects" => () }
                .ignore_then(ident().separated_by(just(Token::Comma)).collect::<Vec<_>>())
                .map(EffectAnnot::closed)
                .or_not(),
        )
        .then(where_clause().or_not())
        .map_with(
            |((((((vis, name), type_params), args), ret_type), effects), where_), e| Function {
                span: to_range(e.span()),
                file: String::new(),
                vis: vis.unwrap_or(Visibility::Private),
                name,
                type_params: type_params.unwrap_or_default(),
                args,
                return_type: ret_type,
                where_constraints: where_.unwrap_or_default(),
                effects: effects.unwrap_or_else(EffectAnnot::pure),
                body: None,
            },
        )
}

fn trait_def<'src>()
-> impl Parser<'src, &'src [Token], TraitDef, extra::Err<Rich<'src, Token, Span>>> + Clone {
    just(Token::KeywordTrait)
        .ignore_then(ident().labelled("trait name"))
        .then(
            just(Token::Less)
                .ignore_then(
                    type_param()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Greater))
                .or_not(),
        )
        .then(
            just(Token::LBrace)
                .labelled("'{' for trait body")
                .ignore_then(function_decl().repeated().collect::<Vec<_>>())
                .then_ignore(just(Token::RBrace).labelled("'}' for trait body")),
        )
        .map_with(|((name, type_params), methods), e| TraitDef {
            span: to_range(e.span()),
            name,
            type_params: type_params.unwrap_or_default(),
            super_traits: vec![],
            methods,
            associated_types: vec![],
        })
        .labelled("trait")
}

// Fix effect_def to not require "def" keyword
fn effect_def<'src>()
-> impl Parser<'src, &'src [Token], EffectDef, extra::Err<Rich<'src, Token, Span>>> + Clone {
    let vis = just(Token::KeywordPub).to(Visibility::Public).or_not();

    // Effect operation WITHOUT "def" keyword: log(string) -> ()
    let operation = ident()
        .then(
            just(Token::LParen)
                .ignore_then(
                    type_annot()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::RParen)),
        )
        .then_ignore(just(Token::Arrow))
        .then(type_annot())
        .map_with(|((name, params), return_type), e| EffectOperation {
            span: to_range(e.span()),
            name,
            params,
            return_type,
        });

    just(Token::KeywordEffect)
        .ignore_then(vis)
        .then(ident().labelled("effect name"))
        .then(
            just(Token::Less)
                .ignore_then(
                    type_param()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Greater))
                .or_not(),
        )
        .then(where_clause().or_not())
        .then(
            just(Token::LBrace)
                .labelled("'{' for effect body")
                .ignore_then(
                    operation
                        .separated_by(just(Token::Semicolon).or(just(Token::Comma)))
                        .allow_trailing()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::RBrace).labelled("'}' for effect body"))
                .recover_with(via_parser(nested_delimiters(
                    Token::LBrace,
                    Token::RBrace,
                    [
                        (Token::LParen, Token::RParen),
                        (Token::LBracket, Token::RBracket),
                    ],
                    |_| vec![],
                ))),
        )
        .map_with(
            |((((vis, name), type_params), where_), operations), e| EffectDef {
                span: to_range(e.span()),
                file: String::new(),
                vis: vis.unwrap_or(Visibility::Private),
                name,
                type_params: type_params.unwrap_or_default(),
                operations,
                where_constraints: where_.unwrap_or_default(),
            },
        )
        .labelled("effect")
}

pub fn parser<'src>()
-> impl Parser<'src, &'src [Token], Vec<ASTNode>, extra::Err<Rich<'src, Token, Span>>> {
    // Parse attributes (can appear before any item)
    let attributes = attribute().repeated().collect::<Vec<_>>();

    let item = attributes
        .then(choice((
            expr().map(ASTNodeKind::Expr),
            struct_def().map(ASTNodeKind::Struct),
            enum_def().map(ASTNodeKind::Enum),
            type_alias().map(ASTNodeKind::TypeAlias),
            impl_block().map(ASTNodeKind::Impl),
            trait_def().map(ASTNodeKind::Trait),
            effect_def().map(ASTNodeKind::EffectDef),
            function().map(|f| ASTNodeKind::Function(Box::new(f))),
        )))
        .map_with(|(attrs, node), e| ASTNode {
            span: to_range(e.span()),
            file: String::new(),
            node,
            attributes: attrs,
        })
        .recover_with(skip_then_retry_until(
            any().ignored(),
            choice((
                just(Token::At).ignored(), // For next attribute
                just(Token::KeywordDef).ignored(),
                just(Token::KeywordStruct).ignored(),
                just(Token::KeywordEnum).ignored(),
                just(Token::KeywordType).ignored(),
                just(Token::KeywordImpl).ignored(),
                just(Token::KeywordTrait).ignored(),
                just(Token::KeywordEffect).ignored(),
                just(Token::KeywordLet).ignored(),
                end().ignored(),
            )),
        ))
        .labelled("top-level declaration");

    item.repeated().collect::<Vec<_>>().then_ignore(end())
}
