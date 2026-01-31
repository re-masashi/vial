// src/parser.rs
use crate::ast::*;
use crate::lexer::Token;
use crate::meta::{AttrLiteral, Attribute, AttributeArg, Location, Meta};
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub location: Location,
}

impl ParseError {
    fn new(message: String, location: Location) -> Self {
        Self { message, location }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser {
    tokens: Vec<(Token, Range<usize>)>,
    current: usize,
    file: String,
}

impl Parser {
    pub fn new(tokens: Vec<(Token, Range<usize>)>, file: String) -> Self {
        Self {
            tokens,
            current: 0,
            file,
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current).map(|(t, _)| t)
    }

    fn peek_nth(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.current + n).map(|(t, _)| t)
    }

    fn advance(&mut self) -> Option<Token> {
        if self.current < self.tokens.len() {
            let token = self.tokens[self.current].0.clone();
            self.current += 1;
            Some(token)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: Token) -> ParseResult<()> {
        let current_span = self.current_span();
        match self.peek() {
            Some(token) if std::mem::discriminant(token) == std::mem::discriminant(&expected) => {
                self.advance();
                Ok(())
            }
            Some(token) => Err(ParseError::new(
                format!("Expected {:?}, found {:?}", expected, token),
                Location {
                    span: current_span,
                    file: self.file.clone(),
                },
            )),
            None => Err(ParseError::new(
                format!("Expected {:?}, found EOF", expected),
                Location {
                    span: current_span,
                    file: self.file.clone(),
                },
            )),
        }
    }

    fn check(&self, token: &Token) -> bool {
        if let Some(current) = self.peek() {
            std::mem::discriminant(current) == std::mem::discriminant(token)
        } else {
            false
        }
    }

    fn match_token(&mut self, token: Token) -> bool {
        if self.check(&token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn current_span(&self) -> Range<usize> {
        if self.current < self.tokens.len() {
            self.tokens[self.current].1.clone()
        } else if self.current > 0 {
            let last = &self.tokens[self.current - 1].1;
            last.end..last.end
        } else {
            0..0
        }
    }

    fn span_from(&self, start: usize) -> Range<usize> {
        let end = if self.current > 0 && self.current <= self.tokens.len() {
            self.tokens[self.current - 1].1.end
        } else if self.current < self.tokens.len() {
            self.tokens[self.current].1.start
        } else {
            start
        };
        start..end
    }

    fn make_meta(&self, span: Range<usize>, attributes: Vec<Attribute>) -> Meta {
        Meta {
            location: Location {
                span,
                file: self.file.clone(),
            },
            attributes,
        }
    }

    fn parse_attributes(&mut self) -> ParseResult<Vec<Attribute>> {
        let mut attributes = Vec::new();
        while self.check(&Token::At) {
            self.advance(); // consume @
            let name = self.parse_identifier()?;

            let args = if self.match_token(Token::LParen) {
                let mut args = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        args.push(self.parse_attribute_arg()?);
                        if !self.match_token(Token::Comma) {
                            break;
                        }
                    }
                }
                self.expect(Token::RParen)?;
                args
            } else {
                Vec::new()
            };

            attributes.push(Attribute { name, args });
        }
        Ok(attributes)
    }

    fn parse_attribute_arg(&mut self) -> ParseResult<AttributeArg> {
        if let Some(Token::Identifier(key)) = self.peek() {
            let key_clone = key.clone();
            let saved = self.current;
            self.advance();

            if self.match_token(Token::Eq) {
                // It's a key-value pair
                let value = self.parse_attribute_value()?;
                return Ok(AttributeArg::KeyValue {
                    key: key_clone,
                    value,
                });
            } else {
                // Not a key-value, restore and parse as identifier
                self.current = saved;
                let id = self.parse_identifier()?;
                return Ok(AttributeArg::Identifier(id));
            }
        }

        // Parse literal
        match self.peek() {
            Some(Token::Integer(i)) => {
                let val = *i;
                self.advance();
                Ok(AttributeArg::Literal(AttrLiteral::Integer(val)))
            }
            Some(Token::Float(f)) => {
                let val = *f;
                self.advance();
                Ok(AttributeArg::Literal(AttrLiteral::Float(val)))
            }
            Some(Token::String(s)) => {
                let val = s.clone();
                self.advance();
                Ok(AttributeArg::Literal(AttrLiteral::String(val)))
            }
            Some(Token::True) => {
                self.advance();
                Ok(AttributeArg::Literal(AttrLiteral::Boolean(true)))
            }
            Some(Token::False) => {
                self.advance();
                Ok(AttributeArg::Literal(AttrLiteral::Boolean(false)))
            }
            _ => Err(ParseError::new(
                "Expected attribute argument".to_string(),
                Location {
                    span: self.current_span(),
                    file: self.file.clone(),
                },
            )),
        }
    }

    fn parse_attribute_value(&mut self) -> ParseResult<String> {
        match self.peek() {
            Some(Token::Identifier(id)) => {
                let val = id.clone();
                self.advance();
                Ok(val)
            }
            Some(Token::Integer(i)) => {
                let val = i.to_string();
                self.advance();
                Ok(val)
            }
            Some(Token::Float(f)) => {
                let val = f.to_string();
                self.advance();
                Ok(val)
            }
            Some(Token::String(s)) => {
                let val = s.clone();
                self.advance();
                Ok(val)
            }
            Some(Token::True) => {
                self.advance();
                Ok("true".to_string())
            }
            Some(Token::False) => {
                self.advance();
                Ok("false".to_string())
            }
            _ => Err(ParseError::new(
                "Expected attribute value".to_string(),
                Location {
                    span: self.current_span(),
                    file: self.file.clone(),
                },
            )),
        }
    }

    fn parse_identifier(&mut self) -> ParseResult<String> {
        match self.advance() {
            Some(Token::Identifier(id)) => Ok(id),
            x => {
                println!("{:?}", x);
                Err(ParseError::new(
                    format!("Expected identifier. found {:?} ", x).to_string(),
                    Location {
                        span: self.current_span(),
                        file: self.file.clone(),
                    },
                ))
            }
        }
    }

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut items = Vec::new();
        while self.peek().is_some() {
            items.push(self.parse_item()?);
        }
        Ok(items)
    }

    fn parse_item(&mut self) -> ParseResult<Node<Item>> {
        let start = self.current_span().start;
        let attributes = self.parse_attributes()?;

        let item = match self.peek() {
            Some(Token::Identifier(_)) => {
                // self.advance();
                Item::Function(Box::new(self.parse_function()?))
            }
            Some(Token::Extern) => {
                self.advance();
                Item::ExternFunction(self.parse_extern_function()?)
            }
            Some(Token::Type) => {
                self.advance();
                Item::TypeAlias(self.parse_type_alias()?)
            }
            Some(Token::Use) => {
                self.advance();
                Item::Use(self.parse_use()?)
            }
            Some(Token::Trait) => {
                self.advance();
                Item::Trait(self.parse_trait()?)
            }
            Some(Token::Impl) => {
                self.advance();
                Item::Impl(self.parse_impl()?)
            }
            _ => {
                return Err(ParseError::new(
                    format!("Expected top-level item, found {:?}", self.peek()),
                    Location {
                        span: self.current_span(),
                        file: self.file.clone(),
                    },
                ));
            }
        };

        let span = self.span_from(start);
        Ok(Node {
            data: item,
            meta: self.make_meta(span, attributes),
        })
    }

    fn parse_function(&mut self) -> ParseResult<Function> {
        let name = self.parse_identifier()?;

        let generics = if self.match_token(Token::LBracket) {
            self.parse_generic_params()?
        } else {
            Vec::new()
        };

        let params = self.parse_function_params()?;

        let return_type = if self.match_token(Token::Arrow) {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        self.expect(Token::Eq)?;
        let body = self.parse_expr()?;

        Ok(Function {
            name,
            generics,
            params,
            return_type,
            body,
        })
    }

    fn parse_extern_function(&mut self) -> ParseResult<ExternFunction> {
        let name = self.parse_identifier()?;
        self.expect(Token::LParen)?;

        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                params.push(self.parse_type_annotation()?);
                if !self.match_token(Token::Comma) {
                    break;
                }
            }
        }
        self.expect(Token::RParen)?;

        self.expect(Token::Arrow)?;
        let return_type = self.parse_type_annotation()?;

        Ok(ExternFunction {
            name,
            params,
            return_type,
        })
    }

    fn parse_type_alias(&mut self) -> ParseResult<TypeAlias> {
        let name = self.parse_identifier()?;

        let generics = if self.match_token(Token::LBracket) {
            self.parse_generic_params()?
        } else {
            Vec::new()
        };

        self.expect(Token::Eq)?;
        let definition = self.parse_type_annotation()?;

        Ok(TypeAlias {
            name,
            generics,
            definition,
        })
    }

    fn parse_use(&mut self) -> ParseResult<Use> {
        let path = match self.advance() {
            Some(Token::String(s)) => s,
            _ => {
                return Err(ParseError::new(
                    "Expected string path in use statement".to_string(),
                    Location {
                        span: self.current_span(),
                        file: self.file.clone(),
                    },
                ));
            }
        };

        let imports = if self.match_token(Token::LParen) {
            let mut items = Vec::new();
            if !self.check(&Token::RParen) {
                loop {
                    let name = self.parse_identifier()?;
                    let alias = if self.match_token(Token::As) {
                        Some(self.parse_identifier()?)
                    } else {
                        None
                    };
                    items.push(ImportItem { name, alias });
                    if !self.match_token(Token::Comma) {
                        break;
                    }
                }
            }
            self.expect(Token::RParen)?;
            Some(items)
        } else {
            None
        };

        Ok(Use { path, imports })
    }

    fn parse_trait(&mut self) -> ParseResult<Trait> {
        let name = self.parse_identifier()?;

        let generics = if self.match_token(Token::LBracket) {
            self.parse_generic_params()?
        } else {
            Vec::new()
        };

        print!("1 ");

        // Parse supertrait bounds (inherits)
        let mut inherits = Vec::new();
        if self.match_token(Token::Colon) {
            loop {
                inherits.push(self.parse_type_annotation()?);
                if !self.match_token(Token::Plus) {
                    break;
                }
            }
        }

        print!("2 ");
        self.expect(Token::LBrace)?;
        let mut items = Vec::new();

        print!("3 ");
        while !self.check(&Token::RBrace) {
            let start = self.current_span().start;
            let item_attrs = self.parse_attributes()?;

            print!("4 ");

            let item = if self.match_token(Token::Type) {
                TraitItem::TypeAlias(self.parse_type_alias()?)
            } else if matches!(self.peek(), Some(&Token::Identifier(_))) {
                print!("4.5 ");

                TraitItem::FunctionSignature(self.parse_function_signature()?)
            } else {
                return Err(ParseError::new(
                    "Expected type alias or function signature in trait".to_string(),
                    Location {
                        span: self.current_span(),
                        file: self.file.clone(),
                    },
                ));
            };

            print!("5 ");

            let span = self.span_from(start);
            items.push(Node {
                data: item,
                meta: self.make_meta(span, item_attrs),
            });

            // Optional comma
            self.match_token(Token::Comma);
        }

        self.expect(Token::RBrace)?;

        Ok(Trait {
            name,
            generics,
            inherits,
            items,
        })
    }

    fn parse_function_signature(&mut self) -> ParseResult<FunctionSignature> {
        let name = self.parse_identifier()?;

        print!("pfs 1 ");

        let generics = if self.match_token(Token::LBracket) {
            self.parse_generic_params()?
        } else {
            Vec::new()
        };

        print!("pfs 2 ");

        let params = self.parse_function_params()?;

        print!("pfs 2.5 ");

        let return_type = if self.match_token(Token::Arrow) {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        print!("pfs 3 ");

        Ok(FunctionSignature {
            name,
            generics,
            params,
            return_type,
        })
    }

    fn parse_impl(&mut self) -> ParseResult<Impl> {
        // Check if this is a trait impl or just a type impl
        let mut trait_name = None;
        let target_type;

        let first_id = self.parse_identifier()?;

        if self.match_token(Token::For) {
            // It's a trait impl: impl TraitName for TypeName
            trait_name = Some(first_id);
            target_type = self.parse_identifier()?;
        } else {
            // It's a type impl: impl TypeName
            target_type = first_id;
        }

        let generics = if self.match_token(Token::LBracket) {
            self.parse_generic_params()?
        } else {
            Vec::new()
        };

        self.expect(Token::LBrace)?;
        let mut items = Vec::new();

        while !self.check(&Token::RBrace) {
            let start = self.current_span().start;
            let item_attrs = self.parse_attributes()?;

            let item = if self.match_token(Token::Type) {
                ImplItem::TypeAlias(self.parse_type_alias()?)
            } else if matches!(self.peek(), Some(&Token::Identifier(_))) {
                ImplItem::Function(Box::new(self.parse_function()?))
            } else {
                return Err(ParseError::new(
                    "Expected type alias or function in impl".to_string(),
                    Location {
                        span: self.current_span(),
                        file: self.file.clone(),
                    },
                ));
            };

            let span = self.span_from(start);
            items.push(Node {
                data: item,
                meta: self.make_meta(span, item_attrs),
            });

            // Optional comma
            self.match_token(Token::Comma);
        }

        self.expect(Token::RBrace)?;

        Ok(Impl {
            trait_name,
            target_type,
            generics,
            items,
        })
    }

    fn parse_generic_params(&mut self) -> ParseResult<Vec<GenericParam>> {
        let mut params = Vec::new();

        if !self.check(&Token::RBracket) {
            loop {
                let name = self.parse_identifier()?;

                // Parse kind annotation and trait constraints
                let (kind, trait_constraints) = if self.match_token(Token::Colon) {
                    // Check the next token to decide between kind annotation and trait constraint
                    if self.check(&Token::Star) {
                        // This is a kind annotation like * or *->*
                        let parsed_kind = self.parse_kind_annotation()?;

                        // Check if there are trait constraints after the kind annotation using + syntax
                        let trait_constraints = if self.match_token(Token::Plus) {
                            let mut constraints = vec![self.parse_type_annotation()?];
                            while self.match_token(Token::Plus) {
                                constraints.push(self.parse_type_annotation()?);
                            }
                            constraints
                        } else {
                            Vec::new()
                        };
                        (parsed_kind, trait_constraints)
                    } else {
                        // It's a trait constraint like Functor, MyTrait, etc.
                        // Parse it as a type annotation (can be simple or complex)
                        let trait_constraint = self.parse_type_annotation()?;
                        let mut trait_constraints = vec![trait_constraint];

                        // Parse additional trait constraints with +
                        while self.match_token(Token::Plus) {
                            trait_constraints.push(self.parse_type_annotation()?);
                        }

                        (KindAnnot::Star, trait_constraints) // Use Star for trait constraints
                    }
                } else {
                    // No colon, default to Star kind with no constraints
                    (KindAnnot::Star, Vec::new())
                };

                params.push(GenericParam {
                    name,
                    kind,
                    bounds: Vec::new(), // Initialize bounds as empty
                    trait_constraints,
                });

                if !self.match_token(Token::Comma) {
                    break;
                }
            }
        }

        self.expect(Token::RBracket)?;
        Ok(params)
    }

    fn parse_kind_annotation(&mut self) -> ParseResult<KindAnnot> {
        if self.match_token(Token::Star) {
            // Check for arrow
            if self.match_token(Token::Arrow) {
                let right = self.parse_kind_annotation()?;
                Ok(KindAnnot::Arrow(Box::new(KindAnnot::Star), Box::new(right)))
            } else {
                Ok(KindAnnot::Star)
            }
        } else if self.match_token(Token::LParen) {
            let left = self.parse_kind_annotation()?;
            self.expect(Token::RParen)?;

            if self.match_token(Token::Arrow) {
                let right = self.parse_kind_annotation()?;
                Ok(KindAnnot::Arrow(Box::new(left), Box::new(right)))
            } else {
                Ok(left)
            }
        } else {
            Ok(KindAnnot::Star)
        }
    }

    fn parse_function_params(&mut self) -> ParseResult<Vec<FunctionArg>> {
        let mut params = Vec::new();

        print!("pfp 1 ");

        // Handle self parameter
        if self.match_token(Token::SelfValue) {
            params.push(FunctionArg {
                name: "self".to_string(),
                type_ann: None,
            });

            if self.match_token(Token::Comma)
                && !self.check(&Token::Eq)
                && !self.check(&Token::Arrow)
            {
                // Continue parsing other parameters
            } else {
                return Ok(params);
            }
        }

        print!("pfp 2 ");
        // Parse regular parameters
        loop {
            // Check if we're at the end (= or ->)
            print!("pfp 3 ");
            if self.check(&Token::Eq) || self.check(&Token::Arrow) {
                break;
            }

            print!("pfp 4 ");

            let name = self.parse_identifier()?;
            print!("pfp 5 ");

            let type_ann = if self.match_token(Token::Colon) {
                Some(self.parse_type_annotation()?)
            } else {
                None
            };

            print!("pfp 6 ");
            params.push(FunctionArg { name, type_ann });

            if !self.match_token(Token::Comma) {
                break;
            }

            // Check again after comma
            if self.check(&Token::Eq) || self.check(&Token::Arrow) {
                break;
            }
        }

        Ok(params)
    }

    fn parse_type_annotation(&mut self) -> ParseResult<Node<TypeAnn>> {
        let start = self.current_span().start;

        // Check for enum type (variant1 | variant2 | ...)
        let first = self.parse_type_annotation_primary()?;

        if self.match_token(Token::Pipe) {
            // This is an enum type
            let mut variants = vec![self.type_ann_to_enum_variant(&first)?];

            loop {
                let variant_node = self.parse_type_annotation_primary()?;
                variants.push(self.type_ann_to_enum_variant(&variant_node)?);

                if !self.match_token(Token::Pipe) {
                    break;
                }
            }

            let span = self.span_from(start);
            return Ok(Node {
                data: TypeAnn::Enum(variants),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        Ok(first)
    }

    fn type_ann_to_enum_variant(&self, node: &Node<TypeAnn>) -> ParseResult<EnumVariant> {
        match &node.data {
            TypeAnn::Primary(name, args) if args.is_empty() => Ok(EnumVariant::Unit(name.clone())),
            TypeAnn::Primary(name, args) => {
                // Could be a tuple variant with type arguments
                // If args contains TypeAnn::Struct, it's a struct variant
                // Otherwise it's a tuple variant

                // Check if any of the args contains a struct type
                if args
                    .iter()
                    .any(|arg| matches!(arg.data, TypeAnn::Struct(_)))
                {
                    // This is a struct variant
                    if args.len() == 1 {
                        if let TypeAnn::Struct(fields) = &args[0].data {
                            Ok(EnumVariant::Struct(name.clone(), fields.clone()))
                        } else {
                            Err(ParseError::new(
                                "Invalid enum variant syntax".to_string(),
                                Location {
                                    span: node.meta.location.span.clone(),
                                    file: self.file.clone(),
                                },
                            ))
                        }
                    } else {
                        Err(ParseError::new(
                            "Invalid enum variant syntax".to_string(),
                            Location {
                                span: node.meta.location.span.clone(),
                                file: self.file.clone(),
                            },
                        ))
                    }
                } else {
                    // Tuple variant with type arguments
                    Ok(EnumVariant::Tuple(name.clone(), args.clone()))
                }
            }
            TypeAnn::Struct(fields) => {
                // For struct variants, we need a name
                Ok(EnumVariant::Struct("Anonymous".to_string(), fields.clone())) // This case shouldn't happen with updated logic
            }
            _ => Err(ParseError::new(
                "Invalid enum variant syntax".to_string(),
                Location {
                    span: node.meta.location.span.clone(),
                    file: self.file.clone(),
                },
            )),
        }
    }

    fn parse_type_annotation_primary(&mut self) -> ParseResult<Node<TypeAnn>> {
        let start = self.current_span().start;

        if self.match_token(Token::Fn) {
            self.expect(Token::LParen)?;
            let mut params = Vec::new();

            if !self.check(&Token::RParen) {
                loop {
                    params.push(self.parse_type_annotation()?);
                    if !self.match_token(Token::Comma) {
                        break;
                    }
                }
            }

            self.expect(Token::RParen)?;
            self.expect(Token::Arrow)?;
            let return_type = self.parse_type_annotation()?;

            let span = self.span_from(start);
            return Ok(Node {
                data: TypeAnn::Function {
                    params,
                    return_type: Box::new(return_type),
                },
                meta: self.make_meta(span, Vec::new()),
            });
        }

        if self.match_token(Token::Trait) {
            let mut bounds = Vec::new();
            loop {
                bounds.push(self.parse_type_annotation_atom()?);
                if !self.match_token(Token::Plus) {
                    break;
                }
            }

            let span = self.span_from(start);
            return Ok(Node {
                data: TypeAnn::TraitBound(bounds),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        if self.check(&Token::LBrace) {
            return self.parse_struct_type();
        }

        if self.check(&Token::LParen) {
            let saved = self.current;
            self.advance(); // consume (

            // Check for empty tuple (unit type)
            if self.match_token(Token::RParen) {
                let span = self.span_from(start);
                return Ok(Node {
                    data: TypeAnn::Tuple(Vec::new()),
                    meta: self.make_meta(span, Vec::new()),
                });
            }

            // Try to parse as tuple
            let mut types = Vec::new();
            loop {
                types.push(self.parse_type_annotation()?);
                if !self.match_token(Token::Comma) {
                    break;
                }
                // Allow trailing comma
                if self.check(&Token::RParen) {
                    break;
                }
            }

            if self.match_token(Token::RParen) {
                let span = self.span_from(start);
                return Ok(Node {
                    data: TypeAnn::Tuple(types),
                    meta: self.make_meta(span, Vec::new()),
                });
            }

            // Not a valid tuple, restore
            self.current = saved;
        }

        if let Some(Token::Identifier(name)) = self.peek() {
            let name = name.clone();
            let saved = self.current;
            self.advance();

            if self.check(&Token::LBrace) {
                // This is a struct variant
                self.advance(); // consume {
                let mut fields = Vec::new();

                if !self.check(&Token::RBrace) {
                    loop {
                        let field_name = self.parse_identifier()?;
                        self.expect(Token::Colon)?;
                        let field_type = self.parse_type_annotation()?;

                        fields.push(StructField {
                            name: field_name,
                            type_ann: field_type,
                        });

                        if !self.match_token(Token::Comma) {
                            break;
                        }
                        if self.check(&Token::RBrace) {
                            break;
                        }
                    }
                }

                self.expect(Token::RBrace)?;
                let span = self.span_from(start);

                // Return as a struct variant (we'll wrap it in EnumVariant later if needed)
                return Ok(Node {
                    data: TypeAnn::Primary(
                        name,
                        vec![Node {
                            data: TypeAnn::Struct(fields.clone()),
                            meta: self.make_meta(span.clone(), Vec::new()),
                        }],
                    ),
                    meta: self.make_meta(span, Vec::new()),
                });
            } else if self.check(&Token::LParen) {
                // This might be a tuple variant - consume the LParen and parse the tuple
                self.advance(); // consume (

                let mut tuple_elements = Vec::new();

                if !self.check(&Token::RParen) {
                    loop {
                        tuple_elements.push(self.parse_type_annotation()?);
                        if !self.match_token(Token::Comma) {
                            break;
                        }
                    }
                }

                self.expect(Token::RParen)?;
                let span = self.span_from(start);

                // Return as a tuple variant (we'll wrap it in EnumVariant later if needed)
                return Ok(Node {
                    data: TypeAnn::Primary(
                        name,
                        tuple_elements, // For tuple variants, each element is a type in the tuple
                    ),
                    meta: self.make_meta(span, Vec::new()),
                });
            } else {
                // Not a struct or tuple variant, restore and parse normally
                self.current = saved;
            }
        }

        self.parse_type_annotation_atom()
    }

    fn parse_type_annotation_atom(&mut self) -> ParseResult<Node<TypeAnn>> {
        let start = self.current_span().start;

        if self.match_token(Token::SelfType) {
            let span = self.span_from(start);
            return Ok(Node {
                data: TypeAnn::Primary("Self".to_string(), Vec::new()),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        let name = self.parse_identifier()?;

        let generics = if self.match_token(Token::LBracket) {
            let mut args = Vec::new();
            if !self.check(&Token::RBracket) {
                loop {
                    args.push(self.parse_type_annotation()?);
                    if !self.match_token(Token::Comma) {
                        break;
                    }
                }
            }
            self.expect(Token::RBracket)?;
            args
        } else {
            Vec::new()
        };

        let span = self.span_from(start);
        Ok(Node {
            data: TypeAnn::Primary(name, generics),
            meta: self.make_meta(span, Vec::new()),
        })
    }

    fn parse_struct_type(&mut self) -> ParseResult<Node<TypeAnn>> {
        let start = self.current_span().start;
        self.expect(Token::LBrace)?;

        let mut fields = Vec::new();
        if !self.check(&Token::RBrace) {
            loop {
                let field_name = self.parse_identifier()?;
                self.expect(Token::Colon)?;
                let field_type = self.parse_type_annotation()?;

                fields.push(StructField {
                    name: field_name,
                    type_ann: field_type,
                });

                if !self.match_token(Token::Comma) {
                    break;
                }
                // Allow trailing comma
                if self.check(&Token::RBrace) {
                    break;
                }
            }
        }

        self.expect(Token::RBrace)?;
        let span = self.span_from(start);

        Ok(Node {
            data: TypeAnn::Struct(fields),
            meta: self.make_meta(span, Vec::new()),
        })
    }

    fn parse_expr(&mut self) -> ParseResult<Node<Expr>> {
        self.parse_expr_assignment()
    }

    fn parse_expr_assignment(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let expr = self.parse_expr_pipe()?;

        if self.match_token(Token::Eq) {
            let right = self.parse_expr_assignment()?;
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Assign {
                    left: Box::new(expr),
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            });
        }

        Ok(expr)
    }

    fn parse_expr_pipe(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let mut left = self.parse_expr_or()?;

        while self.match_token(Token::PipeRight) {
            let right = self.parse_expr_or()?;
            let span = self.span_from(start);
            left = Node {
                data: Expr::Pipe {
                    left: Box::new(left),
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            };
        }

        Ok(left)
    }

    fn parse_expr_or(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let mut left = self.parse_expr_and()?;

        while self.match_token(Token::OrOr) {
            let right = self.parse_expr_and()?;
            let span = self.span_from(start);
            left = Node {
                data: Expr::Binary {
                    left: Box::new(left),
                    op: BinOp::Or,
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            };
        }

        Ok(left)
    }

    fn parse_expr_and(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let mut left = self.parse_expr_equality()?;

        while self.match_token(Token::AndAnd) {
            let right = self.parse_expr_equality()?;
            let span = self.span_from(start);
            left = Node {
                data: Expr::Binary {
                    left: Box::new(left),
                    op: BinOp::And,
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            };
        }

        Ok(left)
    }

    fn parse_expr_equality(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let mut left = self.parse_expr_comparison()?;

        while let Some(op) = self.match_equality_op() {
            let right = self.parse_expr_comparison()?;
            let span = self.span_from(start);
            left = Node {
                data: Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            };
        }

        Ok(left)
    }

    fn match_equality_op(&mut self) -> Option<BinOp> {
        if self.match_token(Token::EqEq) {
            Some(BinOp::Eq)
        } else if self.match_token(Token::Neq) {
            Some(BinOp::Neq)
        } else {
            None
        }
    }

    fn parse_expr_comparison(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let mut left = self.parse_expr_range()?;

        while let Some(op) = self.match_comparison_op() {
            let right = self.parse_expr_range()?;
            let span = self.span_from(start);
            left = Node {
                data: Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            };
        }

        Ok(left)
    }

    fn match_comparison_op(&mut self) -> Option<BinOp> {
        if self.match_token(Token::Lt) {
            Some(BinOp::Lt)
        } else if self.match_token(Token::Gt) {
            Some(BinOp::Gt)
        } else if self.match_token(Token::Leq) {
            Some(BinOp::Leq)
        } else if self.match_token(Token::Geq) {
            Some(BinOp::Geq)
        } else {
            None
        }
    }

    fn parse_expr_range(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let left = self.parse_expr_bit_or()?;

        if self.match_token(Token::DotDot) {
            let right = self.parse_expr_bit_or()?;
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Binary {
                    left: Box::new(left),
                    op: BinOp::Range,
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            });
        }

        Ok(left)
    }

    fn parse_expr_bit_or(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let mut left = self.parse_expr_bit_xor()?;

        while self.check(&Token::Pipe) && !self.check_nth(1, &Token::Gt) {
            self.advance();
            let right = self.parse_expr_bit_xor()?;
            let span = self.span_from(start);
            left = Node {
                data: Expr::Binary {
                    left: Box::new(left),
                    op: BinOp::BitOr,
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            };
        }

        Ok(left)
    }

    fn check_nth(&self, n: usize, token: &Token) -> bool {
        if let Some(t) = self.peek_nth(n) {
            std::mem::discriminant(t) == std::mem::discriminant(token)
        } else {
            false
        }
    }

    fn parse_expr_bit_xor(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let mut left = self.parse_expr_bit_and()?;

        while self.match_token(Token::Caret) {
            let right = self.parse_expr_bit_and()?;
            let span = self.span_from(start);
            left = Node {
                data: Expr::Binary {
                    left: Box::new(left),
                    op: BinOp::Xor,
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            };
        }

        Ok(left)
    }

    fn parse_expr_bit_and(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let mut left = self.parse_expr_shift()?;

        while self.match_token(Token::Ampersand) {
            let right = self.parse_expr_shift()?;
            let span = self.span_from(start);
            left = Node {
                data: Expr::Binary {
                    left: Box::new(left),
                    op: BinOp::BitAnd,
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            };
        }

        Ok(left)
    }

    fn parse_expr_shift(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let mut left = self.parse_expr_additive()?;

        while let Some(op) = self.match_shift_op() {
            let right = self.parse_expr_additive()?;
            let span = self.span_from(start);
            left = Node {
                data: Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            };
        }

        Ok(left)
    }

    fn match_shift_op(&mut self) -> Option<BinOp> {
        if self.match_token(Token::Shl) {
            Some(BinOp::Shl)
        } else if self.match_token(Token::Shr) {
            Some(BinOp::Shr)
        } else {
            None
        }
    }

    fn parse_expr_additive(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let mut left = self.parse_expr_multiplicative()?;

        while let Some(op) = self.match_additive_op() {
            let right = self.parse_expr_multiplicative()?;
            let span = self.span_from(start);
            left = Node {
                data: Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            };
        }

        Ok(left)
    }

    fn match_additive_op(&mut self) -> Option<BinOp> {
        if self.match_token(Token::Plus) {
            Some(BinOp::Add)
        } else if self.match_token(Token::Minus) {
            Some(BinOp::Sub)
        } else {
            None
        }
    }

    fn parse_expr_multiplicative(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let mut left = self.parse_expr_power()?;

        while let Some(op) = self.match_multiplicative_op() {
            let right = self.parse_expr_power()?;
            let span = self.span_from(start);
            left = Node {
                data: Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            };
        }

        Ok(left)
    }

    fn match_multiplicative_op(&mut self) -> Option<BinOp> {
        if self.match_token(Token::Star) {
            Some(BinOp::Mul)
        } else if self.match_token(Token::Slash) {
            Some(BinOp::Div)
        } else if self.match_token(Token::Percent) {
            Some(BinOp::Mod)
        } else {
            None
        }
    }

    fn parse_expr_power(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let left = self.parse_expr_unary()?;

        if self.match_token(Token::Power) {
            let right = self.parse_expr_power()?; // Right associative
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Binary {
                    left: Box::new(left),
                    op: BinOp::Pow,
                    right: Box::new(right),
                },
                meta: self.make_meta(span, Vec::new()),
            });
        }

        Ok(left)
    }

    fn parse_expr_unary(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;

        if self.match_token(Token::Minus) {
            let expr = self.parse_expr_unary()?;
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Unary {
                    op: UnOp::Neg,
                    expr: Box::new(expr),
                },
                meta: self.make_meta(span, Vec::new()),
            });
        }

        self.parse_expr_postfix()
    }

    fn parse_expr_postfix(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;
        let mut expr = self.parse_expr_primary()?;

        loop {
            if self.match_token(Token::LParen) {
                // Function call
                let mut args = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        args.push(self.parse_expr()?);
                        if !self.match_token(Token::Comma) {
                            break;
                        }
                    }
                }
                self.expect(Token::RParen)?;

                let span = self.span_from(start);
                expr = Node {
                    data: Expr::Call {
                        fun: Box::new(expr),
                        args,
                    },
                    meta: self.make_meta(span, Vec::new()),
                };
            } else if self.match_token(Token::Dot) {
                // Field access
                let field = self.parse_identifier()?;
                let span = self.span_from(start);
                expr = Node {
                    data: Expr::FieldAccess {
                        expr: Box::new(expr),
                        field,
                    },
                    meta: self.make_meta(span, Vec::new()),
                };
            } else if self.check(&Token::LabelOpen) {
                // This is #[ which starts an index
                self.advance(); // consume #
                self.expect(Token::LBracket)?;
                let index = self.parse_expr()?;
                self.expect(Token::RBracket)?;

                let span = self.span_from(start);
                expr = Node {
                    data: Expr::Index {
                        expr: Box::new(expr),
                        index: Box::new(index),
                    },
                    meta: self.make_meta(span, Vec::new()),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_expr_primary(&mut self) -> ParseResult<Node<Expr>> {
        let start = self.current_span().start;

        // Literals
        if let Some(Token::Integer(i)) = self.peek() {
            let val = *i;
            self.advance();
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Literal(Literal::Int(val)),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        if let Some(Token::Float(f)) = self.peek() {
            let val = *f;
            self.advance();
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Literal(Literal::Float(val)),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        if let Some(Token::String(s)) = self.peek() {
            let val = s.clone();
            self.advance();
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Literal(Literal::String(val)),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        if let Some(Token::Char(c)) = self.peek() {
            let val = *c;
            self.advance();
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Literal(Literal::Char(val)),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        if self.match_token(Token::True) {
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Literal(Literal::Bool(true)),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        if self.match_token(Token::False) {
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Literal(Literal::Bool(false)),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Let expression
        if self.match_token(Token::Let) {
            let name = self.parse_identifier()?;
            let type_ann = if self.match_token(Token::Colon) {
                Some(self.parse_type_annotation()?)
            } else {
                None
            };
            self.expect(Token::Eq)?;
            let value = self.parse_expr()?;
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Let {
                    name,
                    type_ann,
                    value: Box::new(value),
                },
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // If expression
        if self.match_token(Token::If) {
            let cond = self.parse_expr()?;
            self.expect(Token::Then)?;
            let then_branch = self.parse_expr()?;
            let else_branch = if self.match_token(Token::Else) {
                Some(Box::new(self.parse_expr()?))
            } else {
                None
            };
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::If {
                    cond: Box::new(cond),
                    then_branch: Box::new(then_branch),
                    else_branch,
                },
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // While loop
        if self.match_token(Token::While) {
            let cond = self.parse_expr()?;
            let body = self.parse_expr()?;
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::While {
                    cond: Box::new(cond),
                    body: Box::new(body),
                },
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // For loop
        if self.match_token(Token::For) {
            let var = self.parse_identifier()?;
            self.expect(Token::In)?;
            let iter = self.parse_expr()?;
            let body = self.parse_expr()?;
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::For {
                    var,
                    iter: Box::new(iter),
                    body: Box::new(body),
                },
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Match expression
        if self.match_token(Token::Match) {
            print!("pm 0");
            let expr = self.parse_expr()?;

            print!("pm 1");
            self.expect(Token::LBrace)?;

            print!("pm 2");
            let mut arms = Vec::new();
            while !self.check(&Token::RBrace) {
                let pattern = self.parse_pattern()?;
                print!("pm 3");
                self.expect(Token::FatArrow)?;
                print!("pm 4");
                let body = self.parse_expr()?;
                print!("pm 5");
                arms.push(MatchArm { pattern, body });

                // Optional comma
                self.match_token(Token::Comma);
            }

            self.expect(Token::RBrace)?;
            print!("pm 6");
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Match {
                    expr: Box::new(expr),
                    arms,
                },
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Block expression
        if self.match_token(Token::LBrace) {
            let mut exprs = Vec::new();

            while !self.check(&Token::RBrace) {
                exprs.push(self.parse_expr()?);

                // Semicolons are optional but can be used
                if self.check(&Token::Semicolon) {
                    self.advance();
                    // Allow trailing semicolon before }
                    if self.check(&Token::RBrace) {
                        break;
                    }
                } else if self.check(&Token::RBrace) {
                    break;
                }
            }

            self.expect(Token::RBrace)?;
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Block(exprs),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Array literal
        if self.match_token(Token::LBracket) {
            let mut elements = Vec::new();

            if !self.check(&Token::RBracket) {
                loop {
                    elements.push(self.parse_expr()?);
                    if !self.match_token(Token::Comma) {
                        break;
                    }
                    // Allow trailing comma
                    if self.check(&Token::RBracket) {
                        break;
                    }
                }
            }

            self.expect(Token::RBracket)?;
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Array(elements),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Parenthesized expression or tuple
        if self.match_token(Token::LParen) {
            // Check for empty tuple
            if self.match_token(Token::RParen) {
                let span = self.span_from(start);
                return Ok(Node {
                    data: Expr::Tuple(Vec::new()),
                    meta: self.make_meta(span, Vec::new()),
                });
            }

            let first = self.parse_expr()?;

            // Check if it's a tuple
            if self.match_token(Token::Comma) {
                let mut elements = vec![first];

                // Allow trailing comma in single-element tuple
                if !self.check(&Token::RParen) {
                    loop {
                        elements.push(self.parse_expr()?);
                        if !self.match_token(Token::Comma) {
                            break;
                        }
                        // Allow trailing comma
                        if self.check(&Token::RParen) {
                            break;
                        }
                    }
                }

                self.expect(Token::RParen)?;
                let span = self.span_from(start);
                return Ok(Node {
                    data: Expr::Tuple(elements),
                    meta: self.make_meta(span, Vec::new()),
                });
            }

            // Just a parenthesized expression
            self.expect(Token::RParen)?;
            return Ok(first);
        }

        // Lambda function
        if self.match_token(Token::Fn) {
            self.expect(Token::LParen)?;
            let params = self.parse_function_params()?;
            self.expect(Token::RParen)?;

            let return_type = if self.match_token(Token::Arrow) {
                Some(self.parse_type_annotation()?)
            } else {
                None
            };

            let body = self.parse_expr()?;
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Lambda {
                    params,
                    return_type,
                    body: Box::new(body),
                },
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Break
        if self.match_token(Token::Break) {
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Break,
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Continue
        if self.match_token(Token::Continue) {
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Continue,
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Return
        if self.match_token(Token::Return) {
            let value = if self.check(&Token::Semicolon)
                || self.check(&Token::RBrace)
                || self.check(&Token::Comma)
            {
                None
            } else {
                Some(Box::new(self.parse_expr()?))
            };
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Return(value),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Static keyword (for static method calls)
        if self.match_token(Token::Static) {
            let type_name = self.parse_identifier()?;
            let method = self.parse_identifier()?;

            self.expect(Token::LParen)?;
            let mut args = Vec::new();
            if !self.check(&Token::RParen) {
                loop {
                    args.push(self.parse_expr()?);
                    if !self.match_token(Token::Comma) {
                        break;
                    }
                }
            }
            self.expect(Token::RParen)?;

            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::StaticCall {
                    type_name,
                    method,
                    args,
                },
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Identifier, struct literal, enum variant, or static call
        if let Some(Token::Identifier(name)) = self.peek() {
            let name = name.clone();
            self.advance();

            // Check for :: (static call or enum variant)
            if self.match_token(Token::PathSep) {
                let method_or_variant = self.parse_identifier()?;

                // Check if it's followed by ( or { to determine context
                if self.check(&Token::LParen) {
                    self.advance(); // consume (

                    let mut args = Vec::new();
                    if !self.check(&Token::RParen) {
                        loop {
                            args.push(self.parse_expr()?);
                            if !self.match_token(Token::Comma) {
                                break;
                            }
                        }
                    }
                    self.expect(Token::RParen)?;

                    let span = self.span_from(start);
                    // Could be either static call or enum tuple variant
                    // Defaulting to enum variant here
                    return Ok(Node {
                        data: Expr::EnumVariant {
                            enum_name: name,
                            variant_name: method_or_variant,
                            data: Some(EnumVariantData::Tuple(args)),
                        },
                        meta: self.make_meta(span, Vec::new()),
                    });
                } else if self.check(&Token::LBrace) {
                    // Enum struct variant
                    self.advance(); // consume {
                    let fields = self.parse_struct_literal_fields()?;
                    self.expect(Token::RBrace)?;

                    let span = self.span_from(start);
                    return Ok(Node {
                        data: Expr::EnumVariant {
                            enum_name: name,
                            variant_name: method_or_variant,
                            data: Some(EnumVariantData::Struct(fields)),
                        },
                        meta: self.make_meta(span, Vec::new()),
                    });
                } else {
                    // Enum unit variant
                    let span = self.span_from(start);
                    return Ok(Node {
                        data: Expr::EnumVariant {
                            enum_name: name,
                            variant_name: method_or_variant,
                            data: None,
                        },
                        meta: self.make_meta(span, Vec::new()),
                    });
                }
            }

            // Check for struct literal
            if self.check(&Token::LBrace) {
                self.advance(); // consume {
                let fields = self.parse_struct_literal_fields()?;
                self.expect(Token::RBrace)?;

                let span = self.span_from(start);
                return Ok(Node {
                    data: Expr::StructLiteral {
                        name,
                        fields,
                        base: None,
                    },
                    meta: self.make_meta(span, Vec::new()),
                });
            }

            // Just a variable
            let span = self.span_from(start);
            return Ok(Node {
                data: Expr::Variable(name),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        Err(ParseError::new(
            format!("Unexpected token in expression: {:?}", self.peek()),
            Location {
                span: self.current_span(),
                file: self.file.clone(),
            },
        ))
    }

    fn parse_struct_literal_fields(&mut self) -> ParseResult<Vec<StructLitField>> {
        let mut fields = Vec::new();

        while !self.check(&Token::RBrace) {
            // Check for base expression (..expr)
            if self.match_token(Token::DotDot) {
                // We can't handle base in the middle of fields
                // Skip for now
                let _base = self.parse_expr()?;
                break;
            }

            let name = self.parse_identifier()?;

            let value = if self.match_token(Token::Colon) {
                Some(self.parse_expr()?)
            } else {
                // Shorthand: {name} means {name: name}
                None
            };

            fields.push(StructLitField { name, value });

            if !self.match_token(Token::Comma) {
                break;
            }

            // Allow trailing comma
            if self.check(&Token::RBrace) {
                break;
            }
        }

        Ok(fields)
    }

    // Parse patterns
    fn parse_pattern(&mut self) -> ParseResult<Node<Pat>> {
        self.parse_pattern_union()
    }

    fn parse_pattern_union(&mut self) -> ParseResult<Node<Pat>> {
        let start = self.current_span().start;
        let mut patterns = vec![self.parse_pattern_primary()?];

        while self.match_token(Token::Pipe) {
            patterns.push(self.parse_pattern_primary()?);
        }

        if patterns.len() == 1 {
            Ok(patterns.into_iter().next().unwrap())
        } else {
            let span = self.span_from(start);
            Ok(Node {
                data: Pat::Union(patterns),
                meta: self.make_meta(span, Vec::new()),
            })
        }
    }

    fn parse_pattern_primary(&mut self) -> ParseResult<Node<Pat>> {
        let start = self.current_span().start;

        // Wildcard
        if let Some(Token::Identifier(id)) = self.peek()
            && id == "_"
        {
            self.advance();
            let span = self.span_from(start);
            return Ok(Node {
                data: Pat::Wildcard,
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Literals
        if let Some(Token::Integer(i)) = self.peek() {
            let val = *i;
            self.advance();

            // Check for range pattern
            if self.match_token(Token::DotDot) {
                let end_start = self.current_span().start;
                let end_val = match self.advance() {
                    Some(Token::Integer(i)) => i,
                    _ => {
                        return Err(ParseError::new(
                            "Expected integer in range pattern".to_string(),
                            Location {
                                span: self.current_span(),
                                file: self.file.clone(),
                            },
                        ));
                    }
                };
                let end_span = self.span_from(end_start);
                let end_node = Node {
                    data: Pat::Literal(Literal::Int(end_val)),
                    meta: self.make_meta(end_span, Vec::new()),
                };

                let span = self.span_from(start);
                let start_node = Node {
                    data: Pat::Literal(Literal::Int(val)),
                    meta: self.make_meta(start..start + 1, Vec::new()),
                };

                return Ok(Node {
                    data: Pat::Range {
                        start: Box::new(start_node),
                        end: Box::new(end_node),
                    },
                    meta: self.make_meta(span, Vec::new()),
                });
            }

            let span = self.span_from(start);
            return Ok(Node {
                data: Pat::Literal(Literal::Int(val)),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        if let Some(Token::Float(f)) = self.peek() {
            let val = *f;
            self.advance();
            let span = self.span_from(start);
            return Ok(Node {
                data: Pat::Literal(Literal::Float(val)),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        if let Some(Token::String(s)) = self.peek() {
            let val = s.clone();
            self.advance();
            let span = self.span_from(start);
            return Ok(Node {
                data: Pat::Literal(Literal::String(val)),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        if let Some(Token::Char(c)) = self.peek() {
            let val = *c;
            self.advance();
            let span = self.span_from(start);
            return Ok(Node {
                data: Pat::Literal(Literal::Char(val)),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        if self.match_token(Token::True) {
            let span = self.span_from(start);
            return Ok(Node {
                data: Pat::Literal(Literal::Bool(true)),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        if self.match_token(Token::False) {
            let span = self.span_from(start);
            return Ok(Node {
                data: Pat::Literal(Literal::Bool(false)),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Tuple pattern
        if self.match_token(Token::LParen) {
            let mut patterns = Vec::new();

            if !self.check(&Token::RParen) {
                loop {
                    patterns.push(self.parse_pattern()?);
                    if !self.match_token(Token::Comma) {
                        break;
                    }
                    // Allow trailing comma
                    if self.check(&Token::RParen) {
                        break;
                    }
                }
            }

            self.expect(Token::RParen)?;
            let span = self.span_from(start);
            return Ok(Node {
                data: Pat::Tuple(patterns),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        // Array pattern
        if self.match_token(Token::LBracket) {
            let mut patterns = Vec::new();
            let mut rest = None;

            print!("pap 0");

            if !self.check(&Token::RBracket) {
                loop {
                    // Check for rest pattern
                    if self.match_token(Token::DotDot) {
                        // Optional identifier for rest
                        rest = if let Some(Token::Identifier(_)) = self.peek() {
                            print!("pap 1");
                            Some(self.parse_identifier()?)
                        } else {
                            None
                        };
                        print!("pap 2");

                        // No more patterns after rest
                        self.match_token(Token::Comma);
                        break;
                    }

                    patterns.push(self.parse_pattern()?);

                    print!("pap 3");
                    if !self.match_token(Token::Comma) {
                        break;
                    }

                    // Allow trailing comma
                    if self.check(&Token::RBracket) {
                        break;
                    }
                }
            }

            self.expect(Token::RBracket)?;
            let span = self.span_from(start);

            if rest.is_some() {
                return Ok(Node {
                    data: Pat::ArrayRest(patterns, rest),
                    meta: self.make_meta(span, Vec::new()),
                });
            } else {
                return Ok(Node {
                    data: Pat::Array(patterns),
                    meta: self.make_meta(span, Vec::new()),
                });
            }
        }

        // Identifier, struct pattern, or enum pattern
        if let Some(Token::Identifier(name)) = self.peek() {
            let name = name.clone();
            self.advance();

            // Check for :: (enum variant)
            if self.match_token(Token::PathSep) {
                let variant = self.parse_identifier()?;

                let data = if self.match_token(Token::LParen) {
                    // Tuple variant
                    let mut patterns = Vec::new();
                    if !self.check(&Token::RParen) {
                        loop {
                            patterns.push(self.parse_pattern()?);
                            if !self.match_token(Token::Comma) {
                                break;
                            }
                        }
                    }
                    self.expect(Token::RParen)?;
                    Some(EnumPatData::Tuple(patterns))
                } else if self.match_token(Token::LBrace) {
                    // Struct variant
                    let fields = self.parse_struct_pattern_fields()?;
                    self.expect(Token::RBrace)?;
                    Some(EnumPatData::Struct(fields))
                } else {
                    None
                };

                let span = self.span_from(start);
                return Ok(Node {
                    data: Pat::EnumVariant {
                        enum_name: name,
                        variant_name: variant,
                        data,
                    },
                    meta: self.make_meta(span, Vec::new()),
                });
            }

            // Check for struct pattern
            if self.match_token(Token::LBrace) {
                let fields = self.parse_struct_pattern_fields()?;
                self.expect(Token::RBrace)?;

                let span = self.span_from(start);
                return Ok(Node {
                    data: Pat::Struct { name, fields },
                    meta: self.make_meta(span, Vec::new()),
                });
            }

            // Just an identifier pattern
            let span = self.span_from(start);
            return Ok(Node {
                data: Pat::Identifier(name),
                meta: self.make_meta(span, Vec::new()),
            });
        }

        Err(ParseError::new(
            format!("Unexpected token in pattern: {:?}", self.peek()),
            Location {
                span: self.current_span(),
                file: self.file.clone(),
            },
        ))
    }

    fn parse_struct_pattern_fields(&mut self) -> ParseResult<Vec<StructPatField>> {
        let mut fields = Vec::new();

        while !self.check(&Token::RBrace) {
            // Check for wildcard
            if let Some(Token::Identifier(id)) = self.peek()
                && id == "_"
            {
                self.advance();
                // Wildcard in struct pattern - ignore remaining fields
                break;
            }

            let name = self.parse_identifier()?;

            let pattern = if self.match_token(Token::Colon) {
                self.parse_pattern()?
            } else {
                // Shorthand: {name} means {name: name}
                let start = self.current_span().start;
                let span = self.span_from(start);
                Node {
                    data: Pat::Identifier(name.clone()),
                    meta: self.make_meta(span, Vec::new()),
                }
            };

            fields.push(StructPatField { name, pattern });

            if !self.match_token(Token::Comma) {
                break;
            }

            // Allow trailing comma
            if self.check(&Token::RBrace) {
                break;
            }
        }

        Ok(fields)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;

    fn parse(input: &str) -> ParseResult<Program> {
        let lex = Token::lexer(input);
        let mut tokens = Vec::new();

        for (tok, span) in lex.spanned() {
            match tok {
                Ok(t) => tokens.push((t, span)),
                Err(_) => {
                    return Err(ParseError::new(
                        "Lexer error".to_string(),
                        Location {
                            span,
                            file: "test".to_string(),
                        },
                    ));
                }
            }
        }

        let mut parser = Parser::new(tokens, "test".to_string());
        parser.parse_program()
    }

    #[test]
    fn test_simple_function() {
        let input = "main x, y = x + y";
        let result = parse(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_function_with_types() {
        let input = "add x: int, y: int -> int = x + y";
        let result = parse(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_alias() {
        let input = "type Point = {x: int, y: int}";
        let result = parse(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_enum() {
        let input = "type Color = Red | Green | Blue";
        let result = parse(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_trait() {
        let input = r#"
            trait Show {
                show self -> str
            }
        "#;
        let result = parse(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_impl() {
        let input = r#"
            impl Show for Person {
                show self -> str = "Person"
            }
        "#;
        let result = parse(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_complex_expression() {
        let input = r#"
            main _ = {
                let x = 5;
                let y = x * 2 + 3;
                if y > 10 then 
                    println("big")
                else 
                    println("small")
                a.b(c)
                static Integer from_string(s)
                
            }
        "#;
        let result = parse(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_match_expression() {
        let input = r#"
            test color = match (color) { // (color) because just doing color leads to a syntax error. 
                // todo: fix this. ambiguity with struct literal
                Red => 1,
                Green => 2,
                Blue => 3
            }
        "#;
        let result = parse(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_attributes() {
        let input = r#"
            @inline
            @jit(llvm, opt=3)
            fast_function x = x * 2
        "#;
        let result = parse(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_enum_variants() {
        let input = r#"
            type Color = White | Black | RGB (int, int, int) | Struct { x: int, y: int }
        "#;
        let result = parse(input);
        println!("{:#?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn test_pipe_operator() {
        let input = r#"
            main _ = 5 |> double |> add(3)
        "#;
        let result = parse(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_array_pattern() {
        let input = r#"
            test arr = match (arr) {
                [a, b, ..rest] => a + b,
                _ => 0
            }
        "#;
        let result = parse(input);
        assert!(result.is_ok());
    }
}
