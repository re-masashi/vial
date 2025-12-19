use crate::ast::*;
use crate::lexer::Token;

use std::iter::Peekable;
use std::ops::Range;
use std::vec::IntoIter;

type TokenIter = Peekable<IntoIter<(Token, Range<usize>)>>;

pub fn parse(tokens: Vec<(Token, Range<usize>)>, file: &String) -> Result<Vec<Item>, Range<usize>> {
    let mut iter = tokens.into_iter().peekable();
    let mut items = Vec::new();

    while let Some(_) = iter.peek() {
        match parse_item(&mut iter, file) {
            Ok(item) => items.push(item),
            Err(pos) => return Err(pos),
        }
    }

    Ok(items)
}

pub fn parse_item(_iter: &mut TokenIter, _file: &String) -> Result<Item, Range<usize>> {
    todo!()
}

pub fn parse_expression(_iter: &mut TokenIter, _file: &String) -> Result<Expr, Range<usize>> {
    todo!()
}

pub fn parse_attribute(_iter: &mut TokenIter, _file: &String) -> Result<Attribute, Range<usize>> {
    todo!()
}

pub fn parse_type(_iter: &mut TokenIter, _file: &String) -> Result<Type, Range<usize>> {
    todo!()
}

pub fn parse_pattern(_iter: &mut TokenIter, _file: &String) -> Result<Pattern, Range<usize>> {
    todo!()
}

pub fn parse_struct(_iter: &mut TokenIter, _file: &String) -> Result<StructDecl, Range<usize>> {
    todo!()
}

pub fn parse_enum(_iter: &mut TokenIter, _file: &String) -> Result<EnumDecl, Range<usize>> {
    todo!()
}

pub fn parse_function(_iter: &mut TokenIter, _file: &String) -> Result<FnDecl, Range<usize>> {
    todo!()
}

pub fn parse_impl(_iter: &mut TokenIter, _file: &String) -> Result<ImplDecl, Range<usize>> {
    todo!()
}

pub fn parse_trait(_iter: &mut TokenIter, _file: &String) -> Result<TraitDecl, Range<usize>> {
    todo!()
}

pub fn parse_type_alias(
    _iter: &mut TokenIter,
    _file: &String,
) -> Result<TypeAliasDecl, Range<usize>> {
    todo!()
}

pub fn parse_const_decl(
    _iter: &mut TokenIter,
    _file: &String,
) -> Result<(String, Option<Type>, Expr), Range<usize>> {
    todo!()
}

pub fn parse_macro_decl(_iter: &mut TokenIter, _file: &String) -> Result<MacroDecl, Range<usize>> {
    todo!()
}

pub fn parse_use(_iter: &mut TokenIter, _file: &String) -> Result<UseDecl, Range<usize>> {
    todo!()
}
