use logos::Logos;
use std::env;
use std::fs;

use vial::desugar::desugar_ast;
use vial::lexer::Token;
use vial::parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];

    // Read the source file
    let source = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", file_path, err);
            std::process::exit(1);
        }
    };

    // Lex the source code
    let tokens = match lex_source(&source) {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("Lexing error: {}", err);
            std::process::exit(1);
        }
    };

    // Parse the tokens into an AST
    let mut parser = Parser::new(tokens, file_path.clone());
    match parser.parse_program() {
        Ok(ast) => {
            // Desugar the AST (convert pipes to calls)
            let desugared_ast = desugar_ast(ast);
            println!("Successfully parsed and desugared:");
            println!("{:#?}", desugared_ast);
        }
        Err(err) => {
            eprintln!("Parsing error at {}: {}", err.location.file, err.message);
            eprintln!("Span: {:?}", err.location.span);
            std::process::exit(1);
        }
    }
}

fn lex_source(source: &str) -> Result<Vec<(Token, std::ops::Range<usize>)>, String> {
    let lexer = Token::lexer(source);
    let mut tokens = Vec::new();

    for (token, span) in lexer.spanned() {
        match token {
            Ok(token) => tokens.push((token, span)),
            Err(_) => return Err(format!("Invalid token at position {:?}", span)),
        }
    }

    Ok(tokens)
}
