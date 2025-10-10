use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use logos::Logos;
use vial::ast::Interner;
use vial::lexer::Token;
use vial::parser;
use vial::typechecker::TypeChecker;
use vial::typechecker::monomorphizer::Monomorphizer;
use vial::validation::{TypedValidator, UntypedValidator};

use std::collections::HashMap;
use std::env;
use std::fs;

use std::path::PathBuf;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <file.ni>", args[0]);
        eprintln!("\nExample files:");
        eprintln!("  cargo run -- examples/basic.ni");
        eprintln!("  cargo run -- examples/example.ni");
        std::process::exit(1);
    }

    let filename = &args[1];
    let file_path = PathBuf::from(filename).canonicalize().unwrap_or_else(|_| {
        eprintln!("Error: Cannot find file '{}'", filename);
        std::process::exit(1);
    });

    let project_root = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

    let source = fs::read_to_string(filename).unwrap_or_else(|e| {
        eprintln!("Error reading file '{}': {}", filename, e);
        std::process::exit(1);
    });

    let token_iter = Token::lexer(&source).spanned();
    let mut tokens = Vec::new();
    let mut token_spans = Vec::new();

    for (token_result, byte_span) in token_iter {
        match token_result {
            Ok(token) => {
                tokens.push(token);
                token_spans.push(byte_span);
            }
            Err(_) => {
                eprintln!("Lexing error at {:?}", byte_span);
                std::process::exit(1);
            }
        }
    }

    let (ast, errors) = parser::parser()
        .parse(tokens.as_slice())
        .into_output_errors();

    if !errors.is_empty() {
        for error in &errors {
            let token_span = error.span();

            let byte_span = if token_span.end <= token_spans.len() {
                let start = token_spans[token_span.start].start;
                let end = if token_span.end > 0 && token_span.end <= token_spans.len() {
                    token_spans[token_span.end - 1].end
                } else {
                    token_spans[token_span.start].end
                };
                start..end
            } else {
                let last = token_spans.last().map(|s| s.end).unwrap_or(0);
                last..last
            };

            let msg = match error.reason() {
                chumsky::error::RichReason::ExpectedFound { expected, found } => {
                    let exp = if expected.len() <= 3 {
                        expected
                            .iter()
                            .map(|t| format!("{}", t))
                            .collect::<Vec<_>>()
                            .join(", ")
                    } else {
                        format!("one of {} things", expected.len())
                    };

                    let fnd = if let Some(f) = found {
                        format!("`{}`", f.clone().into_inner())
                    } else {
                        "end of input".to_string()
                    };

                    format!("Expected {} but found {}", exp, fnd)
                }
                chumsky::error::RichReason::Custom(msg) => msg.clone(),
            };

            Report::build(ReportKind::Error, (filename.as_str(), byte_span.clone()))
                .with_message("Parse Error")
                .with_label(
                    Label::new((filename.as_str(), byte_span.clone()))
                        .with_message(msg)
                        .with_color(Color::Red),
                )
                .finish()
                .eprint((filename.as_str(), Source::from(&source)))
                .unwrap();
        }
        std::process::exit(1);
    }

    let ast = ast.expect("No AST produced despite no errors");

    println!("Validation");
    let mut validator = UntypedValidator::new(project_root);
    let validated_ast = validator.validate(ast, &file_path);

    if validator.diagnostics.has_errors() {
        let mut sources = HashMap::new();
        sources.insert(filename.clone(), source.clone());
        validator.diagnostics.report_all(&sources);
        eprintln!(
            "Validation failed with {} error(s)",
            validator.diagnostics.errors.len()
        );
        std::process::exit(1);
    }

    if !validator.diagnostics.warnings.is_empty() {
        println!(
            "{} warning(s) during validation",
            validator.diagnostics.warnings.len()
        );
        let mut sources = HashMap::new();
        sources.insert(filename.clone(), source.clone());
        validator.diagnostics.report_all(&sources);
    }

    println!("Untyped ast validation passed");

    println!("Type Checking:");
    let mut type_checker = TypeChecker::new(Interner::new());
    let typed_ast = type_checker.check_program(validated_ast);

    if type_checker.diagnostics.has_errors() {
        let mut sources = HashMap::new();
        sources.insert(filename.clone(), source.clone());
        type_checker
            .diagnostics
            .report_all(&type_checker.interner, &sources);
        eprintln!(
            "Type checking failed with {} error(s)",
            type_checker.diagnostics.type_errors.len()
        );
        std::process::exit(1);
    }

    println!("Monomorphizer");
    let mut monomorphizer = Monomorphizer::new(type_checker.interner);
    let monomorphized_ast = monomorphizer.monomorphize_program(typed_ast);

    // println!("Nodes: {:#?}", monomorphized_ast);

    println!("TYPED Validation");
    let mut typed_validator = TypedValidator::new();
    if !typed_validator.validate(&monomorphized_ast) {
        eprintln!("TypedAST validation failed:");
        for err in &typed_validator.errors {
            eprintln!("  - {}", err);
        }
        std::process::exit(1);
    }

    println!("Type checking validation passed");

    // Output
    println!("Compilation Successful");
    println!("File: {}", filename);

    if args.contains(&"--debug".to_string()) || args.contains(&"-d".to_string()) {
        println!("Nodes: {}", monomorphized_ast.len());
        println!("Typed AST");
        for (i, node) in monomorphized_ast.iter().enumerate() {
            println!("Node {}: {:#?}", i, node);
        }
    }

    // TODO: IR
    // TODO: Optimization
    // TODO: Code generation
    // TODO: Bytecode and VM

    println!("\n✨ Compilation complete!");
}
