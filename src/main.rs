use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use logos::Logos;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::PathBuf;

use vial::ast::Interner;
use vial::desugar::lambda::LambdaDesugarer;
use vial::desugar::methods::MethodCallDesugarer;
use vial::lexer::Token;
use vial::parser;
use vial::typechecker::TypeChecker;
use vial::typechecker::monomorphizer::Monomorphizer;
use vial::validation::{TypedValidator, UntypedValidator};

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

    let project_root = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

    let source = fs::read_to_string(filename).unwrap_or_else(|e| {
        eprintln!("Error reading file '{}': {}", filename, e);
        std::process::exit(1);
    });

    println!("[1/7] Lexing...");
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

    println!("[2/7] Parsing...");
    let (ast, errors) = parser::parser()
        .parse(tokens.as_slice())
        .into_output_errors();

    if !errors.is_empty() {
        report_parse_errors(&errors, &token_spans, filename, &source);
        std::process::exit(1);
    }

    let ast = ast.expect("No AST produced despite no errors");

    println!("[3/7] Validating untyped AST...");
    let mut validator = UntypedValidator::new(project_root);
    let validated_ast = validator.validate(ast, &file_path);

    if validator.diagnostics.has_errors() {
        report_diagnostics(&validator.diagnostics, filename, &source);
        eprintln!(
            "Validation failed with {} error(s)",
            validator.diagnostics.errors.len()
        );
        std::process::exit(1);
    }

    if !validator.diagnostics.warnings.is_empty() {
        println!(
            "  {} warning(s) during validation",
            validator.diagnostics.warnings.len()
        );
        report_diagnostics(&validator.diagnostics, filename, &source);
    }

    println!("[4/7] Desugaring method calls...");
    let mut method_desugarer = MethodCallDesugarer::new();
    let method_desugared_ast = method_desugarer.desugar_program(validated_ast);

    println!("[5/7] Type checking...");
    let interner = Interner::new();
    let mut type_checker = TypeChecker::new(interner);
    let typed_ast = type_checker.check_program(method_desugared_ast);

    if type_checker.diagnostics.has_errors() {
        report_type_diagnostics(&type_checker, filename, &source);
        eprintln!(
            "Type checking failed with {} error(s)",
            type_checker.diagnostics.type_errors.len()
        );
        std::process::exit(1);
    }

    println!("[6/7] Desugaring lambdas...");
    let final_interner = type_checker.interner;
    let mut lambda_desugarer = LambdaDesugarer::new(final_interner);
    let lambda_desugared_ast = lambda_desugarer.desugar_program(typed_ast);

    println!("[7/7] Monomorphizing...");
    let final_interner = lambda_desugarer.interner;
    let mut monomorphizer = Monomorphizer::new(final_interner);
    let monomorphized_ast = monomorphizer.monomorphize_program(lambda_desugared_ast);

    println!("[Validation] Validating typed AST...");
    let mut typed_validator = TypedValidator::new();
    if !typed_validator.validate(&monomorphized_ast) {
        eprintln!("TypedAST validation failed:");
        for err in &typed_validator.errors {
            eprintln!("  - {}", err);
        }
        std::process::exit(1);
    }

    println!("[IR Generation] Generating IR...");
    // Use the same interner that was potentially modified during monomorphization
    let final_interner = monomorphizer.interner;
    let mut ir_builder =
        vial::ir::builder::IRBuilder::new(vial::ir::TargetInfo::vm_target(), final_interner);
    let ir_module = ir_builder.lower_program(monomorphized_ast);

    // Add IR dump option
    if args.contains(&"--ir-dump".to_string()) || args.contains(&"-i".to_string()) {
        println!("\n[IR Dump]");
        println!("{:#?}", ir_module);
    }

    println!("[Code Generation] Compiling to bytecode...");
    let mut compiler = vial::compiler::BytecodeCompiler::new();
    let compiled_bytecode = compiler.compile_module(&ir_module);

    println!("[Bytecode Validation] Validating generated bytecode...");
    let mut bytecode_validator = vial::bytecode_validator::BytecodeValidator::new();
    if !bytecode_validator.validate_bytecode(
        &compiled_bytecode.bytecode,
        &compiled_bytecode.function_metadata,
    ) {
        eprintln!("Bytecode validation failed:");
        for err in &bytecode_validator.errors {
            eprintln!("  - {}", err);
        }
        std::process::exit(1);
    }

    println!("\n✓ Compilation successful!");
    println!("  File: {}", filename);

    // Add VM execution
    if args.contains(&"--execute".to_string()) || args.contains(&"-e".to_string()) {
        println!("[VM Execution] Starting execution...");
        match vial::vm::executor::run_bytecode(
            &compiled_bytecode.bytecode,
            &compiled_bytecode.function_metadata,
            Some(&compiled_bytecode.constant_pool),
            Some(&compiled_bytecode.struct_layouts),
            Some(&compiled_bytecode.enum_layouts),
        ) {
            Ok(result) => {
                println!("[VM Execution] Program exited with code: {}", result);
            }
            Err(e) => {
                eprintln!("[VM Execution] Error: {}", e);
                std::process::exit(1);
            }
        }
    } else if args.contains(&"--debug".to_string()) || args.contains(&"-d".to_string()) {
        // Use the disassembler to format the bytecode
        let disassembly = vial::vm::disassembler::pretty_dump_bytecode(
            &compiled_bytecode.bytecode,
            &compiled_bytecode.function_metadata,
            Some(compiled_bytecode.constant_pool.as_slice()),
            Some(compiled_bytecode.struct_layouts.as_slice()),
            Some(compiled_bytecode.enum_layouts.as_slice()),
        );

        println!("\n[Bytecode]");
        println!("{}", disassembly);
    }
    println!("\nNext steps:");
    println!("  - TODO: Optimization passes");
    println!("  - Run with --execute or -e to execute the program");
}

fn report_parse_errors(
    errors: &[chumsky::error::Rich<'_, Token>],
    token_spans: &[std::ops::Range<usize>],
    filename: &str,
    source: &str,
) {
    for error in errors {
        let token_span = error.span();

        let byte_span = if token_span.start < token_spans.len()
            && (token_span.end == 0 || token_span.end <= token_spans.len())
        {
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

        Report::build(ReportKind::Error, (filename, byte_span.clone()))
            .with_message("Parse Error")
            .with_label(
                Label::new((filename, byte_span.clone()))
                    .with_message(msg)
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((filename, Source::from(source)))
            .unwrap();
    }
}

fn report_diagnostics(
    diagnostics: &vial::validation::ValidationDiagnostics,
    filename: &str,
    source: &str,
) {
    let mut sources = HashMap::new();
    sources.insert(filename.to_string(), source.to_string());
    diagnostics.report_all(&sources);
}

fn report_type_diagnostics(type_checker: &TypeChecker, filename: &str, source: &str) {
    let mut sources = HashMap::new();
    sources.insert(filename.to_string(), source.to_string());
    type_checker
        .diagnostics
        .report_all(&type_checker.interner, &sources);
}
