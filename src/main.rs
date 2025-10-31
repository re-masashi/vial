use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use logos::Logos;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::PathBuf;

use vial::ast::Interner;
use vial::ast::*;
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
                println!("{:#?}", source[byte_span].to_string());
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

    let mut ast = ast.expect("No AST produced despite no errors");

    // println!("PRE FILE NAME: {:#?}", ast);

    // FIXME: i have a feeling that this might misbehave when programs have imports.
    // instead, pass file names to parser functions.
    set_file_names_in_ast(&mut ast, filename);

    // println!("POST FILE NAME: {:#?}", ast);

    println!("[3/7] Validating untyped AST...");
    let mut validator = UntypedValidator::new(project_root);
    let validated_ast = validator.validate(ast, &file_path);

    if validator.diagnostics.has_errors() {
        report_diagnostics(&validator.diagnostics, &token_spans, filename, &source);
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
        report_diagnostics(&validator.diagnostics, &token_spans, filename, &source);
    }

    println!("[4/7] Desugaring method calls...");
    let mut method_desugarer = MethodCallDesugarer::new();
    let method_desugared_ast = method_desugarer.desugar_program(validated_ast);

    println!("[5/7] Type checking...");
    let interner = Interner::new();
    let mut type_checker = TypeChecker::new(interner);
    let typed_ast = type_checker.check_program(method_desugared_ast);

    if type_checker.diagnostics.has_errors() {
        report_type_diagnostics(&type_checker, &token_spans, filename, &source);
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

    println!("\nCompilation successful!");
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
    diagnostics: &vial::error::ErrorReporter,
    token_spans: &[std::ops::Range<usize>],
    filename: &str,
    source: &str,
) {
    let mut sources = HashMap::new();
    sources.insert(filename.to_string(), source.to_string());
    diagnostics.report_all(token_spans, &sources);
}

fn set_file_names_in_ast(nodes: &mut [ASTNode], filename: &str) {
    for node in nodes {
        set_file_names_in_ast_node(node, filename);
    }
}

fn set_file_names_in_ast_node(node: &mut ASTNode, filename: &str) {
    node.file = filename.to_string();
    set_file_names_in_ast_node_kind(&mut node.node, filename);
}

fn set_file_names_in_ast_node_kind(node_kind: &mut ASTNodeKind, filename: &str) {
    match node_kind {
        ASTNodeKind::Expr(expr) => set_file_names_in_expr(expr, filename),
        ASTNodeKind::Function(func) => {
            func.file = filename.to_string();
            if let Some(ref mut body) = func.body {
                set_file_names_in_expr(body, filename);
            }
        }
        ASTNodeKind::Struct(s) => {
            s.file = filename.to_string();
            for method in &mut s.methods {
                method.file = filename.to_string();
                if let Some(ref mut body) = method.body {
                    set_file_names_in_expr(body, filename);
                }
            }
        }
        ASTNodeKind::Enum(e) => {
            e.file = filename.to_string();
            for method in &mut e.methods {
                method.file = filename.to_string();
                if let Some(ref mut body) = method.body {
                    set_file_names_in_expr(body, filename);
                }
            }
        }
        ASTNodeKind::TypeAlias(alias) => {
            alias.file = filename.to_string();
        }
        ASTNodeKind::Impl(impl_block) => {
            impl_block.file = filename.to_string();
            for method in &mut impl_block.methods {
                method.file = filename.to_string();
                if let Some(ref mut body) = method.body {
                    set_file_names_in_expr(body, filename);
                }
            }
        }
        ASTNodeKind::Trait(trait_def) => {
            for method in &mut trait_def.methods {
                method.file = filename.to_string();
                if let Some(ref mut body) = method.body {
                    set_file_names_in_expr(body, filename);
                }
            }
        }
        ASTNodeKind::EffectDef(effect) => {
            effect.file = filename.to_string();
        }
        ASTNodeKind::ExternFunction(extern_func) => {
            extern_func.file = filename.to_string();
        }
        ASTNodeKind::MacroDef(macro_def) => {
            macro_def.file = filename.to_string();
        }
        ASTNodeKind::Error => {}
    }
}

fn set_file_names_in_expr(expr: &mut Expr, filename: &str) {
    expr.file = filename.to_string();
    match &mut expr.expr {
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Bool(_)
        | ExprKind::String(_)
        | ExprKind::Variable(_)
        | ExprKind::Continue
        | ExprKind::Error => {}
        ExprKind::Let {
            var: _,
            type_annot: _,
            value,
        } => {
            set_file_names_in_expr(value, filename);
        }
        ExprKind::BinOp(left, _op, right) => {
            set_file_names_in_expr(left, filename);
            set_file_names_in_expr(right, filename);
        }
        ExprKind::Call(func, args) => {
            set_file_names_in_expr(func, filename);
            for arg in args {
                set_file_names_in_expr(arg, filename);
            }
        }
        ExprKind::Block(exprs) => {
            for expr in exprs {
                set_file_names_in_expr(expr, filename);
            }
        }
        ExprKind::IfElse {
            condition,
            then,
            else_,
        } => {
            set_file_names_in_expr(condition, filename);
            set_file_names_in_expr(then, filename);
            if let Some(else_expr) = else_ {
                set_file_names_in_expr(else_expr, filename);
            }
        }
        ExprKind::Lambda {
            args: _,
            expression,
        } => {
            set_file_names_in_expr(expression, filename);
        }
        ExprKind::Match(scrutinee, arms) => {
            set_file_names_in_expr(scrutinee, filename);
            for arm in arms {
                if let Some(ref mut guard) = arm.guard {
                    set_file_names_in_expr(guard, filename);
                }
                set_file_names_in_expr(&mut arm.body, filename);
            }
        }
        ExprKind::Array(elements) => {
            for element in elements {
                set_file_names_in_expr(element, filename);
            }
        }
        ExprKind::Tuple(elements) => {
            for element in elements {
                set_file_names_in_expr(element, filename);
            }
        }
        ExprKind::Index(target, index) => {
            set_file_names_in_expr(target, filename);
            set_file_names_in_expr(index, filename);
        }
        ExprKind::Return(val) => {
            if let Some(val_expr) = val {
                set_file_names_in_expr(val_expr, filename);
            }
        }
        ExprKind::UnOp(_op, operand) => {
            set_file_names_in_expr(operand, filename);
        }
        ExprKind::Assign {
            l_val,
            r_val,
            op: _,
        } => {
            set_file_names_in_expr(l_val, filename);
            set_file_names_in_expr(r_val, filename);
        }
        ExprKind::Map(entries) => {
            for (key, value) in entries {
                set_file_names_in_expr(key, filename);
                set_file_names_in_expr(value, filename);
            }
        }
        ExprKind::EnumConstruct {
            name: _,
            variant: _,
            args,
        } => {
            for arg in args {
                set_file_names_in_expr(arg, filename);
            }
        }
        ExprKind::StructConstruct { name: _, fields } => {
            for (_, field_expr) in fields {
                set_file_names_in_expr(field_expr, filename);
            }
        }
        ExprKind::FieldAccess(target, _) => {
            set_file_names_in_expr(target, filename);
        }
        ExprKind::OptionalChain(target, _) => {
            set_file_names_in_expr(target, filename);
        }
        ExprKind::Cast {
            expr: inner,
            target_type: _,
        } => {
            set_file_names_in_expr(inner, filename);
        }
        ExprKind::With {
            context,
            var: _,
            body,
        } => {
            set_file_names_in_expr(context, filename);
            set_file_names_in_expr(body, filename);
        }
        ExprKind::Loop { label: _, body } => {
            set_file_names_in_expr(body, filename);
        }
        ExprKind::For {
            iterator,
            value: _,
            expression,
        } => {
            set_file_names_in_expr(iterator, filename);
            set_file_names_in_expr(expression, filename);
        }
        ExprKind::While(condition, body) => {
            set_file_names_in_expr(condition, filename);
            set_file_names_in_expr(body, filename);
        }
        ExprKind::IfLet {
            pattern: _,
            expr: scrutinee,
            then,
            else_,
        } => {
            set_file_names_in_expr(scrutinee, filename);
            set_file_names_in_expr(then, filename);
            if let Some(else_expr) = else_ {
                set_file_names_in_expr(else_expr, filename);
            }
        }
        ExprKind::WhileLet {
            pattern: _,
            expr: scrutinee,
            body,
        } => {
            set_file_names_in_expr(scrutinee, filename);
            set_file_names_in_expr(body, filename);
        }
        ExprKind::Break(val) => {
            if let Some(val_expr) = val {
                set_file_names_in_expr(val_expr, filename);
            }
        }
        ExprKind::Perform { effect: _, args } => {
            for arg in args {
                set_file_names_in_expr(arg, filename);
            }
        }
        ExprKind::Handle { body, handlers } => {
            set_file_names_in_expr(body, filename);
            for handler in handlers {
                set_file_names_in_expr(&mut handler.body, filename);
            }
        }
        ExprKind::MacroCall(_name, args, _delimiter) => {
            for arg in args {
                set_file_names_in_expr(arg, filename);
            }
        }
        ExprKind::Import(import) => {
            import.file = filename.to_string();
        }
    }
}

fn report_type_diagnostics(
    type_checker: &TypeChecker,
    token_spans: &[std::ops::Range<usize>],
    filename: &str,
    source: &str,
) {
    let mut sources = HashMap::new();
    sources.insert(filename.to_string(), source.to_string());
    type_checker
        .diagnostics
        .report_all(&type_checker.interner, token_spans, &sources);
}
