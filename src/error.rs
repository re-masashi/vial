use crate::typechecker::token_range_to_char_range;
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::ops::Range;

#[derive(Debug, Clone)]
pub enum VialError {
    LexerError {
        span: Range<usize>,
        file: String,
        kind: LexerErrorKind,
    },
    ParseError {
        span: Range<usize>,
        file: String,
        kind: ParseErrorKind,
    },
    ValidationError {
        span: Range<usize>,
        file: String,
        kind: ValidationErrorKind,
    },
    TypeError {
        span: Range<usize>,
        file: String,
        kind: TypeErrorKind,
    },
    IRError {
        span: Range<usize>,
        file: String,
        kind: IRErrorKind,
    },
    CodegenError {
        span: Range<usize>,
        file: String,
        kind: CodegenErrorKind,
    },
    RuntimeError {
        message: String,
    },
    IOError {
        message: String,
    },
}

#[derive(Debug, Clone)]
pub enum LexerErrorKind {
    InvalidCharacter { character: char },
    UnterminatedString,
    InvalidEscapeSequence { sequence: String },
    UnterminatedComment,
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnexpectedToken {
        expected: Vec<String>,
        found: String,
    },
    UnexpectedEof,
    ExpectedToken {
        expected: String,
    },
    MismatchedDelimiter {
        expected: String,
        found: String,
    },
    InvalidExpression,
    InvalidStatement,
    InvalidType,
    InvalidPattern,
    InvalidAttribute,
}

#[derive(Debug, Clone)]
pub enum ValidationErrorKind {
    DuplicateDefinition {
        name: String,
        first_defined: Range<usize>,
    },
    DuplicateField {
        field: String,
    },
    DuplicateVariant {
        variant: String,
    },
    BreakOutsideLoop,
    ContinueOutsideLoop,
    ReturnOutsideFunction,
    UnreachableCode,
    ImportNotFound {
        path: String,
    },
    CircularImport {
        cycle: Vec<String>,
    },
    AmbiguousImport {
        name: String,
        sources: Vec<String>,
    },
    MacroNotFound {
        name: String,
    },
    MacroExpansionFailed {
        name: String,
        reason: String,
    },
    PrivateItemAccess {
        item: String,
    },
    InvalidPattern {
        reason: String,
    },
    IrrefutablePatternRequired,
    EffectNotDeclared {
        effect: String,
    },
    TooManyTypeParameters,
    InvalidTypeParameter,
    InvalidAttribute {
        name: String,
        reason: String,
    },
    InvalidSyntax {
        message: String,
    },
    InvalidOperation {
        operation: String,
        reason: String,
    },
}

#[derive(Debug, Clone)]
pub enum TypeErrorKind {
    TypeMismatch {
        expected: String,
        found: String,
        context: String,
    },
    UnboundVariable {
        name: String,
    },
    UnboundType {
        name: String,
    },
    ArityMismatch {
        expected: usize,
        found: usize,
        function: String,
    },
    EffectMismatch {
        required: Vec<String>,
        found: Vec<String>,
    },
    OccursCheck {
        var: String,
        in_type: String,
    },
    InvalidFieldAccess {
        type_: String,
        field: String,
    },
    MissingField {
        struct_name: String,
        field: String,
    },
    DuplicateField {
        field: String,
    },
    NotAFunction {
        type_: String,
    },
    PrivacyViolation {
        item: String,
    },
    TraitNotImplemented {
        trait_: String,
        type_: String,
    },
    AmbiguousType {
        candidates: Vec<String>,
    },
    NonExhaustiveMatch {
        missing_patterns: Vec<String>,
    },
    InvalidOperator {
        op: String,
    },
    TypeUnificationFailed {
        reason: String,
    },
    UnresolvedTypeVariable {
        variable: String,
    },
    InvalidReturnType {
        expected: String,
        found: String,
    },
}

#[derive(Debug, Clone)]
pub enum IRErrorKind {
    InvalidInstruction {
        instruction: String,
        reason: String,
    },
    UndefinedValue {
        value: String,
    },
    InvalidTypeInIR {
        ir_type: String,
        reason: String,
    },
    MismatchedBlock {
        block_id: usize,
        expected: String,
        found: String,
    },
    InvalidFunctionSignature {
        function: String,
        expected: String,
        found: String,
    },
    InvalidMemoryOperation {
        operation: String,
        reason: String,
    },
}

#[derive(Debug, Clone)]
pub enum CodegenErrorKind {
    InvalidBytecodeInstruction { instruction: String, reason: String },
    InvalidRegisterAllocation { register: u8, reason: String },
    InvalidFunctionCall { function: String, reason: String },
    UnsupportedOperation { operation: String, reason: String },
    StackOverflow { size: usize, limit: usize },
    InvalidAddressingMode { mode: String, reason: String },
    InvalidConstantPoolAccess { index: usize, reason: String },
    InvalidLayout { item: String, reason: String },
}

impl VialError {
    pub fn report(&self, token_spans: &[std::ops::Range<usize>], source: &str) {
        match self {
            VialError::LexerError { span, file, kind } => {
                let (message, labels) = self.format_lexer_error(kind, span);
                let mut report = Report::build(ReportKind::Error, (file, span.clone()))
                    .with_code("LEX001")
                    .with_message("Lexer Error")
                    .with_label(
                        Label::new((file, span.clone()))
                            .with_message(message)
                            .with_color(Color::Red),
                    );

                for (range, msg) in labels {
                    report = report.with_label(
                        Label::new((file, range))
                            .with_message(msg)
                            .with_color(Color::Blue),
                    );
                }

                report
                    .finish()
                    .eprint((file, Source::from(source)))
                    .unwrap();
            }

            VialError::ParseError { span, file, kind } => {
                let char_span = token_range_to_char_range(span, token_spans);
                let (message, labels) = self.format_parse_error(kind, &char_span);
                let mut report = Report::build(ReportKind::Error, (file, char_span.clone()))
                    .with_code("PAR001")
                    .with_message("Parse Error")
                    .with_label(
                        Label::new((file, char_span.clone()))
                            .with_message(message)
                            .with_color(Color::Red),
                    );

                for (range, msg) in labels {
                    let actual_range = token_range_to_char_range(&range, token_spans);
                    report = report.with_label(
                        Label::new((file, actual_range))
                            .with_message(msg)
                            .with_color(Color::Blue),
                    );
                }

                report
                    .finish()
                    .eprint((file, Source::from(source)))
                    .unwrap();
            }

            VialError::ValidationError { span, file, kind } => {
                let char_span = token_range_to_char_range(span, token_spans);
                let (message, labels) = self.format_validation_error(kind, &char_span);
                let mut report = Report::build(ReportKind::Error, (file, char_span.clone()))
                    .with_code("VAL001")
                    .with_message("Validation Error")
                    .with_label(
                        Label::new((file, char_span.clone()))
                            .with_message(message)
                            .with_color(Color::Red),
                    );

                for (range, msg) in labels {
                    let actual_range = token_range_to_char_range(&range, token_spans);
                    report = report.with_label(
                        Label::new((file, actual_range))
                            .with_message(msg)
                            .with_color(Color::Blue),
                    );
                }

                report
                    .finish()
                    .eprint((file, Source::from(source)))
                    .unwrap();
            }

            VialError::TypeError { span, file, kind } => {
                let char_span = token_range_to_char_range(span, token_spans);
                let (message, labels) = self.format_type_error(kind, &char_span);
                let mut report = Report::build(ReportKind::Error, (file, char_span.clone()))
                    .with_code("TYP001")
                    .with_message("Type Error")
                    .with_label(
                        Label::new((file, char_span.clone()))
                            .with_message(message)
                            .with_color(Color::Red),
                    );

                for (range, msg) in labels {
                    let actual_range = token_range_to_char_range(&range, token_spans);
                    report = report.with_label(
                        Label::new((file, actual_range))
                            .with_message(msg)
                            .with_color(Color::Blue),
                    );
                }

                report
                    .finish()
                    .eprint((file, Source::from(source)))
                    .unwrap();
            }

            VialError::IRError { span, file, kind } => {
                let char_span = token_range_to_char_range(span, token_spans);
                let (message, labels) = self.format_ir_error(kind, &char_span);
                let mut report = Report::build(ReportKind::Error, (file, char_span.clone()))
                    .with_code("IR001")
                    .with_message("IR Generation Error")
                    .with_label(
                        Label::new((file, char_span.clone()))
                            .with_message(message)
                            .with_color(Color::Red),
                    );

                for (range, msg) in labels {
                    let actual_range = token_range_to_char_range(&range, token_spans);
                    report = report.with_label(
                        Label::new((file, actual_range))
                            .with_message(msg)
                            .with_color(Color::Blue),
                    );
                }

                report
                    .finish()
                    .eprint((file, Source::from(source)))
                    .unwrap();
            }

            VialError::CodegenError { span, file, kind } => {
                let char_span = token_range_to_char_range(span, token_spans);
                let (message, labels) = self.format_codegen_error(kind, &char_span);
                let mut report = Report::build(ReportKind::Error, (file, char_span.clone()))
                    .with_code("CG001")
                    .with_message("Code Generation Error")
                    .with_label(
                        Label::new((file, char_span.clone()))
                            .with_message(message)
                            .with_color(Color::Red),
                    );

                for (range, msg) in labels {
                    let actual_range = token_range_to_char_range(&range, token_spans);
                    report = report.with_label(
                        Label::new((file, actual_range))
                            .with_message(msg)
                            .with_color(Color::Blue),
                    );
                }

                report
                    .finish()
                    .eprint((file, Source::from(source)))
                    .unwrap();
            }

            VialError::RuntimeError { message } => {
                eprintln!("Runtime Error: {}", message);
            }

            VialError::IOError { message } => {
                eprintln!("IO Error: {}", message);
            }
        }
    }

    fn format_lexer_error(
        &self,
        kind: &LexerErrorKind,
        _span: &Range<usize>,
    ) -> (String, Vec<(Range<usize>, String)>) {
        match kind {
            LexerErrorKind::InvalidCharacter { character } => {
                (format!("Invalid character: '{}'", character), vec![])
            }
            LexerErrorKind::UnterminatedString => {
                ("Unterminated string literal".to_string(), vec![])
            }
            LexerErrorKind::InvalidEscapeSequence { sequence } => {
                (format!("Invalid escape sequence: \\{}", sequence), vec![])
            }
            LexerErrorKind::UnterminatedComment => ("Unterminated comment".to_string(), vec![]),
        }
    }

    fn format_parse_error(
        &self,
        kind: &ParseErrorKind,
        _span: &Range<usize>,
    ) -> (String, Vec<(Range<usize>, String)>) {
        match kind {
            ParseErrorKind::UnexpectedToken { expected, found } => (
                format!(
                    "Unexpected token: expected one of {}, found '{}'",
                    expected.join(", "),
                    found
                ),
                vec![],
            ),
            ParseErrorKind::UnexpectedEof => ("Unexpected end of file".to_string(), vec![]),
            ParseErrorKind::ExpectedToken { expected } => {
                (format!("Expected token: {}", expected), vec![])
            }
            ParseErrorKind::MismatchedDelimiter { expected, found } => (
                format!(
                    "Mismatched delimiter: expected '{}', found '{}'",
                    expected, found
                ),
                vec![],
            ),
            ParseErrorKind::InvalidExpression => ("Invalid expression".to_string(), vec![]),
            ParseErrorKind::InvalidStatement => ("Invalid statement".to_string(), vec![]),
            ParseErrorKind::InvalidType => ("Invalid type".to_string(), vec![]),
            ParseErrorKind::InvalidPattern => ("Invalid pattern".to_string(), vec![]),
            ParseErrorKind::InvalidAttribute => ("Invalid attribute".to_string(), vec![]),
        }
    }

    fn format_validation_error(
        &self,
        kind: &ValidationErrorKind,
        _span: &Range<usize>,
    ) -> (String, Vec<(Range<usize>, String)>) {
        match kind {
            ValidationErrorKind::DuplicateDefinition {
                name,
                first_defined,
            } => (
                format!("Duplicate definition of `{}`", name),
                vec![(first_defined.clone(), "First defined here".to_string())],
            ),
            ValidationErrorKind::DuplicateField { field } => {
                (format!("Duplicate field `{}`", field), vec![])
            }
            ValidationErrorKind::DuplicateVariant { variant } => {
                (format!("Duplicate variant `{}`", variant), vec![])
            }
            ValidationErrorKind::BreakOutsideLoop => {
                ("`break` outside of loop".to_string(), vec![])
            }
            ValidationErrorKind::ContinueOutsideLoop => {
                ("`continue` outside of loop".to_string(), vec![])
            }
            ValidationErrorKind::ReturnOutsideFunction => {
                ("`return` outside of function".to_string(), vec![])
            }
            ValidationErrorKind::UnreachableCode => ("Unreachable code".to_string(), vec![]),
            ValidationErrorKind::ImportNotFound { path } => {
                (format!("Cannot find module `{}`", path), vec![])
            }
            ValidationErrorKind::CircularImport { cycle } => (
                format!("Circular import detected: {}", cycle.join(" -> ")),
                vec![],
            ),
            ValidationErrorKind::AmbiguousImport { name, sources } => (
                format!(
                    "Ambiguous import for `{}`: could refer to {}",
                    name,
                    sources.join(", ")
                ),
                vec![],
            ),
            ValidationErrorKind::MacroNotFound { name } => {
                (format!("Macro `{}` not found", name), vec![])
            }
            ValidationErrorKind::MacroExpansionFailed { name, reason } => (
                format!("Macro `{}` expansion failed: {}", name, reason),
                vec![],
            ),
            ValidationErrorKind::PrivateItemAccess { item } => {
                (format!("Cannot access private item `{}`", item), vec![])
            }
            ValidationErrorKind::InvalidPattern { reason } => {
                (format!("Invalid pattern: {}", reason), vec![])
            }
            ValidationErrorKind::IrrefutablePatternRequired => {
                ("Irrefutable pattern required".to_string(), vec![])
            }
            ValidationErrorKind::EffectNotDeclared { effect } => {
                (format!("Effect `{}` not declared", effect), vec![])
            }
            ValidationErrorKind::TooManyTypeParameters => {
                ("Too many type parameters".to_string(), vec![])
            }
            ValidationErrorKind::InvalidTypeParameter => {
                ("Invalid type parameter".to_string(), vec![])
            }
            ValidationErrorKind::InvalidAttribute { name, reason } => {
                (format!("Invalid attribute `{}`: {}", name, reason), vec![])
            }
            ValidationErrorKind::InvalidSyntax { message } => (message.clone(), vec![]),
            ValidationErrorKind::InvalidOperation { operation, reason } => (
                format!("Invalid operation `{}`: {}", operation, reason),
                vec![],
            ),
        }
    }

    fn format_type_error(
        &self,
        kind: &TypeErrorKind,
        _span: &Range<usize>,
    ) -> (String, Vec<(Range<usize>, String)>) {
        match kind {
            TypeErrorKind::TypeMismatch {
                expected,
                found,
                context,
            } => (
                format!(
                    "Type mismatch: expected `{}`, but found `{}` in {}",
                    expected, found, context
                ),
                vec![],
            ),
            TypeErrorKind::UnboundVariable { name } => {
                (format!("Cannot find variable `{}`", name), vec![])
            }
            TypeErrorKind::UnboundType { name } => (format!("Cannot find type `{}`", name), vec![]),
            TypeErrorKind::ArityMismatch {
                expected,
                found,
                function,
            } => (
                format!(
                    "Function `{}` takes {} arguments but {} were supplied",
                    function, expected, found
                ),
                vec![],
            ),
            TypeErrorKind::EffectMismatch { required, found } => (
                format!("Required effects {:?} but found {:?}", required, found),
                vec![],
            ),
            TypeErrorKind::OccursCheck { var, in_type } => (
                format!(
                    "Occurs check failed: cannot construct infinite type {} in {}",
                    var, in_type
                ),
                vec![],
            ),
            TypeErrorKind::InvalidFieldAccess { type_, field } => (
                format!(
                    "Invalid field access: type `{}` has no field `{}`",
                    type_, field
                ),
                vec![],
            ),
            TypeErrorKind::MissingField { struct_name, field } => (
                format!("Missing field `{}` in struct `{}`", field, struct_name),
                vec![],
            ),
            TypeErrorKind::DuplicateField { field } => {
                (format!("Duplicate field `{}`", field), vec![])
            }
            TypeErrorKind::NotAFunction { type_ } => {
                (format!("Expected function, but found {}", type_), vec![])
            }
            TypeErrorKind::PrivacyViolation { item } => (
                format!("Privacy violation: cannot access private item `{}`", item),
                vec![],
            ),
            TypeErrorKind::TraitNotImplemented { trait_, type_ } => (
                format!("Type `{}` does not implement trait `{}`", type_, trait_),
                vec![],
            ),
            TypeErrorKind::AmbiguousType { candidates } => (
                format!("Ambiguous type: could be any of {}", candidates.join(", ")),
                vec![],
            ),
            TypeErrorKind::NonExhaustiveMatch { missing_patterns } => (
                format!(
                    "Non-exhaustive patterns: missing {}",
                    missing_patterns.join(", ")
                ),
                vec![],
            ),
            TypeErrorKind::InvalidOperator { op } => (format!("Invalid operator `{}`", op), vec![]),
            TypeErrorKind::TypeUnificationFailed { reason } => {
                (format!("Type unification failed: {}", reason), vec![])
            }
            TypeErrorKind::UnresolvedTypeVariable { variable } => {
                (format!("Unresolved type variable: {}", variable), vec![])
            }
            TypeErrorKind::InvalidReturnType { expected, found } => (
                format!(
                    "Invalid return type: expected `{}`, but found `{}`",
                    expected, found
                ),
                vec![],
            ),
        }
    }

    fn format_ir_error(
        &self,
        kind: &IRErrorKind,
        _span: &Range<usize>,
    ) -> (String, Vec<(Range<usize>, String)>) {
        match kind {
            IRErrorKind::InvalidInstruction {
                instruction,
                reason,
            } => (
                format!("Invalid IR instruction `{}`: {}", instruction, reason),
                vec![],
            ),
            IRErrorKind::UndefinedValue { value } => {
                (format!("Undefined value: {}", value), vec![])
            }
            IRErrorKind::InvalidTypeInIR { ir_type, reason } => {
                (format!("Invalid IR type `{}`: {}", ir_type, reason), vec![])
            }
            IRErrorKind::MismatchedBlock {
                block_id,
                expected,
                found,
            } => (
                format!(
                    "Mismatched block {}: expected `{}`, found `{}`",
                    block_id, expected, found
                ),
                vec![],
            ),
            IRErrorKind::InvalidFunctionSignature {
                function,
                expected,
                found,
            } => (
                format!(
                    "Invalid signature for function `{}`: expected `{}`, found `{}`",
                    function, expected, found
                ),
                vec![],
            ),
            IRErrorKind::InvalidMemoryOperation { operation, reason } => (
                format!("Invalid memory operation `{}`: {}", operation, reason),
                vec![],
            ),
        }
    }

    fn format_codegen_error(
        &self,
        kind: &CodegenErrorKind,
        _span: &Range<usize>,
    ) -> (String, Vec<(Range<usize>, String)>) {
        match kind {
            CodegenErrorKind::InvalidBytecodeInstruction {
                instruction,
                reason,
            } => (
                format!("Invalid bytecode instruction `{}`: {}", instruction, reason),
                vec![],
            ),
            CodegenErrorKind::InvalidRegisterAllocation { register, reason } => (
                format!("Invalid register allocation for R{}: {}", register, reason),
                vec![],
            ),
            CodegenErrorKind::InvalidFunctionCall { function, reason } => (
                format!("Invalid function call to `{}`: {}", function, reason),
                vec![],
            ),
            CodegenErrorKind::UnsupportedOperation { operation, reason } => (
                format!("Unsupported operation `{}`: {}", operation, reason),
                vec![],
            ),
            CodegenErrorKind::StackOverflow { size, limit } => (
                format!("Stack overflow: size {} exceeds limit {}", size, limit),
                vec![],
            ),
            CodegenErrorKind::InvalidAddressingMode { mode, reason } => (
                format!("Invalid addressing mode `{}`: {}", mode, reason),
                vec![],
            ),
            CodegenErrorKind::InvalidConstantPoolAccess { index, reason } => (
                format!(
                    "Invalid constant pool access at index {}: {}",
                    index, reason
                ),
                vec![],
            ),
            CodegenErrorKind::InvalidLayout { item, reason } => {
                (format!("Invalid layout for `{}`: {}", item, reason), vec![])
            }
        }
    }

    pub fn is_error(&self) -> bool {
        !matches!(
            self,
            VialError::ValidationError {
                kind: ValidationErrorKind::UnreachableCode,
                ..
            }
        )
    }
}

#[derive(Debug, Default)]
pub struct ErrorReporter {
    pub errors: Vec<VialError>,
    pub warnings: Vec<VialError>,
}

impl ErrorReporter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_error(&mut self, error: VialError) {
        if error.is_error() {
            self.errors.push(error);
        } else {
            self.warnings.push(error);
        }
    }

    pub fn add_warning(&mut self, error: VialError) {
        self.warnings.push(error);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn report_all(
        &self,
        token_spans: &[std::ops::Range<usize>],
        sources: &std::collections::HashMap<String, String>,
    ) {
        for error in &self.errors {
            if let Some(source) = sources.get(&error.get_file()) {
                error.report(token_spans, source);
            } else {
                eprintln!(
                    "Error in file {:?} (source not available): {:?}",
                    error.get_file(),
                    error
                );
            }
        }

        for warning in &self.warnings {
            if let Some(_source) = sources.get(&warning.get_file()) {
                // For warnings, we'll just print them differently
                eprintln!("Warning: {:?}", warning);
            } else {
                eprintln!(
                    "Warning in file {:?} (source not available): {:?}",
                    warning.get_file(),
                    warning
                );
            }
        }
    }

    pub fn clear(&mut self) {
        self.errors.clear();
        self.warnings.clear();
    }
}

impl VialError {
    pub fn get_file(&self) -> String {
        match self {
            VialError::LexerError { file, .. } => file.clone(),
            VialError::ParseError { file, .. } => file.clone(),
            VialError::ValidationError { file, .. } => file.clone(),
            VialError::TypeError { file, .. } => file.clone(),
            VialError::IRError { file, .. } => file.clone(),
            VialError::CodegenError { file, .. } => file.clone(),
            VialError::RuntimeError { .. } => "runtime".to_string(),
            VialError::IOError { .. } => "io".to_string(),
        }
    }
}
