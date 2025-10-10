use ariadne::{Color, Label, Report, ReportKind, Source};
use std::ops::Range;
// use crate::ast::Symbol;

#[derive(Debug, Clone)]
pub struct ValidationError {
    pub span: Range<usize>,
    pub file: String,
    pub kind: ValidationErrorKind,
}

#[derive(Debug, Clone)]
pub enum ValidationErrorKind {
    // Duplicate definitions
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

    // Control flow errors
    BreakOutsideLoop,
    ContinueOutsideLoop,
    ReturnOutsideFunction,

    // Unreachable code
    UnreachableCode,

    // Import errors
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

    // Macro errors
    MacroNotFound {
        name: String,
    },
    MacroExpansionFailed {
        name: String,
        reason: String,
    },

    // Visibility errors
    PrivateItemAccess {
        item: String,
    },

    // Pattern errors
    InvalidPattern {
        reason: String,
    },
    IrrefutablePatternRequired,

    // Effect errors
    EffectNotDeclared {
        effect: String,
    },

    // Generic errors
    TooManyTypeParameters,
    InvalidTypeParameter,

    // Attribute errors
    InvalidAttribute {
        name: String,
        reason: String,
    },

    // Misc
    InvalidSyntax {
        message: String,
    },
}

impl ValidationError {
    pub fn report(&self, source: &str) {
        match &self.kind {
            ValidationErrorKind::DuplicateDefinition {
                name,
                first_defined,
            } => {
                Report::build(ReportKind::Error, (&self.file, self.span.clone()))
                    .with_code("V001")
                    .with_message(format!("Duplicate definition of `{}`", name))
                    .with_label(
                        Label::new((&self.file, self.span.clone()))
                            .with_message("Second definition here")
                            .with_color(Color::Red),
                    )
                    .with_label(
                        Label::new((&self.file, first_defined.clone()))
                            .with_message("First defined here")
                            .with_color(Color::Blue),
                    )
                    .finish()
                    .eprint((&self.file, Source::from(source)))
                    .unwrap();
            }

            ValidationErrorKind::BreakOutsideLoop => {
                Report::build(ReportKind::Error, (&self.file, self.span.clone()))
                    .with_code("V002")
                    .with_message("`break` outside of loop")
                    .with_label(
                        Label::new((&self.file, self.span.clone()))
                            .with_message("Cannot break here")
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint((&self.file, Source::from(source)))
                    .unwrap();
            }

            ValidationErrorKind::ContinueOutsideLoop => {
                Report::build(ReportKind::Error, (&self.file, self.span.clone()))
                    .with_code("V003")
                    .with_message("`continue` outside of loop")
                    .with_label(
                        Label::new((&self.file, self.span.clone()))
                            .with_message("Cannot continue here")
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint((&self.file, Source::from(source)))
                    .unwrap();
            }

            ValidationErrorKind::ReturnOutsideFunction => {
                Report::build(ReportKind::Error, (&self.file, self.span.clone()))
                    .with_code("V004")
                    .with_message("`return` outside of function")
                    .with_label(
                        Label::new((&self.file, self.span.clone()))
                            .with_message("Cannot return here")
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint((&self.file, Source::from(source)))
                    .unwrap();
            }

            ValidationErrorKind::ImportNotFound { path } => {
                Report::build(ReportKind::Error, (&self.file, self.span.clone()))
                    .with_code("V005")
                    .with_message(format!("Cannot find module `{}`", path))
                    .with_label(
                        Label::new((&self.file, self.span.clone()))
                            .with_message("Module not found")
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint((&self.file, Source::from(source)))
                    .unwrap();
            }

            ValidationErrorKind::CircularImport { cycle } => {
                Report::build(ReportKind::Error, (&self.file, self.span.clone()))
                    .with_code("V006")
                    .with_message("Circular import detected")
                    .with_label(
                        Label::new((&self.file, self.span.clone()))
                            .with_message(format!("Cycle: {}", cycle.join(" -> ")))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint((&self.file, Source::from(source)))
                    .unwrap();
            }

            ValidationErrorKind::MacroNotFound { name } => {
                Report::build(ReportKind::Error, (&self.file, self.span.clone()))
                    .with_code("V007")
                    .with_message(format!("Macro `{}` not found", name))
                    .with_label(
                        Label::new((&self.file, self.span.clone()))
                            .with_message("Not defined")
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint((&self.file, Source::from(source)))
                    .unwrap();
            }

            ValidationErrorKind::UnreachableCode => {
                Report::build(ReportKind::Warning, (&self.file, self.span.clone()))
                    .with_code("V008")
                    .with_message("Unreachable code")
                    .with_label(
                        Label::new((&self.file, self.span.clone()))
                            .with_message("This code will never execute")
                            .with_color(Color::Yellow),
                    )
                    .finish()
                    .eprint((&self.file, Source::from(source)))
                    .unwrap();
            }

            _ => {
                Report::build(ReportKind::Error, (&self.file, self.span.clone()))
                    .with_message(format!("Validation error: {:?}", self.kind))
                    .with_label(Label::new((&self.file, self.span.clone())).with_color(Color::Red))
                    .finish()
                    .eprint((&self.file, Source::from(source)))
                    .unwrap();
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct ValidationDiagnostics {
    pub errors: Vec<ValidationError>,
    pub warnings: Vec<ValidationError>,
}

impl ValidationDiagnostics {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_error(&mut self, error: ValidationError) {
        self.errors.push(error);
    }

    pub fn add_warning(&mut self, warning: ValidationError) {
        self.warnings.push(warning);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn report_all(&self, sources: &std::collections::HashMap<String, String>) {
        println!("{:?}", self.errors);

        for error in &self.errors {
            if let Some(source) = sources.get(&error.file) {
                error.report(source);
            }
        }

        for warning in &self.warnings {
            if let Some(source) = sources.get(&warning.file) {
                warning.report(source);
            }
        }
    }
}
