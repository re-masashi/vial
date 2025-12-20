pub mod ast;
pub mod lexer;
pub mod parser;

use std::fs;
use std::path::Path;

/// Run a Vial file
pub fn run_file(file_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    if !file_path.exists() {
        return Err(format!("File '{}' does not exist", file_path.display()).into());
    }

    let source = fs::read_to_string(file_path)?;
    let tokens = lexer::lex_without_comments(&source)
        .map_err(|pos| format!("Lexical error at position {}", pos))?;

    println!("Running file: {}", file_path.display());
    println!("Tokens lexed: {}", tokens.len());

    Ok(())
}

/// Format Vial files in the current directory
pub fn format_current_dir() -> Result<(), Box<dyn std::error::Error>> {
    println!("Formatting files in current directory...");
    // TODO: Implement actual formatting when formatter is ready
    Ok(())
}

/// Check formatting of Vial files in the current directory
pub fn check_format() -> Result<(), Box<dyn std::error::Error>> {
    println!("Checking formatting in current directory...");
    // TODO: Implement actual format checking
    Ok(())
}

/// Show diff from proper formatting for Vial files
pub fn format_diff() -> Result<(), Box<dyn std::error::Error>> {
    println!("Showing format diff for current directory...");
    // TODO: Implement actual diff display
    Ok(())
}

/// Run linter on Vial files
pub fn lint_files() -> Result<(), Box<dyn std::error::Error>> {
    println!("Linting files...");
    // TODO: Implement actual linting when linter is ready
    Ok(())
}

/// Run linter with fixes on Vial files
pub fn lint_fix() -> Result<(), Box<dyn std::error::Error>> {
    println!("Linting and fixing files...");
    // TODO: Implement actual linting with fixes
    Ok(())
}
