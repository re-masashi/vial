use clap::{Parser, Subcommand};
use std::path::PathBuf;
use vial::{check_format, format_current_dir, format_diff, lint_files, lint_fix, run_file};

#[derive(Parser)]
#[command(name = "vial")]
#[command(about = "Vial programming language compiler and tools")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a Vial source file
    Run {
        /// The file to run
        file: PathBuf,
    },
    /// Format Vial source files
    Fmt {
        /// Check if files are properly formatted
        #[arg(long)]
        check: bool,
        /// Show diff from proper formatting
        #[arg(long)]
        diff: bool,
    },
    /// Lint Vial source files
    Lint {
        /// Automatically fix lint issues
        #[arg(long)]
        fix: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Run { file } => run_file(&file),
        Commands::Fmt { check, diff } => {
            if check {
                check_format()
            } else if diff {
                format_diff()
            } else {
                format_current_dir()
            }
        }
        Commands::Lint { fix } => {
            if fix {
                lint_fix()
            } else {
                lint_files()
            }
        }
    };

    if let Err(e) = result {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
