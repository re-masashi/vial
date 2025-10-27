# Contributing to Vial

Thank you for your interest in contributing to Vial! This document provides guidelines and information about how to contribute to the project.

## Table of Contents

- [Development Setup](#development-setup)
- [Project Structure](#project-structure)
- [Code Style](#code-style)
- [Testing](#testing)
- [Submitting Changes](#submitting-changes)
- [Issue Tracking](#issue-tracking)
- [Community](#community)

## Development Setup

### Prerequisites

- Rust 1.70+ (with Cargo)
- Git
- A text editor or IDE with Rust support

### Setting Up Your Environment

1. Fork the repository on GitHub
2. Clone your fork:
   ```bash
   git clone https://github.com/YOUR-USERNAME/vial.git
   cd vial
   ```
3. Build the project:
   ```bash
   cargo build
   ```
4. Run tests to verify everything works:
   ```bash
   cargo test
   ```

## Project Structure

```
vial/
├── src/
│   ├── ast/          # Abstract Syntax Tree definitions
│   ├── lexer/        # Lexical analysis
│   ├── parser/       # Syntax parsing
│   ├── typechecker/  # Type checking and inference
│   ├── desugar/      # Desugaring transformations
│   ├── validation/   # AST validation
│   ├── ir/           # Intermediate Representation
│   ├── vm/           # VM + Opcode
│   ├── main.rs       # Main entry point
│   └── lib.rs        # Library exports
├── examples/         # Example Vial programs
├── grammar/          # Grammar definition files
├── docs/             # Documentation
├── Cargo.toml        # Project manifest
└── README.md         # Project overview
```

## Code Style

### Rust Code Style

- Follow the [Rust Style Guide](https://doc.rust-lang.org/1.0.0/style/)
- Use `cargo fmt` to format code
- Use `cargo clippy` to lint code
- Write documentation for public items using doc comments

### Git Commit Style

- Use [Conventional Commits](https://www.conventionalcommits.org/)
- Format: `<type>(<scope>): <short summary>`
- Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`
- Example: `feat(parser): add support for match expressions`

### Documentation Style

- Use Markdown for documentation
- Follow consistent formatting
- Include examples where helpful
- Update documentation when making changes

## Testing

### Running Tests

```bash
# Run all tests
cargo test

# Run tests with output
cargo test -- --nocapture

# Run specific test
cargo test test_name
```

### Writing Tests

- Add unit tests for new functionality
- Include integration tests for end-to-end functionality
- Test edge cases and error conditions
- Maintain good test coverage

### Example Test Structure

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_feature() {
        // Arrange
        let input = /* ... */;
        
        // Act
        let result = function_under_test(input);
        
        // Assert
        assert_eq!(result, expected_value);
    }
}
```

## Submitting Changes

### Before Submitting

1. Ensure all tests pass
2. Add tests for new functionality
3. Update documentation as needed
4. Format your code: `cargo fmt`
5. Lint your code: `cargo clippy`

### Pull Request Process

1. Create a new branch for your feature or bug fix
2. Make your changes
3. Commit your changes with a descriptive message
4. Push to your fork
5. Open a pull request to the main repository
6. Fill out the pull request template
7. Address any feedback from reviewers

### Pull Request Template

```
## Description
Brief description of the changes made.

## Related Issues
- Closes #issue-number

## Type of Change
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] This change requires a documentation update

## Testing
- [ ] Tests pass
- [ ] New functionality tested
- [ ] Documentation updated
```

## Issue Tracking

### Creating Issues

When creating issues, please:

- Use a clear and descriptive title
- Provide a detailed description of the problem
- Include steps to reproduce (for bugs)
- Provide example code if applicable
- Tag with appropriate labels

### Good First Issues

Look for issues tagged with `good first issue` if you're new to the project.

## Community

### Code of Conduct

This project adheres to the [Contributor Covenant Code of Conduct](https://www.contributor-covenant.org/). By participating, you are expected to uphold this code.

### Getting Help

- Open an issue for technical questions
- Discuss features and changes in GitHub issues
- Join the discussion in the project's communication channels (if applicable)

## Development Tips

### Working with the AST

- Understand the AST structure before making changes
- Ensure all AST node variants are handled in transformations
- Maintain span information for good error messages

### Type System Changes

- Be careful when changing type checking logic
- Update tests to reflect new type system behavior
- Consider performance implications of type checking changes

### Parser Changes

- Update grammar files when adding new syntax
- Ensure proper error recovery
- Test edge cases and ambiguous constructs

## License

By contributing to Vial, you agree that your contributions will be licensed under the project's MIT License.