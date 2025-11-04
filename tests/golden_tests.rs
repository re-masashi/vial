#[cfg(test)]
mod golden_tests {
    use std::process::Command;

    /// Test that executes example files and checks their output
    /// This is a golden test - it compares the actual output with expected output
    #[test]
    fn test_hello_world_golden() {
        // Find the hello world example
        let example_path = "examples/basic/hello_world.vi";

        // Run the compiler with execute flag on the example
        let output = Command::new("cargo")
            .args(&["run", "--", example_path, "--execute"])
            .output()
            .expect("Failed to execute command");

        let stdout_str = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr_str = String::from_utf8_lossy(&output.stderr).to_string();

        // For this example, we expect to see "hello world" in the output
        assert!(stdout_str.contains("hello world") || stderr_str.contains("hello world"));
    }

    #[test]
    fn test_factorial_golden() {
        // Find the factorial example
        let example_path = "examples/basic/factorial.vi";

        // Run the compiler with execute flag on the example
        let output = Command::new("cargo")
            .args(&["run", "--", example_path, "--execute"])
            .output()
            .expect("Failed to execute command");

        // The factorial example should execute without error
        assert!(output.status.success());
    }

    #[test]
    fn test_helloworld_output_golden() {
        // Find the hello world example
        let example_path = "examples/basic/hello_world.vi";

        // Run the compiler with execute flag on the example
        let output = Command::new("cargo")
            .args(&["run", "--", example_path, "--execute"])
            .output()
            .expect("Failed to execute command");

        // Should complete execution successfully and print "hello world"
        assert!(output.status.success());

        let stdout_str = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr_str = String::from_utf8_lossy(&output.stderr).to_string();

        // Look for "hello world" in the output (either stdout or stderr)
        assert!(stdout_str.contains("hello world") || stderr_str.contains("hello world"));
    }

    /// Test custom enums with unit variants - should type check without errors
    #[test]
    fn test_unit_enum_type_checking() {
        // Create a temporary file with unit enum usage
        use std::fs::File;
        use std::io::Write;

        let test_content = r#"
enum Color { Red, Green, Blue }

let x = Color::Red;

match x {
    Color::Red => 1,
    Color::Green => 2,
    Color::Blue => 3
}
"#;

        let test_file = "test_unit_enum.vi";
        {
            let mut file = File::create(test_file).unwrap();
            file.write_all(test_content.as_bytes()).unwrap();
        }

        // Run type checking only (no execution to avoid IR issues)
        let output = Command::new("cargo")
            .args(&["run", "--", test_file])
            .output()
            .expect("Failed to execute command");

        // Should complete type checking successfully without errors
        assert!(
            output.status.success(),
            "Type checking failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Clean up
        std::fs::remove_file(test_file).unwrap();
    }

    /// Test custom enums with value variants - should type check without errors
    #[test]
    fn test_value_enum_type_checking() {
        // Create a temporary file with value enum usage
        use std::fs::File;
        use std::io::Write;

        let test_content = r#"
enum Maybe { Just(int), Nothing }

let x = Maybe::Just(42);

match x {
    Maybe::Just(n) => n * 2,
    Maybe::Nothing => 0
}
"#;

        let test_file = "test_value_enum.vi";
        {
            let mut file = File::create(test_file).unwrap();
            file.write_all(test_content.as_bytes()).unwrap();
        }

        // Run type checking only (no execution to avoid IR issues)
        let output = Command::new("cargo")
            .args(&["run", "--", test_file])
            .output()
            .expect("Failed to execute command");

        // Should complete type checking successfully without errors
        assert!(
            output.status.success(),
            "Type checking failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Clean up
        std::fs::remove_file(test_file).unwrap();
    }

    /// Test that builtin Option enum still works after our changes
    #[test]
    fn test_builtin_option_enum_still_works() {
        let test_content = r#"
let x = Option::Some(5);

match x {
    Option::Some(n) => n * 2,
    Option::None => 0
}
"#;

        let test_file = "test_builtin_option.vi";
        {
            use std::fs::File;
            use std::io::Write;

            let mut file = File::create(test_file).unwrap();
            file.write_all(test_content.as_bytes()).unwrap();
        }

        // Run type checking only
        let output = Command::new("cargo")
            .args(&["run", "--", test_file, "-e"])
            .output()
            .expect("Failed to execute command");

        // Should complete successfully
        assert!(
            output.status.success(),
            "Builtin Option enum failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Clean up
        std::fs::remove_file(test_file).unwrap();
    }

    /// Test enum with multiple value fields - should type check without errors
    #[test]
    fn test_enum_multiple_fields() {
        let test_content = r#"
enum Point { 
    TwoD(int, int), 
    ThreeD(int, int, int) 
}

let point = Point::TwoD(3, 4);

match point {
    Point::TwoD(x, y) => x + y,
    Point::ThreeD(x, y, z) => x + y + z
}
"#;

        let test_file = "test_enum_multi_fields.vi";
        {
            use std::fs::File;
            use std::io::Write;

            let mut file = File::create(test_file).unwrap();
            file.write_all(test_content.as_bytes()).unwrap();
        }

        // Run type checking only
        let output = Command::new("cargo")
            .args(&["run", "--", test_file])
            .output()
            .expect("Failed to execute command");

        // Should complete type checking successfully
        assert!(
            output.status.success(),
            "Enum with multiple fields failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Clean up
        std::fs::remove_file(test_file).unwrap();
    }

    /// Test that enum patterns work correctly in match expressions
    #[test]
    fn test_enum_pattern_matching() {
        let test_content = r#"
enum Direction { North, South, East, West }

let dir = Direction::North;

let result = match dir {
    Direction::North => "up",
    Direction::South => "down", 
    Direction::East => "right",
    Direction::West => "left"
};

result
"#;

        let test_file = "test_enum_patterns.vi";
        {
            use std::fs::File;
            use std::io::Write;

            let mut file = File::create(test_file).unwrap();
            file.write_all(test_content.as_bytes()).unwrap();
        }

        // Run type checking only
        let output = Command::new("cargo")
            .args(&["run", "--", test_file])
            .output()
            .expect("Failed to execute command");

        // Should complete successfully
        assert!(
            output.status.success(),
            "Enum pattern matching failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Clean up
        std::fs::remove_file(test_file).unwrap();
    }

    /// Test enum with nested usage - should type check without errors
    #[test]
    fn test_nested_enum_usage() {
        let test_content = r#"
enum Status { Active, Inactive }
enum User { Guest, Registered(Status, string) }

let user = User::Registered(Status::Active, "alice");

match user {
    User::Guest => 0,
    User::Registered(status, name) => match status {
        Status::Active => 1,
        Status::Inactive => 2
    }
}
"#;

        let test_file = "test_nested_enum.vi";
        {
            use std::fs::File;
            use std::io::Write;

            let mut file = File::create(test_file).unwrap();
            file.write_all(test_content.as_bytes()).unwrap();
        }

        // Run type checking only
        let output = Command::new("cargo")
            .args(&["run", "--", test_file])
            .output()
            .expect("Failed to execute command");

        // Should complete type checking successfully
        assert!(
            output.status.success(),
            "Nested enum usage failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Clean up
        std::fs::remove_file(test_file).unwrap();
    }

    /// Test multiple enums in same file - should type check without errors
    #[test]
    fn test_multiple_enums_in_same_file() {
        let test_content = r#"
enum Color { Red, Green, Blue }
enum Size { Small, Medium, Large }

let color = Color::Red;
let size = Size::Large;

match color {
    Color::Red => match size {
        Size::Large => 10,
        _ => 1
    },
    _ => 0
}
"#;

        let test_file = "test_multiple_enums.vi";
        {
            use std::fs::File;
            use std::io::Write;

            let mut file = File::create(test_file).unwrap();
            file.write_all(test_content.as_bytes()).unwrap();
        }

        // Run type checking only
        let output = Command::new("cargo")
            .args(&["run", "--", test_file])
            .output()
            .expect("Failed to execute command");

        // Should complete type checking successfully
        assert!(
            output.status.success(),
            "Multiple enums in same file failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Clean up
        std::fs::remove_file(test_file).unwrap();
    }

    /// Test forward reference enum usage - should type check without errors
    #[test]
    fn test_forward_ref_enum_usage() {
        let test_content = r#"
// Use enum before definition
let x = MyEnum::Second(10);

match x {
    MyEnum::First => 1,
    MyEnum::Second(n) => n * 2,
    MyEnum::Third(_, b) => if b { 5 } else { 0 }
}

enum MyEnum { 
    First, 
    Second(int), 
    Third(string, bool) 
}
"#;

        let test_file = "test_forward_ref.vi";
        {
            use std::fs::File;
            use std::io::Write;

            let mut file = File::create(test_file).unwrap();
            file.write_all(test_content.as_bytes()).unwrap();
        }

        // Run type checking only
        let output = Command::new("cargo")
            .args(&["run", "--", test_file])
            .output()
            .expect("Failed to execute command");

        // Should complete type checking successfully
        assert!(
            output.status.success(),
            "Forward reference enum usage failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Clean up
        std::fs::remove_file(test_file).unwrap();
    }

    /// Test enum with mixed types - should type check without errors
    #[test]
    fn test_mixed_types_enum() {
        let test_content = r#"
enum Container { 
    IntVal(int), 
    FloatVal(float), 
    StrVal(string), 
    Empty 
}

let x = Container::IntVal(42);

match x {
    Container::IntVal(n) => n,
    Container::FloatVal(f) => f as int,
    Container::StrVal(_) => 0,
    Container::Empty => -1
}
"#;

        let test_file = "test_mixed_types_enum.vi";
        {
            use std::fs::File;
            use std::io::Write;

            let mut file = File::create(test_file).unwrap();
            file.write_all(test_content.as_bytes()).unwrap();
        }

        // Run type checking only
        let output = Command::new("cargo")
            .args(&["run", "--", test_file])
            .output()
            .expect("Failed to execute command");

        // Should complete type checking successfully
        assert!(
            output.status.success(),
            "Mixed types enum failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Clean up
        std::fs::remove_file(test_file).unwrap();
    }

    /// Test complex nested matching - should type check without errors
    #[test]
    fn test_complex_nested_matching() {
        let test_content = r#"
enum Result { Ok(int), Err(string) }
enum Status { Success, Failure }

let res = Result::Ok(100);

match res {
    Result::Ok(val) => match Status::Success {
        Status::Success => val,
        Status::Failure => 0
    },
    Result::Err(_) => -1
}
"#;

        let test_file = "test_complex_nested.vi";
        {
            use std::fs::File;
            use std::io::Write;

            let mut file = File::create(test_file).unwrap();
            file.write_all(test_content.as_bytes()).unwrap();
        }

        // Run type checking only
        let output = Command::new("cargo")
            .args(&["run", "--", test_file])
            .output()
            .expect("Failed to execute command");

        // Should complete type checking successfully
        assert!(
            output.status.success(),
            "Complex nested matching failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Clean up
        std::fs::remove_file(test_file).unwrap();
    }

    /// Test simple enum without matching (construction only) - should type check without errors
    #[test]
    fn test_enum_construct_only() {
        let test_content = r#"
enum Simple { A }

let x = Simple::A;
1
"#;

        let test_file = "test_enum_construct_only.vi";
        {
            use std::fs::File;
            use std::io::Write;

            let mut file = File::create(test_file).unwrap();
            file.write_all(test_content.as_bytes()).unwrap();
        }

        // Run type checking only
        let output = Command::new("cargo")
            .args(&["run", "--", test_file])
            .output()
            .expect("Failed to execute command");

        // Should complete type checking successfully
        assert!(
            output.status.success(),
            "Enum construction only failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Clean up
        std::fs::remove_file(test_file).unwrap();
    }

    /// Test simple enum with match - should type check without errors
    #[test]
    fn test_simple_enum_with_match() {
        let test_content = r#"
enum Simple { A, B }

let x = Simple::A;

match x {
    Simple::A => 10,
    Simple::B => 20
}
"#;

        let test_file = "test_simple_match.vi";
        {
            use std::fs::File;
            use std::io::Write;

            let mut file = File::create(test_file).unwrap();
            file.write_all(test_content.as_bytes()).unwrap();
        }

        // Run type checking only
        let output = Command::new("cargo")
            .args(&["run", "--", test_file])
            .output()
            .expect("Failed to execute command");

        // Should complete type checking successfully
        assert!(
            output.status.success(),
            "Simple enum with match failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Clean up
        std::fs::remove_file(test_file).unwrap();
    }
}
