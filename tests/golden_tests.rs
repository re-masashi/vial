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
}
