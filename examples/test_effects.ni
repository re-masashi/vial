// Test effect system - function with Exception effect

effect Exception {
    throw(string) -> !
}

// Function that throws an exception
def dangerous_func() -> int effects Exception {
    perform throw("Something went wrong!")
}

// Function that handles the exception
def safe_wrapper() -> int {
    handle {
        dangerous_func()
    } with {
        throw(msg, k) => 42  // Handle exception by returning 42
    }
}

This should fail - calling effectful function without handling effects
def bad_call() -> int {
 dangerous_func()  // This should cause an error if uncommented
}

def pub main() -> int {
    let result = safe_wrapper();
    result
}