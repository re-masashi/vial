effect IO {
    print(string) -> ()
}

def do_print() -> () effects IO {
    perform print("Hello from effectful function!")
}

def bad_wrapper() -> () {
    do_print()  // This should fail - calling effectful function without declaring effects
}

def pub main() -> int {
    0
}