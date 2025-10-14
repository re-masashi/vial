effect IO {
    print(string) -> ()
}

def do_print() -> () effects IO {
    perform print("Hello from effectful function!")
}

def wrapper() -> () effects IO {
    do_print()
}

def pub main() -> int {
    0
}