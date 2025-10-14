trait Display {
    def show(self) -> string
}

def print_generic<T: Display>(x: T) -> string {
    x.show()
}

struct Person {
    name: string,
}

impl Person: Display {
    def show(self) -> string {
        "Person: " + self.name
    }
}

def pub main() -> int {
    let person = Person { name: "Alice" };
    let result = print_generic(person);
    0
}