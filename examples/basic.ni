// def add(x: Int, y: Int) -> Int {
//     x
// }

type ID = int

struct User {
    id: ID,
    name: string,
}

effect Logger {
    log(string) -> ();
}

@inline
def main() {
    let user = User(1, "Alice");
    perform log(user.show())
}
