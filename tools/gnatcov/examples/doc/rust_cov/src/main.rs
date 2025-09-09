fn foo(a: bool, b: bool, c: bool) {
    if a && b && c {
        println!("A and B and C is TRUE")
    }
}

fn main() {
    println!("Hello, world!");
    foo(true, true, true);
    foo(false, true, true);
}
