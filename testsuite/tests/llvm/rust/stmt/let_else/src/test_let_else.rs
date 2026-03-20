mod foo;

fn main() {
    let _ = foo::let_else(Some(42));
}

//# foo.rs
// /in-else-1/  l= ## s-
// /in-else/    l- ## 0
