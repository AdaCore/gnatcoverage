mod foo;

fn main() {
    let _ = foo::foo(Some(3));
}

//# foo.rs
// /body-some-5/    l- ## s-
// /some-i/         l- ## s-
// /body-some-i/    l- ## 0
// /body-none/      l- ## s-
