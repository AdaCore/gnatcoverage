mod foo;

fn main() {
    let _ = foo::foo(Some(30));
}

//# foo.rs
// /body-some-[^i]/ l- ## s-
// /body-none/      l- ## s-
