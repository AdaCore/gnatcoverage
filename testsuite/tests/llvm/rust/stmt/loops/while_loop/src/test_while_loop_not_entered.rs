mod foo;

fn main() {
    let _ = foo::foo(0);
}

//# foo.rs
// /return/ l- ## s-
// /body/   l- ## s-
// /body-1/ l- ## s-
// /bcont/  l- ## 0
