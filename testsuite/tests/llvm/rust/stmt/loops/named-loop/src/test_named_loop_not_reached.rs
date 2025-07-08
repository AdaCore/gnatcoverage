mod foo;

fn main() {
    let _ = foo::foo(11);
}

//# foo.rs
// /after-if-1/  l- ## s-
// /if/          l- ## s-
// /break/       l- ## s-
// /body-1/      l= ## s-
// /body/        l- ## 0
