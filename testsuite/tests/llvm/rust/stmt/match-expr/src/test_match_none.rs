mod foo;

fn main() {
    let _ = foo::foo(None);
}

//# foo.rs
// /some-\D/        l- ## s-
// /body-some-[^i]/ l= ## s-
// /body-some/      l- ## 0
