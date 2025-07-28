mod foo;

fn main() {
    let _ = foo::foo(5);
}

//# foo.rs
// /closure-arg/        l= ## 0
// /closure-if-guard/   l! ## s-
// /closure-else/       l! ## 0
// /closure-body/       l- ## 0
// /else/               l! ## s-
