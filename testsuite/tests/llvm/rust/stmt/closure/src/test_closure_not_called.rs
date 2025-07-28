mod foo;

fn main() {
    let _ = foo::foo(42);
}

//# foo.rs
// /closure-arg/        l= ## s-
// /closure-if-guard/   l= ## s-
// /closure-else/       l= ## s-
// /closure-end/        l= ## s-
// /closure/            l- ## 0
// /if-guard/           l! ## s-
// /call/               l- ## 0
// /else/               l! ## 0
