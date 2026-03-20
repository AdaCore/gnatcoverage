mod foo;

fn main() {
    let _ = foo::foo(11);
    let _ = foo::bar(11);
}

//# foo.rs
// /after-if-1/  l= ## s-
// /after-if/    l- ## 0
// /var/         l- ## s-
// /guard/       l- ## s-
// /body-1/      l= ## s-
// /body/        l- ## 0
