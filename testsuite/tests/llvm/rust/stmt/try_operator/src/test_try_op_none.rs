mod foo;

fn main() {
    let _ = foo::foo(None);
}

//# foo.rs
// /fn/     l+ ## 0
// /try/    l! ## s-
// /return/ l- ## s-
