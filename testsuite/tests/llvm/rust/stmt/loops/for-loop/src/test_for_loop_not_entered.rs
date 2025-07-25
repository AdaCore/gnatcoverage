mod foo;

fn main() {
    let _ = foo::foo(0);
    let _ = foo::bar(0);
}

//# foo.rs
// /return/ l- ## s-
// /var/    l- ## s-
// /body-1/ l= ## s-
// /body/   l- ## 0
