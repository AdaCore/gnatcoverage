mod foo;

fn main() {
    let _ = foo::foo(Some(5));
}

//# foo.rs
// /some-\D/            l- ## s-
// /body-some-range/    l- ## s-
// /body-some-i/        l- ## s-,s-
// /body-none/          l- ## s-
