mod foo;

fn main() {
    foo::foo(true);
}

//# foo.rs
// /else-1/     l= ## s-
// /else/       l- ## 0
// /(?!else)/   l+ ## 0
