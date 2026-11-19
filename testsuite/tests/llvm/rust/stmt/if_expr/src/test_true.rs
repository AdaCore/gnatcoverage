mod foo;

fn main() {
    foo::foo(true);
}

//# foo.rs
// /else-1/     l- ## s-
// /celse/      l- ## 0
// /(?!c?else)/   l+ ## 0
