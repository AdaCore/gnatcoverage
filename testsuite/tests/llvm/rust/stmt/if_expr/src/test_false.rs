mod foo;

fn main() {
    foo::foo(false);
}

//# foo.rs
// /if-1/     l- ## s-
// /cif/       l- ## 0
// /(?!c?if)/   l+ ## 0
