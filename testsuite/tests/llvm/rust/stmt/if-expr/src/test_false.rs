mod foo;

fn main() {
    foo::foo(false);
}

//# foo.rs
// /if-1/     l= ## s-
// /if/       l- ## 0
// /(?!if)/   l+ ## 0
