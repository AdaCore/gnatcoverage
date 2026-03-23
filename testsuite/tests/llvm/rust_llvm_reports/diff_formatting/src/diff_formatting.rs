use std::hint::black_box;

fn foo(x : u32) {
    let y =
        if x % 2
            ==
            0
        {
            black_box("even")
        } else {
            black_box("odd")
        };

    black_box(y);
}

// bar is the same as foo, but formatted differently
fn bar(x : u32) {
    let y = if x % 2 == 0 { black_box("even") } else { black_box("odd") };

    black_box(y);
}

fn main() {
    foo(3);
    bar(3);
}
