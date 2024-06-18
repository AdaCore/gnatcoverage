use std::hint::black_box;

fn foo(a: bool, b: bool, c: bool) {
    if a && if b || c { true } else { false } {
        black_box("yes");
    } else {
        black_box("no");
    }
}

fn foo_uncovered_nested(a: bool, b: bool, c: bool) {
    if a && if b || c { true } else { false } {
        black_box("yes");
    } else {
        black_box("no");
    }
}

fn foo_uncovered_root(a: bool, b: bool, c: bool) {
    if a && if b || c { true } else { false } {
        black_box("yes");
    } else {
        black_box("no");
    }
}

fn nested_3(a: bool, b: bool, c: bool, d: bool) {
    if a &&
        if b ||
            if c && d
            { true } else { false }
        { true } else { false }
    {
        black_box("yes");
    } else {
        black_box("no");
    }
}

// This test currently make llvm utility functions fail.
// (See https://github.com/llvm/llvm-project/pull/91600)
//
// fn nested_first(a: bool, b: bool, c: bool) {
//     if if b || c { true } else { false } && a {
//         black_box("yes");
//     } else {
//         black_box("no");
//     }
// }

fn main() {
    foo(true, false, false);
    foo(true, true, true);
    foo(true, false, true);
    foo(false, true, true);

    foo_uncovered_nested(false, true, true);
    foo_uncovered_nested(true, true, false);
    foo_uncovered_nested(true, false, false);

    foo_uncovered_root(true, false, false);
    foo_uncovered_root(true, true, false);
    foo_uncovered_root(true, false, true);

    nested_3(true, true, true, true);
    nested_3(false, true, true, true);
    nested_3(true, true, false, true);
    nested_3(true, true, false, false);
    nested_3(true, true, true, false);

    // nested_first(true, false, false);
    // nested_first(true, true, true);
    // nested_first(true, false, true);
    // nested_first(false, true, true);
}
