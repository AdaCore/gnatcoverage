use std::hint::black_box;

fn bar(b: u32) {
    if b > 6 {
        black_box("yes");
    } else {
        black_box("no");
    }
}

fn several_decisions_in_one_fct(a: bool, b: bool, c: bool,
                                d: bool, e: bool) {
    let mut x = 0;

    if
        a && (b || c)
    {
        x = 3;
    } else {
        x = 8;
    }

    if
        (a || d) && (e || b)
    {
        x += 3;
    } else {
        x += 8;
    }

    bar(x);
}

fn simple_not_covered(a: bool, b: bool, c: bool) {
    if
        a && b || c
    {
        black_box("yes");
    } else {
        black_box("no");
    }
}

fn condition_too_long(a: bool, b: bool, c: bool, d: bool,
                      e: bool, f: bool, g: bool) {
    // This condition should not be instrumented by the
    // current implementation, because it is too long
    if a && b || c || d && e || f && g {
        black_box(33);
    }
}

fn main() {
    several_decisions_in_one_fct(true, false, false, true, true);
    several_decisions_in_one_fct(true, true, false, true, true);
    several_decisions_in_one_fct(true, false, true, true, true);
    several_decisions_in_one_fct(false, false, true, true, true);
    several_decisions_in_one_fct(false, false, false, false, false);
    several_decisions_in_one_fct(true, false, false, false, false);
    several_decisions_in_one_fct(true, true, false, false, false);

    simple_not_covered(true, true, false);
    simple_not_covered(true, false, false);

    condition_too_long(true, true, true, true, true, true, false);
}
