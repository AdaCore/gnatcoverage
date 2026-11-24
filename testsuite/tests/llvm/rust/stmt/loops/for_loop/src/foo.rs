use std::hint::black_box;

// In for loops, the variable has its own SCO, which is covered by assignment.
// In case the iterator given to the loop is empty, this SCO is never covered.
// However, if the variable name is `_`, meaning the value produced by the
// iterator is unbound, there is no associated SCO.

pub fn foo(input: u32) {
    if input > 10 {
        return;             // # return
    }                       // # after-if-1
    for
        _
        in 0 .. input       // # guard
    {                       // # body-1
        black_box(1);       // # cbody
    }                       // # cbody
}

pub fn bar(input: u32) {
    if input > 10 {
        return;             // # return
    }                       // # after-if-1
    for
        i                   // # var
        in 0 .. input       // # guard
    {                       // # body-1
        black_box(i);       // # cbody
    }                       // # cbody
}
