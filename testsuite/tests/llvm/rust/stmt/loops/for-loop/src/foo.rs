use std::hint::black_box;

// In for loops, the variable has its own SCO, which is covered by assignment.
// In case the iterator given to the loop is empty, this SCO is never covered.
// However, if the variable name is `_`, meaning the value produced by the
// iterator is unbound, there is no associated SCO.
// Additionally, if the variable is bound, a SCO is made for the iterator.
// Otherwise, it is attached in the SCO preceding the for loop.

pub fn foo(input: u32) {
    if input > 10 {
        return;             // # return
    }                       // # after-if-1
    for                     // # after-if
        _                   // # after-if
        in 0 .. input       // # after-if
    {                       // # body-1
        black_box(1);       // # body
    }                       // # body
}

pub fn bar(input: u32) {
    if input > 10 {
        return;             // # return
    }                       // # after-if-1
    for
        i                   // # var
        in 0 .. input       // # guard
    {                       // # body-1
        black_box(i);       // # body
    }                       // # body
}
