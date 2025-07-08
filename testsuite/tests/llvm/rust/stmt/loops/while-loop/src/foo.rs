use std::hint::black_box;

pub fn foo(mut input: u32) {
    if input > 10 {
        return;             // # return
    }                       // # after-if-1
    while input > 0         // # guard
    {                       // # body-1
        black_box(input);   // # body
        input -= 1;         // # body
    }                       // # body
}
