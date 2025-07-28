use std::hint::black_box;

pub fn foo(input: u32) {
    let print_even =        // # var
    |i| {                   // # closure-arg
        if i % 2 == 0 {     // # closure-if-guard
            black_box(i);   // # closure-body
        }                   // # closure-else
    };                      // # closure-end

    if input < 10 {         // # if-guard
        print_even(input);  // # call
    }                       // # else
}
