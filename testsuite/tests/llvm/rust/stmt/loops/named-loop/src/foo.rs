use std::hint::black_box;

pub fn foo(mut input: u32) {
    if input > 10 {
        return;             // # return
    }                       // # after-if-1
    'named: loop {
        if input == 0 {     // # if
            break 'named;   // # break
        }                   // # body-1
        black_box(input);   // # body
        input -= 1;         // # body
    }
}
