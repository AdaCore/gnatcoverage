use std::hint::black_box;

pub fn foo(mut input: u32) {
    if input > 10 {
        return;             // # return
    }                       // # after-if-1
    'named: loop {
        if input == 0 {     // # if
            break 'named;   // # break
        }                   // # cbody
        black_box(input);   // # body-1
        input -= 1;         // # cbody
    }
}
