use std::hint::black_box;

pub fn foo(input: u32) {
    'named_block: {
        let _ = black_box(1);
        if input == 42 {
            break 'named_block; // # break
        }                       // # bb2-1
        let _ = black_box(2);   // # bb2
    }
    let _ = black_box(3);
}
