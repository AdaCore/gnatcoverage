use std::hint::black_box;

pub fn foo(input: Option<u32>) {
    let _ = match input {
        Some(5) =>              // # some-5
            black_box(5),       // # body-some-5
        Some(0..=4) =>          // # some-range
            black_box(1),       // # body-some-range
        Some(i) =>              // # some-i
            black_box(i),       // # body-some-i
        None =>                 // # none
            black_box(1)        // # body-none
    };
}
