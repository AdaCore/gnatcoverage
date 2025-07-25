use std::hint::black_box;

pub fn foo(b: bool) { // # decl-1
    if b              // # condition
    {                 // # if-1
        black_box(1); // # if
    }                 // # if
    else
    {                 // # else-1
        black_box(2); // # else
    }                 // # else
}                     // # end-1
