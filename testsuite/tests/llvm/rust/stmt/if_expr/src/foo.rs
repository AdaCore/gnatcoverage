use std::hint::black_box;

pub fn foo(b: bool) { // # decl-1
    if b              // # condition-1
    {                 // # if-1
        black_box(1); // # cif
    }                 // # cif
    else
    {                 // # else-1
        black_box(2); // # celse
    }                 // # celse
}                     // # end-1
