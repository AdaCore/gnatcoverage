use std::hint::black_box;

pub fn let_else(input: Option<u32>) {

    // Let statements is covered if the assignement happens, meaning it was not
    // short-circuited by a an error in the expression evaluation.
    //
    // In the `let-else` case, the statement is covered if the pattern matching
    // succeeds. In that case, it means the else block is not executed, thus
    // not covered. In the other case, the else block shall be executed (and
    // covered), whereas the let statement won't.

    let Some(x) = input else {   // # let
        println!("No input");    // # in-else-1
        return;                  // # in-else
    };

    black_box(x);                // # black-box
}
