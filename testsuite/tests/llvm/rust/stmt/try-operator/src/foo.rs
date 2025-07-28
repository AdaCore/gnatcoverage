// The try '?' operator induces an implicit break of the control flow.
// Therefore, at instrumentation, Rust inserts a counter at the beginning of
// the function, one for the short-circuit of `?`, and one after the evaluation
// of '?'.

pub fn foo(input: Option<u32>) -> Option<String> {  // # fn
    let number = input?;                            // # try
    return Some(number.to_string());                // # return
}
