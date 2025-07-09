use std::hint::black_box;

fn closure_in_var(x: u32) {
    let is_even = |n: &u32| (n & 1) == 0;

    let res: Vec<_> = (0..x)
        .filter(is_even)
        .collect();
    black_box(res);
}

fn closure_imm(x: u32) {
    let res: Vec<_> = (0..x)
        .filter(|n: &u32| (n & 1) == 0)
        .collect();
    black_box(res);
}

fn closure_uncovered(_: u32) {
    let res: Vec<_> = (0..0)
        .filter(|n: &u32| (n & 1) == 0)
        .collect();
    black_box(res);
}

fn main() {
    closure_in_var(35);
    closure_imm(35);
    closure_uncovered(35);
}
