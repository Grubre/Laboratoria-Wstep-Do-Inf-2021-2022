mod common;
mod iterative;
mod recursive;

fn main() {
    println!("iter vs recur: {}! = {}, {}! = {}", 0, iterative::factorial(0), 0, recursive::factorial(0));
    println!("iter vs recur: {}! = {}, {}! = {}", 5, iterative::factorial(5), 5, recursive::factorial(5));
    assert_eq!(iterative::factorial(0), recursive::factorial(0));
    assert_eq!(iterative::factorial(5), recursive::factorial(5));
}
