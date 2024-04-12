#[repr(C)]
pub struct ExtendedGcd {
    pub x: i32,
    pub y: i32,
    pub d: i32,
}

#[repr(C)]
pub struct DiophantineEq {
    pub x: i32,
    pub y: i32,
}

extern "C" {
    pub fn factorial(n: i32) -> i32;
    pub fn gcd(a: i32, b: i32) -> i32;
    pub fn extendedGcd(a: i32, b: i32) -> ExtendedGcd;
    pub fn diophantineEq(a: i32, b: i32, c: i32) -> DiophantineEq;
}

pub fn safe_factorial(n: i32) -> i32 {
    unsafe { factorial(n) }
}

pub fn safe_gcd(a: i32, b: i32) -> i32 {
    unsafe { gcd(a, b) }
}

pub fn safe_extended_gcd(a: i32, b: i32) -> ExtendedGcd {
    unsafe { extendedGcd(a, b) }
}

pub fn safe_diophantine_eq(a: i32, b: i32, c: i32) -> DiophantineEq {
    unsafe { diophantineEq(a, b, c) }
}

fn main() {
    println!("{}! = {}", 5, safe_factorial(5));
}
