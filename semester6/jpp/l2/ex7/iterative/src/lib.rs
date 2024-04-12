use std::process::exit;

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

#[no_mangle]
pub extern "C" fn factorial(n: i32) -> i32 {
    let mut result = 1;
    for i in 1..=n {
        result *= i;
    }
    result
}

#[no_mangle]
pub extern "C" fn gcd(mut a: i32, mut b: i32) -> i32 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

#[no_mangle]
pub extern "C" fn extendedGcd(mut a: i32, mut b: i32) -> ExtendedGcd {
    let mut x0 = 1;
    let mut y0 = 0;
    let mut x1 = 0;
    let mut y1 = 1;

    while b != 0 {
        let q = a / b;
        let r = a % b;

        a = b;
        b = r;

        let mut temp = x1;
        x1 = x0 -q * x1;
        x0 = temp;

        temp = y1;
        y1 = y0 - q * y1;
        y0 = temp;
    }

    ExtendedGcd{x: x0, y: y0, d: a}
}

#[no_mangle]
pub extern "C" fn diophantineEq(a: i32, b: i32, c: i32) -> DiophantineEq {
    let gcdResult = extendedGcd(a, b);
    if c % gcdResult.d == 0 {
        DiophantineEq{x: gcdResult.x * (c / gcdResult.d), y: gcdResult.y * (c / gcdResult.d)}
    } else {
        exit(1);
    }
}
