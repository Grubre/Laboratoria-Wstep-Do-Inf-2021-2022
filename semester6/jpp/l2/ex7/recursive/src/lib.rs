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
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}

#[no_mangle]
pub extern "C" fn gcd(a: i32, b: i32) -> i32 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

#[no_mangle]
pub extern "C" fn extendedGcd(a: i32, b: i32) -> ExtendedGcd {
    if b == 0 {
        ExtendedGcd{x: 1, y: 0, d: a}
    } else {
        let result = extendedGcd(b, a % b);
        ExtendedGcd{x: result.y, y: result.x - (a / b) * result.y, d: result.d}
    }
}

#[no_mangle]
pub extern "C" fn diophantineEq(a: i32, b: i32, c: i32) -> DiophantineEq {
    let gcdResult = extendedGcd(a, b);
    if c % gcdResult.d == 0 {
        DiophantineEq{x: gcdResult.x * (c / gcdResult.d), y: gcdResult.y * (c / gcdResult.d)}
    } else {
        std::process::exit(1);
    }
}
