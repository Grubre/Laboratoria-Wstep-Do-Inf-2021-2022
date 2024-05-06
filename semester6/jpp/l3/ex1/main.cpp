#include "gf.hpp"
#include <iostream>
#include <cassert>

using gf = GF<1234577>;

void test_GF() {
    gf a(5);
    gf b(7);

    // Test addition
    gf sum = a + b;
    assert(sum == 12);

    // Test subtraction
    gf diff = a - b;
    assert(diff == 1234575);

    // Test multiplication
    gf product = a * b;
    assert(product == 35);

    // Test division
    gf quotient = a / b;
    assert(quotient == gf{5291045});

    // Test equality
    assert(a == gf(5));
    assert(b != gf(10));
}

auto main() -> int {
    gf a{5}, b{6};

    std::cout << gf{7}.inverse() << std::endl;

    // test_GF();
    std::cout << a + 5 << std::endl;
    std::cout << a.inverse() << std::endl;
    return 0;
}
