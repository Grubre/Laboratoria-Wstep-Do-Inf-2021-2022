#include <iostream>
#include "complex.hpp"

auto main() -> int {
    Gauss a{3,4};
    Gauss b{1,3};
    const auto [q,r] = div_with_remainder(a,b);
    const auto ab_gcd = gcd(a,b);
    const auto ab_lcm = lcm(a,b);

    std::cout << "a: " << a << ", b: " << b << "\n";
    std::cout << "q: " << q << ", r: " << r << "\n";
    std::cout << "gcd(a,b): " << ab_gcd << "\n";
    std::cout << "lcm(a,b): " << ab_lcm << "\n";
    return 0;
}