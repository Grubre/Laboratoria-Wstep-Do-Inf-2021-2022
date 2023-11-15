#include <cmath>
#include <cstdint>
#include <functional>
#include <iostream>
#include <cassert>
#include <vector>

auto inverse_series(uint32_t n, std::function<double(uint32_t)> f) -> std::vector<double> {
    assert(f(0) != 0);

    auto elements = std::vector<double>{};
    elements.reserve(n);
    elements.push_back(1.0 / f(0));

    for(auto i = 1u; i < n; i++) {
        auto bi = 0.0;
        for(auto k = 1u; k <= i; k++) {
            bi += f(k) * elements[i - k];
        }
        bi *= -1.0 / f(0);
        elements.push_back(bi);
    }

    return elements;
}

auto factorial(uint32_t n) -> double {
    auto s = 1.0;
    for(auto i = 2; i <= n; i++) {
        s *= i;
    }
    return s;
}

auto main() -> int {
    const auto f_a = [](uint32_t n) -> double { return 1.0; };
    const auto f_b = [](uint32_t n) -> double { return std::pow(2.0,n); };
    const auto f_c = [](uint32_t n) -> double { return factorial(n); };
    const auto f_d = [](uint32_t n) -> double { return 1.0/factorial(n); };
    const auto n = 10u;
    const auto b_a = inverse_series(n, f_a);
    const auto b_b = inverse_series(n, f_b);
    const auto b_c = inverse_series(n, f_c);
    const auto b_d = inverse_series(n, f_d);
    std::cout << "f(n) = 1" << std::endl;
    for(auto i = 0u; i < b_a.size(); i++) {
        std::cout << "b_" << i << " = " << b_a[i] << std::endl;
    }
    std::cout << "f(n) = 2^n" << std::endl;
    for(auto i = 0u; i < b_b.size(); i++) {
        std::cout << "b_" << i << " = " << b_b[i] << std::endl;
    }
    std::cout << "f(n) = n!" << std::endl;
    for(auto i = 0u; i < b_c.size(); i++) {
        std::cout << "b_" << i << " = " << b_c[i] << std::endl;
    }
    std::cout << "f(n) = 1/n!" << std::endl;
    for(auto i = 0u; i < b_d.size(); i++) {
        std::cout << "b_" << i << " = " << b_d[i] << std::endl;
    }

    return 0;
}
