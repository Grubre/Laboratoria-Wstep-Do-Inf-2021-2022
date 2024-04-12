#pragma once

#include <cassert>
#include <utility>
#include <ostream>

struct Gauss {
    int a;
    int b;
};

inline auto operator<<(std::ostream& os, const Gauss& z) -> std::ostream& {
    os << z.a;
    os << "+";
    os << z.b << "i";

    return os;
}

inline int N(const Gauss& z) {
    return z.a * z.a + z.b * z.b;
}

inline Gauss operator+(const Gauss& lhs, const Gauss& rhs) {
    return {lhs.a + rhs.a, lhs.b + rhs.b};
}

inline Gauss operator-(const Gauss& lhs, const Gauss& rhs) {
    return {lhs.a - rhs.a, lhs.b - rhs.b};
}

inline Gauss operator*(const Gauss& lhs, const Gauss& rhs) {
    return {lhs.a * rhs.a - lhs.b * rhs.b, lhs.a * rhs.b + lhs.b * rhs.a};
}

inline bool operator==(const Gauss& lhs, const Gauss& rhs) {
    return lhs.a == rhs.a && lhs.b == rhs.b;
}

inline bool operator!=(const Gauss& lhs, const Gauss& rhs) {
    return !(lhs == rhs);
}

inline auto div_with_remainder(const Gauss& lhs, const Gauss& rhs) -> std::pair<Gauss, Gauss> {
    const auto n_rhs = N(rhs);
    assert(n_rhs != 0);

    const auto re = (lhs.a * rhs.a + lhs.b * rhs.b) / n_rhs;
    const auto im = (lhs.b * rhs.a - lhs.a * rhs.b) / n_rhs;
    const auto q = Gauss{a: re, b: im};
    const auto r = lhs - rhs * q;
    return {q, r};
}

inline auto gcd(Gauss a, Gauss b) -> Gauss {
    while(N(b) != 0) {
        const auto r = div_with_remainder(a, b).second;
        a = b;
        b = r;
    }

    return a;
}

inline auto lcm(const Gauss& a, const Gauss& b) -> Gauss {
    return div_with_remainder(a*b, gcd(a,b)).first;
}