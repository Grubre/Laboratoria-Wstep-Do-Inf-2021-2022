#pragma once
#include <cstdint>
#include <ostream>

consteval auto is_prime(auto n) -> bool {
    if (n <= 1)
        return false;
    for (auto i = 2; i * i <= n; i++) {
        if (n % i == 0)
            return false;
    }
    return true;
}

template <auto p, typename T = std::uint64_t>
    requires(is_prime(p) && std::unsigned_integral<T>)
struct GF {
  private:
    T value;

  public:
    GF() = default;
    GF(T value) : value(value % p) {}

    auto characteristic() const -> T { return p; }

    auto inverse() const -> GF {
        if (value == 0) {
            throw std::runtime_error("Inverse of zero does not exist");
        }

        T t = 0, r = p, tt = 1, rt = value;

        while (rt != 0) {
            auto q = r / rt;

            auto temp = t - q * tt;
            t = tt;
            tt = temp;

            temp = r - q * rt;
            r = rt;
            rt = temp;
        }

        if (t < 0) {
            t += p;
        }

        return GF(t);
    }

    explicit operator T() const { return value; }
    auto operator<=>(const GF &) const = default;

    auto operator-() const -> GF { return GF(p - value); }
    auto operator+(const GF &rhs) const -> GF { return GF((value + rhs.value) % p); }
    auto operator-(const GF &rhs) const -> GF { return GF(((value + p) - rhs.value) % p); }
    auto operator*(const GF &rhs) const -> GF { return GF((value * rhs.value) % p); }
    auto operator/(const GF &rhs) const -> GF {
        if (rhs.value == 0) {
            throw std::runtime_error("Division by zero");
        }

        return GF((value * rhs.inverse().value) % p);
    }

    auto operator+=(const GF &rhs) -> GF & {
        value = (value + rhs.value) % p;
        return *this;
    }
    auto operator-=(const GF &rhs) -> GF & {
        value = ((value + p) - rhs.value) % p;
        return *this;
    }
    auto operator*=(const GF &rhs) -> GF & {
        value = (value * rhs.value) % p;
        return *this;
    }
    auto operator/=(const GF &rhs) -> GF & {
        if (rhs.value == 0) {
            throw std::runtime_error("Division by zero");
        }

        value = (value * rhs.inverse().value) % p;
        return *this;
    }

    friend std::ostream &operator<<(std::ostream &os, const GF &gf) { return os << gf.value; }
};
