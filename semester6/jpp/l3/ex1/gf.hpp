#pragma once
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

template <auto p, typename T = unsigned int>
  requires(is_prime(p) && std::unsigned_integral<T>)
struct GF {
private:
  T value;

public:
  GF() = default;
  GF(T value) : value(value % p) {}

  auto characteristic() const -> T { return p; }

  auto inverse() const -> GF {
    auto a = value;
    auto b = p;
    T x = 0, y = 1, x0 = 1, y0 = 0, q, t;
    while (b != 0) {
      q = a / b;
      t = a % b;
      a = b;
      b = t;
      t = x;
      x = x0 - q * x;
      x0 = t;
      t = y;
      y = y0 - q * y;
      y0 = t;
    }
    return GF{x0};
  }

  explicit operator T() const { return value; }
  auto operator<=>(const GF &) const = default;

  auto operator-() const -> GF { return GF(p - value); }
  auto operator+(const GF &rhs) const -> GF {
    return GF((value + rhs.value) % p);
  }
  auto operator-(const GF &rhs) const -> GF {
    return GF(((value + p) - rhs.value) % p);
  }
  auto operator*(const GF &rhs) const -> GF {
    return GF((value * rhs.value) % p);
  }
  auto operator/(const GF &rhs) const -> GF {
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
    value = (value * rhs.inverse().value) % p;
    return *this;
  }

  friend std::ostream &operator<<(std::ostream &os, const GF &gf) {
    return os << gf.value;
  }
};
