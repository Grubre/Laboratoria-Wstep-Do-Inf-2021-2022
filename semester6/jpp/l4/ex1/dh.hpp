#pragma once
#include "gf.hpp"
#include <random>

template <typename T> class DHSetup {
  public:
    DHSetup() : generator(get_random_generator()) {}
    auto getGenerator() const -> T { return generator; }
    auto power(T a, unsigned long b) -> T {
        T result = T{1};
        while (b > 0) {
            if (b % 2 == 1) {
                result = result * a;
            }
            a = a * a;
            b /= 2;
        }
        return result;
    }

  private:
    // used by the constructor to generate a random generator
    constexpr auto get_random_generator() -> T {
        static constexpr auto p = T::characteristic();
        static constexpr auto divisors = get_prime_divisors<p - 1>();

        T generator;
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<typename T::value_type> dis(1, p - 1);

        while (true) {
            generator = T{dis(gen)};
            bool is_generator = true;
            for (auto q : divisors) {
                if (generator * ((p - 1) / q) == T{1}) {
                    is_generator = false;
                    break;
                }
            }

            if (is_generator) {
                break;
            }
        }

        return generator;
    }

    T generator;
};

template <typename T> class User {
  public:
    User(DHSetup<T> &setup);

    auto getPublicKey() -> T;
    void setKey(T a);
    auto encrypt(T m) -> T;
    auto decrypt(T c) -> T;

  private:
};
