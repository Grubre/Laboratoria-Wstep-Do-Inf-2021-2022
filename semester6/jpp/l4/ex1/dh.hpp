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

        T generator;
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<typename T::value_type> dis(1, p - 1);

        bool is_generator = false;
        while (!is_generator) {
            is_generator = true;
            generator = T{dis(gen)};
            for (auto q : divisors) {
                if (generator * ((p - 1) / q) == T{1}) {
                    is_generator = false;
                    break;
                }
            }
        }

        return generator;
    }

    static constexpr auto divisors = get_prime_divisors<T::characteristic() - 1>();
    T generator;
};

template <typename T> class User {
  public:
    using secret_type = T::value_type;
    User(DHSetup<T> &setup) : setup(setup) {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<secret_type> dis(1, T::characteristic() - 1);

        secret = dis(gen);
    }

    auto getPublicKey() const -> T { return setup.power(setup.getGenerator(), secret); }
    void setKey(T a) { key = setup.power(a, secret); }
    auto encrypt(T m) -> T { return m * key; }
    auto decrypt(T c) -> T { return c / key; }

  private:
    DHSetup<T> &setup;
    secret_type secret;
    T key;
};
