#include <print>
#include <random>

struct ExtendedEuclideanType {
    std::int64_t gcd;
    std::int64_t x;
    std::int64_t y;
};

using Key = std::pair<std::int64_t, std::int64_t>;

struct RSAKeys {
    Key pk;
    Key sk;
};

constexpr auto is_prime(std::int64_t n) -> bool {
    if (n <= 1) return false;
    if (n <= 3) return true;
    if (n % 2 == 0 || n % 3 == 0) return false;
    for (auto i = 5; i * i <= n; i += 6) {
        if (n % i == 0 || n % (i + 2) == 0) return false;
    }
    return true;
}

constexpr auto gcd(std::int64_t a, std::int64_t b) -> decltype(a) {
    while (b != 0) {
        auto t = b;
        b = a % b;
        a = t;
    }
    return a;
}

auto extended_euclidean(std::int64_t a, std::int64_t b) -> ExtendedEuclideanType {
    auto t = 0;
    auto r = b;
    auto tt = 1;
    auto rt = a;

    while (rt != 0) {
        auto q = r / rt;
        auto ttt = t - q * tt;
        t = tt;
        tt = ttt;
        auto rrt = r - q * rt;
        r = rt;
        rt = rrt;
    }

    if (t < 0) {
        t += b;
    }

    return {r, t, (r - a * t) / b};
}

auto get_e(std::int64_t phi) -> decltype(phi) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<std::uint32_t> dis(1, phi);
    auto e = dis(gen);

    while (gcd(e, phi) != 1) {
        e = dis(gen);
    }

    return e;
}

void print_key(const RSAKeys& keys) {
    std::println("Public key: (n: {}, e: {})", keys.pk.first, keys.pk.second);
    std::println("Secret key: (n: {}, d: {})", keys.sk.first, keys.sk.second);
}

auto generate_keys(std::int64_t p, std::int64_t q) -> RSAKeys {
    const auto n = p * q;
    const auto phi = (p - 1) * (q - 1);
    const auto e = get_e(phi);
    const auto d = extended_euclidean(e, phi).x;

    return {{n, e}, {n, d}};
}

auto pow(std::int64_t a, std::int64_t b, std::int64_t n) -> decltype(a) {
    auto res = 1;
    while (b > 0) {
        if (b % 2 == 1) {
            res = (res * a) % n;
        }
        a = (a * a) % n;
        b /= 2;
    }
    return res;
}

auto nsp_to_pq(std::int64_t n, std::int64_t s, std::int64_t p) -> std::pair<decltype(n), decltype(n)> {
    const auto kphi = s * p - 1;
    auto t = kphi;

    while (t % 2 == 0) {
        t /= 2;
    }

    auto a = 2;
    while (true) {
        auto k = t;
        while (k < kphi) {
            auto x = pow(a, k, n);
            if (x != 1 and x != (n - 1) and (x * x) % n == 1) {
                auto p = gcd(x - 1, n);
                return {p, n / p};
            }
            k *= 2;
        }
        a += 2;
    }
}


auto main() -> int {
    const std::uint32_t p = 5737, q = 6547;
    const auto n = p * q;
    const auto phi = (p - 1) * (q - 1);
    // const auto A = RSAKeys{pk};
    // const auto B = generate_keys(p, q);

    // std::println("A:");
    // print_key(A);
    // std::println("B:");
    // print_key(B);

    const auto e_a = 17;
    const auto e_b = 23;
    const auto d_b = extended_euclidean(e_b, phi).x;
    const auto [p1, q1] = nsp_to_pq(n, d_b, e_b);
    auto nphi = (p1 - 1) * (q1 - 1);
    auto sb = extended_euclidean(e_a, nphi).x;

    std::println("p: {}, q: {}", p1, q1);
    std::println("Secret key: {}", sb);
    std::println("d_b: {}", d_b);

    return 0;
}
