#include "common.hpp"
#include <iomanip>
#include <optional>
#include <vector>

auto compute_prefix_function(const std::string& P) {
    const auto m = P.size();
    auto pi = std::vector<std::size_t>(m);
    pi[0] = 0;
    auto k = 0;
    for(auto q = 1u; q < m; q++) {
        while(k > 0 && P[k] != P[q]) {
            k = pi[k - 1];
        }
        if (P[k] == P[q]) {
            k++;
        }
        pi[q] = k;
    }
    return pi;
}

auto kmp_matcher(const std::string& T, const std::string& P) -> std::optional<std::size_t> {
    const auto n = T.size();
    const auto m = P.size();
    auto pi = compute_prefix_function(P);
    auto q = 0llu;
    for(auto i = 0llu; i < n; i++) {
        while (q > 0 && P[q] != T[i]) {
            q = pi[q - 1];
        }
        if (P[q] == T[i]) {
            q++;
        }
        if (q == m) {
            return i - m + 1;
        }
    }
    return std::nullopt;
}

auto main(int argc, char **argv) -> int {
    auto [P, T] = get_args(argc, argv);
    std::cout << "T <- " << std::quoted(T) << std::endl;
    std::cout << "P <- " << std::quoted(P) << std::endl;
    auto pi = compute_prefix_function(P);
    for(auto i = 0u; i < pi.size(); i++) {
        std::cout << "π('" << P[i] << "') = " << pi[i] << std::endl;
    }
    auto match = kmp_matcher(T, P);
    if(match) {
        std::cout << "match at: " << *match << std::endl;
        print_match(T, *match, P.size());
    } else {
        std::cout << "no match." << std::endl;
    }
    return 0;
}
