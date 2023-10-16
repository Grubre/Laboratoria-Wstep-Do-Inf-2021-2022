#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <optional>
#include <limits>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>
#include <array>
#include "common.hpp"

size_t count_codepoints(const u8str &str)
{
    size_t count = 0;
    for (auto &c : str)
        if ((c & 0b1100'0000) != 0b1000'0000)
            ++count;
    return count;
}

struct PairHash {
    template <class T1, class T2>
    std::size_t operator () (const std::pair<T1, T2>& p) const {
        auto h1 = std::hash<T1>{}(p.first);
        auto h2 = std::hash<T2>{}(p.second);
        return h1 ^ (h2 << 1);
    }
};

bool is_suffix(const u8str& str, const u8str& suffix) {
    if (suffix.length() > str.length())
        return false;

    auto str_offset = str.length() - suffix.length();
    return str.compare(str_offset, std::string::npos, suffix) == 0;
}

struct Automaton {
    Automaton(std::size_t m) : m(m) {}
    std::size_t m;
    std::unordered_map<std::pair<std::size_t, ch>, std::size_t, PairHash> delta{};
    std::size_t operator()(std::size_t q, ch T) {
        return delta.at({q,T});
    }
    void print() const {
        std::cout << "accepting state: " << m << std::endl;
        for(auto const&[q, t] : delta) {
            // std::cout << "(" << q.first << ", '" << q.second << "') -> " << t << std::endl;
        }
    }
};

auto finite_automaton_matcher(const u8str& T, Automaton delta) -> std::optional<std::size_t> {
    const auto n = count_codepoints(T);
    const auto m = delta.m;
    auto q = std::size_t{0};
    for(auto i = 0u; i < n; i++) {
        q = delta(q, T[i]);
        if(q == m) {
            return i - m + 1;
        }
    }

    return std::nullopt;
}

auto compute_transition_function(const u8str& P, const std::vector<ch>& alphabet) -> Automaton {
    const auto m = count_codepoints(P);
    auto automaton = Automaton{m};
    for(std::size_t q = 0; q <= m; q++) {
        for(auto a : alphabet) {
            auto rhs = P.substr(0, q);
            rhs += a;
            auto k = std::min(m + 1, q + 2);
            while (k > 0) {
                --k;
                const auto lhs = P.substr(0, k);

                if (is_suffix(rhs, lhs))
                    break;
            }
            automaton.delta[{q, a}] = k;
        }
    }
    return automaton;
}

auto get_alphabet(const u8str& T) -> std::vector<ch> {
    auto alphabet_set = std::unordered_set<ch>{};

    for (ch c : T) {
        alphabet_set.insert(c);
    }

    auto alphabet = std::vector<ch>(alphabet_set.begin(), alphabet_set.end());

    std::cout << "alphabet: {";
    for(auto i = 0u; i < alphabet.size(); i++) {
        // std::cout << alphabet[i];
        if(i < alphabet.size() - 1)
            std::cout << ", ";
    }
    std::cout << "}" << std::endl;

    return alphabet;
}

auto main(int argc, char **argv) -> int {
    auto [P, T] = get_args_u32(argc, argv);
    // std::cout << "T: " << std::quoted(T) << std::endl;
    // std::cout << "P: " << std::quoted(P) << std::endl;
    auto alphabet = get_alphabet(T);
    auto delta = compute_transition_function(P, alphabet);
    std::cout << "computed transition function:" << std::endl;
    delta.print();
    auto match = finite_automaton_matcher(T, delta);
    if(!match) {
        std::cout << "match not found." << std::endl;
    } else {
        std::cout << "match at index: " << *match << std::endl;
        print_match(std::string{T.begin(), T.end()}, *match, count_codepoints(P));
    }
    return 0;
}
