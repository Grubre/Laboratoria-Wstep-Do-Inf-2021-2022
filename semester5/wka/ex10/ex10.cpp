#include <iomanip>
#include <iostream>
#include <random>

auto generate_random_string(unsigned int n, const std::string& a, const std::string& b) -> std::string {
    auto result = std::string{};
    auto rd = std::random_device{};
    auto gen = std::mt19937(rd());
    auto dis = std::uniform_int_distribution<>(0, 1);

    for (auto i = 0u; i < n; ++i) {
        result += (dis(gen) == 0) ? a : b;
    }

    return result;
}

auto count_substr_occurences(const std::string& str, const std::string& pattern) -> unsigned int {
    auto occ = 0u;
    for(auto i = 0u; i < str.size(); i++) {
        auto j = 0u;
        while(j < pattern.size() && i + j < str.size() && str[i + j] == pattern[j]) {
            j++;
        }
        if(j >= pattern.size()) {
            occ++;
        }
    }
    return occ;
}

auto main() -> int {
    const auto str = generate_random_string(5, "a", "b");
    const auto pattern = "ab";
    std::cout << "str: " << std::quoted(str) << ", pattern: " << std::quoted(pattern) << std::endl;
    std::cout << "number of occurences: " << count_substr_occurences(str, pattern) << std::endl;
    return 0;
}
