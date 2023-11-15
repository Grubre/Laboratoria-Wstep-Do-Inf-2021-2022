#include <iomanip>
#include <ios>
#include <iostream>
#include <random>
#include <fstream>
#include <string>

auto generate_random_string(unsigned int n, const std::string& a, const std::string& b, double f) -> std::string {
    auto result = std::string{};
    auto rd = std::random_device{};
    auto gen = std::mt19937(rd());
    auto dis = std::bernoulli_distribution(f);

    for (auto i = 0u; i < n; ++i) {
        auto choice = dis(gen);
        result += choice ? a : b;
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

auto main(int argc, char **argv) -> int {
    if (argc < 2) {
        std::cerr << "you must provide probability from interval [0,1] of string a occuring." << std::endl;
        return 1;
    }
    const double prob = std::stod(argv[1]);
    if (0.0 > prob || prob > 1.0 ) {
        std::cerr << "probability must be in the interval [0, 1]" << std::endl;
        return 1;
    }

    const auto n_min = 5;
    const auto n_max = 50;
    const auto k_max = 10'000;
    const auto a = std::string{"a"};
    const auto b = std::string{"b"};
    const auto pattern_aaa = a + a + a;
    const auto pattern_abb = a + b + b;

    auto output = std::ofstream("data.csv");
    output << "n," << "aaa," << "abb," << "avg_aaa" << std::endl;
    
    for(auto n = n_min; n < n_max; n++) {
        std::cout << "n: " << n << std::endl;
        auto aaa_cnt = 0u;
        auto abb_cnt = 0u;
        auto avg_aaa = 0.0;
        for(auto k = 0; k < k_max; k++) {
            const auto str = generate_random_string(n, a, b, prob);
            auto aaa_occ = count_substr_occurences(str, pattern_aaa);
            auto abb_occ = count_substr_occurences(str, pattern_abb);
            aaa_cnt += aaa_occ > 0;
            abb_cnt += abb_occ > 0;
            avg_aaa += aaa_occ;
        }
        output << n << "," << aaa_cnt << "," << abb_cnt << "," << avg_aaa / k_max << std::endl;
    }
    return 0;
}
