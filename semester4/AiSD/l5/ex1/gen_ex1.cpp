#include "lcs.hpp"

#include <chrono>
#include <fstream>
#include <random>
#include <string>
#include <vector>

auto generate_random_string(int n) -> std::string{
    static constexpr char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";

    std::random_device rd; // obtain a random number from hardware
    std::mt19937 eng(rd()); // seed the generator
    std::uniform_int_distribution<> distr(0, sizeof(alphanum) - 2); // define the range

    std::string result;
    for (int i = 0; i < n; ++i) {
        result.push_back(alphanum[distr(eng)]);
    }
    return result;
}

auto main() -> int {
    std::ofstream out("times.csv");
    out << "n,time\n"; // write header to CSV file
    const auto reps = 50;

    for (int n = 1000; n <= 5000; n += 1000) {
        auto accumulator = 0.0;
        for(int k = 0; k < reps; k++) {
            std::string str1 = generate_random_string(n);
            std::string str2 = generate_random_string(n);

            auto start = std::chrono::high_resolution_clock::now();
            auto lcs = longest_common_subsequence(str1, str2);
            auto end = std::chrono::high_resolution_clock::now();

            auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
            accumulator += duration;
        }
        out << n << "," << accumulator / (double)reps << "\n"; // write n and duration to CSV file
    }

    out.close();
    return 0;
}

