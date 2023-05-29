#include <iostream>
#include <vector>
#include <chrono>
#include <random>
#include <fstream>
#include <string>
#include "common.hpp"

auto main() -> int {
    std::ofstream fixed_values_file("data/ex4.csv");
    fixed_values_file << "n,v,comparisons,time\n";

    std::ofstream random_values_file("data/ex4_random.csv");
    random_values_file << "n,v,comparisons,time\n";

    std::random_device rd;
    std::mt19937 gen(rd());

    const int num_trials = 1000000;


    for (int n = 1000; n <= 100000; n += 1000) {
        std::vector<int> nums(n);
        for (int i = 1; i <= n; ++i) {
            nums[i - 1] = i;
        }

        std::vector<std::pair<int, std::string>> values = {
            {1, "1"},
            {n - 1, "n-1"},
            {n + 1, "n+1"}
        };

        for (const auto& [value, label] : values) {
            int total_comparisons = 0;   // Changed this line
            auto start_time = std::chrono::high_resolution_clock::now();
            for (int trial = 0; trial < num_trials; ++trial) {   // Adding this loop
                int comparisons = 0;
                bool found = binary_search(nums, value, comparisons);
                total_comparisons += comparisons;   // Moved this line into the loop
            }
            auto end_time = std::chrono::high_resolution_clock::now();
            auto time_taken = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time).count();

            fixed_values_file << n << "," << label << "," << total_comparisons / num_trials << "," << time_taken / num_trials << "\n";   // Changed this line
        }

        // n/2 +/- offset
        std::uniform_int_distribution<> distr_offset(-n/16, n/16);
        long long total_comparisons = 0;
        long long total_time = 0;
        for (int i = 0; i < 100000; ++i) {
            int offset = distr_offset(gen);
            int value = n / 2 + offset;
            int comparisons = 0;
            auto start_time = std::chrono::high_resolution_clock::now();
            bool found = binary_search(nums, value, comparisons);
            auto end_time = std::chrono::high_resolution_clock::now();
            auto time_taken = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time).count();

            total_comparisons += comparisons;
            total_time += time_taken;
        }
        fixed_values_file << n << ",n/2," << total_comparisons / 100000 << "," << total_time / 100000 << "\n";

        // Random v
        std::uniform_int_distribution<> distr(0, n - 1);
        for (int i = 0; i < 1000000; ++i) {
            int value = distr(gen);
            int comparisons = 0;
            auto start_time = std::chrono::high_resolution_clock::now();
            bool found = binary_search(nums, value, comparisons);
            auto end_time = std::chrono::high_resolution_clock::now();
            auto time_taken = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time).count();

            random_values_file << n << ",random," << comparisons << "," << time_taken << "\n";
        }
    }

    fixed_values_file.close();
    random_values_file.close();

    return 0;
}
