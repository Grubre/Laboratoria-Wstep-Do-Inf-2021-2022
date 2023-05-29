#include <iostream>
#include <fstream>
#include <random>
#include <chrono>
#include "common.hpp"

auto main() -> int {
    std::ofstream out("data/ex3.csv");
    out << "n,group_size,comparisons,swaps\n";

    for (int n = 100; n <= 10000; n += 100) {
        std::cout << n << std::endl;
        for (int group_size = 3; group_size <= 9; group_size += 2) {
            Counters total_counters;
            int reps = 10000;
            for (int rep = 0; rep < reps; ++rep) {
                auto arr = generate_numbers(n);
                Counters counters;
                select(arr, 0, n - 1, n / 2, group_size, counters);
                total_counters.comparisons += counters.comparisons;
                total_counters.swaps += counters.swaps;
            }
            out << n << ',' << group_size << ',' << (total_counters.comparisons / reps) << ',' << (total_counters.swaps / reps) << '\n';
        }
    }

    out.close();
    return 0;
}
