#include "common.hpp"
#include <fstream>

auto main() -> int {
    std::ofstream output_file("data/ex2.csv");
    output_file << "n,algorithm,repetition,comparisons,swaps\n";

    for (auto n = 100; n <= 10000; n += 100) {
        std::cout << "n: " << n << std::endl;
        for (auto rep = 0; rep < 100000; ++rep) {
            auto nums = generate_numbers(n);
            auto k = n / 2;  // Or choose a different k if you prefer

            Counters counters;
            select(nums, 0, n - 1, k - 1, counters);
            output_file << n << ",quickselect," << rep << "," << counters.comparisons << "," << counters.swaps << "\n";

            counters = {0, 0};  // reset counters
            randomized_select(nums, 0, n - 1, k - 1, counters);
            output_file << n << ",randomized_select," << rep << "," << counters.comparisons << "," << counters.swaps << "\n";
        }
    }

    output_file.close();
    return 0;
}
