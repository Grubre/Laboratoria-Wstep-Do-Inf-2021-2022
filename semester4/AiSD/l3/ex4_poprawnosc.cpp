#include <iostream>
#include <vector>
#include <chrono>
#include <random>
#include <fstream>
#include <string>
#include <algorithm>
#include "common.hpp"

auto main() -> int {
    int n = 30;

    auto vec = std::vector<int>(n);

    std::iota(vec.begin(), vec.end(), 1);

    std::cout << "array: ";

    for(int i = 0; i < n; i++) {
        std::cout << vec[i];
        if(i == n - 1) {
            std::cout << "\n";
        } else {
            std::cout << ", ";
        }
    }

    for(auto v : {0, 1, 30, 15}) {
        std::cout << "v = '" << v << "':" << std::endl;

        int comparisons = 0;
        auto bin = binary_search(vec, v, comparisons);

        std::cout << "value found: " << ((bin == true) ? "yes" : "no") << std::endl;
        std::cout << "comparisons: " << comparisons << std::endl;
    }

    return 0;
}
