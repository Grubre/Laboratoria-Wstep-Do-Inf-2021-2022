#include <iostream>
#include <vector>
#include <utility>
#include "insertion_sort.hpp"

auto main(int argc, char** argv) -> int {
    for(int n = 10; n <= 200; n+=10) {
        for(int i = 0; i < k; i++) {
            auto numbers = random_vec(n);
            auto[comparisons, swaps] = insertion_sort(numbers);
            std::cout << n << " " << comparisons << " " << swaps << std::endl;
        }
    }

    return 0;
}
