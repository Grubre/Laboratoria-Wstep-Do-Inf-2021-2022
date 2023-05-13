#include "common.hpp"
#include <iostream>
#include <vector>
#include <stack>

auto partition(std::vector<int>& numbers, int l, int r, int& comparisons, int& swaps) -> int{
    int pivot = numbers[r];
    int i = l - 1;

    for (int j = l; j <= r - 1; ++j) {
        comparisons++;
        if (numbers[j] < pivot) {
            swaps++;
            i++;
            std::swap(numbers[i], numbers[j]);
        }
    }
    swaps++;
    std::swap(numbers[i + 1], numbers[r]);

    return i + 1;
}

void quick_sort_helper(std::vector<int>& numbers, int l, int r, int& comparisons, int& swaps) {
    if (l < r) {
        int pi = partition(numbers, l, r, comparisons, swaps);

        quick_sort_helper(numbers, l, pi - 1, comparisons, swaps);
        quick_sort_helper(numbers, pi + 1, r, comparisons, swaps);
    }
}

auto quick_sort(std::vector<int>& numbers) -> std::pair<int, int> {
    int comparisons = 0, swaps = 0;
    quick_sort_helper(numbers, 0, numbers.size() - 1, comparisons, swaps);
    return std::make_pair(comparisons, swaps);
}


auto main(int argc, char** argv) -> int {
    for(int n = 10; n <= 200; n+=10) {
        for(int i = 0; i < k; i++) {
            auto numbers = random_vec(n);
            auto[comparisons, swaps] = quick_sort(numbers);
            std::cout << n << " " << comparisons << " " << swaps << std::endl;
        }
    }

    return 0;
}
