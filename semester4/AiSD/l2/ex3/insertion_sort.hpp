#pragma once
#include <vector>
#include <utility>
#include "common.hpp"

auto insertion_sort(std::vector<int>& numbers) -> std::pair<int,int> {
    auto n = numbers.size();
    auto comparisons = 0;
    auto swaps = 0;

    for (int i = 1; i < n; ++i) {
        int key = numbers[i];
        int j = i - 1;

        while (j >= 0 && numbers[j] > key) {
            comparisons++;
            swaps++;
            numbers[j + 1] = numbers[j];
            j = j - 1;
        }

        if (j >= 0) {
            comparisons++;
        }

        numbers[j + 1] = key;
    }

    return {comparisons, swaps};
}


