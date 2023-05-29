#pragma once
#include <iostream>
#include <vector>
#include <stack>
#include "../common.hpp"
#include "select_index.hpp"

// SELECT QS
auto select_qs_partition(std::vector<int>& numbers, int l, int r, Counters& counters) -> int{
    int pivot_index = select_index(numbers, l, r, (r - l) / 2);
    int pivot = numbers[pivot_index];

    std::swap(numbers[pivot_index], numbers[r]);  // move pivot to end
    counters.swaps++;

    int i = l - 1;

    for (int j = l; j <= r - 1; ++j) {
        counters.comparisons++;
        if (numbers[j] < pivot) {
            counters.swaps++;
            i++;
            std::swap(numbers[i], numbers[j]);
        }
    }
    counters.swaps++;
    std::swap(numbers[i + 1], numbers[r]);

    return i + 1;
}

void select_quick_sort_helper(std::vector<int>& numbers, int l, int r, Counters& counters) {
    if (l <= r) {
        int pi = select_qs_partition(numbers, l, r, counters);

        select_quick_sort_helper(numbers, l, pi - 1, counters);
        select_quick_sort_helper(numbers, pi + 1, r, counters);
    }
}

auto select_quick_sort(std::vector<int>& numbers) -> std::pair<int, int> {
    Counters counters;
    select_quick_sort_helper(numbers, 0, numbers.size() - 1, counters);
    return std::make_pair(counters.comparisons, counters.swaps);
}

// REGULAR QS
auto qs_partition(std::vector<int>& numbers, int l, int r, Counters& counters) -> int{
    int pivot = numbers[r];
    int i = l - 1;

    for (int j = l; j <= r - 1; ++j) {
        counters.comparisons++;
        if (numbers[j] < pivot) {
            counters.swaps++;
            i++;
            std::swap(numbers[i], numbers[j]);
        }
    }
    counters.swaps++;
    std::swap(numbers[i + 1], numbers[r]);

    return i + 1;
}

void quick_sort_helper(std::vector<int>& numbers, int l, int r, Counters& counters) {
    if (l < r) {
        int pi = qs_partition(numbers, l, r, counters);

        quick_sort_helper(numbers, l, pi - 1, counters);
        quick_sort_helper(numbers, pi + 1, r, counters);
    }
}

auto quick_sort(std::vector<int>& numbers) -> std::pair<int, int> {
    Counters counters;
    quick_sort_helper(numbers, 0, numbers.size() - 1, counters);
    return std::make_pair(counters.comparisons, counters.swaps);
}

