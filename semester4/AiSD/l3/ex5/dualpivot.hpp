#pragma once
#include <random>
#include <algorithm>
#include <vector>
#include <iostream>
#include "../common.hpp"
#include "select_index.hpp"

inline auto print_vector(const std::vector<int>& numbers) -> void {
    std::cout << "(";
    for(int i = 0; i < numbers.size(); i++) {
        std::cout << numbers[i];
        if(i < numbers.size() - 1) {
            std::cout << ", ";
        }
    }
    std::cout << ")";
}


// SELECT DUAL PIVOT

inline auto select_dual_pivot_quick_sort_helper(std::vector<int>& numbers, int left, int right, Counters& counters) -> void {
    if (left < right) {
        int mid = left + (right - left) / 2;

        // Select two pivots using the select function
        int pivot1_index = select_index(numbers, left, mid, mid / 2);
        int pivot2_index = select_index(numbers, mid + 1, right, mid + (right - mid) / 2);

        int pivot1 = numbers[pivot1_index];
        int pivot2 = numbers[pivot2_index];

        // Move pivots to correct positions
        std::swap(numbers[left], numbers[pivot1_index]);
        std::swap(numbers[right], numbers[pivot2_index]);
        counters.swaps += 2;

        int i = left + 1;
        int lt = left + 1;
        int gt = right - 1;

        while (i <= gt) {
            counters.comparisons++;
            if (numbers[i] < pivot1) {
                std::swap(numbers[i], numbers[lt]);
                counters.swaps++;
                ++i;
                ++lt;
            } else if (numbers[i] > pivot2) {
                std::swap(numbers[i], numbers[gt]);
                counters.swaps++;
                --gt;
            } else {
                ++i;
            }
        }

        --lt;
        ++gt;

        std::swap(numbers[left], numbers[lt]);
        std::swap(numbers[right], numbers[gt]);
        counters.swaps += 2;

        select_dual_pivot_quick_sort_helper(numbers, left, lt - 1, counters);
        select_dual_pivot_quick_sort_helper(numbers, lt + 1, gt - 1, counters);
        select_dual_pivot_quick_sort_helper(numbers, gt + 1, right, counters);
    }
}

inline auto select_dual_pivot_quick_sort(std::vector<int>& numbers) -> std::pair<int, int> {
    Counters counters;
    select_dual_pivot_quick_sort_helper(numbers, 0, numbers.size() - 1, counters);
    return std::make_pair(counters.comparisons, counters.swaps);
}



//REGULAR DUAL PIVOT

inline auto dual_pivot_quick_sort_helper(std::vector<int>& numbers, int left, int right) -> std::pair<int, int> {
    int comparisons = 0;
    int swaps = 0;

    if (left < right) {
        ++comparisons;
        if (numbers[left] > numbers[right]) {
            std::swap(numbers[left], numbers[right]);
            ++swaps;
        }

        int pivot1 = numbers[left];
        int pivot2 = numbers[right];

        int i = left + 1;
        int lt = left + 1;
        int gt = right - 1;

        while (i <= gt) {
            ++comparisons;
            if (numbers[i] < pivot1) {
                std::swap(numbers[i], numbers[lt]);
                ++swaps;
                ++i;
                ++lt;
            } else if (numbers[i] > pivot2) {
                std::swap(numbers[i], numbers[gt]);
                ++swaps;
                --gt;
            } else {
                ++i;
            }
        }

        --lt;
        ++gt;

        std::swap(numbers[left], numbers[lt]);
        std::swap(numbers[right], numbers[gt]);
        swaps += 2;

        auto left_stats = dual_pivot_quick_sort_helper(numbers, left, lt - 1);
        auto middle_stats = dual_pivot_quick_sort_helper(numbers, lt + 1, gt - 1);
        auto right_stats = dual_pivot_quick_sort_helper(numbers, gt + 1, right);

        comparisons += left_stats.first + middle_stats.first + right_stats.first;
        swaps += left_stats.second + middle_stats.second + right_stats.second;
    }

    return {comparisons, swaps};
}

inline auto dual_pivot_quick_sort(std::vector<int>& numbers) -> std::pair<int, int> {
    return dual_pivot_quick_sort_helper(numbers, 0, numbers.size() - 1);
}


