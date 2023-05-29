#pragma once
#include <iostream>
#include <vector>
#include <algorithm>
#include <random>

struct Counters {
    int swaps = 0;
    int comparisons = 0;
};

auto partition(std::vector<int>& nums, int low, int high, int pivot_index, Counters& counters) -> int {
    auto pivot = nums[pivot_index];
    std::swap(nums[pivot_index], nums[high]);  // move pivot to end
    counters.swaps++;  // swap counter

    int i = low;
    for (int j = low; j < high; ++j) {
        counters.comparisons++;  // comparison counter
        if (nums[j] <= pivot) {
            std::swap(nums[i], nums[j]);
            counters.swaps++;  // swap counter
            i++;
        }
    }

    std::swap(nums[i], nums[high]);  // move pivot to its final place
    counters.swaps++;  // swap counter
    return i;
}

auto select(std::vector<int>& nums, int low, int high, int index, int group_size, Counters& counters) -> int {
    if (low == high) {
        counters.comparisons++;
        return nums[low];
    }

    // Divide the array into groups of size 'group_size'
    int n = high - low + 1;
    std::vector<int> medians((n + group_size - 1) / group_size);  // Holds the medians of the groups
    for (int i = 0; i < medians.size(); ++i) {
        counters.comparisons++;
        int group_start = low + i * group_size;
        int group_end = std::min(group_start + group_size, high + 1);
        std::sort(nums.begin() + group_start, nums.begin() + group_end);
        medians[i] = nums[group_start + (group_end - group_start - 1) / 2];  // Median of the group
    }

    // Recursively find the median of medians
    int median_of_medians = select(medians, 0, medians.size() - 1, medians.size() / 2, group_size, counters);

    // Find the index of the median of medians in the original nums array
    int pivot_index = std::distance(nums.begin(), std::find(nums.begin() + low, nums.begin() + high + 1, median_of_medians));

    int pivot_position = partition(nums, low, high, pivot_index, counters);
    if (index == pivot_position) {
        counters.comparisons++;
        return nums[pivot_position];
    } else if (index < pivot_position) {
        counters.comparisons++;
        return select(nums, low, pivot_position - 1, index, group_size, counters);
    } else {
        counters.comparisons++;
        return select(nums, pivot_position + 1, high, index, group_size, counters);
    }
}

auto select(std::vector<int>& nums, int low, int high, int index, Counters& counters) -> int {
    if (low == high) {
        return nums[low];
    }

    // Divide the array into groups of 5
    int n = high - low + 1;
    std::vector<int> medians((n + 4) / 5);  // Holds the medians of the groups
    for (int i = 0; i < medians.size(); ++i) {
        int group_start = low + i * 5;
        int group_end = std::min(group_start + 5, high + 1);
        std::sort(nums.begin() + group_start, nums.begin() + group_end);
        medians[i] = nums[group_start + (group_end - group_start - 1) / 2];  // Median of the group
    }

    // Recursively find the median of medians
    int median_of_medians = select(medians, 0, medians.size() - 1, medians.size() / 2, counters);

    // Find the index of the median of medians in the original nums array
    int pivot_index = std::distance(nums.begin(), std::find(nums.begin() + low, nums.begin() + high + 1, median_of_medians));

    int pivot_position = partition(nums, low, high, pivot_index, counters);
    if (index == pivot_position) {
        return nums[pivot_position];
    } else if (index < pivot_position && low < pivot_position) {
        return select(nums, low, pivot_position - 1, index, counters);
    } else if (index > pivot_position && pivot_position + 1 < high) {
        return select(nums, pivot_position + 1, high, index, counters);
    } else {
        return nums[pivot_position];
    }
}

auto partition(std::vector<int>& nums, int low, int high, Counters& counters) -> int {
    auto pivot = nums[high];
    auto i = low - 1;

    for (auto j = low; j <= high - 1; ++j) {
        counters.comparisons++; // comparison counter
        if (nums[j] <= pivot) {
            i++;
            std::swap(nums[i], nums[j]);
            counters.swaps++; // swap counter
        }
    }

    std::swap(nums[i + 1], nums[high]);
    counters.swaps++; // swap counter
    return (i + 1);
}

auto randomized_partition(std::vector<int>& nums, int low, int high, Counters& counters) -> int {
    std::random_device rd;  
    std::mt19937 eng(rd()); 
    std::uniform_int_distribution<> distr(low, high);

    auto random_pivot = distr(eng);
    std::swap(nums[random_pivot], nums[high]);
    counters.swaps++; // swap counter
    return partition(nums, low, high, counters);
}

auto randomized_select(std::vector<int>& nums, int low, int high, int index, Counters& counters) -> int {
    if (low == high) {
        return nums[low];
    }

    auto pivot_index = randomized_partition(nums, low, high, counters);
    if (index == pivot_index) {
        return nums[pivot_index];
    } else if (index < pivot_index) {
        return randomized_select(nums, low, pivot_index - 1, index, counters);
    } else {
        return randomized_select(nums, pivot_index + 1, high, index, counters);
    }
}

auto generate_numbers(int n) -> std::vector<int> {
    std::vector<int> nums(n);
    std::random_device rd;
    std::mt19937 eng(rd());
    std::uniform_int_distribution<> distr(0, 2 * n - 1);

    for (auto& num : nums) {
        num = distr(eng);
    }

    return nums;
}

auto binary_search(const std::vector<int>& arr, int v, int& comparisons) -> bool {
    int left = 0;
    int right = arr.size() - 1;

    while (left <= right) {
        comparisons++;
        int mid = left + (right - left) / 2;

        if (arr[mid] == v) {
            comparisons++;
            return true;
        } else if (arr[mid] < v) {
            comparisons++;
            left = mid + 1;
        } else {
            comparisons++;
            right = mid - 1;
        }
    }

    return false;
}

