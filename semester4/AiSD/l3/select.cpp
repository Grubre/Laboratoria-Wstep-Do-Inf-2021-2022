#include <iostream>
#include <vector>
#include <algorithm>

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

auto select(std::vector<int>& nums, int low, int high, int index, Counters& counters, const int initial_n) -> int {
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
    int median_of_medians = select(medians, 0, medians.size() - 1, medians.size() / 2, counters, initial_n);

    // Find the index of the median of medians in the original nums array
    int pivot_index = std::distance(nums.begin(), std::find(nums.begin() + low, nums.begin() + high + 1, median_of_medians));
    int pivot_position = partition(nums, low, high, pivot_index, counters);

    if (nums.size() <= 50 && nums.size() == initial_n) {
        for (const auto& num : nums) {
            std::cout << num << " ";
        }
        std::cout << std::endl;
    }

    if (index == pivot_position) {
        return nums[pivot_position];
    } else if (index < pivot_position) {
        return select(nums, low, pivot_position - 1, index, counters, initial_n);
    } else {
        return select(nums, pivot_position + 1, high, index, counters, initial_n);
    }
}

auto main(int argc, char** argv) -> int {
    if (argc != 3) {
        std::cout << "Please provide two integer arguments\n";
        return 1;
    }

    int n = std::stoi(argv[1]);
    int k = std::stoi(argv[2]);

    if (k > n) {
        std::cout << "k can't be larger than n\n";
        return 1;
    }

    auto nums = std::vector<int>(n);
    for (auto& num : nums) {
        std::cin >> num;
    }

    if (n < 50) {
        std::cout << "Before select: ";
        for (const auto& num : nums) {
            std::cout << num << ' ';
        }
        std::cout << std::endl;;
    }

    Counters counters;
    auto kth_smallest = select(nums, 0, n - 1, k - 1, counters, nums.size());

    std::cout << "Number of comparisons: " << counters.comparisons << '\n';
    std::cout << "Number of swaps: " << counters.swaps << '\n';
    std::cout << "kth smallest element: " << kth_smallest << '\n';

    if (n < 50) {
        std::cout << "After select: ";
        for (const auto& num : nums) {
            std::cout << num << ' ';
        }
        std::cout << '\n';

        std::sort(nums.begin(), nums.end());
        std::cout << "Sorted: ";
        for (const auto& num : nums) {
            std::cout << num << ' ';
        }
        std::cout << '\n';
    }

    return 0;
}

