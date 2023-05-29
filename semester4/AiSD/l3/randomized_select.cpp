#include <iostream>
#include <vector>
#include <algorithm>
#include <random>

struct Counters {
    int swaps = 0;
    int comparisons = 0;
};

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
        counters.comparisons++;
        return nums[low];
    }

    if (nums.size() <= 50) {
        for (const auto& num : nums) {
            std::cout << num << " ";
        }
        std::cout << std::endl;
    }

    auto pivot_index = randomized_partition(nums, low, high, counters);
    if (index == pivot_index) {
        counters.comparisons++;
        return nums[pivot_index];
    } else if (index < pivot_index) {
        counters.comparisons++;
        return randomized_select(nums, low, pivot_index - 1, index, counters);
    } else {
        counters.comparisons++;
        return randomized_select(nums, pivot_index + 1, high, index, counters);
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
        std::cout << '\n';
    }

    Counters counters;
    auto kth_smallest = randomized_select(nums, 0, n - 1, k - 1, counters);

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

