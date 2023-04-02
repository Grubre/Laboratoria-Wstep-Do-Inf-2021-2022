#include "common.hpp"

auto dual_pivot_quick_sort(std::vector<int>& numbers, int left, int right) -> std::pair<int, int> {
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

        // print_vector(numbers);

        auto left_stats = dual_pivot_quick_sort(numbers, left, lt - 1);
        auto middle_stats = dual_pivot_quick_sort(numbers, lt + 1, gt - 1);
        auto right_stats = dual_pivot_quick_sort(numbers, gt + 1, right);

        comparisons += left_stats.first + middle_stats.first + right_stats.first;
        swaps += left_stats.second + middle_stats.second + right_stats.second;
    }

    return {comparisons, swaps};
}

auto dual_pivot_quick_sort(std::vector<int>& numbers) -> std::pair<int, int> {
    return dual_pivot_quick_sort(numbers, 0, numbers.size() - 1);
}


auto main(int argc, char** argv) -> int {
    for(int n = 10; n <= 200; n+=10) {
        for(int i = 0; i < k; i++) {
            auto numbers = random_vec(n);
            auto[comparisons, swaps] = dual_pivot_quick_sort(numbers);
            std::cout << n << " " << comparisons << " " << swaps << std::endl;
        }
    }

    return 0;
}
