#include <iostream>
#include <vector>
#include <utility>
#include "common.hpp"

const int INSERTION_SORT_THRESHOLD = 16;

void insertion_sort(std::vector<int>& arr, int left, int right, int& comparisons, int& swaps) {
    for (int i = left + 1; i <= right; ++i) {
        int key = arr[i];
        int j = i - 1;
        comparisons++;

        while (j >= left && arr[j] > key) {
            arr[j + 1] = arr[j];
            --j;
        }

        arr[j + 1] = key;
    }
}

int partition(std::vector<int>& arr, int left, int right, int& comparisons, int& swaps) {
    int pivot = arr[right];
    int i = left - 1;

    for (int j = left; j < right; ++j) {
        ++comparisons;
        if (arr[j] < pivot) {
            ++i;
            std::swap(arr[i], arr[j]);
            ++swaps;
        }
    }
    std::swap(arr[i + 1], arr[right]);

    return i + 1;
}

void hybrid_sort(std::vector<int>& arr, int left, int right, int& comparisons, int& swaps) {
    if (left < right) {
        if (right - left <= INSERTION_SORT_THRESHOLD) {
            insertion_sort(arr, left, right, comparisons, swaps);
        } else {
            int pivot_index = partition(arr, left, right, comparisons, swaps);
            hybrid_sort(arr, left, pivot_index - 1, comparisons, swaps);
            hybrid_sort(arr, pivot_index + 1, right, comparisons, swaps);
        }
    }
}

auto hybrid_sort(std::vector<int>& numbers) -> std::pair<int, int> {
    int comparisons = 0;
    int swaps = 0;
    hybrid_sort(numbers, 0, numbers.size() - 1, comparisons, swaps);
    return {comparisons, swaps};
}

auto main(int argc, char** argv) -> int {
    for(int n = 10; n <= 200; n+=10) {
        for(int i = 0; i < k; i++) {
            auto numbers = random_vec(n);
            auto[comparisons, swaps] = hybrid_sort(numbers);
            std::cout << n << " " << comparisons << " " << swaps << std::endl;
        }
    }

    return 0;
}
