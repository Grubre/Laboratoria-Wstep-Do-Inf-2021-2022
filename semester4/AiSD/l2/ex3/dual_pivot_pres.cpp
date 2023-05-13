#include "common.hpp"

int n = 0;

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

        if(n < 40) {
            print_vector(numbers);
            std::cout << std::endl;
        }

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
    std::vector<int> numbers;

    int a;
    while(std::cin >> a) {
        n++;
        numbers.push_back(a);
    }

    if(n < 40)
    {
        std::cout << "dane wejsciowe: ";
        print_vector(numbers);
        std::cout << std::endl;
    }

    auto[comparisons, swaps] = dual_pivot_quick_sort(numbers);

    if(n < 40)
    {
        std::cout << "posortowana tablica: ";
        print_vector(numbers);
        std::cout << std::endl;
    }

    std::cout << "ilosc porownan: " << comparisons << std::endl;
    std::cout << "ilosc przestawien: " << swaps << std::endl;


    return 0;
}
