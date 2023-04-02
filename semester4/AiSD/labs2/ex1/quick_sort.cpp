#include <iostream>
#include <vector>
#include <stack>

int n = 0;

auto print_vector(const std::vector<int>& numbers) -> void {
    std::cout << "(";
    for(int i = 0; i < numbers.size(); i++) {
        std::cout << numbers[i];
        if(i < numbers.size() - 1) {
            std::cout << ", ";
        }
    }
    std::cout << ")";
}


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

    if(n < 40) {
        print_vector(numbers);
        std::cout << std::endl;
    }

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

    auto[comparisons, swaps] = quick_sort(numbers);

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
