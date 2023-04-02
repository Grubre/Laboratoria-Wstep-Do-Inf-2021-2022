#include <iostream>
#include <vector>

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

void merge(std::vector<int>& arr, int left, int mid, int right, int& comparisons, int& swaps) {
    int n1 = mid - left + 1;
    int n2 = right - mid;

    std::vector<int> L(n1), R(n2);

    for (int i = 0; i < n1; ++i) {
        L[i] = arr[left + i];
    }
    for (int j = 0; j < n2; ++j) {
        R[j] = arr[mid + 1 + j];
    }

    int i = 0;
    int j = 0;
    int k = left;

    while (i < n1 && j < n2) {
        comparisons++;
        if (L[i] <= R[j]) {
            arr[k] = L[i];
            i++;
        } else {
            arr[k] = R[j];
            j++;
            swaps += n1 - i;
        }
        k++;
    }

    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }

    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }

    if(n < 40) {
        print_vector(arr);
        std::cout << std::endl;
    }
}

void merge_sort_helper(std::vector<int>& arr, int left, int right, int& comparisons, int& swaps) {
    if (left < right) {
        int mid = left + (right - left) / 2;

        merge_sort_helper(arr, left, mid, comparisons, swaps);
        merge_sort_helper(arr, mid + 1, right, comparisons, swaps);

        merge(arr, left, mid, right, comparisons, swaps);
    }
}

auto merge_sort(std::vector<int>& numbers) -> std::pair<int,int> {
    auto comparisons = 0, swaps = 0;
    merge_sort_helper(numbers, 0, numbers.size() - 1, comparisons, swaps);
    return {comparisons, swaps};
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

    auto[comparisons, swaps] = merge_sort(numbers);

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
