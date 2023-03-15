#include <iostream>
#include <vector>

volatile int num_of_comparisons = 0;
volatile int num_of_swaps = 0;

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


auto merge(std::vector<int>& numbers, const unsigned int left,
        const unsigned int mid, const unsigned int right) -> void {
    auto const sub_array_one_size = mid - left + 1;
    auto const sub_array_two_size = right - mid;

    std::vector<int> left_array(sub_array_one_size);
    std::vector<int> right_array(sub_array_two_size);

    for (auto i = 0; i < sub_array_one_size; i++)
        left_array[i] = numbers[left + i];
    for (auto j = 0; j < sub_array_two_size; j++)
        right_array[j] = numbers[mid + 1 + j];

    auto sub_array_one_index = 0;
    auto sub_array_two_index = 0;
    auto index_of_merged_array = left;

    num_of_comparisons += 2;
    while(sub_array_one_index < sub_array_one_size
            && sub_array_two_index < sub_array_two_size) {
        num_of_comparisons++;
        if(left_array[sub_array_one_index] <= right_array[sub_array_two_index]) {
            num_of_swaps++;
            numbers[index_of_merged_array] = left_array[sub_array_one_index];
            sub_array_one_index++;
        } else {
            num_of_swaps++;
            numbers[index_of_merged_array] = right_array[sub_array_two_index];
            sub_array_two_index++;
        }
        index_of_merged_array++;
    }

    num_of_comparisons++;
    while(sub_array_one_index < sub_array_one_size) {
        num_of_comparisons++;
        num_of_swaps++;
        numbers[index_of_merged_array] = left_array[sub_array_one_index];
        sub_array_one_index++;
        index_of_merged_array++;
    }

    num_of_comparisons++;
    while(sub_array_two_index < sub_array_two_size) {
        num_of_comparisons++;
        num_of_swaps++;
        numbers[index_of_merged_array] = right_array[sub_array_two_index];
        sub_array_two_index++;
        index_of_merged_array++;
    }
}


auto merge_sort_helper(std::vector<int>& numbers, unsigned int begin, unsigned int end) -> void {
    if(begin >= end) {
        return;
    }

    auto mid = begin + (end - begin) / 2;
    merge_sort_helper(numbers, begin, mid);
    merge_sort_helper(numbers, mid + 1, end);
    merge(numbers, begin, mid, end);
    print_vector(numbers);
    std::cout << std::endl;
}


auto merge_sort(const std::vector<int>& numbers) -> std::vector<int> {
    if(numbers.empty()) {
        return {};
    }

    std::vector<int> ret = numbers;

    merge_sort_helper(ret, 0, ret.size() - 1);

    return ret;
}

auto main(int argc, char** argv) -> int {
    std::vector<int> numbers;
    numbers.reserve(argc);
    for(int i = 1; i < argc; i++) {
        numbers.push_back(std::stoi(argv[i]));
    }

    std::cout << "dane wejsciowe: ";
    print_vector(numbers);
    std::cout << std::endl;

    std::vector<int> sorted = merge_sort(numbers);

    std::cout << "posortowana tablica: ";
    print_vector(sorted);
    std::cout << std::endl;

    std::cout << "ilosc porownan: " << num_of_comparisons << std::endl;
    std::cout << "ilosc przestawien: " << num_of_swaps << std::endl;
    return 0;
}
