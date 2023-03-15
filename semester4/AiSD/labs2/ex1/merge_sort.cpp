#include <iostream>
#include <vector>

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

auto merge_sort(const std::vector<int>& numbers) -> std::vector<int> {
    std::vector<int> ret = numbers;

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
    return 0;
}
