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

auto insertion_sort(const std::vector<int>& numbers) -> std::vector<int> {
    std::vector<int> ret = numbers;

    int key;

    for(int i = 1; i < ret.size(); i++) {
        std::cout << "krok " << i << ": ";
        print_vector(ret);
        std::cout << std::endl;
        key = ret[i];
        int j = i - 1;

        num_of_comparisons++;
        while(j >= 0 && ret[j] > key) {
            num_of_comparisons++;
            num_of_swaps++;
            ret[j + 1] = ret[j];
            j--;
        }
        ret[j + 1] = key;
    }
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

    std::vector<int> sorted = insertion_sort(numbers);

    std::cout << "posortowana tablica: ";
    print_vector(sorted);
    std::cout << std::endl;

    std::cout << "ilosc porownan: " << num_of_comparisons << std::endl;
    std::cout << "ilosc przestawien: " << num_of_swaps << std::endl;
    return 0;
}
