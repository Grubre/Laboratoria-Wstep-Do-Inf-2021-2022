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

auto insertion_sort(const std::vector<int>& numbers) -> std::vector<int> {
    std::vector<int> ret = numbers;

    int key;

    for(int i = 1; i < ret.size(); i++) {
        std::cout << "krok " << i << ": ";
        print_vector(ret);
        std::cout << std::endl;
        key = ret[i];
        int j = i - 1;

        while(j >= 0 && ret[j] > key) {
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
    return 0;
}
