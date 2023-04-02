#include <iostream>
#include <vector>
#include <utility>
#include "insertion_sort.hpp"

auto main(int argc, char** argv) -> int {
    std::vector<int> numbers;

    int a;
    int n = 0;
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

    auto[comparisons, swaps] = insertion_sort(numbers);

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
