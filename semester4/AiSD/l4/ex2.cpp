#include "bst.hpp"
#include <ranges>
#include <random>

auto main() -> int {
    std::vector<int> numbers;

    int a;
    int n = 0;

    while(std::cin >> a) {
        n++;
        numbers.push_back(a);
    }

    auto bst = BST{};

    for(auto i : numbers) {
        std::cout << "insert " << i << std::endl;
        bst.insert(i);
        bst.printBT();
    }


    std::random_device rd;
    std::mt19937 gen(rd());
    std::ranges::shuffle(numbers, gen);

    for(auto i : numbers) {
        std::cout << "delete " << i << std::endl;
        bst.deleteValue(i);
        bst.printBT();
    }


    return 0;
}
