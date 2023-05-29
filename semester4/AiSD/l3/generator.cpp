#include <iostream>
#include <random>

auto main(int argc, char** argv) -> int {
    if (argc != 2) {
        std::cout << "Please provide one integer argument\n";
        return 1;
    }

    auto n = std::stoi(argv[1]);
    auto max_value = 2*n - 1;

    std::random_device rd; 
    std::mt19937 eng(rd()); 
    std::uniform_int_distribution<> distr(0, max_value);

    for (auto i = 0; i < n; ++i) {
        std::cout << distr(eng) << ' ';
    }
    std::cout << '\n';

    return 0;
}

