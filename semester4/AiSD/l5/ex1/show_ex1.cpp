#include "lcs.hpp"
#include <iostream>
#include <chrono>
#include <fstream>
#include <random>
#include <string>
#include <vector>

auto generate_random_string(int n) -> std::string{
    static constexpr char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";

    std::random_device rd; // obtain a random number from hardware
    std::mt19937 eng(rd()); // seed the generator
    std::uniform_int_distribution<> distr(0, sizeof(alphanum) - 2); // define the range

    std::string result;
    for (int i = 0; i < n; ++i) {
        result.push_back(alphanum[distr(eng)]);
    }
    return result;
}


auto main() -> int {
    for(int i = 10; i < 20; i++) {
        std::string str1 = generate_random_string(i);
        std::string str2 = generate_random_string(i);

        std::cout << "string 1: " << str1 << std::endl;
        std::cout << "string 2: " << str2 << std::endl;
        std::cout << "longest common subsequence: " << longest_common_subsequence(str1, str2) << std::endl;
    }
    return 0;
}
