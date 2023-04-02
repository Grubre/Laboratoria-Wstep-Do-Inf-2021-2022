#pragma once
#include <random>
#include <algorithm>
#include <vector>
#include <iostream>

const int k = 100;

inline auto print_vector(const std::vector<int>& numbers) -> void {
    std::cout << "(";
    for(int i = 0; i < numbers.size(); i++) {
        std::cout << numbers[i];
        if(i < numbers.size() - 1) {
            std::cout << ", ";
        }
    }
    std::cout << ")";
}

inline auto random_vec(const int n) -> std::vector<int> {
    auto v = std::vector<int>(n);
    std::iota(v.begin(), v.end(), 0);
 
    auto rd = std::random_device{};
    auto g = std::mt19937(rd());
 
    std::shuffle(v.begin(), v.end(), g);

    return v;
}
