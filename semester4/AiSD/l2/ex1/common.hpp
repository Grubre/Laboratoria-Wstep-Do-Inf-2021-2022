#pragma once
#include <vector>
#include <iostream>

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
