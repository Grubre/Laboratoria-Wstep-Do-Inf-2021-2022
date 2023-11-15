#pragma once
#include <cstring>
#include <fstream>
#include <iostream>

inline void print_match(const std::string T, std::size_t index, std::size_t len) {
    std::cout << T << std::endl;
    for(auto i = 0llu; i < index; i++) {
        std::cout << " ";
    }
    for(auto i = 0llu; i < len; i++) {
        std::cout << "↑";
    }
    std::cout << std::endl;

}

inline auto get_args(int argc, char**argv) -> std::pair<std::string, std::string> {
    if (argc < 3) {
        std::cerr << "usage: program <pattern> <filepath>" << std::endl;
        exit(1);
    }

    auto P = std::string{argv[1]};
    auto T = std::string{};
    auto file = std::ifstream{argv[2]};
    file >> T;
    return {P, T};
}

inline auto get_args_u32(int argc, char**argv) -> std::pair<std::u32string, std::u32string> {
    if (argc < 3) {
        std::cerr << "usage: program <pattern> <filepath>" << std::endl;
        exit(1);
    }

    auto P = std::string{argv[1]};
    auto T = std::string{};
    auto file = std::ifstream{argv[2]};
    file >> T;
    return {std::u32string{P.begin(), P.end()}, std::u32string{T.begin(), T.end()}};
}
