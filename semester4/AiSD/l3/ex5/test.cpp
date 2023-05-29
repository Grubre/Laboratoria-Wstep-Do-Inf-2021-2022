#include <fstream>
#include <functional>
#include <thread>
#include <mutex>
#include <algorithm>
#include <numeric>
#include "quicksort.hpp"
#include "dualpivot.hpp"

auto random_vec(const int n) -> std::vector<int> {
    auto v = std::vector<int>(n);
    std::iota(v.begin(), v.end(), 1);
 
    auto rd = std::random_device{};
    auto g = std::mt19937(rd());
 
    std::shuffle(v.begin(), v.end(), g);

    return v;
}

auto main(int argc, char** argv) -> int {
    auto vec = random_vec(10);

    std::cout << "random data: ";
    print_vector(vec);
    std::cout << std::endl;

    auto vec2 = vec;
    auto vec3 = vec;
    auto s1 = quick_sort(vec2);
    auto s2 = quick_sort(vec3);

    std::cout << "select: ";
    print_vector(vec2);
    std::cout << std::endl;
    std::cout << "non select: ";
    print_vector(vec3);
    std::cout << std::endl;

    return 0;
}

