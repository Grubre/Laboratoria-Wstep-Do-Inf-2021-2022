#include "binomial_heap.hpp"
#include <cassert>

int main() {
    binomial_heap h1;
    h1.insert(3);
    h1.insert(2);
    std::cout <<  h1.extract_min()->key << std::endl;
    auto y = h1.extract_min()->key;
    std::cout << y << std:: endl;
    return 0;
}

