#include <iostream>
#include "ex1.hpp"

int main() {
    FIFO<int> a;

    a.push(5);
    a.push(10);

    FIFO<int> b = std::move(a);

    // std::cout << "size = " << a.size() << std::endl;

    // std::cout << a.pop() << std::endl;
    // std::cout << "size = " << a.size() << std::endl;
    std::cout << b.pop() << std::endl;
    std::cout << b.pop() << std::endl;
    return 0;
}
