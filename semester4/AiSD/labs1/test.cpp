#include <iostream>
#include <queue>
#include <stack>
#include "queue.hpp"
#include "stack.hpp"

int main() {
    stack<int> a;
    a.push(4);
    a.push(5);
    a.push(6);
    stack<int> b = std::move(a);
    std::cout << b.pop() << std::endl;
    std::cout << b.pop() << std::endl;
    std::cout << b.pop() << std::endl;

    return 0;
}
