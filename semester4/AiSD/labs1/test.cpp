#include <iostream>
#include <queue>
#include <stack>
#include <list>
#include "queue.hpp"
#include "stack.hpp"
#include "list.hpp"
#include "cdeque.hpp"

int main() {
    cdeque<int> a;
    a.insert_back(1);
    a.insert_back(2);
    a.insert_back(3);

    for(auto i : a) {
        std::cout << i << std::endl;
    }
    return 0;
}
