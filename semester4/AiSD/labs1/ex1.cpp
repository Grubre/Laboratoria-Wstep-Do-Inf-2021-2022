#include <iostream>
#include <queue>
#include <stack>
#include <list>
#include "queue.hpp"
#include "stack.hpp"
#include "list.hpp"
#include "cdeque.hpp"
#include "timer.hpp"

int main() {
    constexpr int N = 100;
    std::cout << "Queue(FIFO) test:" << std::endl;
    {
        timer t;
        queue<int> fifo;
        for(int i = 0; i < N; i++)
        {
            std::cout << "Pushing: " << i << std::endl;
            fifo.push(i);
        }
        for(int i = 0; i < N; i++) {
            std::cout << "Poping: " << fifo.front() << std::endl;
            fifo.pop();
        }
    }
    std::cout << "Stack(LIFO) test:" << std::endl;
    {
        timer t;
        stack<int> lifo;
        for(int i = 0; i < N; i++)
        {
            std::cout << "Pushing: " << i << std::endl;
            lifo.push(i);
        }
        for(int i = 0; i < N; i++) {
            std::cout << "Poping: " << lifo.top() << std::endl;
            lifo.pop();
        }
    }
 
    return 0;
}
