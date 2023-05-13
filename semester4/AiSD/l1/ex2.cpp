#include <iostream>
#include "list.hpp"
#include "timer.hpp"

auto main() -> int
{
    srand(time(NULL));
    constexpr int N = 10000;
    std::cout << "list test:" << std::endl;
    list<int> a;
    {
        std::cout << "Pushing 10000 random elements..." << std::endl;
        timer t;
        for(int i = 0; i < N; i++)
        {
            a.push_back(rand());
        }
    }
    std::cout << "============================================" << std::endl;
    {
        std::cout << "Accessing first element 10000 times" << std::endl;
        timer t;
        for(int i = 0; i < N; i++)
        {
            a.front();
        }
    }
    std::cout << "============================================" << std::endl;
    {
        std::cout << "Accessing last element 10000 times" << std::endl;
        timer t;
        for(int i = 0; i < N; i++) {
            auto it = a.begin();
            for(int i = 0; i < a.size() - 1; i++) {
                ++it;
            }
        }
    }
    std::cout << "============================================" << std::endl;
    {
        std::cout << "Accessing middle element 10000 times" << std::endl;
        timer t;
        for(int i = 0; i < N; i++) {
            auto it = a.begin();
            for(int i = 0; i < (a.size() - 1) / 2; i++) {
                ++it;
            }
        }
    }
    std::cout << "============================================" << std::endl;
    {
        std::cout << "Accessing random element 10000 times" << std::endl;
        timer t;
        for(int i = 0; i < N; i++) {
            auto it = a.begin();
            for(int i = 0; i < rand() % (a.size() - 1); i++) {
                ++it;
            }
        }
    }
    std::cout << "============================================" << std::endl;
    list<int> b = {1,2,3,4,5,6,7,8,9};
    list<int> c = {10,11,12,13,14,15};
    std::string str = "merging b = {";
    for(auto i : b)
    {
        str+= (std::to_string(i) + ", ");
    }
    str.erase(str.size() - 2, 2);
    str+="}";
    std::cout << str << std::endl;
    str = "with c = {";
    for(auto i : c)
    {
        str+= (std::to_string(i) + ", ");
    }
    str.erase(str.size() - 2, 2);
    str+="}";
    std::cout << str << std::endl;
    b.merge(c);
    str = "equals = {";
    for(auto i : b)
    {
        str+= (std::to_string(i) + ", ");
    }
    str.erase(str.size() - 2, 2);
    str+="}";
    std::cout << str << std::endl;

    return 0;
}
