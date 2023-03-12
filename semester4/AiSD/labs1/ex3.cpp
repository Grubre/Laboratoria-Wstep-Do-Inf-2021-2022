#include <iostream>
#include "timer.hpp"
#include "cdeque.hpp"


template<typename T>
auto print(cdeque<T> a) -> void
{
    auto it = a.begin();
    do
    {
        std::cout << *it << std::endl;
        ++it;
    } while(it != a.end());
}


auto main() -> int
{
    srand(time(NULL));
    constexpr int N = 10000;
    cdeque<int> a;
    {
        std::cout << "Pushing 10000 random elements..." << std::endl;
        timer t;
        for(int i = 0; i < N; i++) {
            a.insert_back(rand());
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
    cdeque<int> b = {1,2,3,4,5,6,7,8,9};
    cdeque<int> c = {10,11,12,13,14,15};
    std::string str = "merging b = {";
    auto it = b.begin();
    do
    {
        str+= (std::to_string(*it) + ", ");
        ++it;
    }
    while(it != b.end());
    str.erase(str.size() - 2, 2);
    str+="}";
    std::cout << str << std::endl;
    str = "with c = {";
    it = c.begin();
    do
    {
        str+= (std::to_string(*it) + ", ");
        ++it;
    }
    while(it != c.end());
    str.erase(str.size() - 2, 2);
    str+="}";
    std::cout << str << std::endl;
    b.merge(c);
    str = "equals = {";
    it = b.begin();
    do
    {
        str+= (std::to_string(*it) + ", ");
        ++it;
    }
    while(it != b.end());
    str.erase(str.size() - 2, 2);
    str+="}";
    std::cout << str << std::endl;

    return 0;
}
