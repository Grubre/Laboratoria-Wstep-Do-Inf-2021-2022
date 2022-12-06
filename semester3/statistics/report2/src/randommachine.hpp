#pragma once
#include <random>

class RandomMachine
{
    std::random_device rd;
public:
    int rand_int(int a, int b)
    {
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> distrib(a, b);
        return distrib(gen);
    }
};
