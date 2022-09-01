#pragma once
#include <vector>
#include <random>
#include <algorithm>

template<class T>
std::vector<T> fisher_yates_shuffle(const std::vector<T>& sequence)
{
    int n = sequence.size();
    std::vector<T> shuffled = sequence;

    // nowoczesny sposob generowania losowych liczb w c++ w ktorym wszystkie 
    // liczby z przedzialu sa jednakowo prawdopodobne do bycia wylosowanymi
    std::random_device rd;
    std::mt19937 gen(rd()); // implementacja Mersenne Twistera https://pl.wikipedia.org/wiki/Mersenne_Twister

    for(int i = 0; i < n-1; i++)
    {
        std::uniform_int_distribution<> rand_index(i, n-1);
        int j = rand_index(gen);
        std::iter_swap(shuffled.begin() + i, shuffled.begin() + j); // zamiana elementow na indeksach i oraz j
    }

    return shuffled;
}