// rowniez kompilujemy przy uzyciu c++11 (flaga - std=c++11)
// jako wejscie program pobiera:
// liczbe naturalna n - ilosc elementow
// a nastepnie n liczb naturalnych tworzacych ciag ktorego permutacje bedziemy generowac
// dla wejscia:
// 5
// 2 1 4 5 3
// otrzymujemy:
// (1, 2)
// (3, 4, 5)
#include <iostream>
#include "cycles.hpp"


int main()
{
    int n; // ilosc elementow wczytywanego ciagu
    std::cin >> n;

    std::vector<unsigned long long> sequence(n);

    for(auto& i : sequence)
        std::cin >> i;

    std::vector<std::vector<unsigned long long>> cycles = split_cycles(sequence);

    for(int i = 0; i < cycles.size(); i++)
    {
        std::cout << "(";
        for(int j = 0; j < cycles[i].size(); j++)
        {
            std::cout << cycles[i][j];
            if(j != cycles[i].size()-1)
                std::cout << ", ";
        }
        std::cout << ")\n";
    }
    return 0;
}