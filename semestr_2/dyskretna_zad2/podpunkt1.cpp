// kompilujemy przy uzyciu c++11 (flaga - std=c++11)
// jako wejscie program pobiera:
// liczbe naturalna n - ilosc elementow
// a nastepnie n liczb naturalnych tworzacych ciag ktorego permutacje bedziemy generowac
// dla wejscia:
// 5
// 1 2 3 4 5
// mozemy przykladowo otrzymac:
// 3 5 2 4 1
// lub dowolna inna permutacje tego ciagu (wszystkie sa tak samo prawdopodobne)

#include <iostream>
#include "fisher_yates_shuffle.hpp"

int main()
{
    int n; // ilosc elementow wczytywanego ciagu
    std::cin >> n;

    std::vector<long long> sequence(n);

    for(auto& i : sequence)
        std::cin >> i;

    for(const auto& i : fisher_yates_shuffle(sequence))
        std::cout << i << " ";
    return 0;
}