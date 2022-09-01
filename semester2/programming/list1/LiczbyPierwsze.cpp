#include "LiczbyPierwsze.h"

#include <iostream>
#include <vector>
#include <exception>

LiczbyPierwsze::LiczbyPierwsze(int n)
{
    std::cout << n << std::endl;
    bool sito_eratostenesa[n + 1];

    for(int i = 0; i <= n; i++)
    {
        sito_eratostenesa[i] = true;
    }

    for(int i = 2; i * i <= n; i++)
    {
        if(sito_eratostenesa[i] == true)
        {
            for(int j = i * i; j <= n; j += i)
            {
                sito_eratostenesa[j] = false;
            }
        }
    }

    for(int i = 2; i <= n; i++)
    {
        if(sito_eratostenesa[i] == true)
        {
            pierwsze.push_back(i);
        }
    }
}

LiczbyPierwsze::~LiczbyPierwsze()
{

}

int LiczbyPierwsze::liczba(int m)
{
    return pierwsze.at(m);
}