#include "WierszTrojkataPaskala.h"
#include <iostream>
#include <vector>
#include <exception>

int main(int argc, char *argv[])
{
    int n = 1;
    try
    {
        n = std::stoi(argv[1]);
    }
    catch(const std::exception& e)
    {
        std::cout << "Nieprawidlowy typ ilosc liczb" << std::endl;
        return 0;
    }

    WierszTrojkataPaskala a(n);

    for(int i = 2; i < argc; i++)
    {
        std::cout << argv[i] << " - ";
        try
        {
            int liczba = std::stoi(argv[i]);
            std::cout << a.wspolczynnik(liczba) << std::endl;
        }
        catch(const std::out_of_range& e)
        {
            std::cout << "liczba spoza zakresu" << std::endl;
        }
        catch(const std::invalid_argument &e)
        {
            std::cout << "nieprawidlowa dana" << std::endl;
        }
    }
    return 0;
}