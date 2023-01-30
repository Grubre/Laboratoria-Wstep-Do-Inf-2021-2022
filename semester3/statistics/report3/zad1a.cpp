#include <cstdlib>
#include <iostream>
#include <random>
#include <fstream>

int main()
{
    std::cout << RAND_MAX << std::endl;
    srand(time(NULL));
    std::ofstream out("zad1a.txt");
    for(int i = 0; i < 1500000; i++)
    {
        out << rand()%2;
    }
    out.close();
    return 0;
}