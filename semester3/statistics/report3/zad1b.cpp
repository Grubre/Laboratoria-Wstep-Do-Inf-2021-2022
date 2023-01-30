#include <iostream>
#include <random>
#include <fstream>

int main()
{
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distrib(0, 1);

    std::ofstream out("zad1b.txt");
    for(int i = 0; i < 1500000; i++)
    {
        out << (distrib(gen));
    }
    out.close();
    return 0;
}