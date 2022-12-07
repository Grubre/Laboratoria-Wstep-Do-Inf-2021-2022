#include "ballmachine.hpp"
#include "simulation.hpp"
#include <iostream>
#include <ranges>

int main()
{
    constexpr int k = 50;
    constexpr int n = 100000;

    for(int n = 1000; n <=100000; n+=1000)
    {
        std::cout << "n: " << n << std::endl;
        for(int i = 0; i < k; i++)
        {
            Simulation a(n);
            Simulation::Result result = a.simulate();
        }
        // result.print();
    }
    return 0;
}
