#include <iostream>
#include <chrono>
#include "fisher_yates_shuffle.hpp"
#include "cycles.hpp"
int main()
{
    const unsigned int k = 10000;
    const unsigned int max_n = 100;

    std::vector<unsigned int> sequence;
    auto start = std::chrono::steady_clock::now();
    for(unsigned int n = 1; n <= max_n; n++)
    {
        double local_average = 0.f;
        sequence.push_back(n);
        for(unsigned int j = 0; j < k; j++)
        {
            double cycles_count = split_cycles(fisher_yates_shuffle(sequence)).size();
            local_average += (double)cycles_count;
        }
        local_average /= k;
        std::cout << n << "\n";
        std::cout << harmonic(n) << '\n';
        std::cout << local_average << std::endl;
    }
    auto end = std::chrono::steady_clock::now();
    std::cout << "ELAPSED TIME: " << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count() << "ms" << std::endl;
    return 0;
}