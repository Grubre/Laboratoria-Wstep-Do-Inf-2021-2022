#include "simulation.hpp"
#include <iostream>
#include <ranges>
#include <fstream>
#include <thread>

int main()
{
    constexpr int k = 50;
    std::ofstream output("data.csv");
    std::ofstream means_output("means.csv");

    Simulation::Result result_arr[50][1000];

    output << "n,B,U,L,C,D,D-C" << std::endl;
    means_output << "n,B,U,L,C,D,D-C" << std::endl;

    for(int n = 1000; n <=100000; n+=1000)
    {
        std::cout << n << std::endl;
        Simulation::Result mean_result(n);
        for(int i = 0; i < k; i++)
        {
            Simulation a(n);
            Simulation::Result result = a.simulate();
            mean_result.B += result.B;
            mean_result.U += result.U;
            mean_result.L += result.L;
            mean_result.C += result.C;
            mean_result.D += result.D;
            result.print_to_stream(output);
        }
        means_output << n << ",";
        means_output << (double)mean_result.B / 50 << ",";
        means_output << (double)mean_result.U / 50 << ",";
        means_output << (double)mean_result.L / 50 << ",";
        means_output << (double)mean_result.C / 50 << ",";
        means_output << (double)mean_result.D / 50 << ",";
        means_output << (double)(mean_result.D - mean_result.C) / 50;
        means_output << std::endl;
    }

    output.close();
    means_output.close();
    return 0;
}
