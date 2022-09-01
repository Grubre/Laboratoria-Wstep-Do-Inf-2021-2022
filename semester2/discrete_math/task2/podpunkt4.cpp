#include <iostream>
#include <chrono>
#include <thread>
#include <atomic>
#include <algorithm>
#include <fstream>
#include "fisher_yates_shuffle.hpp"
#include "cycles.hpp"

void multithread_compute_cycles(const std::vector<unsigned int>& sequence, 
std::atomic<double>& average, unsigned int amount)
{
    for(int i = 0; i < amount; i++)
    {
        unsigned long long longest_cycle = 0;
        for(const auto &cycle : split_cycles(fisher_yates_shuffle(sequence)))
            longest_cycle = std::max(longest_cycle,cycle.size());
        average+=longest_cycle;
    }
}

int main()
{
    const unsigned int k = 100000;
    const unsigned int max_n = 200;
    const unsigned int num_of_threads = 40;

    std::vector<unsigned int> sequence;
    auto start = std::chrono::steady_clock::now();

    std::ofstream output("k_"+std::to_string(k)+"_podp4_graph_output.txt");

    for(unsigned int n = 1; n <= max_n; n++)
    {
        std::atomic<double> local_average(0.f);
        sequence.push_back(n);

        std::vector<std::thread> threadpool;
        for(unsigned int j = 0; j < num_of_threads; j++)
            threadpool.push_back(std::thread(multithread_compute_cycles,sequence,std::ref(local_average),k/num_of_threads));
        for(unsigned int j = 0; j < num_of_threads; j++)
            threadpool[j].join();

        local_average = local_average / k;
        output << n << std::endl;
        output << local_average.load() << std::endl;
    }
    auto end = std::chrono::steady_clock::now();
    //output << "k = " << k << ", ELAPSED TIME: " << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count() << "ms" << std::endl;
    output.close();
    return 0;
}