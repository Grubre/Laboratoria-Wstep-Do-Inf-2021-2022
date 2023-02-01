#include <functional>
#include <iostream>
#include <fstream>
#include <thread>
#include <chrono>
#include "graph.hpp"


void simulate(int n1, int nend, int njump, int k, std::string output_file,
        std::function<Graph(int)> func, std::function<int(int)> starting_pos) {
    std::ofstream output("diagrams/" + output_file);

    std::cout << "Started simulating " + output_file << std::endl;

    for(int i = n1; i <= nend; i+=njump) {
        auto start = std::chrono::steady_clock::now();
        auto graph = func(i);
        std::vector<long long> ret(k);

        std::vector<std::thread> threads;
        threads.reserve(k);

        for(int j = 0; j < k; j++) {
            threads.push_back(std::thread(simulate_graph_coverage, &ret[j], graph, starting_pos(i)));
        }

        for (auto &th : threads) {
            th.join();
        }

        auto end = std::chrono::steady_clock::now();
        std::cout << "n = " << i << ": elapsed time in seconds: "
        << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count()
        << " ms" << std::endl;

        for(int j = 0; j < k; j++) {
            output << i << " " << ret[j] << std::endl;
        }
    }

    std::cout << "Finished simulating... Results written to " + output_file << std::endl;

    output.close();
}


inline void simulate_functional(int n1, int nend, int njump, int k, std::string output_file,
        std::function<int(int, int)> func, std::function<int(int)> starting_pos) {
    std::ofstream output("diagrams/" + output_file);

    std::cout << "Started simulating " + output_file << std::endl;

    for(int i = n1; i <= nend; i+=njump) {
        auto start = std::chrono::steady_clock::now();
        std::vector<long long> ret(k);

        std::vector<std::thread> threads;
        threads.reserve(k);

        for(int j = 0; j < k; j++) {
            threads.push_back(std::thread(simulate_graph_coverage_functional, &ret[j], i, func, starting_pos(i)));
        }

        for (auto &th : threads) {
            th.join();
        }

        auto end = std::chrono::steady_clock::now();
        std::cout << "n = " << i << ": elapsed time in seconds: "
        << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count()
        << " ms" << std::endl;

        for(int j = 0; j < k; j++) {
            output << i << " " << ret[j] << std::endl;
        }
    }

    std::cout << "Finished simulating... Results written to " + output_file << std::endl;

    output.close();
}



int main()
{
    constexpr int n1 = 100, nend = 2000;
    constexpr int k = 50;

    // simulate(n1, nend, 50, k, "output_clique.txt", construct_clique, [](int n) {
    //     return 0;
    // });
    simulate_functional(n1, nend, 50, k, "output_clique_functional.txt", generate_clique, [](int n) {
        return 0;
    });

    // simulate(n1, nend, 50, k, "output_path.txt", construct_path, [](int n) {
    //     return n / 2;
    // });

    // simulate(n1, nend, 50, k, "output_path_beginning.txt", construct_path, [](int n) {
    //     return 0;
    // });
    // simulate(n1, nend, 50, k, "output_binary_tree.txt", construct_binary_tree, [](int n) {
    //     return 0;
    // });
    // simulate(n1, nend, 50, k, "output_lolipop.txt", construct_lolipop, [](int n) {
    //     return 0;
    // });
    // simulate_functional(n1, nend, 50, k, "output_lolipop_functional.txt", generate_lollipop, [](int n) {
    //     return 0;
    // });

    return 0;
}
