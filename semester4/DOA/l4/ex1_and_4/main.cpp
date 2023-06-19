#include <iostream>
#include <chrono>
#include <fstream>
#include "hipercube.hpp"
#include "cmdlineargs.hpp"

auto main(int argc, char* argv[]) -> int {
    // cmdline_args args(argc, argv);

    auto output = std::ofstream("dinic_vs_karp.csv");
    // auto output = std::ofstream("karp.csv");

    output << "k,algorithm,time,maxflow,paths" << std::endl;
    // output << "k,time,maxflow,paths" << std::endl;

    auto tries = 1u;

    for(int k = 1; k <= 16; k++) {
        std::cout << "k: " << k << std::endl;
        unsigned int avg_max_flow{};
        unsigned int avg_num_paths{};
        hipercube a(k);

        auto start = std::chrono::high_resolution_clock::now();
        for(auto t = 0u; t < tries; t++) {
            auto copy = a;
            auto[max_flow, num_paths] = copy.dinic();
            avg_max_flow += max_flow;
            avg_num_paths += num_paths;
        }
        auto end = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();

        output << k << ",dinic," << duration << "," << avg_max_flow / tries << "," << avg_num_paths / tries << std::endl;

        avg_max_flow = {};
        avg_num_paths = {};

        start = std::chrono::high_resolution_clock::now();
        for(auto t = 0u; t < tries; t++) {
            auto copy = a;
            auto[max_flow, num_paths] = copy.edmonds_karp();
            avg_max_flow += max_flow;
            avg_num_paths += num_paths;
        }
        end = std::chrono::high_resolution_clock::now();
        duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();

        output << k << ",edmonds-karp," << duration << "," << avg_max_flow / tries << "," << avg_num_paths / tries << std::endl;

        // if(args.printFlow) {
        //     for(auto i = 0u; i <= a.n; i++) {
        //         for(auto j = 0u; j < a.k; j++) {
        //             auto key = static_cast<unsigned int>(std::pow(2, j)) ^ i;
        //             if(key > i) {
        //                 std::cout << "x[" << i << "]" << "[" << key << "] = " << a.f[i][key] << std::endl;
        //             }
        //         }
        //     }
        // }

        // std::cerr << "czas wykonania edmonds_karp: " << duration << " ms" << std::endl;
    }

    return 0;
}
