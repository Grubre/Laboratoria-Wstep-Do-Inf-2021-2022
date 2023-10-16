#include "binomial_heap.hpp"
#include "fibonacci_heap.hpp"
#include <random>
#include <fstream>

auto random_vec(const int n) -> std::vector<int> {
    auto v = std::vector<int>(n);
    std::iota(v.begin(), v.end(), 1);
 
    auto rd = std::random_device{};
    auto g = std::mt19937(rd());
 
    std::shuffle(v.begin(), v.end(), g);

    return v;
}


auto main() -> int {
    auto output = std::ofstream("ex2_part2.csv");
    output << "n, heap_type, comparisons" << std::endl;
    for(int n = 100; n < 10000; n+=100) {
        std::cout << "n: " << n << std::endl;
        FibonacciHeap fib_heap_1{}, fib_heap_2{};
        binomial_heap bin_heap_1{}, bin_heap_2{};

        int fib_comparisons = 0, bin_comparisons = 0;

        for(auto j : random_vec(n)) {
            bin_heap_1.insert(j, bin_comparisons);
        }

        for(auto j : random_vec(n)) {
            bin_heap_2.insert(j, bin_comparisons);
        }

        for(auto j : random_vec(n)) {
            // TODO:
            //fib_heap_1.insert(j, fib_comparisons);
        }

        for(auto j : random_vec(n)) {
            // TODO:
            //fib_heap_2.insert(j, fib_comparisons);
        }

        bin_heap_1.mergeHeap(bin_heap_2, bin_comparisons);
        // TODO:
        // fib_heap_1.mergeHeap(fib_heap_2, fib_comparisons);

        for(int i = 0; i < 2 * n; i++) {
            auto bin_element = bin_heap_1.extractMin(bin_comparisons);
            if(!bin_element) {
                std::cout << i << ", n * 2: " << n * 2 << std::endl;
            }
            // TODO:
            //auto fib_element = *fib_heap_1.extractMin(bin_comparisons);
        }

        output << n << ", bin, " << bin_comparisons << std::endl;
        //output << n << ", fib, " << fib_comparisons << std::endl;
    }

    output.close();
    return 0;
}
