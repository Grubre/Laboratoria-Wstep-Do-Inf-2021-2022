#include <fstream>
#include <functional>
#include <thread>
#include <mutex>
#include <algorithm>
#include <numeric>
#include "quicksort.hpp"
#include "dualpivot.hpp"

auto random_vec(const int n) -> std::vector<int> {
    auto v = std::vector<int>(n);
    std::iota(v.begin(), v.end(), 0);
 
    auto rd = std::random_device{};
    auto g = std::mt19937(rd());
 
    std::shuffle(v.begin(), v.end(), g);

    return v;
}

std::mutex file_mutex;

void run_and_record(const std::vector<int>& numbers, std::function<std::pair<int,int>(std::vector<int>&)> sort_func, 
                    const std::string& name, int n, std::ofstream& file) {
    auto numbers_copy = numbers;
    auto [comparisons, swaps] = sort_func(numbers_copy);
    std::lock_guard<std::mutex> lock(file_mutex);
    file << n << "," << name << "," << comparisons << "," << swaps << "\n";
}

auto main(int argc, char** argv) -> int {
    int k = 100;
    std::ofstream file;
    file.open("ex5.csv");

    file << "n,algorithm,comparisons,swaps\n";

    for(int n = 100; n <= 10000; n+=100) {
        std::cout << "n: " << n << std::endl;
        // pessimistic
        // auto numbers = std::vector<int>(n);
        // std::iota(numbers.begin(), numbers.end(), 1);
        auto numbers = random_vec(n);
        for(int i = 0; i < k; i++) {
            std::thread quick_sort_thread(run_and_record, numbers, quick_sort, "quick_sort", n, std::ref(file));
            std::thread select_quick_sort_thread(run_and_record, numbers, dual_pivot_quick_sort, "select_quick_sort", n, std::ref(file));
            std::thread dual_pivot_quick_sort_thread(run_and_record, numbers, dual_pivot_quick_sort, "dual_pivot_quick_sort", n, std::ref(file));
            std::thread select_dual_pivot_quick_sort_thread(run_and_record, numbers, select_dual_pivot_quick_sort, "select_dual_pivot_quick_sort", n, std::ref(file));

            quick_sort_thread.join();
            select_quick_sort_thread.join();
            dual_pivot_quick_sort_thread.join();
            select_dual_pivot_quick_sort_thread.join();
        }
    }

    file.close();
    return 0;
}

