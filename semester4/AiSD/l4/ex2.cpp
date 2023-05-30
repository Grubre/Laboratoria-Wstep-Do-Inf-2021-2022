#include "bst.hpp"
#include <fstream>
#include <cmath>
#include <random>
#include <algorithm>
#include <future>
#include <numeric>

struct Result {
    size_t max_comparisons = 0;
    size_t max_pointer_reads = 0;
    size_t max_pointer_substitutions = 0;
    size_t total_comparisons = 0;
    size_t total_pointer_reads = 0;
    size_t total_pointer_substitutions = 0;
};

Result perform_repetition(int n) {
    BST tree;

    Result result;

    std::vector<int> data(n);

    std::iota(data.begin(), data.end(), 1);
    std::random_device rd;
    std::mt19937 g(rd());
    std::shuffle(data.begin(), data.end(), g);

    for (int i = 1; i <= n; ++i) {
        auto stats = tree.insert(data[i]);
        result.max_comparisons = std::max(result.max_comparisons, stats.comparisons);
        result.max_pointer_reads = std::max(result.max_pointer_reads, stats.pointer_reads);
        result.max_pointer_substitutions = std::max(result.max_pointer_substitutions, stats.pointer_substitutions);

        result.total_comparisons += stats.comparisons;
        result.total_pointer_reads += stats.pointer_reads;
        result.total_pointer_substitutions += stats.pointer_substitutions;
    }

    std::vector<int> deletion_order(n);
    std::iota(deletion_order.begin(), deletion_order.end(), 1);

    std::mt19937 g2(rd());
    std::shuffle(deletion_order.begin(), deletion_order.end(), g2);

    for (int i : deletion_order) {
        auto stats = tree.deleteValue(i);
        result.max_comparisons = std::max(result.max_comparisons, stats.comparisons);
        result.max_pointer_reads = std::max(result.max_pointer_reads, stats.pointer_reads);
        result.max_pointer_substitutions = std::max(result.max_pointer_substitutions, stats.pointer_substitutions);

        result.total_comparisons += stats.comparisons;
        result.total_pointer_reads += stats.pointer_reads;
        result.total_pointer_substitutions += stats.pointer_substitutions;
    }

    return result;
}

int main() {
    std::ofstream file;
    file.open("./data/random_data.csv");

    file << "n,max_comparisons,max_pointer_reads,max_pointer_substitutions,avg_comparisons,avg_pointer_reads,avg_pointer_substitutions\n";

    for (int n = 10000; n <= 100000; n += 10000) {
        std::cout << "n: " << n << std::endl;
        std::vector<std::future<Result>> futures;
        for (int rep = 0; rep < 20; ++rep) {
            futures.push_back(std::async(std::launch::async, perform_repetition, n));
        }

        std::vector<Result> results;
        for (auto& future : futures) {
            results.push_back(future.get());
        }

        size_t max_comparisons = 0;
        size_t max_pointer_reads = 0;
        size_t max_pointer_substitutions = 0;

        size_t total_comparisons = 0;
        size_t total_pointer_reads = 0;
        size_t total_pointer_substitutions = 0;

        
        for (const auto& result : results) {
            std::cout << "pointer sub: " << max_pointer_substitutions << std::endl;
            max_comparisons = std::max(max_comparisons, result.max_comparisons);
            max_pointer_reads = std::max(max_pointer_reads, result.max_pointer_reads);
            max_pointer_substitutions = std::max(max_pointer_substitutions, result.max_pointer_substitutions);

            total_comparisons += result.total_comparisons;
            total_pointer_reads += result.total_pointer_reads;
            total_pointer_substitutions += result.total_pointer_substitutions;
        }

        float avg_comparisons = static_cast<float>(total_comparisons) / (20 * 2 * n);
        float avg_pointer_reads = static_cast<float>(total_pointer_reads) / (20 * 2 * n);
        float avg_pointer_substitutions = static_cast<float>(total_pointer_substitutions) / (20 * 2 * n);

        std::cout << "n: " << n << ", " << max_pointer_substitutions << std::endl;

        file << n << ','
             << max_comparisons << ','
             << max_pointer_reads << ','
             << max_pointer_substitutions << ','
             << avg_comparisons << ','
             << avg_pointer_reads << ','
             << avg_pointer_substitutions << '\n';
    }

    file.close();
    return 0;
}
