#include "common.hpp"
#include <thread>
#include <mutex>
#include <vector>
#include <cmath>
#include <limits>
#include <random>
#include <cstring>

void thread_work(const std::vector<Vec2>& points, const MST& mst, 
                 std::size_t start, std::size_t end, std::size_t n,
                 int64_t& best_cycle_weight, std::vector<std::size_t>& best_cycle,
                 uint64_t& cycle_weight_accumulator, uint64_t& number_of_steps_accumulator,
                 std::mutex& mutex) {
    auto rd = std::random_device{};
    auto gen = std::mt19937(rd());
    auto dis = std::uniform_int_distribution<>(0, n - 1);

    for(auto i = start; i < end; ++i) {
        auto random_starting_vertex = dis(gen);
        auto cycle = get_tsp_cycle(mst, random_starting_vertex);
        auto [improved_cycle, number_of_moves, improved_cycle_weight] = local_search(points, std::move(cycle));

        std::lock_guard<std::mutex> lock(mutex);
        if (improved_cycle_weight < best_cycle_weight) {
            best_cycle_weight = improved_cycle_weight;
            best_cycle = improved_cycle;
        }
        cycle_weight_accumulator += improved_cycle_weight;
        number_of_steps_accumulator += number_of_moves;
    }
}

auto main(int argc, char** argv) -> int {
    const auto file_name = argv[1];
    const auto should_print_cycle = argc > 2 && !std::strcmp(argv[2], "true");
    const auto points = read_file(file_name);

    const auto mst = build_mst(points);
    const auto mst_weight = calculate_mst_weight(points, mst);

    const auto n = points.size();
    const auto sqrt_n = std::sqrt(n);

    auto best_cycle_weight = std::numeric_limits<int64_t>::max();
    auto best_cycle = get_tsp_cycle(mst, 0u);

    uint64_t cycle_weight_accumulator = 0llu;
    uint64_t number_of_steps_accumulator = 0llu;

    const auto num_threads = std::thread::hardware_concurrency();
    const auto work_per_thread = static_cast<std::size_t>(std::ceil(sqrt_n / static_cast<double>(num_threads)));

    std::vector<std::thread> threads;
    std::mutex mutex;

    for (std::size_t i = 0; i < num_threads; ++i) {
        std::size_t start = i * work_per_thread;
        std::size_t end = std::min((i + 1) * work_per_thread, static_cast<std::size_t>(sqrt_n));

        threads.emplace_back(thread_work, std::ref(points), std::ref(mst), start, end, n,
                             std::ref(best_cycle_weight), std::ref(best_cycle),
                             std::ref(cycle_weight_accumulator), std::ref(number_of_steps_accumulator),
                             std::ref(mutex));
    }

    for (auto& thread : threads) {
        thread.join();
    }

    auto average_cycle_weight = cycle_weight_accumulator / sqrt_n;
    auto average_number_of_moves = number_of_steps_accumulator / sqrt_n;

    std::cout << mst_weight << " " << average_cycle_weight << " " << average_number_of_moves << std::endl;
    std::cout << n << std::endl;

    if (should_print_cycle) {
        print_cycle(points, best_cycle);
    }

    return 0;
}
