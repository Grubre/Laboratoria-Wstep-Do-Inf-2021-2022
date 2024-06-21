// https://en.wikipedia.org/wiki/Dining_philosophers_problem
// specifically this solution: https://en.wikipedia.org/wiki/Dining_philosophers_problem#Resource_hierarchy_solution
#include <print>
#include <random>
#include <thread>
#include <mutex>
#include <string_view>
#include <array>


auto get_rand_num(int lower, int upper) -> int {
    static std::random_device rd{};
    static std::mt19937 gen(rd());
    static std::uniform_int_distribution<int> dist(lower, upper);
    return dist(gen);
}

void print_message(std::mutex &output, const std::string_view &message) {
    std::lock_guard<std::mutex> lock(output);
    std::println("{}", message);
}

void print_thinking_message(int id, std::mutex &output) {
    print_message(output, "Philosopher " + std::to_string(id) + " is thinking");
}

void print_eating_message(int id, std::mutex &output) {
    print_message(output, "Philosopher " + std::to_string(id) + " is eating");
}

constexpr auto philospher_number = 10000;
constexpr auto lower_sleep_time = 5;
constexpr auto upper_sleep_time = 20;
constexpr auto finish_meals_count = 20;

void philosopher_behaviour(int id, std::mutex &left_fork, std::mutex &right_fork, std::mutex &output) {
    auto meals_count = 0u;
    while (meals_count < finish_meals_count) {
        print_thinking_message(id, output);
        std::this_thread::sleep_for(std::chrono::milliseconds(get_rand_num(lower_sleep_time, upper_sleep_time)));

        std::lock_guard<std::mutex> left(left_fork);
        std::lock_guard<std::mutex> right(right_fork);

        print_eating_message(id, output);
        std::this_thread::sleep_for(std::chrono::milliseconds(get_rand_num(lower_sleep_time, upper_sleep_time)));
        meals_count++;
    }
    print_message(output, "Philosopher " + std::to_string(id) + " finished his meals");
}

auto main() -> int {
    auto forks = std::array<std::mutex, philospher_number>{};
    auto output_mutex = std::mutex{};

    auto philosophers = std::array<std::thread, philospher_number>{};
    for (auto i = 0u; i < philospher_number; ++i) {
        std::println("Philosopher {} created", i);
        philosophers[i] = std::thread(philosopher_behaviour, i, std::ref(forks[i]),
                                      std::ref(forks[(i + 1) % philospher_number]), std::ref(output_mutex));
    }

    for (auto i = 0u; i < philospher_number; ++i) {
        philosophers[i].join();
    }

    return 0;
}
