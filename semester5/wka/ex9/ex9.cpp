#include <iostream>
#include <vector>
#include <random>
#include <concepts>
#include <algorithm>
#include <fstream>

template <typename T>
auto random_permutation(std::vector<T> vec) -> std::vector<T>  {
    static auto rd = std::random_device{};
    static auto gen = std::mt19937(rd());
    for(auto i = 1u; i <= vec.size(); i++) {
        auto s = vec.size() - i;
        auto distribution = std::uniform_int_distribution<>(0, s);
        auto offset = distribution(gen);
        auto rand_index = vec.begin() + offset;
        auto last_index = vec.begin() + s;
        std::iter_swap(rand_index, last_index);
    }
    return vec;
}

template <std::equality_comparable T>
auto fixed_points(const std::vector<T>& vec1, const std::vector<T>& vec2) -> unsigned int {
    const auto smaller_size = std::min(vec1.size(), vec2.size());
    auto count = 0u;
    for(auto i = 0; i < smaller_size; i++) {
        if(vec1[i] == vec2[i]) count++;
    }
    return count;
}

template <std::integral T>
auto count_cycles(const std::vector<T>& vec) -> unsigned int {
    auto count = 0u;
    auto visited = std::vector<bool>(vec.size(), false);
    auto i = 0u;
    while(i < vec.size()) {
        if(visited[i]) {
            i++;
            continue;
        }
        auto j = i;
        while(!visited[j]) {
            visited[j] = true;
            j = vec[j];
        }
        count++;
        i++;
    }
    return count;
}

template <typename T>
auto print_vector(const std::vector<T>& vec) {
    std::cout << "[";
    for(auto i = 0u; i < vec.size(); i++) {
        std::cout << vec[i];
        if(i < vec.size() - 1) {
            std::cout << ", ";
        }
    }
    std::cout << "]" << std::endl;;
}

template <std::integral T>
auto get_iota_vec(unsigned int n) -> std::vector<T> {
    auto vec = std::vector<T>(n);
    std::iota(vec.begin(), vec.end(), 0);
    return vec;
}

auto main() -> int {
    constexpr auto k_max = 10'000;
    constexpr auto n_min = 5;
    constexpr auto n_max = 5000;
    constexpr auto jump = 5;

    auto output = std::ofstream("data.csv");

    output << "n,no_fixed_points_percentage,one_fixed_point_percentage,cycles" << std::endl;

    for(auto n = n_min; n <= n_max; n+=jump) {
        std::cout << "n: " << n << std::endl;;
        auto vec = get_iota_vec<int>(n);
        auto no_fixed_points = 0u;
        auto one_fixed_point = 0u;
        auto avg_cycles = 0.L;
        for(auto k = 0; k < k_max; k++) {
            auto perm = random_permutation(vec);
            auto fp = fixed_points(vec, perm);
            auto cycles = count_cycles(perm);

            no_fixed_points += (fp == 0);
            one_fixed_point += (fp == 1);
            avg_cycles += cycles;
        }
        output << n << ",";
        output << (double)no_fixed_points / (double)k_max << ",";
        output << (double)one_fixed_point / (double)k_max << ",";
        output << avg_cycles / (double)k_max << "\n";
    }

    return 0;
}
