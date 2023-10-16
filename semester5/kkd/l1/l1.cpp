#include <cmath>
#include <cstdint>
#include <iostream>
#include <map>
#include <numeric>
#include <optional>
#include <unordered_map>
#include <cstdint>
#include <array>
#include <fstream>
#include <sstream>
#include <concepts>

constexpr auto char_size = 256;

struct Maps {
    std::array<uint32_t, char_size> char_freq;
    std::array<std::array<uint32_t, char_size>, char_size> predecesor_freq;
};

auto count_characters(const std::string& str) -> Maps {
    auto char_freq = std::array<uint32_t, char_size>{};
    auto predecesor_freq = std::array<std::array<uint32_t, char_size>, char_size>{};
    for(auto i = 0u; i < str.size(); i++) {
        auto curr = static_cast<uint8_t>(str[i]);
        char_freq[curr]++;
        auto prev = i > 0 ? str[i - 1] : 0;
        predecesor_freq[curr][prev]++;
    }
    return Maps{.char_freq = char_freq, .predecesor_freq = predecesor_freq};
}

auto open_file(const std::string& filename) -> std::optional<std::string> {
    auto input_file = std::ifstream(filename);
    if(!input_file) {
        return std::nullopt;
    }

    auto buffer = std::stringstream{};
    buffer << input_file.rdbuf();

    return buffer.str();
}

auto print_map(const std::array<uint32_t, char_size>& map, int indentation_level = 1) {
    for(auto i = 0u; i < indentation_level - 1; i++)
        std::cout << '\t';
    std::cout << "[" << std::endl;
    for(auto i = 0u; i < map.size(); i++) {
        if(map[i] > 0u) {
            for(auto i = 0u; i < indentation_level; i++)
                std::cout << '\t';
            std::cout << '\'' << i << "': " << map[i] << ',' << std::endl;
        }
    }
    for(auto i = 0u; i < indentation_level - 1; i++)
        std::cout << '\t';
    std::cout << "]";
}

auto count_present_chars(const std::array<uint32_t, char_size>& arr) {
    return std::accumulate(arr.begin(), arr.end(), 0);
}

auto print_map(const std::array<std::array<uint32_t, char_size>, char_size>& map) {
    std::cout << "{" << std::endl;
    for(auto i = 0u; i < map.size(); i++) {
        auto predecesors = map[i];
        auto sum = count_present_chars(predecesors);
        if(sum > 0u) {
            std::cout << "\t'" << i << ": ";
            print_map(predecesors, 2);
            std::cout << "," << std::endl;
        }
    }
    std::cout << "}" << std::endl;
}

auto probability(uint32_t count, uint32_t max) -> double {
    if(count == 0) {
        return 0.l;
    }
    auto p_x = (double)count / max;
    return p_x * -std::log2(p_x);
}

auto main(int argc, char** argv) -> int {
    if(argc < 2) {
        std::cout << "Provide filepath" << std::endl;
        return 1;
    }
    const auto filename = argv[1];
    const auto input = open_file(filename);
    if (!input) {
        std::cout << "ERROR: Failed to open the file: " << filename << std::endl;
        return 1;
    }

    const auto [char_freq, predecesor_freq] = count_characters(*input);

    const auto char_cnt = count_present_chars(char_freq);

    auto entropy = 0.0l;
    for(auto i = 0u; i < char_freq.size(); i++) {
        entropy += probability(char_freq[i], char_cnt);
    }
    std::cout << "entropy = " << entropy << std::endl;

    auto conditional_entropy = 0.0l;
    for(auto i = 0u; i < predecesor_freq.size(); i++) {
        auto p_x = (double)char_freq[i] / char_cnt;

        auto pred_size = 0u;
        for(auto j = 0u; j < predecesor_freq[i].size(); j++) {
            pred_size += predecesor_freq[j][i];
        }

        auto H = 0.l;
        for(auto j = 0u; j < predecesor_freq[i].size(); j++) {
            auto prob = probability(predecesor_freq[j][i], pred_size);
            H += prob;
        }

        conditional_entropy += p_x * H;
    }
    std::cout << "conditional_entropy = " << conditional_entropy << std::endl;

    // print_map(char_freq);
    // std::cout << std::endl;
    // print_map(predecesor_freq);
    return 0;
}
