#include "bitstream.hpp"
#include "lzw.hpp"
#include <cmath>
#include <iostream>
#include <fstream>
#include <memory>
#include <numeric>
#include <sstream>
#include <array>

auto open_file(const std::string& filename) -> std::optional<std::string> {
    auto input_file = std::ifstream(filename);
    if(!input_file) {
        return std::nullopt;
    }

    auto buffer = std::stringstream{};
    buffer << input_file.rdbuf();

    return buffer.str();
}

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
        predecesor_freq[curr][(unsigned long)prev]++;
    }
    return Maps{.char_freq = char_freq, .predecesor_freq = predecesor_freq};
}


auto count_present_chars(const std::array<uint32_t, char_size>& arr) {
    return std::accumulate(arr.begin(), arr.end(), 0);
}



auto probability(uint32_t count, uint32_t max) -> double {
    if(count == 0) {
        return 0.l;
    }
    auto p_x = (double)count / max;
    return p_x * -std::log2(p_x);
}

long double calculate_entropy(const std::string& filename) {
    const auto input = open_file(filename);
    if (!input) {
        std::cout << "ERROR: Failed to open the file: " << filename << std::endl;
        return 1;
    }

    const auto [char_freq, predecesor_freq] = count_characters(*input);
    const auto char_cnt = count_present_chars(char_freq);

    auto entropy = 0.0l;
    for(auto i = 0u; i < char_freq.size(); i++) {
        entropy += probability(char_freq[i], (uint32_t)char_cnt);
    }

    return entropy;
}



auto main(int argc, char** argv) -> int {
    if (argc < 4) {
        std::cout << "Usage: ./encoder input_file_path output_file_path encoding_type" << std::endl;
    }

    std::cout << "entropy of the file before encoding = " << calculate_entropy(argv[1]) << std::endl;

    auto input_file = std::ifstream(argv[1]);
    auto output = OutputBitstream(argv[2]);
    auto encoder = std::unique_ptr<UniversalEncoder>();

    if(argv[3] == std::string{"gamma"}) {
        encoder = std::make_unique<EliasGammaEncoder>(output);
    } else if(argv[3] == std::string{"delta"}) {
        encoder = std::make_unique<EliasDeltaEncoder>(output);
    } else if(argv[3] == std::string{"omega"}) {
        encoder = std::make_unique<EliasOmegaEncoder>(output);
    } else if(argv[3] == std::string{"fib"}) {
        encoder = std::make_unique<EliasFibonacciEncoder>(output);
    } else {
        std::cout << "Unknown encoding type" << std::endl;
        return 1;
    }

    encode(input_file, encoder.get());

    std::cout << "entropy of the file after encoding = " << calculate_entropy(argv[2]) << std::endl;

    input_file.close();
    return 0;
}
