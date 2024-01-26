#include "../bitstream.hpp"
#include "../tga.hpp"
#include <cstdint>
#include <iostream>

template <typename T>
auto decode_k_bit_number(int k, InputBitstream &input) -> T {
    auto result = 0;
    for (auto i = 0; i < k; i++) {
        auto bit = input.next_bit();
        if (!bit) {
            throw std::runtime_error("Unexpected end of file");
        }
        result = (result << 1) | *bit;
    }
    return result;
}

auto decode_sequence(size_t size, int k, InputBitstream &input)
    -> std::vector<int> {
    auto result = std::vector<int>{};

    for (auto i = 0u; i < size; i++) {
        result.push_back(decode_k_bit_number<int>(k, input));
    }

    return result;
}

auto main(int argc, char **argv) -> int {
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <input> <output.tga> <input.tga>"
                  << std::endl;
        return 1;
    }

    auto file = read_image(argv[3]);
    auto input = InputBitstream{argv[1]};

    const auto width = decode_k_bit_number<uint16_t>(16, input);
    const auto height = decode_k_bit_number<uint16_t>(16, input);

    const auto y_b = decode_sequence(width * height / 2, 32, input);
    const auto y_g = decode_sequence(width * height / 2, 32, input);
    const auto y_r = decode_sequence(width * height / 2, 32, input);

    const auto z_b = decode_sequence(width * height / 2, 32, input);
    const auto z_g = decode_sequence(width * height / 2, 32, input);
    const auto z_r = decode_sequence(width * height / 2, 32, input);

    auto x = std::vector<Color>(width * height);

    for (auto i = 0u; i < width * height / 2; i++) {
        x[2 * i + 1].b = y_b[i] + z_b[i];
        x[2 * i + 1].g = y_g[i] + z_g[i];
        x[2 * i + 1].r = y_r[i] + z_r[i];

        x[2 * i].b = y_b[i] - z_b[i];
        x[2 * i].g = y_g[i] - z_g[i];
        x[2 * i].r = y_r[i] - z_r[i];
    }

    Image output = *file;
    output.colors = x;

    save_image(output, argv[2]);
}
