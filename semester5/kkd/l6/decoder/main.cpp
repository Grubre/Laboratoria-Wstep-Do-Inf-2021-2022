#include "../tga.hpp"
#include <bitset>
#include <cstdint>
#include <iostream>

void read_bits(std::ifstream &inputFile, std::vector<bool> &bitsVector) {
  uint8_t byteValue{0};
  inputFile.read(reinterpret_cast<char *>(&byteValue), sizeof(byteValue));

  for (ssize_t i = 7; i >= 0; i--) {
 bitsVector.push_back((byteValue & (1 << (7 - i))) != 0);
  }
}

template<size_t N>
std::bitset<N> read_n_bits(std::ifstream &inputFile,
           std::vector<bool> &buffer) {
  std::bitset<N> result;

  while (buffer.size() < N) {
 read_bits(inputFile, buffer);
  }

  for (size_t i = 0; i < N; i++) {
 result[i] = buffer[i];
  }

  buffer.erase(buffer.begin(), buffer.begin() + N);

  return result;
}


template<typename T, size_t N>
T read_num_from_file(std::ifstream &inputFile,
      std::vector<bool> &buffer) {
  std::bitset<N> bits = read_n_bits<N>(inputFile, buffer);

  return (T)bits.to_ulong();
}


auto clamp(int16_t value) -> int16_t {
    if (value > 255) {
        return 255;
    }

    if (value < 0) {
        return 0;
    }

    return value;
}

auto main(int argc, char** argv) -> int {
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <input.tga> <output.tga>" << std::endl;
        return 1;
    }

    const auto input_filename = argv[1];
    const auto output_filename = argv[2];

    auto input = std::ifstream(input_filename, std::ios::binary);

    if (!input) {
        std::cerr << "Failed to read input image" << std::endl;
        return 1;
    }

    auto image = Image{};
    image.header = read_header(input);

    auto buffer = std::vector<bool>{};

    size_t bits_num = (image.header.height * image.header.width) / 2;

    uint8_t significant_bits = read_num_from_file<uint8_t, 8>(input, buffer);
    (void)significant_bits;

    auto red_channel_low = std::vector<int16_t>{};
    auto red_channel_high = std::vector<int16_t>{};
    auto green_channel_low = std::vector<int16_t>{};
    auto green_channel_high = std::vector<int16_t>{};
    auto blue_channel_low = std::vector<int16_t>{};
    auto blue_channel_high = std::vector<int16_t>{};

    for (size_t i = 0; i < bits_num; i++) {
        red_channel_low.push_back(read_num_from_file<int16_t, 16>(input, buffer));
        green_channel_low.push_back(read_num_from_file<int16_t, 16>(input, buffer));
        blue_channel_low.push_back(read_num_from_file<int16_t, 16>(input, buffer));
    }

    for (size_t i = 0; i < bits_num; i++) {
        red_channel_high.push_back(read_num_from_file<int16_t, 16>(input, buffer));
        green_channel_high.push_back(read_num_from_file<int16_t, 16>(input, buffer));
        blue_channel_high.push_back(read_num_from_file<int16_t, 16>(input, buffer));
    }

    auto red_channel = std::vector<int16_t>(2 * red_channel_low.size(), 0);
    auto green_channel = std::vector<int16_t>(2 * green_channel_low.size(), 0);
    auto blue_channel = std::vector<int16_t>(2 * blue_channel_low.size(), 0);
    int16_t red_channel_acc = clamp(red_channel_low[0]);
    int16_t green_channel_acc = clamp(green_channel_low[0]);
    int16_t blue_channel_acc = clamp(blue_channel_low[0]);
    red_channel[0] = red_channel_acc;
    for (auto i = 1u; i < red_channel_low.size(); i++) {
        red_channel_acc = clamp(red_channel_acc + red_channel_low[i]);
        red_channel[2 * i] = red_channel_acc + red_channel_high[i];
        red_channel[2 * i - 1] = red_channel_acc - red_channel_high[i];

        green_channel_acc = clamp(green_channel_acc + green_channel_low[i]);
        green_channel[2 * i] = green_channel_acc + green_channel_high[i];
        green_channel[2 * i - 1] = green_channel_acc - green_channel_high[i];

        blue_channel_acc = clamp(blue_channel_acc + blue_channel_low[i]);
        blue_channel[2 * i] = blue_channel_acc + blue_channel_high[i];
        blue_channel[2 * i - 1] = blue_channel_acc - blue_channel_high[i];
    }

    image.colors = std::vector<Color>(red_channel.size(), Color{});

    for (auto i = 0u; i < red_channel.size(); i++) {
        image.colors[i].r = clamp(red_channel[i]);
        image.colors[i].g = clamp(green_channel[i]);
        image.colors[i].b = clamp(blue_channel[i]);
    }

    save_image(image, output_filename);

    return 0;
}
