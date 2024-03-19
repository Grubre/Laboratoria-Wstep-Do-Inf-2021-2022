#include "../bitstream.hpp"
#include "../tga.hpp"
#include <iostream>
#include <span>
#include <bitset>

void write_bits_to_file(std::ofstream &outputFile, std::vector<bool> &bitsVector) {
    size_t residue = bitsVector.size() % 8;
    size_t bitsToRead = bitsVector.size() - residue;

    for (size_t i = 0; i < bitsToRead; i += 8) {
            uint8_t byteValue{0};
        for (ssize_t j = 7; j >= 0; j--) {
            byteValue <<= 1;
            byteValue |= bitsVector[j + i];
        }

        outputFile.write(reinterpret_cast<char *>(&byteValue), sizeof(byteValue));
    }

    bitsVector.erase(bitsVector.begin(), bitsVector.begin() + bitsToRead);
}

template<size_t N>
void write_bits(std::bitset<N> number,
         std::vector<bool> &buffer) {
    for (size_t i = 0; i < N; i++) {
        buffer.emplace_back(number[i]);
    }
}

void flushBuffer(std::ofstream &outputFile, std::vector<bool> &buffer) {
    if (!buffer.empty()) {
        while (buffer.size() % 8 != 0) {
            buffer.push_back(false);
        }
        write_bits_to_file(outputFile, buffer);
    }
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

auto distance(double a, double b) -> double { return std::pow(a - b, 2); }

auto collection_distortion(const std::span<const double> span1,
        const std::span<const double> span2) -> double {
    auto distortion = 0.0;
    for (auto i = 0u; i < span1.size(); i++) {
        distortion += distance(span1[i], span2[i]);
    }

    return distortion / span1.size();
}

auto collection_element_distortion(const std::span<const double> span,
                                   double element) -> double {
    auto distortion = 0.0;
    for (auto i = 0u; i < span.size(); i++) {
        distortion += distance(span[i], element);
    }

    return distortion / span.size();
}

auto collection_average(const std::span<const double> span) -> double {
    double sum = 0;
    for (const auto &value : span) {
        sum += value;
    }

    return sum / span.size();
}

auto find_nearest_value(const std::vector<double> &codebook, int16_t value) -> int16_t {
    double min_dist = std::numeric_limits<double>::max();
    int16_t quantized_value = 0;
    for (const auto value : codebook) {
        const auto casted_value = static_cast<int16_t>(value);
        const auto dist = distance(casted_value, value);
        if (dist < min_dist) {
            min_dist = dist;
            quantized_value = casted_value;
        }
    }

    return quantized_value;
}

void gen_codebook(std::vector<double>& input, std::vector<double> &codebook, uint8_t bits, double epsilon) {
    const uint32_t codebook_size = 1 << bits;
    const double average = collection_average(input);
    auto distortion = collection_element_distortion(input, average);

    codebook.push_back(average);

    auto new_codebook = std::vector<double>{};
    const auto delta = 0.7;
    while (codebook.size() < codebook_size) {
        for(uint32_t i = 0; i < codebook.size(); i++) {

            new_codebook.push_back(codebook[i] + delta);
            new_codebook.push_back(codebook[i] - delta);
        }
    }

    auto relative_error = 1.0 + epsilon;
    auto average_distortion = 0.0;
    auto prev_distortion = 0.0;

    while(relative_error > epsilon) {
        auto nearest_centroid_indexes = std::vector<std::size_t>(input.size());
        auto elements_for_centroid = std::vector<std::vector<size_t>>(codebook.size(), std::vector<size_t>());

        for (auto i = 0u; i < input.size(); i++) {
            auto nearest_centroid_index = 0u;
            auto nearest_centroid_dist = distance(input[i], new_codebook[0]);

            for(auto j = 1u; j < new_codebook.size(); j++) {
                const auto dist = distance(input[i], new_codebook[j]);
                if(dist < nearest_centroid_dist) {
                    nearest_centroid_dist = dist;
                    nearest_centroid_index = j;
                }
            }

            nearest_centroid_indexes[i] = nearest_centroid_index;
            elements_for_centroid[nearest_centroid_index].push_back(i);
        }


        for (auto i = 0u; i < new_codebook.size(); i++) {
            if (!elements_for_centroid[i].empty()) {
                std::vector<double> elements;
                for(const auto idx : elements_for_centroid[i]) {
                    elements.push_back(input[idx]);
                }
                new_codebook[i] = collection_average(elements);
            }
        }

        auto closest_centroids = std::vector<double>{};
        for(const auto idx : nearest_centroid_indexes) {
            closest_centroids.push_back(new_codebook[idx]);
        }

        prev_distortion = average_distortion > 0 ? average_distortion : distortion;
        average_distortion = collection_distortion(input, closest_centroids);

        relative_error = std::abs(average_distortion - prev_distortion) / average_distortion;
    }

    codebook = new_codebook;
}

auto quantize_collection(const std::span<const int16_t> span, uint8_t bits) -> std::vector<double> {
    auto input_double = std::vector<double>{};
    input_double.reserve(span.size());

    for (const auto value : span) {
        input_double.push_back(value);
    }

    auto codebook = std::vector<double>{};
    gen_codebook(input_double, codebook, bits, 0.1);

    return codebook;
}

void process_channel(std::vector<int16_t>& input,
                     std::vector<int16_t> &output_low,
                     std::vector<int16_t> &output_high,
                     uint8_t bits) {
    auto low = std::vector<int16_t>{};
    for (auto i = 0u; i < input.size(); i += 2) {
        size_t j = i == 0 ? 0 : i - 1;
        low.push_back((input[i] + input[j]) / 2);
    }

    auto high = std::vector<int16_t>{};
    for (auto i = 0u; i < input.size(); i += 2) {
        size_t j = i == 0 ? 0 : i - 1;
        high.push_back(input[i] - low[j]);
    }

    auto low_diff = std::vector<int16_t>{low[0]};
    for (auto i = 1u; i < low.size(); i++) {
        low_diff.push_back(low[i] - low[i - 1]);
    }

    auto low_diff_quantization_codebook = quantize_collection(low_diff, bits);

    auto low_diff2 = std::vector<int16_t>{};
    low_diff2.emplace_back(low[0]);
    auto decoded_low = clamp(find_nearest_value(low_diff_quantization_codebook, low[0]));

    for(auto i = 1; i < low.size(); i++) {
        auto diff = low[i] - decoded_low;
        low_diff2.emplace_back(diff);
        decoded_low = clamp(decoded_low + find_nearest_value(low_diff_quantization_codebook, diff));
    }

    for (auto i = 0u; i < low_diff2.size(); i++) {
        output_low.push_back(find_nearest_value(low_diff_quantization_codebook, low_diff2[i]));
    }

    auto high_quantization_codebook = quantize_collection(high, bits);

    for (auto i = 0u; i < high.size(); i++) {
        output_high.push_back(find_nearest_value(high_quantization_codebook, high[i]));
    }
}

auto main(int argc, char **argv) -> int {
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <input.tga> <output.tga> <bits>" << std::endl;
    }

    const auto input_filename = argv[1];
    const auto output_filename = argv[2];
    const auto bits = static_cast<uint8_t>(std::stoi(argv[3]));

    auto input = read_image(input_filename);

    if(!input) {
        std::cerr << "Failed to read input image" << std::endl;
        return 1;
    }

    auto output_image = *input;
    auto output = std::ofstream(output_filename, std::ios::binary);

    auto red_channel = std::vector<int16_t>{};
    auto green_channel = std::vector<int16_t>{};
    auto blue_channel = std::vector<int16_t>{};

    for (auto i = 0u; i < (*input).colors.size(); i++) {
        red_channel.push_back((*input).colors[i].r);
        green_channel.push_back((*input).colors[i].g);
        blue_channel.push_back((*input).colors[i].b);
    }

    if (red_channel.size() % 2 != 0) {
        red_channel.push_back(0);
        green_channel.push_back(0);
        blue_channel.push_back(0);
    }

    auto red_channel_low = std::vector<int16_t>{};
    auto red_channel_high = std::vector<int16_t>{};
    auto green_channel_low = std::vector<int16_t>{};
    auto green_channel_high = std::vector<int16_t>{};
    auto blue_channel_low = std::vector<int16_t>{};
    auto blue_channel_high = std::vector<int16_t>{};

    std::cout << "Processing red channel" << std::endl;

    process_channel(red_channel, red_channel_low, red_channel_high, bits);
    process_channel(green_channel, green_channel_low, green_channel_high, bits);
    process_channel(blue_channel, blue_channel_low, blue_channel_high, bits);

    std::cout << "Writing output" << std::endl;

    write_header(output_image, output);

    auto bits_buffer = std::vector<bool>{};

    write_bits<8>(bits, bits_buffer);

    for (auto i = 0u; i < red_channel_low.size(); i++) {
        write_bits<16>(red_channel_low[i], bits_buffer);
        write_bits<16>(green_channel_low[i], bits_buffer);
        write_bits<16>(blue_channel_low[i], bits_buffer);
    }

    for (auto i = 0u; i < red_channel_high.size(); i++) {
        write_bits<16>(red_channel_high[i], bits_buffer);
        write_bits<16>(green_channel_high[i], bits_buffer);
        write_bits<16>(blue_channel_high[i], bits_buffer);
    }

    flushBuffer(output, bits_buffer);

    write_footer(output_image, output);

    return 0;
}
