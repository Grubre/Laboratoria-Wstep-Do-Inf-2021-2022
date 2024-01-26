#include "../bitstream.hpp"
#include "../tga.hpp"
#include <iostream>

auto leave_even_indexes(const std::vector<Color> &colors)
    -> std::vector<Color> {
    auto result = std::vector<Color>{};
    result.reserve(colors.size() / 2);

    for (auto i = 0u; i < colors.size(); i += 2) {
        result.push_back(colors[i]);
    }

    return result;
}

auto average_color(const std::vector<Color> &colors) -> Color {
    auto acc_blue = uint64_t{0};
    auto acc_green = uint64_t{0};
    auto acc_red = uint64_t{0};

    for (const auto &color : colors) {
        acc_blue += color.b;
        acc_green += color.g;
        acc_red += color.r;
    }

    return {
        static_cast<uint8_t>(static_cast<double>(acc_blue) / colors.size()),
        static_cast<uint8_t>(static_cast<double>(acc_green) / colors.size()),
        static_cast<uint8_t>(static_cast<double>(acc_red) / colors.size())};
}

auto average_value(const std::vector<int> &vec) -> int {
    int acc = 0;

    for (const auto i : vec) {
        acc += i;
    }

    return acc / (int)vec.size();
}

auto average_distortion(const std::vector<int> &vec,
                        const std::vector<int> &codebook) -> double {
    auto distortion = 0.0;

    for (auto i = 0llu; i < vec.size(); i++) {
        distortion += std::abs(vec[i] - codebook[i]);
    }

    return distortion;
}

auto average_distortion(const std::vector<int> &vec, int avg) -> double {
    auto distortion = 0.0;

    for (auto i = 0llu; i < vec.size(); i++) {
        distortion += std::abs(vec[i] - avg);
    }

    return distortion;
}

auto average_distortion(const std::vector<Color> &colors,
                        const std::vector<Color> &codebook) -> double {
    auto distortion = 0.0;
    for (auto i = 0llu; i < colors.size(); i++) {
        distortion += distance(colors[i], codebook[i]);
    }

    return distortion / colors.size();
}

auto average_distortion(const std::vector<Color> &colors,
                        const Color &avg_color) -> double {
    auto acc = 0.0;

    for (const auto &color : colors) {
        acc += std::sqrt(std::pow(color.b - avg_color.b, 2) +
                         std::pow(color.g - avg_color.g, 2) +
                         std::pow(color.r - avg_color.r, 2));
    }

    return acc / static_cast<double>(colors.size());
}

void split_codebook(const std::vector<Color> &colors,
                    std::vector<Color> &codebook, double &distortion,
                    double epsilon) {
    auto new_codebook = std::vector<Color>{};
    new_codebook.reserve(codebook.size() * 2);
    std::ranges::for_each(codebook, [&new_codebook](const auto &color) {
        const auto adjust_color = [](uint8_t channel) {
            return std::pair<uint8_t, uint8_t>{
                (channel == 255) ? channel : channel + 1,
                (channel == 0) ? channel : channel - 1};
        };

        const auto [newBlue1, newBlue2] = adjust_color(color.b);
        const auto [newGreen1, newGreen2] = adjust_color(color.g);
        const auto [newRed1, newRed2] = adjust_color(color.r);

        new_codebook.emplace_back(Color{newBlue1, newGreen1, newRed1});
        new_codebook.emplace_back(Color{newBlue2, newGreen2, newRed2});
    });

    auto error = 1.0 + epsilon;
    auto avg_distortion = 0.0;
    auto prev_distortion = 0.0;
    while (error > epsilon) {
        auto centroid_idxs = std::vector<std::size_t>(colors.size());

        auto centroid_pixels = std::vector<std::vector<size_t>>(
            new_codebook.size(), std::vector<size_t>());

        // Assigning pixels to nearest centroids
        for (std::size_t i = 0; i < colors.size(); ++i) {
            const auto &color = colors[i];
            auto nearest = std::ranges::min_element(
                new_codebook, [&, i](const auto &a, const auto &b) {
                    return distance(color, a) < distance(color, b);
                });
            std::size_t nearest_idx =
                std::distance(new_codebook.begin(), nearest);
            centroid_idxs[i] = nearest_idx;
            centroid_pixels[nearest_idx].push_back(i);
        }

        // Updating centroids
        for (std::size_t i = 0; i < new_codebook.size(); ++i) {
            if (!centroid_pixels[i].empty()) {
                std::vector<Color> pixels_for_centroid;
                std::transform(centroid_pixels[i].begin(),
                               centroid_pixels[i].end(),
                               std::back_inserter(pixels_for_centroid),
                               [&](std::size_t idx) { return colors[idx]; });

                new_codebook[i] = average_color(pixels_for_centroid);
            }
        }

        prev_distortion = avg_distortion > 0 ? avg_distortion : distortion;
        avg_distortion = average_distortion(colors, new_codebook);

        error = std::abs(avg_distortion - prev_distortion) / avg_distortion;
    }

    codebook = new_codebook;
}

void split_codebook(const std::vector<int> &vec, std::vector<int> &codebook,
                    double &distortion, double epsilon) {
    auto new_codebook = std::vector<int>{};
    new_codebook.reserve(codebook.size() * 2);

    for (auto i : codebook) {
        new_codebook.emplace_back(i - 1);
        new_codebook.emplace_back(i + 1);
    }

    auto error = 1.0 + epsilon;
    auto avg_distortion = 0.0;
    auto prev_distortion = 0.0;
    while (error > epsilon) {
        auto centroid_idxs = std::vector<std::size_t>(vec.size());

        auto centroid_pixels = std::vector<std::vector<size_t>>(
            new_codebook.size(), std::vector<size_t>());

        // Assigning pixels to nearest centroids
        for (std::size_t i = 0; i < vec.size(); ++i) {
            const auto &value = vec[i];
            auto nearest = std::ranges::min_element(
                new_codebook, [&, i](const auto &a, const auto &b) {
                    return std::abs(value - a) < std::abs(value - b);
                });
            std::size_t nearest_idx =
                std::distance(new_codebook.begin(), nearest);
            centroid_idxs[i] = nearest_idx;
            centroid_pixels[nearest_idx].push_back(i);
        }

        // Updating centroids
        for (std::size_t i = 0; i < new_codebook.size(); ++i) {
            if (!centroid_pixels[i].empty()) {
                std::vector<int> pixels_for_centroid;
                std::transform(centroid_pixels[i].begin(),
                               centroid_pixels[i].end(),
                               std::back_inserter(pixels_for_centroid),
                               [&](std::size_t idx) { return vec[idx]; });

                new_codebook[i] = average_value(pixels_for_centroid);
            }
        }

        prev_distortion = avg_distortion > 0 ? avg_distortion : distortion;
        avg_distortion = average_distortion(vec, new_codebook);

        error = std::abs(avg_distortion - prev_distortion) / avg_distortion;
    }

    codebook = new_codebook;
}

auto gen_codebook(const std::vector<int> &vec, int colors_num, double epsilon)
    -> std::vector<int> {
    const auto avg = average_value(vec);
    auto avg_distortion = average_distortion(vec, avg);

    auto result = std::vector{avg};

    while (result.size() < colors_num) {
        split_codebook(vec, result, avg_distortion, epsilon);
    }

    return result;
}

auto quantify(const std::vector<int> &vec, int val_num)
    -> std::pair<std::vector<int>, std::vector<int>> {
    const auto codebook = gen_codebook(vec, val_num, 0.01);

    auto quantized_vec = vec;
    for (auto i = 0u; i < vec.size(); i++) {
        const auto &current_value = vec[i];

        const auto nearest_color_it =
            std::min_element(codebook.begin(), codebook.end(),
                             [&current_value](const int &a, const int &b) {
                                 return std::abs(current_value - a) <
                                        std::abs(current_value - b);
                             });

        quantized_vec[i] = *nearest_color_it;
    }
    return {quantized_vec, codebook};
}

template <typename T>
void encode_k_bit_number(T number, uint64_t k, OutputBitstream &output) {
    for (auto i = 0u; i < k; i++) {
        output.put_bit((number >> (k - 1 - i)) & 1);
    }
}

void encode_sequence(const std::vector<Color> &sequence, uint64_t k,
                     OutputBitstream &output) {
    for (auto color_idx = 0u; color_idx < 3; color_idx++) {
        for (const auto &color : sequence) {
            encode_k_bit_number(color[color_idx], k, output);
        }
    }
}

void encode_sequence(const std::vector<int> &sequence, uint64_t k,
                     OutputBitstream &output) {
    for (const auto &value : sequence) {
        encode_k_bit_number(value, k, output);
    }
}

auto main(int argc, char **argv) -> int {
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <input.tga> <output> <k>"
                  << std::endl;
        return 1;
    }

    const uint8_t k = std::stoi(argv[3]);

    if (k > 7) {
        std::cerr << "k must be in [0,7]" << std::endl;
        return 1;
    }

    const auto image = read_image(argv[1]);
    const auto &x = image.value().colors;
    auto y = x;
    auto z = x;

    y = leave_even_indexes(y);
    z = leave_even_indexes(z);

    for (auto i = 0u; i < y.size() - 1; i++) {
        y[i] = average(x[2 * i + 1], x[2 * i]);
    }

    for (auto i = 0u; i < z.size(); i++) {
        z[i] = x[2 * i + 1] - y[i];
    }

    auto y_b = std::vector<int>{y[0].b};
    auto y_g = std::vector<int>{y[0].g};
    auto y_r = std::vector<int>{y[0].r};

    auto z_b = std::vector<int>{z[0].b};
    auto z_g = std::vector<int>{z[0].g};
    auto z_r = std::vector<int>{z[0].r};

    for (auto i = 1u; i < y.size(); i++) {
        y_b.push_back(y[i].b);
        y_g.push_back(y[i].g);
        y_r.push_back(y[i].r);

        z_b.push_back(z[i].b);
        z_g.push_back(z[i].g);
        z_r.push_back(z[i].r);
    }

    auto y_b_diff = std::vector<int>{y[0].b};
    auto y_g_diff = std::vector<int>{y[0].g};
    auto y_r_diff = std::vector<int>{y[0].r};

    for (auto i = 1u; i < y.size(); i++) {
        y_b_diff.push_back(y[i].b - y[i - 1].b);
        y_g_diff.push_back(y[i].g - y[i - 1].g);
        y_r_diff.push_back(y[i].r - y[i - 1].r);
    }

    auto [quantized_y_b, codebook_y_b] = quantify(y_b_diff, 1 << k);
    auto [quantized_y_g, codebook_y_g] = quantify(y_g_diff, 1 << k);
    auto [quantized_y_r, codebook_y_r] = quantify(y_r_diff, 1 << k);
    const auto [quantized_z_b, _1] = quantify(z_b, 1 << k);
    const auto [quantized_z_g, _2] = quantify(z_g, 1 << k);
    const auto [quantized_z_r, _3] = quantify(z_r, 1 << k);

    const auto find_closest_in_codebook = [](const auto &codebook,
                                             int value) -> int {
        return *std::ranges::min_element(
            codebook, [value](const auto &a, const auto &b) {
                return std::abs(value - a) < std::abs(value - b);
            });
    };

    auto acc_b = y_b[0];
    auto acc_g = y_g[0];
    auto acc_r = y_r[0];

    for (auto i = 1u; i < y.size(); i++) {
        auto t_b = y_b[i] - acc_b;
        auto t_g = y_g[i] - acc_g;
        auto t_r = y_r[i] - acc_r;

        t_b = find_closest_in_codebook(codebook_y_b, t_b);
        t_g = find_closest_in_codebook(codebook_y_g, t_g);
        t_r = find_closest_in_codebook(codebook_y_r, t_r);

        quantized_y_b[i] = t_b;
        quantized_y_g[i] = t_g;
        quantized_y_r[i] = t_r;

        acc_b += t_b;
        acc_g += t_g;
        acc_r += t_r;
    }

    auto output = OutputBitstream{argv[2]};

    // encode width
    encode_k_bit_number(image.value().header.width, 16, output);

    // encode height
    encode_k_bit_number(image.value().header.height, 16, output);

    // encode y
    encode_sequence(quantized_y_b, 32, output);
    encode_sequence(quantized_y_g, 32, output);
    encode_sequence(quantized_y_r, 32, output);

    // encode z
    encode_sequence(quantized_z_b, 32, output);
    encode_sequence(quantized_z_g, 32, output);
    encode_sequence(quantized_z_r, 32, output);

    return 0;
}
