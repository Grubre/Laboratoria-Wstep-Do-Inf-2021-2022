#include "tga.hpp"
#include <filesystem>
#include <iostream>

auto average_color(const std::vector<Color> &colors) -> Color {
    auto acc_blue = uint64_t{0};
    auto acc_green = uint64_t{0};
    auto acc_red = uint64_t{0};
    auto pixels_num = uint64_t{0};

    for (const auto &color : colors) {
        acc_blue += color.b;
        acc_green += color.g;
        acc_red += color.r;
        pixels_num++;
    }

    return {static_cast<uint8_t>(static_cast<double>(acc_blue) /
                                 static_cast<double>(pixels_num)),
            static_cast<uint8_t>(static_cast<double>(acc_green) /
                                 static_cast<double>(pixels_num)),
            static_cast<uint8_t>(static_cast<double>(acc_red) /
                                 static_cast<double>(pixels_num))};
}

auto average_distortion(const std::vector<Color> &colors,
                        const std::vector<Color> &codebook) -> double {
    auto distortion = 0.0;
    auto pixels_num = uint64_t{0};
    for (auto i = 0llu; i < colors.size(); i++) {
        distortion += distance(colors[i], codebook[i]);
        pixels_num++;
    }

    return distortion / static_cast<double>(pixels_num);
}

auto average_distortion(const std::vector<Color> &colors,
                        const Color &avg_color) -> double {
    auto acc = 0.0;
    auto pixels_num = uint64_t{0};

    for (const auto &color : colors) {
        acc += std::sqrt(std::pow(color.b - avg_color.b, 2) +
                         std::pow(color.g - avg_color.g, 2) +
                         std::pow(color.r - avg_color.r, 2));
        pixels_num++;
    }

    return acc / static_cast<double>(pixels_num);
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

auto calculate_mse(const Image &original, const Image &quantized) -> double {
    auto mse = 0.0;
    for (auto i = 0llu; i < original.colors.size(); i++) {
        mse += std::pow(distance(original.colors[i], quantized.colors[i]), 2);
    }

    return mse / static_cast<double>(original.colors.size());
}

auto calculate_variance(const Image &image) -> double {
    const auto mean = average_color(image.colors);

    double variance = 0.0;
    for (const auto &color : image.colors) {
        Color diff = color - mean;
        variance += diff[0] * diff[0] + diff[1] * diff[1] + diff[2] * diff[2];
    }

    return variance / (double)image.colors.size();
}

auto calculate_snr(const Image &original, const Image &quantized) -> double {
    const auto mse = calculate_mse(original, quantized);
    const auto variance = calculate_variance(original);

    return 10 * std::log10(variance / mse);
}

auto gen_codebook(const std::vector<Color> &colors, unsigned colors_num,
                  double epsilon) -> std::vector<Color> {
    const auto avg_color = average_color(colors);
    auto avg_distortion = average_distortion(colors, avg_color);

    auto result = std::vector<Color>{avg_color};

    while (result.size() < colors_num) {
        split_codebook(colors, result, avg_distortion, epsilon);
    }

    return result;
}

auto quantify(const Image &image, uint32_t colors_num) -> Image {
    const auto codebook = gen_codebook(image.colors, colors_num, 0.001);

    auto quantized_image = image;
    for (auto i = 0u; i < image.header.width * image.header.height; i++) {
        const auto &current_color = image.colors[i];

        const auto nearest_color_it = std::min_element(
            codebook.begin(), codebook.end(),
            [&current_color](const Color &a, const Color &b) {
                return distance(current_color, a) < distance(current_color, b);
            });

        quantized_image.colors[i] = *nearest_color_it;
    }
    return quantized_image;
}

auto main(int argc, char **argv) -> int {
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0]
                  << " <input.tga> <output.tga> <number of colors>"
                  << std::endl;
        return 1;
    }

    const auto image = read_image(argv[1]);
    auto output = Image{};
    const auto colors_num = std::stoull(argv[3]);

    if (colors_num > 24) {
        std::cerr << "Number of colors must be in [0, 24]" << std::endl;
        return 1;
    }

    const auto quantified_image = quantify(image.value(), 1 << colors_num);

    std::cout << "MSE: " << calculate_mse(image.value(), quantified_image)
              << std::endl;
    std::cout << "SNR: " << calculate_snr(image.value(), quantified_image)
              << " dB" << std::endl;

    if (!save_image(quantified_image, argv[2])) {
        std::cerr << "Failed to save image" << std::endl;
        return 1;
    } else {
        std::cout << "Image saved to " << argv[2] << std::endl;
    }

    return 0;
}
