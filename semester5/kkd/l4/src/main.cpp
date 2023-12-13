#include <functional>
#include <iomanip>
#include <iostream>
#include <unordered_map>
#include "tga.hpp"

struct processed_image {
    std::vector<std::vector<std::vector<uint8_t>>> corrections;
    std::vector<Image> predicted_images;
};

auto predictor(const Color& A, const Color& B, const Color& C, unsigned id) -> Color {
    switch(id) {
        case 0: return A;
        case 1: return B;
        case 2: return C;
        case 3: return A + B - C;
        case 4: return A + (B - C) / 2;
        case 5: return B + (A - C) / 2;
        case 6: return (A + B) / 2;
        case 7:
            Color color;
            for(auto color_idx = 0u; color_idx < 3u; color_idx++) {
                if(C[color_idx] >= std::max(A[color_idx], B[color_idx])) {
                    color[color_idx] = std::min(A[color_idx], B[color_idx]);
                } else if(C[color_idx] <= std::min(A[color_idx], B[color_idx])) {
                    color[color_idx] = std::max(A[color_idx], B[color_idx]);
                } else {
                    color[color_idx] = A[color_idx] + B[color_idx] - C[color_idx];
                }
            }
            return color;
    }
    std::cerr << "Invalid predictor id: " << id << std::endl;
    exit(1);
}

processed_image process_image(const Image& image) {
    auto predicted_images = std::vector<Image>(8, image);
    std::vector<std::vector<std::vector<uint8_t>>> difference(8, {{}, {}, {}, {}});

    auto default_color = Color{0, 0, 0};
    for(auto y = 0u; y < image.header.height; ++y) {
        for (auto x = 0u; x < image.header.width; x++) {
            auto A = (x == 0) ? default_color : image.colors[y * image.header.width + x - 1];
            auto B = (y == 0) ? default_color : image.colors[(y - 1) * image.header.width + x];
            auto C = (x == 0 || y == 0) ? default_color : image.colors[(y - 1) * image.header.width + x - 1];

            for(auto i = 0u; i < 8; i++) {
                predicted_images[i].colors[y * image.header.width + x] = predictor(A, B, C, i);
            }
        }
    }

    for(auto i = 0u; i < 8; i++) {
        for(auto j = 0u; j < image.colors.size(); j++) {
            const auto color = image.colors[j];
            const auto predicted_color = predicted_images[i].colors[j];
            uint8_t r = color.r - predicted_color.r;
            uint8_t g = color.g - predicted_color.g;
            uint8_t b = color.b - predicted_color.b;

            difference[i][0].push_back(r);
            difference[i][0].push_back(g);
            difference[i][0].push_back(b);
            difference[i][1].push_back(r);
            difference[i][2].push_back(g);
            difference[i][3].push_back(b);
        }
    }

    return processed_image{.corrections = difference, .predicted_images = predicted_images};
}

auto main(int argc, char** argv) -> int {
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <input file>" << std::endl;
        return 1;
    }

    auto image_exp = read_image(argv[1]);
    if (!image_exp) {
        std::cout << "Error: " << (int)image_exp.error() << std::endl;
        return 1;
    }
    auto image = *image_exp;

    auto orig_entropy = entropy(image);
    std::cout << "Original entropy: " << std::setprecision(10) << orig_entropy << std::endl;

    auto [corrections, predicted_images] = process_image(image);

    auto best_predictor = std::array<double, 4>{std::numeric_limits<double>::max(), std::numeric_limits<double>::max(), std::numeric_limits<double>::max(), std::numeric_limits<double>::max()};
    auto best_predictor_idx = std::array<unsigned, 4>{};

    auto names = std::array{"A", "B", "C", "A+B-C", "A+(B-C)/2", "B+(A-C)/2", "(A+B)/2", "New format"};
    auto color_map = std::array{"T", "R", "G", "B"};

    for (auto i = 0u; i < 8; i++) {
        for (auto j = 0u; j < 4; j++) {
            auto ent = entropy(corrections[i][j]);

            if (ent < best_predictor[j]) {
                best_predictor[j] = ent;
                best_predictor_idx[j] = i;
            }
            std::cout << "predictor [" << names[i] << "] for channel [" << color_map[j] << "] has entropy " << std::setprecision(10) << ent << std::endl;
        }
        std::cout << std::endl;
    }

    for (auto i = 0u; i < 4; i++) {
        std::cout << "best predictor [" << names[best_predictor_idx[i]] << "] for channel [" << color_map[i] << "] has entropy " << std::setprecision(10) << best_predictor[i] << std::endl;
    }
}
