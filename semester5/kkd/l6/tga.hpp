#pragma once
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <expected>
#include <fstream>
#include <iostream>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

template <typename T, typename E> struct Result {
    Result(T &&value) : value_(std::forward<T>(value)), has_value_(true) {}
    Result(E &&error) : error_(std::forward<E>(error)), has_value_(false) {}

    using value_type = T;
    using error_type = E;

    bool has_value() const { return has_value_; }

    T &value() & { return value_; }

    const T &value() const & { return value_; }

    E &error() & { return error_; }

    const E &error() const & { return error_; }

    static Result unexpected(E &&error) { return Result(std::move(error)); }

    static Result expected(T &&value) { return Result(std::move(value)); }

    auto operator!() const -> bool { return !has_value_; }

    auto operator*() & -> T & { return value_; }

    ~Result() {
        if (has_value_) {
            value_.~T();
        } else {
            error_.~E();
        }
    }

  private:
    union {
        T value_;
        E error_;
    };
    bool has_value_ = false;
};

struct Color {
    uint8_t b = 0;
    uint8_t g = 0;
    uint8_t r = 0;

    auto operator+(const Color other) const -> Color {
        return Color{
            static_cast<uint8_t>(((int)b + other.b) % 256),
            static_cast<uint8_t>(((int)g + other.g) % 256),
            static_cast<uint8_t>(((int)r + other.r) % 256),
        };
    }

    auto operator-(const Color other) const -> Color {
        return Color{
            static_cast<uint8_t>(((int)b - other.b + 256) % 256),
            static_cast<uint8_t>(((int)g - other.g + 256) % 256),
            static_cast<uint8_t>(((int)r - other.r + 256) % 256),
        };
    }

    auto operator/(const uint8_t scalar) const -> Color {
        return Color{
            static_cast<uint8_t>(b / scalar),
            static_cast<uint8_t>(g / scalar),
            static_cast<uint8_t>(r / scalar),
        };
    }

    auto operator[](const unsigned index) const -> uint8_t {
        switch (index) {
        case 0:
            return b;
        case 1:
            return g;
        case 2:
            return r;
        }
        std::cerr << "Invalid color index: " << index << std::endl;
        exit(1);
    }

    auto operator[](const unsigned index) -> uint8_t & {
        switch (index) {
        case 0:
            return b;
        case 1:
            return g;
        case 2:
            return r;
        }
        std::cerr << "Invalid color index: " << index << std::endl;
        exit(1);
    }

    auto operator==(const Color &other) const -> bool {
        return r == other.r && g == other.g && b == other.b;
    }
};

inline auto operator<<(std::ostream &os, const Color &color) -> std::ostream & {
    os << "(" << (int)color.b << ", " << (int)color.g << ", " << (int)color.r
       << ")";
    return os;
}

inline auto average(const Color &color1, const Color &color2) {
    return Color(std::midpoint(color1.b, color2.b),
                 std::midpoint(color1.g, color2.g),
                 std::midpoint(color1.r, color2.r));
}

inline auto distance(const Color &color1, const Color &color2) -> double {
    return std::sqrt(std::pow(color1.b - color2.b, 2) +
                     std::pow(color1.g - color2.g, 2) +
                     std::pow(color1.r - color2.r, 2));
}

inline auto max(const Color &color1, const Color &color2) -> Color {
    return Color{
        std::max(color1.b, color2.b),
        std::max(color1.g, color2.g),
        std::max(color1.r, color2.r),
    };
}

inline auto min(const Color &color1, const Color &color2) -> Color {
    return Color{
        std::min(color1.b, color2.b),
        std::min(color1.g, color2.g),
        std::min(color1.r, color2.r),
    };
}

namespace std {
template <> struct hash<Color> {
    std::size_t operator()(const Color &color) const noexcept {
        return std::hash<uint8_t>()(color.r) ^ std::hash<uint8_t>()(color.g) ^
               std::hash<uint8_t>()(color.b);
    }
};
} // namespace std

enum class Err { FileNotFound, InvalidFormat };

struct Header {
    uint8_t id_length = 0;
    uint8_t color_map_type = 0;
    uint8_t image_type = 0;
    uint16_t color_map_origin = 0;
    uint16_t color_map_length = 0;
    uint8_t color_map_depth = 0;
    uint16_t x_origin = 0;
    uint16_t y_origin = 0;
    uint16_t width = 0;
    uint16_t height = 0;
    uint8_t pixel_depth = 0;
    uint8_t image_descriptor = 0;

    std::string id;
};

struct Image {
    Header header;
    std::string id;
    std::vector<Color> colors;
    std::string footer;
};

inline auto read_header(std::ifstream &file) -> Header {
    Header header;
    file.read(reinterpret_cast<char *>(&header.id_length),
              sizeof(header.id_length));
    file.read(reinterpret_cast<char *>(&header.color_map_type),
              sizeof(header.color_map_type));
    file.read(reinterpret_cast<char *>(&header.image_type),
              sizeof(header.image_type));
    file.read(reinterpret_cast<char *>(&header.color_map_origin),
              sizeof(header.color_map_origin));
    file.read(reinterpret_cast<char *>(&header.color_map_length),
              sizeof(header.color_map_length));
    file.read(reinterpret_cast<char *>(&header.color_map_depth),
              sizeof(header.color_map_depth));
    file.read(reinterpret_cast<char *>(&header.x_origin),
              sizeof(header.x_origin));
    file.read(reinterpret_cast<char *>(&header.y_origin),
              sizeof(header.y_origin));
    file.read(reinterpret_cast<char *>(&header.width), sizeof(header.width));
    file.read(reinterpret_cast<char *>(&header.height), sizeof(header.height));
    file.read(reinterpret_cast<char *>(&header.pixel_depth),
              sizeof(header.pixel_depth));
    file.read(reinterpret_cast<char *>(&header.image_descriptor),
              sizeof(header.image_descriptor));

    return header;
}

inline Result<Image, Err> read_image(const std::string &filename) {
    auto file = std::ifstream(filename);
    if (!file) {
        return Result<Image, Err>::unexpected(Err::FileNotFound);
    }

    Header header = read_header(file);

    auto id = std::string(header.id_length, '\0');
    file.read(id.data(), header.id_length);

    auto colors = std::vector<Color>(header.width * header.height);

    if (header.image_type == 2) {
        for (uint16_t y = 0u; y < header.height; y++) {
            for (uint16_t x = 0u; x < header.width; x++) {
                auto yd = header.height - y - 1u;
                auto xd = x;
                auto index = yd * header.width + xd;
                file.read(reinterpret_cast<char *>(&colors[index]),
                          sizeof(colors[index]));
            }
        }
    } else {
        return Result<Image, Err>::unexpected(Err::InvalidFormat);
    }

    auto footer = std::string(26, '\0');

    for (auto j = 0u; j < footer.size(); ++j) {
        auto byte = char{};
        file.read(&byte, sizeof(byte));
        footer[j] = byte;
    }

    return Result<Image, Err>::expected(
        Image{header, std::move(id), std::move(colors), std::move(footer)});
}

inline Result<bool, Err> save_image(const Image &image, const char *filename) {
    auto file = std::ofstream(filename);
    if (!file) {
        return Result<bool, Err>::unexpected(Err::FileNotFound);
    }

    file.write(reinterpret_cast<const char *>(&image.header.id_length),
               sizeof(image.header.id_length));
    file.write(reinterpret_cast<const char *>(&image.header.color_map_type),
               sizeof(image.header.color_map_type));
    file.write(reinterpret_cast<const char *>(&image.header.image_type),
               sizeof(image.header.image_type));
    file.write(reinterpret_cast<const char *>(&image.header.color_map_origin),
               sizeof(image.header.color_map_origin));
    file.write(reinterpret_cast<const char *>(&image.header.color_map_length),
               sizeof(image.header.color_map_length));
    file.write(reinterpret_cast<const char *>(&image.header.color_map_depth),
               sizeof(image.header.color_map_depth));
    file.write(reinterpret_cast<const char *>(&image.header.x_origin),
               sizeof(image.header.x_origin));
    file.write(reinterpret_cast<const char *>(&image.header.y_origin),
               sizeof(image.header.y_origin));
    file.write(reinterpret_cast<const char *>(&image.header.width),
               sizeof(image.header.width));
    file.write(reinterpret_cast<const char *>(&image.header.height),
               sizeof(image.header.height));
    file.write(reinterpret_cast<const char *>(&image.header.pixel_depth),
               sizeof(image.header.pixel_depth));
    file.write(reinterpret_cast<const char *>(&image.header.image_descriptor),
               sizeof(image.header.image_descriptor));

    file.write(image.id.c_str(), image.header.id_length);

    for (uint16_t y = 0u; y < image.header.height; y++) {
        for (uint16_t x = 0u; x < image.header.width; x++) {
            auto yd = image.header.height - y - 1u;
            auto xd = x;
            auto index = yd * image.header.width + xd;
            file.write(reinterpret_cast<const char *>(&image.colors[index]),
                       sizeof(image.colors[index]));
        }
    }

    file.write(image.footer.c_str(), (unsigned)image.footer.size());

    return Result<bool, Err>::expected(true);
}

inline unsigned long unique_colors(const Image &image) {
    std::unordered_map<Color, bool> encountered_colors;

    for (auto &color : image.colors) {
        encountered_colors[color] = true;
    }

    return encountered_colors.size();
}

inline int compare_images(const Image &image1, const Image &image2) {
    if (image1.header.width != image2.header.width ||
        image1.header.height != image2.header.height) {
        return -1;
    }

    auto count = 0;

    for (auto i = 0u; i < image1.colors.size(); ++i) {
        if (image1.colors[i] != image2.colors[i]) {
            ++count;
        }
    }

    return count;
}

struct Entropy {
    double total = 0;
    double r = 0;
    double g = 0;
    double b = 0;
};

inline std::ostream &operator<<(std::ostream &os, const Entropy &entropy) {
    os << "(total: " << entropy.total << ", r: " << entropy.r
       << ", g: " << entropy.g << ", b: " << entropy.b << ")";
    return os;
}

inline Entropy entropy(const Image &image) {
    constexpr auto channel_size = 256;

    std::vector<uint64_t> red_counter(channel_size, 0),
        green_counter(channel_size, 0), blue_counter(channel_size, 0);

    for (const auto &color : image.colors) {
        ++red_counter[color.r];
        ++green_counter[color.g];
        ++blue_counter[color.b];
    }

    auto calculate_entropy = [&](const std::vector<uint64_t> &counter) {
        return std::accumulate(
            counter.begin(), counter.end(), 0.0,
            [&](double acc, uint64_t count) {
                if (count > 0) {
                    double probability =
                        static_cast<double>(count) /
                        (image.header.width * image.header.height);
                    return acc - probability * std::log2(probability);
                }
                return acc;
            });
    };

    double red_entropy = calculate_entropy(red_counter);
    double green_entropy = calculate_entropy(green_counter);
    double blue_entropy = calculate_entropy(blue_counter);

    std::vector<uint64_t> total_counter(
        channel_size * channel_size * channel_size, 0);
    for (const auto &color : image.colors) {
        ++total_counter[color.b * channel_size * channel_size +
                        color.g * channel_size + color.r];
    }

    double total_entropy = calculate_entropy(total_counter);

    return Entropy{total_entropy, red_entropy, green_entropy, blue_entropy};
}

inline double entropy(const std::vector<uint8_t> &numvec) {
    auto count = std::unordered_map<uint8_t, unsigned>{};

    for (auto num : numvec) {
        count[num]++;
    }

    auto entropy = 0.0;
    for (auto [_, num] : count) {
        auto prob = static_cast<double>(num) / (double)numvec.size();
        entropy -= prob * std::log2(prob);
    }

    return entropy;
}
