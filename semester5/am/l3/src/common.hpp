#pragma once
#include <cstdint>
#include <cstring>
#include <iostream>
#include <fstream>
#include <numeric>
#include <set>
#include <sstream>
#include <algorithm>
#include <vector>
#include <cmath>
#include <limits>
#include <stack>
#include <random>
#include <functional>

struct Vec2 {
    int64_t x{};
    int64_t y{};
};

inline auto is_number(const std::string& s) -> bool
{
    return !s.empty() && std::find_if(s.begin(), 
        s.end(), [](unsigned char c) { return !std::isdigit(c); }) == s.end();
}

inline auto dist(Vec2 a, Vec2 b) -> int64_t {
    int64_t dx = b.x - a.x;
    int64_t dy = b.y - a.y;

    int64_t distance_squared = dx * dx + dy * dy;

    int64_t rounded_distance = static_cast<int64_t>(std::round(std::sqrt(distance_squared)));

    return rounded_distance;
}

inline auto read_file(const std::string& file_path) -> std::vector<Vec2> {
    auto file = std::ifstream{file_path};

    auto line = std::string{};

    auto points = std::vector<Vec2>{};

    while(std::getline(file, line)) {
        auto s = std::stringstream(line);

        auto first = std::string{};
        s >> first;
        if(!is_number(first)) {
            continue;
        }
        
        int64_t x,y;
        s >> x >> y;
        points.push_back({x, y});
    }

    file.close();

    return points;
}

