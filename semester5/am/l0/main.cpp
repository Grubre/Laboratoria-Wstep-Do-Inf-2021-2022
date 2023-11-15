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

using Edge = std::pair<std::size_t, std::size_t>;

struct MST {
    MST(uint32_t n) : neighbours(n, std::vector<uint32_t>{}) {}
    std::vector<std::vector<uint32_t>> neighbours;
};

struct Vec2 {
    int64_t x{};
    int64_t y{};
};

bool is_number(const std::string& s)
{
    return !s.empty() && std::find_if(s.begin(), 
        s.end(), [](unsigned char c) { return !std::isdigit(c); }) == s.end();
}

auto dist(Vec2 a, Vec2 b) -> uint64_t {
    int64_t dx = b.x - a.x;
    int64_t dy = b.y - a.y;

    int64_t distance_squared = dx * dx + dy * dy;

    uint64_t rounded_distance = static_cast<uint64_t>(std::round(std::sqrt(distance_squared)));

    return rounded_distance;
}

auto build_mst(const std::vector<Vec2> points) -> MST {
    const auto n = points.size();

    auto edges = std::vector<Edge>{};
    auto distances = std::vector<std::pair<std::size_t, int64_t>>(n, {-1 ,std::numeric_limits<int64_t>::max()});

    auto current_vertex = 0u;

    while(edges.size() < n - 1) {
        auto min_distance = std::numeric_limits<int64_t>::max();
        auto new_edge = Edge{};
        for(auto i = 0u; i < n; i++) {
            auto distance = dist(points[current_vertex], points[i]);

            if(distance < distances[i].second) {
                distances[i] = {current_vertex, distance};
            }

            if(distances[i].second != 0 && distances[i].second < min_distance) {
                min_distance = distances[i].second;
                new_edge = {distances[i].first, i};
            }
        }
        edges.push_back(new_edge);
        current_vertex = new_edge.second;
    };

    auto mst = MST(n);
    for(auto&[v1, v2] : edges) {
        mst.neighbours[v1].push_back(v2);
    }

    return mst;
}

auto read_file(const std::string& file_path) -> std::vector<Vec2> {
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

auto calculate_mst_weight(const std::vector<Vec2> points, const MST& mst) -> int64_t {
    auto S = 0;
    for(auto i = 0u; i < mst.neighbours.size(); i++) {
        for(auto v : mst.neighbours[i]) {
            S += dist(points[i], points[v]);
        }
    }
    return S;
}

auto get_tsp_cycle(const MST& mst) -> std::vector<std::size_t>{
    const auto n = mst.neighbours.size();
    auto visited = std::vector<bool>(n, false);
    auto to_visit = std::stack<std::size_t>{};

    to_visit.push(0u);
    visited[0u] = true;

    auto cycle = std::vector<std::size_t>{};

    while(!to_visit.empty()) {
        auto current = to_visit.top();
        to_visit.pop();

        cycle.push_back(current);

        for(auto v : mst.neighbours[current]) {
            if(!visited[v]) {
                visited[v] = true;
                to_visit.push(v);
            }
        }
    }

    cycle.push_back(0u);
    return cycle;
}

auto calculate_cycle_weight(const std::vector<Vec2>& points, const std::vector<std::size_t>& cycle) -> int64_t {
    auto cycle_sum = 0u;
    for(auto i = 0u; i < cycle.size() - 1; i++) {
        cycle_sum += dist(points[cycle[i]], points[cycle[i + 1]]);
    }
    return cycle_sum;
}

void permutations(std::vector<Vec2> points) {
    std::random_device rd;
    std::mt19937 g(rd());

    auto avg_10 = 0u;
    auto avg_50 = 0u;
    auto min_weight = std::numeric_limits<int64_t>::max();
    for(auto i = 1u; i <= 1000u; i++) {
        std::shuffle(std::begin(points), std::end(points), g);
        auto cycle_weight = int64_t{0};
        for(auto j = 0u; j < points.size() - 1; j++) { cycle_weight += dist(points[j], points[j + 1]); }
        avg_10 += cycle_weight;
        avg_50 += cycle_weight;
        min_weight = std::min(min_weight, cycle_weight);
        if(i % 10 == 0) {
            std::cout << "avg_10: " << (double)avg_10 / 10.0 << std::endl;;
            avg_10 = 0u;
        }

        if(i % 50 == 0) {
            std::cout << "avg_50: " << (double)avg_50 / 50.0 << std::endl;;
            avg_50 = 0u;
        }
    }
    std::cout << min_weight << std::endl;
}

auto main(int argc, char** argv) -> int {
    const auto file_name = argv[1];
    const auto print_cycle = !std::strcmp(argv[2], "true");
    const auto points = read_file(file_name);

    const auto mst = build_mst(points);
    const auto mst_weight = calculate_mst_weight(points, mst);

    const auto cycle = get_tsp_cycle(mst);

    const auto cycle_sum = calculate_cycle_weight(points, cycle);

    if(print_cycle) {
        for(auto i = 0u; i < cycle.size(); i++) {
            std::cerr << points[cycle[i]].x << " " << points[cycle[i]].y << std::endl;
        }
    }

    std::cout << mst_weight << std::endl;
    std::cout << cycle_sum << std::endl;

    permutations(points);
}
