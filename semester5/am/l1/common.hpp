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

struct SearchReturn {
    std::vector<std::size_t> best_cycle;
    unsigned int number_of_moves;
    int64_t best_cycle_weight;
};

using Edge = std::pair<std::size_t, std::size_t>;

struct MST {
    MST(uint32_t n) : neighbours(n, std::vector<uint32_t>{}) {}
    std::vector<std::vector<uint32_t>> neighbours;
};

struct Vec2 {
    int64_t x{};
    int64_t y{};
};

inline auto is_number(const std::string& s) -> bool
{
    return !s.empty() && std::find_if(s.begin(), 
        s.end(), [](unsigned char c) { return !std::isdigit(c); }) == s.end();
}

inline auto dist(Vec2 a, Vec2 b) -> uint64_t {
    int64_t dx = b.x - a.x;
    int64_t dy = b.y - a.y;

    int64_t distance_squared = dx * dx + dy * dy;

    uint64_t rounded_distance = static_cast<uint64_t>(std::round(std::sqrt(distance_squared)));

    return rounded_distance;
}

inline auto build_mst(const std::vector<Vec2>& points) -> MST {
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
        mst.neighbours[v2].push_back(v1);
    }

    return mst;
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

inline auto calculate_mst_weight(const std::vector<Vec2>& points, const MST& mst) -> int64_t {
    auto S = 0;
    for(auto i = 0u; i < mst.neighbours.size(); i++) {
        for(auto v : mst.neighbours[i]) {
            S += dist(points[i], points[v]);
        }
    }
    return S;
}

inline auto get_tsp_cycle(const MST& mst, std::size_t starting_vertex) -> std::vector<std::size_t> {
    const auto n = mst.neighbours.size();
    auto visited = std::vector<bool>(n, false);
    auto to_visit = std::stack<std::size_t>{};

    to_visit.push(starting_vertex);
    visited[starting_vertex] = true;

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

    cycle.push_back(starting_vertex);
    return cycle;
}

inline auto calculate_cycle_weight(const std::vector<Vec2>& points, const std::vector<std::size_t>& cycle) -> int64_t {
    auto cycle_sum = 0u;
    for(auto i = 0u; i < cycle.size() - 1; i++) {
        cycle_sum += dist(points[cycle[i]], points[cycle[i + 1]]);
    }
    return cycle_sum;
}

inline void print_cycle(const std::vector<Vec2>& points, const std::vector<std::size_t>& cycle) {
    for(auto i = 0u; i < cycle.size(); i++) {
        std::cout << points[cycle[i]].x << " " << points[cycle[i]].y << std::endl;
    }
}

inline void invert(std::vector<std::size_t>& cycle, std::size_t i, std::size_t j) {
    std::reverse(cycle.begin() + i, cycle.begin() + j + 1);
}

inline auto local_search(const std::vector<Vec2>& points, std::vector<std::size_t>&& cycle)
    -> SearchReturn {
    auto best_cycle = std::move(cycle);
    auto best_cycle_weight = calculate_cycle_weight(points, best_cycle);
    auto n = best_cycle.size();
    auto number_of_moves = 0u;
    bool improved = true;

    while (improved) {
        improved = false;
        auto new_best_i = -1;
        auto new_best_j = -1;
        auto best_diff = 0;
        for (std::size_t i = 0; i <= n - 2; ++i) {
            for (std::size_t j = i + 1; j < n; ++j) {
                if (i == 0 && j == n - 1) {
                    continue;
                }
                
                auto diff = 0;

                if ( i == 0 ) {
                    diff -= dist(points[best_cycle[n - 1]], points[best_cycle[i]]);
                    diff += dist(points[best_cycle[n - 1]], points[best_cycle[j]]);
                } else {
                    diff -= dist(points[best_cycle[i - 1]], points[best_cycle[i]]);
                    diff += dist(points[best_cycle[i - 1]], points[best_cycle[j]]);
                }

                if ( j == n - 1 ) {
                    diff -= dist(points[best_cycle[0]], points[best_cycle[j]]);
                    diff += dist(points[best_cycle[0]], points[best_cycle[i]]);
                } else {
                    diff -= dist(points[best_cycle[j + 1]], points[best_cycle[j]]);
                    diff += dist(points[best_cycle[j + 1]], points[best_cycle[i]]);
                }

                if (diff < best_diff) {
                    best_diff = diff;
                    best_cycle_weight = best_cycle_weight + diff;
                    new_best_i = i;
                    new_best_j = j;
                    improved = true;
                }
            }
        }
        if (improved) {
            number_of_moves++;
            invert(best_cycle, new_best_i, new_best_j);
        }
    }
    
    return SearchReturn{best_cycle, number_of_moves, best_cycle_weight};
}


// NOTE: Broken, should be done the same as local_search
inline auto faster_local_search(const std::vector<Vec2>& points, std::vector<std::size_t>&& cycle)
    -> SearchReturn {
    auto best_cycle = std::move(cycle);
    auto best_cycle_weight = calculate_cycle_weight(points, best_cycle);
    auto n = best_cycle.size();
    auto number_of_moves = 0u;
    bool improved = true;

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dist(0, n - 2);

    while (improved) {
        improved = false;

        for (int iter = 0; iter < n; ++iter) {
            std::size_t i = dist(gen);
            std::uniform_int_distribution<> dist_j(i + 1, n - 1);
            std::size_t j = dist_j(gen);

            invert(best_cycle, i, j);
            auto new_weight = calculate_cycle_weight(points, best_cycle);
            invert(best_cycle, i, j);

            if (new_weight < best_cycle_weight) {
                invert(best_cycle, i, j);
                best_cycle_weight = new_weight;
                improved = true;
                number_of_moves++;
            }
        }
    }

    return SearchReturn{best_cycle, number_of_moves, best_cycle_weight};
}


struct tabu_params {
    std::size_t current_tabu_size;
    std::size_t number_of_moves;
    std::size_t iters_since_last_improvement;
};


inline auto tabu_search(const std::vector<Vec2>& points, std::vector<std::size_t>&& cycle,
                        const unsigned tabu_list_size,
                        std::function<bool(tabu_params)> stop_function)
    -> SearchReturn {
    auto best_cycle = std::move(cycle);
    auto best_cycle_weight = calculate_cycle_weight(points, best_cycle);
    auto n = best_cycle.size();
    auto number_of_moves = 0u;
    bool improved = true;

    auto tabu_list = std::deque<std::pair<std::size_t, std::size_t>>{};

    while (stop_function(tabu_params{tabu_list.size(), number_of_moves})) {
        improved = false;
        auto new_best_i = -1;
        auto new_best_j = -1;
        
        auto best_diff = 0;
        for (std::size_t i = 0; i <= n - 2; ++i) {
            for (std::size_t j = i + 1; j < n; ++j) {
                if (i == 0 && j == n - 1) {
                    continue;
                }
                
                auto diff = 0;

                if ( i == 0 ) {
                    diff -= dist(points[best_cycle[n - 1]], points[best_cycle[i]]);
                    diff += dist(points[best_cycle[n - 1]], points[best_cycle[j]]);
                } else {
                    diff -= dist(points[best_cycle[i - 1]], points[best_cycle[i]]);
                    diff += dist(points[best_cycle[i - 1]], points[best_cycle[j]]);
                }

                if ( j == n - 1 ) {
                    diff -= dist(points[best_cycle[0]], points[best_cycle[j]]);
                    diff += dist(points[best_cycle[0]], points[best_cycle[i]]);
                } else {
                    diff -= dist(points[best_cycle[j + 1]], points[best_cycle[j]]);
                    diff += dist(points[best_cycle[j + 1]], points[best_cycle[i]]);
                }

                if (diff < best_diff) {
                    best_diff = diff;
                    best_cycle_weight = best_cycle_weight + diff;
                    new_best_i = i;
                    new_best_j = j;
                    improved = true;
                }
            }
        }

        if (improved) {
            number_of_moves++;
            tabu_list.push_back({new_best_i, new_best_j});
            invert(best_cycle, new_best_i, new_best_j);
        }

        if (tabu_list.size() > tabu_list_size) {
            tabu_list.pop_front();
        }


    }
    
    return SearchReturn{best_cycle, number_of_moves, best_cycle_weight};
}