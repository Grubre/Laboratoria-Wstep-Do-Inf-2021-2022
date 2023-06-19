#pragma once
#include <fstream>
#include <iostream>
#include <limits>
#include <vector>
#include <unordered_map>
#include <cmath>
#include <bitset>
#include <random>
#include <optional>
#include <deque>
#include <cassert>
#include <queue>

inline auto count_ones(unsigned int num) -> unsigned int {
    auto bits = std::bitset<32>(num); // Convert the unsigned integer to a bitset of size 32 (assuming 32-bit unsigned int)
    return bits.count(); // Count the number of ones and return the result
}

inline auto generate_capacity(int l) -> unsigned int {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<unsigned int> dis(1, std::pow(2, l));

    return dis(gen);
}

class hipercube {
public:
    hipercube(unsigned int k) : k(k) {
        n = std::pow(2,k);

        for(auto i = 0u; i < n; i++) {
            auto fi = std::unordered_map<unsigned int, unsigned int>{};
            auto ci = std::unordered_map<unsigned int, unsigned int>{};

            for(auto j = 0u; j < k; j++) {
                auto key = static_cast<unsigned int>(std::pow(2.0, static_cast<double>(j))) ^ i;
                fi[key] = 0;
                if(key > i) {
                    auto l = std::max(count_ones(key), k - count_ones(key));
                    l = std::max(l, count_ones(i));
                    l = std::max(l, k - count_ones(i));
                    auto capacity = generate_capacity(l);
                    ci[key] = capacity;
                } else {
                    ci[key] = 0u;                    
                }
            }
            f.push_back(fi);
            c.push_back(ci);
        }
    }

    auto find_path() -> std::optional<std::vector<unsigned int>> {
        auto t = n - 1;
        auto stack = std::deque<unsigned int>{};
        auto pre = std::vector<unsigned int>{};
        auto visited = std::vector<bool>{};
        stack.push_back(0u);
        for(auto _ = 0u; _ < n; _++) {
            pre.push_back(std::numeric_limits<unsigned int>::max());
            visited.push_back(false);
        }
        visited[0] = true;

        while(!stack.empty() && pre[t] == std::numeric_limits<unsigned int>::max()) {
            assert(!stack.empty());
            auto first = stack.front();
            stack.pop_front();
            auto u = first;
            for(auto i = 0u; i < k; i++) {
                auto v = static_cast<unsigned int>(std::pow(2, i)) ^ u;
                if(!visited[v] && c[u][v] > f[u][v]) {
                    visited[v] = true;
                    stack.push_back(v);
                    pre[v] = u;
                }
            }
        }

        if(pre[t] == std::numeric_limits<unsigned int>::max()) {
            return std::nullopt;
        }

        auto path = std::vector<unsigned int>{t};
        auto x = t;
        while(x != 0) {
            path.push_back(pre[x]);
            x = pre[x];
        }

        return path;
    }

    auto edmonds_karp() -> std::pair<unsigned int, unsigned int> {
        auto num_paths = 0u;

        // set flow to 0
        for(auto i = 0u; i < n; i++) {
            for(auto j = 0u; j < k; j++) {
                auto key = static_cast<unsigned int>(std::pow(2,j)) ^ i;
                f[i][key] = 0;
            }
        }

        // while there exists a path in residual graph
        auto path_opt = find_path();
        while (path_opt.has_value()) {
            auto path = *path_opt;
            num_paths++;

            // we find the smallest possible flow value we can add to each
            // edge in the path so that it is not viable anymore
            auto min_cap = std::numeric_limits<unsigned int>::max();
            for(auto i = 0u; i < path.size() - 1; i++) {
                auto u = path[i + 1];
                auto v = path[i];
                min_cap = std::min(min_cap, c[u][v] - f[u][v]);
            }

            // we add that value to each edge in the path
            for(auto i = 0u; i < path.size() - 1; i++) {
                auto u = path[i + 1];
                auto v = path[i];
                // std::cout << "u: " << u << ", v: " << v << std::endl;
                f[u][v] += min_cap;
                f[v][u] -= min_cap;
            }

            path_opt = find_path();
        }

        auto max_flow = 0u;
        for(auto i = 0u; i < k; i++) {
            auto v = std::pow(2, i);
            max_flow += f[0][v];
        }
        return {max_flow, num_paths};
    }

    auto dinic() -> std::pair<unsigned int, unsigned int> {
        unsigned int max_flow = 0;
        unsigned int num_paths = 0;
        while (bfs(0, std::pow(2,k) - 1)) {
            std::vector<std::unordered_map<unsigned int, unsigned int>::iterator> ptr(n);
            for (unsigned int i = 0; i < n; ++i) {
                ptr[i] = c[i].begin();
            }
            while (auto pushed = dfs(0, std::pow(2,k) - 1, INF, ptr)) {
                max_flow += pushed;
                ++num_paths;
            }
        }
        return {max_flow, num_paths};
    }

private:
    static constexpr unsigned int INF = std::numeric_limits<unsigned int>::max();

    std::vector<int> level;

    auto bfs(unsigned int s, unsigned int t) -> bool {
        level.assign(n, -1);
        level[s] = 0;

        std::queue<unsigned int> q;
        q.push(s);

        while (!q.empty()) {
            auto v = q.front();
            q.pop();

            for (const auto& [to, cap] : c[v]) {
                if (cap <= f[v][to])
                    continue;

                if (level[to] == -1) {
                    level[to] = level[v] + 1;
                    q.push(to);
                }
            }
        }

        return level[t] != -1;
    }

    auto dfs(unsigned int v, unsigned int t, unsigned int flow, std::vector<std::unordered_map<unsigned int, unsigned int>::iterator>& ptr) -> unsigned int {
        if (v == t || flow == 0)
            return flow;

        for (; ptr[v] != c[v].end(); ++ptr[v]) {
            auto to = ptr[v]->first;
            if (level[v] + 1 != level[to] || c[v][to] - f[v][to] < 1)
                continue;

            if (auto pushed = dfs(to, t, std::min(flow, c[v][to] - f[v][to]), ptr)) {
                f[v][to] += pushed;
                f[to][v] -= pushed;
                return pushed;
            }
        }

        return 0;
    }

public:
    unsigned int k;
    unsigned int n;
    std::vector<std::unordered_map<unsigned int, unsigned int>> f;
    std::vector<std::unordered_map<unsigned int, unsigned int>> c;
};
