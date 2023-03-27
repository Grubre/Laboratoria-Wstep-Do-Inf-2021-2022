#include <iostream>
#include "graph.hpp"
#include <memory>
#include <stack>
#include <queue>

template<typename Container>
auto traverse(const graph& g) -> std::vector<int> {
    auto visited = std::vector<bool>(g.n, false);
    auto ret = std::vector<int>{};
    ret.reserve(g.n);

    Container to_visit;

    for(auto i = 0; i < g.n; i++) {
        if(visited[i]) {
            continue;
        }
        to_visit.push(i);

        while (!to_visit.empty()) {
            auto current = int{};

            if constexpr(requires{to_visit.top();}) {
                current = to_visit.top();
            }
            else if constexpr(requires{to_visit.front();}) {
                current = to_visit.front();
            }

            to_visit.pop();

            if (!visited[current]) {
                ret.push_back(current);
                visited[current] = true;
            }

            for (auto j : g.neighbours.at(current)) {
                if (!visited[j]) {
                    to_visit.push(j);
                }
            }
        }
    }

    return ret;
}

using dfs_container = std::stack<int>;
using bfs_container = std::queue<int>;

auto main() -> int {
    auto f = std::ifstream{"test.txt"};

    if(auto g = graph::read_graph(f)) {
        std::cout << "dfs:" << std::endl;
        for(auto i : traverse<dfs_container>(*g)) {
            std::cout << i << " ";
        }
        std::cout << std::endl;
        std::cout << "bfs:" << std::endl;
        for(auto i : traverse<bfs_container>(*g)) {
            std::cout << i << " ";
        }
        std::cout << std::endl;

    }
    else {
        std::cout << "Failed to read the file" << std::endl;
    }

    f.close();

    return 0;
}
