#include <iostream>
#include "graph.hpp"
#include <memory>
#include <stack>
#include <queue>
#include <algorithm>

auto bfs(const graph &g, std::vector<int> &colors) -> bool{
    colors.assign(g.n, -1);

    for (int start = 0; start < g.n; ++start) {
        if (colors[start] == -1) {
            auto container = std::queue<int>{};
            container.push(start);
            colors[start] = 0;

            while (!container.empty()) {
                auto node = container.front();
                container.pop();

                for (auto neighbour : g.neighbours[node]) {
                    if (colors[neighbour] == -1) {
                        colors[neighbour] = 1 - colors[node];
                        container.push(neighbour);
                    } else if (colors[neighbour] == colors[node]) {
                        return false;
                    }
                }
            }
        }
    }

    return true;
}

using bipartition = std::pair<std::vector<int>, std::vector<int>>;
auto bipartition_graph(const graph &g) -> std::optional<bipartition>{
    auto colors = std::vector<int>{};

    if (bfs(g, colors)) {
        std::vector<int> setA, setB;
        for (size_t i = 0; i < colors.size(); ++i) {
            if (colors[i] == 0) {
                setA.push_back(i);
            } else {
                setB.push_back(i);
            }
        }
        return std::make_pair(setA, setB);
    } else {
        return std::nullopt;
    }
}

auto main() -> int {
    auto f = std::ifstream{"./tests/ex4/d4b-2.txt"};

    if(auto g = graph::read_graph(f)) {
        if(auto partitions = bipartition_graph(*g)) {
            auto print = [](const std::vector<int>& partition){
                std::cout << "{";
                for(size_t i = 0; i < partition.size(); i++) {
                    std::cout << partition[i];
                    if(i != partition.size() - 1)
                        std::cout << ", ";
                }
                std::cout << "}" << std::endl;
            };
            print(partitions->first);
            print(partitions->second);
        }
        else {
            std::cout << "Graph can't be partitioned" << std::endl;
        }
    }
    else {
        std::cout << "Failed to read the file" << std::endl;
    }

    f.close();

    return 0;
}
