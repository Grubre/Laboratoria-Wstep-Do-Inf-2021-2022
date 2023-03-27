#include <iostream>
#include "graph.hpp"
#include <memory>
#include <stack>
#include <queue>
#include <algorithm>

void strong_connect(int v, int &index, std::vector<int> &indices, std::vector<int> &lowlinks, std::vector<bool> &onStack, std::stack<int> &stack, const graph &graph, std::vector<std::vector<int>> &sccs) {
    indices[v] = index;
    lowlinks[v] = index;
    index++;
    stack.push(v);
    onStack[v] = true;

    for (auto i : graph.neighbours[v]) {
        if (indices[i] == -1) {
            strong_connect(i, index, indices, lowlinks, onStack, stack, graph, sccs);
            lowlinks[v] = std::min(lowlinks[v], lowlinks[i]);
        }
        else if (onStack[i]) {
            lowlinks[v] = std::min(lowlinks[v], indices[i]);
        }
    }

    if (lowlinks[v] == indices[v]) {
        auto scc = std::vector<int>{};
        int w;
        do {
            w = stack.top();
            stack.pop();
            onStack[w] = false;
            scc.push_back(w);
        } while (w != v);

        sccs.push_back(scc);
    }
}

auto get_sccs(const graph& g) -> std::vector<std::vector<int>> {
    auto indices = std::vector<int>(g.n, -1);
    auto low_links = std::vector<int>(g.n, -1);
    auto on_stack = std::vector<bool>(g.n, false);
    auto stack = std::stack<int>{};

    auto index = 0;

    auto sccs = std::vector<std::vector<int>>{};

    for (auto i = 0; i < g.n; ++i) {
        if (indices[i] == -1) {
            strong_connect(i, index, indices, low_links, on_stack, stack, g, sccs);
        }
    }

    return sccs;
}

auto main() -> int {
    auto f = std::ifstream{"./tests/ex3/g3-2.txt"};

    if(auto g = graph::read_graph(f)) {
        auto sccs = get_sccs(*g);
        for(auto& scc : sccs) {
            std::cout << "{";
            for(size_t i = 0; i < scc.size(); i++) {
                std::cout << scc[i];
                if(i != scc.size() - 1) {
                    std::cout << ", ";
                }
            }
            std::cout << "}" << std::endl;
        }
    }
    else {
        std::cout << "Failed to read the file" << std::endl;
    }

    f.close();

    return 0;
}
