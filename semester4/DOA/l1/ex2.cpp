#include <iostream>
#include "graph.hpp"
#include <memory>
#include <stack>
#include <queue>
#include <algorithm>

auto dfs(const graph& graph, int i, std::vector<bool>& visited, std::vector<bool>& on_stack, std::stack<int>& stack) -> bool {
    visited[i] = true;
    on_stack[i] = true;
    for(auto j : graph.neighbours[i]) {
        if(!visited[j]) {
            if(dfs(graph, j, visited, on_stack, stack)) {
                return true;
            }
        }
        else if(on_stack[j]) {
            return true;
        }
    }
    on_stack[i] = false;
    stack.push(i);
    return false;
}

auto topological_sort(const graph& g) -> std::optional<std::vector<int>> {
    std::vector<bool> visited(g.n, false);
    std::vector<bool> on_stack(g.n, false);
    std::stack<int> stack;
    for(auto i = 0; i < g.n; i++) {
        if(!visited[i]) {
            if(dfs(g, i, visited, on_stack, stack)) {
                return std::nullopt;
            }
        }
    }
    std::vector<int> result;
    while(!stack.empty()) {
        result.push_back(stack.top());
        stack.pop();
    }
    return result;
}

auto main() -> int {
    auto f = std::ifstream{"./tests/g2b-6.txt"};

    if(auto g = graph::read_graph(f)) {
        if(auto sort = topological_sort(*g)) {
            for(auto i : *sort) {
                std::cout << i << " ";
            }
            std::cout << std::endl;
        } else {
            std::cout << "Graph contains a cycle!" << std::endl;
        }
    }
    else {
        std::cout << "Failed to read the file" << std::endl;
    }

    f.close();

    return 0;
}
