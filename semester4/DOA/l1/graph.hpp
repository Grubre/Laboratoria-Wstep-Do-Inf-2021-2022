#pragma once
#include <fstream>
#include <vector>
#include <map>
#include <optional>
#include <ranges>
#include <iostream>

struct graph {
    graph(int n, bool directed = true) : directed(directed), n(n), neighbours(n) {}

    bool directed;
    int n;
    int m;
    std::vector<std::vector<int>> neighbours;

    auto add(int a, int b) {
        neighbours[a].push_back(b);
        m++;
    }

    auto transpose() -> void {
        std::vector<std::vector<int>> new_neighbours(n);
        for(auto i = 0; i < n; i++) {
            for(size_t j = 0; j < neighbours[i].size(); j++) {
                new_neighbours[j].push_back(i);
            }
        }
        neighbours = std::move(new_neighbours);
    }

    static auto read_graph(std::ifstream& s) -> std::optional<graph> {
        char du;
        int n, m;
        if(!(s >> du >> n >> m)) {
            return {};
        }

        bool is_directed = (du == 'D');

        // std::cout << "du: " << du << std::endl;
        // std::cout << "n: " << n << std::endl;
        // std::cout << "m: " << m << std::endl;


        // std::cout << std::boolalpha << is_directed << std::endl;

        graph g(n, is_directed);

        for(auto i = 0; i < m; i++) {
            int a,b;
            if(!(s >> a >> b)) {
                return {};
            }
            g.add(a - 1,b - 1);
            if(!g.directed) {
                g.add(b - 1,a - 1);
            }
        }

        return g;
    }
};


