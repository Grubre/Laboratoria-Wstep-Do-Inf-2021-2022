#include <iostream>
#include <vector>
#include <cmath>
#include <random>
#include <algorithm>
#include <queue>
#include <fstream>
#include <iterator>
#include <unordered_map>
#include <cassert>
#include "hipercube.hpp"

class Graph {
public:
    Graph(unsigned int k, unsigned int i) {
        neighbours.resize(std::pow(2, k) * 2 + 2);  // +2 for source and sink

        std::random_device rd;
        std::mt19937 gen(rd());
        auto range = std::pair<unsigned int, unsigned int>{neighbours.size() / 2, neighbours.size() - 2};  // -2 to exclude source and sink

        for(auto current = 1u; current < neighbours.size() / 2; current++) {  // Start from 1 to exclude source
            while(neighbours[current].size() < i) {
                std::uniform_int_distribution<unsigned int> dis(range.first, range.second);
                auto potential_neighbour = dis(gen);

                if(std::find(neighbours[current].begin(), neighbours[current].end(), potential_neighbour) == neighbours[current].end() &&
                   std::find(neighbours[potential_neighbour].begin(), neighbours[potential_neighbour].end(), current) == neighbours[potential_neighbour].end()) {
                    neighbours[current].push_back(potential_neighbour);
                    neighbours[potential_neighbour].push_back(current);
                }
            }
            // Add edges from source to all vertices in V1
            neighbours[0].push_back(current);  // Add edge from source
            neighbours[current].push_back(0);  // Add reverse edge to source
        }

        // Add edges from all vertices in V2 to sink
        for(auto current = neighbours.size() / 2; current <= neighbours.size() - 2; current++) {
            neighbours[current].push_back(neighbours.size() - 1);  // Add edge to sink
            neighbours[neighbours.size() - 1].push_back(current);  // Add reverse edge from sink
        }
    }

    void printAdjacencyList() const {
        for (auto i = 0u; i < neighbours.size(); ++i) {
            std::cout << "Node(" << neighbours[i].size() <<") " << i << ": ";
            std::copy(neighbours[i].begin(), neighbours[i].end(), std::ostream_iterator<unsigned int>(std::cout, " "));
            std::cout << '\n';
        }
    }

    int maxFlow() {
        int s = 0, t = neighbours.size() - 1;  // Source and Sink

        std::vector<std::vector<int>> capacities(neighbours.size(), std::vector<int>(neighbours.size(), 0));

        for(auto i = 0u; i < neighbours.size(); i++) {
            for(int j : neighbours[i]) {
                capacities[i][j] = 1;  // All edges have capacity 1
            }
        }

        std::vector<int> parent(neighbours.size());
        int new_flow;

        int flow = 0;
        while((new_flow = bfs(s, t, parent, capacities))) {
            flow += new_flow;
            int cur = t;
            while(cur != s) {
                int prev = parent[cur];
                capacities[prev][cur] -= new_flow;
                capacities[cur][prev] += new_flow;
                cur = prev;
            }
        }

        return flow;
    }

private:
    std::vector<std::vector<unsigned int>> neighbours;

    int bfs(int s, int t, std::vector<int>& parent, std::vector<std::vector<int>>& capacities) {
        fill(parent.begin(), parent.end(), -1);
        parent[s] = -2;
        std::queue<std::pair<int, int>> q;
        q.push({s, std::numeric_limits<int>::max()});

        while (!q.empty()) {
            int cur = q.front().first;
            int flow = q.front().second;
            q.pop();

            for (int next : neighbours[cur]) {
                if (parent[next] == -1 && capacities[cur][next] > 0) {
                    parent[next] = cur;
                    int new_flow = std::min(flow, capacities[cur][next]);
                    if (next == t)
                        return new_flow;

                    q.push({next, new_flow});
                }
            }
        }

        return 0;
    }

};
auto main() -> int {
    auto output = std::ofstream("ex2.csv");
    output << "k,i,maxFlow" << std::endl;

    auto reps = 100u;

    for(int k = 3; k <= 10; k++) {
        std::cout << "k: " << k << std::endl;
        for(auto i = 1; i <= k; i++) {
            for(auto r = 0u; r < reps; r++) {
                Graph graph(k, i);
                auto maxFlow = graph.maxFlow();
                std::cout << k << "," << i << "," << maxFlow << std::endl;
                output << k << "," << i << "," << maxFlow << std::endl;
            }

        }
    }

    output.close();
    // Graph a(3,1);
    // auto maxFlow = a.maxFlow();
    // a.printAdjacencyList();
    // std::cout << "max flow: " << maxFlow << std::endl;
    return 0;
}
