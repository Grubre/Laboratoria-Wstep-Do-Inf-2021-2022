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
#include <algorithm>

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


//struct to model Bipartite Graph
struct Bipartite
{
    //Constructor
    Bipartite(int _k, int _i)
    {
        k = _k;
        n = pow(2, k + 1);
        i = _i;

        //init connections vector of size |V2|
        connections = std::vector<std::unordered_map<unsigned, unsigned>>(pow(2, k));

        //Vec to hold possible indices from V2 and shuffle them 
        std::vector<int> indices(pow(2, k)); 
        //Fill them 0, 1, ... , |V2|-1
        std::iota (std::begin(indices), std::end(indices), 0); 

        auto rng = std::default_random_engine {};

        for(auto& node : connections)
        {
            std::shuffle(std::begin(indices), std::end(indices), rng);

            //Every node from V1 has i connections to nodes from V2
            for(int ind = 0; ind < _i; ind++)
            {
                node.insert({static_cast<int>(indices[ind]), 1});
            }

            std::cout<<node.size() <<" "<<_i<<std::endl;
            assert(node.size() == _i);
        }

    }

    //wielkosc liczba z zakresu {1-16}
    int k;
    //ilosc sasiadow które posiada node z V1
    int i;
    //ilosc node'ow w sumie w obu grupkach V1 i V2 czyli 2^(k+1)
    int n;

    //vector z zakodowanymi polaczeniami (key, 1), gdzie key to index w node z V2
    std::vector<std::unordered_map<unsigned, unsigned>> connections;
};


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

    hipercube(Bipartite _b)
    {
        //modelujemy pod source w 0 i t = n-1
        k = _b.k;
        //dwie grupy V1 i V2 po 2^k i dwa nody source i target
        n = pow(2, k+1) + 2;

        f = std::vector<std::unordered_map<unsigned, unsigned>>();
        c = std::vector<std::unordered_map<unsigned, unsigned>>();

        //source is 0
        //convention we enumerate V1 from 1 ... to 2^k
        //V2 from 2^k + 1 to 2^(k+1)
        //target is 2^(k+1) + 1

        const int source = 0;
        const int dest = n - 1;

        for(int i = 0; i < n; ++i)
        {
            auto fi = std::unordered_map<unsigned, unsigned>();
            auto ci = std::unordered_map<unsigned, unsigned>();

            if(i == 0)
            {
                //source is connected to all from V1
                int v2_size = pow(2, k);

                for(int v2_elem = 1; v2_elem < v2_size + 1; v2_elem++)
                {
                    fi.insert({v2_elem, 0});
                    ci.insert({v2_elem, 1});
                }

            }
            else if(i == dest)
            {
                //dest is connected to none
            }
            else if(i < (pow(2, k) + 1))
            {
                std::cout<<"entered"<<std::endl;
                //V1 is connected to v2 as it is encoded in bipartite graph
                const int offset = pow(2, k);

                auto conn = _b.connections[i-1];
                std::cout<<i-1<<" "<<conn.size()<<std::endl;
                for (auto& [key, value]: conn) 
                {  
                    std::cout<<i<<"is connected to"<<std::endl;
                    std::cout<<key + offset<<std::endl;

                    fi.insert({key + offset, 0});
                    ci.insert({key + offset, 1});
                }
            }
            else
            {
                //V2 is only connected to dest
                fi.insert({dest, 0});
                ci.insert({dest, 1});
            }

            f.push_back(fi);
            c.push_back(ci);
        }

            std::cout<<"Flows"<<std::endl;
        std::cout<<std::endl;


        for(auto &elem : f) {
            std::cout<<elem.size()<<std::endl;
        }

        std::cout<<"Costs"<<std::endl;
        std::cout<<std::endl;

        for(auto &elem : c) {
            std::cout<<elem.size()<<std::endl;
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
