#include <functional>
#include <iostream>
#include <vector>
#include <random>
#include <memory>
#include <fstream>
#include <thread>
#include <chrono>

class Graph {
public:
    Graph(int n) {
        this->n = n;
        adjacency_lists.resize(n);
    }

    void print_adjacency_matrix() const {
        for(int i = 0; i < n; i++) {
            std::cout << i << ":" << std::endl;
            for(auto &j : adjacency_lists[i]) {
                std::cout << j << " ";
            }
            std::cout << std::endl;
        }
    }

    int get_size() const {
        return n;
    }

    std::vector<int>& neighbour_list(int node) {
        return adjacency_lists[node];
    }

    const std::vector<int>& neighbour_list(int node) const {
        return adjacency_lists[node];
    }


private:
    std::vector < std::vector <int> >  adjacency_lists;
    int n;
};


inline Graph construct_clique(int n) {
    Graph graph(n);
    for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
            if(i != j)
                graph.neighbour_list(i).push_back(j);
        }
    }
    return graph;
}


inline Graph construct_path(int n) {
    Graph graph(n);
    for(int i = 0; i < n - 1; i++) {
        graph.neighbour_list(i).push_back(i + 1);
        graph.neighbour_list(i + 1).push_back(i);
    }
    return graph;
}


inline Graph construct_binary_tree(int n) {
    Graph graph(n);

    int current_node = 0;

    for(int i = 0; i < n; i++) {
        if(2 * i + 1 < n)
        {
            graph.neighbour_list(2 * i + 1).push_back(i);
            graph.neighbour_list(i).push_back(2 * i + 1);
        }
        if(2 * i + 2 < n)
        {
            graph.neighbour_list(2 * i + 2).push_back(i);
            graph.neighbour_list(i).push_back(2 * i + 2);
        }
    }

    return graph;
}


inline Graph construct_lolipop(int n) {
    Graph graph(n);

    for(int i = 0; i < 2 * n / 3; i++) {
        for(int j = 0; j < 2 * n / 3; j++) {
            if(i != j)
                graph.neighbour_list(i).push_back(j);
        }
    }

    graph.neighbour_list(2 * n / 3).push_back((2 * n / 3) - 1);
    graph.neighbour_list(2 * n / 3 - 1).push_back((2 * n / 3));

    for(int i = 2 * n / 3; i < n - 1; i++) {
        graph.neighbour_list(i).push_back(i + 1);
        graph.neighbour_list(i + 1).push_back(i);
    }

    return graph;
}


inline void simulate_graph_coverage(long long* ret, const Graph& graph, int starting_pos) {
    int n = graph.get_size();
    std::vector<bool> visited(n, false);

    int visited_cnt = 0;
    int current_node = starting_pos;

    std::random_device rd;
    std::mt19937 gen(rd());

    long long iterations_cnt = 0;

    while(visited_cnt < n) {
        if(!visited[current_node]) {
            visited[current_node] = true;
            visited_cnt++;
        }

        auto& neighbour_list = graph.neighbour_list(current_node);
        int neighbours_cnt = neighbour_list.size();
        std::uniform_int_distribution<> distrib(0, neighbours_cnt - 1);

        int next = distrib(gen);
        current_node = neighbour_list[next];
        iterations_cnt++;
    }

    *ret = iterations_cnt;
}


inline int generate_clique(int node_id, int n) {
    static std::random_device rd;
    static std::mt19937 gen(rd());

    std::uniform_int_distribution<> distrib(0, n - 1);

    int next_node = distrib(gen);
    while(next_node == node_id) {
        next_node = distrib(gen);
    }

    return next_node;
}


inline int generate_path(int node_id, int n) {
    static std::random_device rd;
    static std::mt19937 gen(rd());

    if(node_id == 0) {
        return 1;
    }

    if(node_id == n - 1) {
        return n - 2;
    }

    std::uniform_int_distribution<> distrib(0, 1);

    return distrib(gen) == 0 ? node_id - 1 : node_id + 1;

}


inline int generate_lollipop(int node_id, int n) {
    static std::random_device rd;
    static std::mt19937 gen(rd());

    const int n2_3 = 2 * n / 3;

    int next_node;

    if(node_id < n2_3 - 1) {
        std::uniform_int_distribution<> distrib(0, n2_3 - 1);

        next_node = distrib(gen);
        while(next_node == node_id) {
            next_node = distrib(gen);
        }
    }
    else if(node_id == n2_3 - 1) {
        std::uniform_int_distribution<> distrib(0, n2_3);

        next_node = distrib(gen);
        while(next_node == node_id) {
            next_node = distrib(gen);
        }
    }
    else {
        if(node_id == n - 1) {
            return n - 2;
        }

        std::uniform_int_distribution<> distrib(0, 1);

        next_node = distrib(gen) == 0 ? node_id - 1 : node_id + 1;
    }
    return next_node;
}


inline void simulate_graph_coverage_functional(long long *ret, int n, const std::function<int(int, int)>& generate, int starting_pos) {
    std::vector<bool> visited(n, false);

    int visited_cnt = 0;
    int current_node = starting_pos;

    long long iterations_cnt = 0;

    while(visited_cnt < n) {
        if(!visited[current_node]) {
            visited[current_node] = true;
            visited_cnt++;
        }

        current_node = generate(current_node, n);
        iterations_cnt++;
    }

    *ret = iterations_cnt;
}

