#include <cuda_runtime.h>
#include <cuda.h>
#include <curand_kernel.h>
#include "common.hpp"
#include <iostream>
#include <array>
#include <span>
#include <unordered_map>
#include <stdio.h>

constexpr auto population_size = 5;

constexpr auto threadsPerBlock = 256;
constexpr auto blocksPerGrid = (population_size + threadsPerBlock - 1) / threadsPerBlock;

void calculate_dist_between_nodes(int64_t *dist_between_nodes, const std::span<const Vec2> node_coords) {
    for(auto i = 0u; i < node_coords.size(); i++) {
        for(auto j = 0u; j < node_coords.size(); j++) {
            if (i == j)
                continue;
            dist_between_nodes[i + j * node_coords.size()] = dist(node_coords[i], node_coords[j]);
        }
    }
}

__global__ void calculate_path_lengths(int64_t* path_lengths, int64_t *paths, int64_t *dist_between_nodes, unsigned int population_size, unsigned int node_count) {
    const auto individual_id = blockIdx.x * blockDim.x + threadIdx.x;

    if (individual_id >= population_size) {
        return;
    }

    int64_t path_length = 0;
    for (int i = 0; i < population_size - 1; i++) {
        // Assuming the individuals are stored in path_lengths in a flattened manner
        // and each individual is a sequence of node indices.
        const auto from = paths[population_size * individual_id + i];
        const auto to = paths[population_size * individual_id + i + 1];

        path_length += dist_between_nodes[from * node_count + to];
    }

    const auto last_node = paths[population_size * individual_id + population_size - 1];
    const auto first_node = paths[population_size * individual_id + 0];
    path_length += dist_between_nodes[last_node * node_count + first_node];

    // Write the total path length back
    path_lengths[individual_id] = path_length;
}

__global__ void shuffle(int64_t* path, unsigned int node_count) {
    unsigned int individual_id = blockIdx.x * blockDim.x + threadIdx.x;
    // printf("individual_id=%d\n", individual_id);

    if (individual_id >= population_size) {
        return;
    }

    // Initialize the path for this individual
    for (int i = 0; i < node_count; i++) {
        path[individual_id * node_count + i] = i;
        // printf("path[%d]=%d\n", individual_id * node_count + i, (int)path[individual_id * node_count + i]);
    }

    // Shuffle the path using Fisher-Yates algorithm
    curandState state;
    curand_init(individual_id + clock(), 0, 0, &state); // Unique seed for each thread
    for (int i = node_count - 1; i > 0; i--) {
        int j = curand(&state) % (i + 1);

        // Swap i and j
        int64_t temp = path[individual_id * node_count + i];
        path[individual_id * node_count + i] = path[individual_id * node_count + j];
        path[individual_id * node_count + j] = temp;
    }
}

auto main(int argc, char** argv) -> int {
    if(argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <input_file> " << std::endl;
        return 1;
    }
    const auto nodes = read_file(argv[1]);

    // const auto nodes = std::vector<Vec2>{Vec2{0,0}, Vec2{0,1}, Vec2{0,2}, Vec2{0,3}, Vec2{0,4}};

    const auto node_count = nodes.size();

    // Calculate distances between each node
    auto dist_between_nodes_host = new int64_t[node_count * node_count];
    calculate_dist_between_nodes(dist_between_nodes_host, std::span(nodes));

    // Allocate the array holding distances between nodes on the gpu and initialize
    int64_t *dist_between_nodes_device;
    cudaMalloc((void**)&dist_between_nodes_device, node_count * node_count * sizeof(int64_t));
    cudaMemcpy(dist_between_nodes_device, dist_between_nodes_host, node_count * node_count * sizeof(int64_t), cudaMemcpyHostToDevice);

    // Allocate the array holding different paths(specimens) on the gpu and initialize
    int64_t *paths_device;
    cudaMalloc((void**)&paths_device, population_size * node_count * sizeof(int64_t));
    shuffle<<<blocksPerGrid, threadsPerBlock>>>(paths_device, node_count);

    // Allocate the array holding path lengths on the gpu and initialize
    int64_t *path_lengths_device;
    cudaMalloc((void**)&path_lengths_device, population_size * sizeof(int64_t));
    calculate_path_lengths<<<blocksPerGrid, threadsPerBlock>>>(path_lengths_device, paths_device, dist_between_nodes_device, population_size, node_count);

    int64_t *paths_host = (int64_t*)malloc(population_size * node_count * sizeof(int64_t));
    cudaMemcpy(paths_host,paths_device, node_count*population_size*sizeof(int64_t),cudaMemcpyDeviceToHost);
    for(auto i = 0u; i < population_size; i++) {
        for(int j = 0u; j < node_count; j++) {
            std::cout << paths_host[i * node_count + j] << std::endl;
            // printf("paths_host[%zu]=%d\n", i * node_count + j, (int)paths_host[i * node_count + j]);
        }
        std::cout << "======================" << std::endl;
    }

    return 0;
}
