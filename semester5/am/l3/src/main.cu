#include <cuda_runtime.h>
#include <cuda.h>
#include <curand_kernel.h>
#include "common.hpp"
#include <iostream>
#include <array>
#include <span>
#include <unordered_map>
#include <stdio.h>
#include <thrust/sort.h>
#include <thrust/functional.h>
#include <thrust/device_vector.h>
#include <thrust/execution_policy.h>

constexpr auto population_size = 1000;

constexpr auto island_count = 1;
constexpr auto island_size = population_size / island_count;

constexpr auto selected_threshold = 0.8;

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
    const auto starting_id = blockIdx.x * blockDim.x + threadIdx.x;

    if (starting_id >= population_size) {
        return;
    }

    int64_t path_length = 0;
    for (int i = 0; i < node_count - 1; i++) {
        const auto from = paths[node_count * starting_id + i];
        const auto to = paths[node_count * starting_id + i + 1];

        path_length += dist_between_nodes[from * node_count + to];

    }

    const auto last_node = paths[node_count * starting_id + node_count - 1];
    const auto first_node = paths[node_count * starting_id + 0];
    path_length += dist_between_nodes[last_node * node_count + first_node];

    // printf("starting_id = %d, path_length=%d\n", (int)starting_id, (int)path_length);
    path_lengths[starting_id] = path_length;
}

__global__ void shuffle(int64_t* path, unsigned int node_count) {
    const auto starting_id = blockIdx.x * blockDim.x + threadIdx.x;

    if (starting_id >= population_size) {
        return;
    }

    for (int i = 0; i < node_count; i++) {
        path[starting_id * node_count + i] = i;
    }

    curandState state;
    curand_init(starting_id + clock(), 0, 0, &state); // Unique seed for each thread
    for (int i = node_count - 1; i > 0; i--) {
        int j = curand(&state) % (i + 1);

        int64_t temp = path[starting_id * node_count + i];
        path[starting_id * node_count + i] = path[starting_id * node_count + j];
        path[starting_id * node_count + j] = temp;
    }
}

__global__ void get_best(int64_t* best_permutation_host, int64_t* best_length_host, int64_t* path_lengths, int64_t* paths, unsigned int population_size, unsigned int node_count) {
    uint64_t best_permutation_index = 0u;
    for(auto i = 0u; i < population_size; i++) {
        if (path_lengths[i] < path_lengths[best_permutation_index]) {
            best_permutation_index = i;
        }
    }

    for(auto i = 0u; i < node_count; i++) {
        best_permutation_host[i] = paths[best_permutation_index * node_count + i];
    }

    *best_length_host = path_lengths[best_permutation_index];
}

__global__ void sort_by_fitness(int64_t* sorted_paths_indexes, int64_t* path_lengths, unsigned int node_count) {
    const auto island_id = blockIdx.x * blockDim.x + threadIdx.x;

    if (island_id >= island_count) {
        return;
    }

    const int start_idx = island_id * island_size;
    const int end_idx = start_idx + island_size;

    // Initializing the indexes
    for(int i = start_idx; i < end_idx; i++) {
        sorted_paths_indexes[i] = i;
    }

    thrust::sort_by_key(thrust::device, path_lengths + start_idx, path_lengths + end_idx, sorted_paths_indexes + start_idx, thrust::less<int64_t>());
}

__global__ void select(int64_t* paths, int64_t* sorted_paths_indexes, unsigned int node_count) {
    const auto path_id = blockIdx.x * blockDim.x + threadIdx.x;

    if(path_id >= population_size) {
        return;
    }

    const auto island_id = path_id / island_size;
    const auto island_start_idx = island_id * island_size;
    const auto offset_inside_island = path_id % island_size;

    const auto p = (int)(island_size * selected_threshold);

    if(offset_inside_island < p) {
        return;
    }

    const auto selected_path_id = sorted_paths_indexes[path_id - p];
    const auto current_id = sorted_paths_indexes[path_id];

    // copy paths[selected_path_id] to paths[path_id]
    for(auto i = 0u; i < node_count; i++) {
        paths[current_id * node_count + i] = paths[selected_path_id * node_count + i];
    }
}

__global__ void crossover(int64_t* paths, int64_t* sorted_paths_indexes, unsigned int node_count) {
    const auto path_id = blockIdx.x * blockDim.x + threadIdx.x;

    if(path_id >= population_size / 2) {
        return;
    }

    const auto parent_a = sorted_paths_indexes[2 * path_id];
    const auto parent_b = sorted_paths_indexes[2 * path_id + 1];


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
    auto dist_between_nodes_host = (int64_t*)malloc(node_count * node_count * sizeof(int64_t));
    calculate_dist_between_nodes(dist_between_nodes_host, std::span(nodes));

    // Allocate the array holding distances between nodes on the gpu and initialize
    int64_t *dist_between_nodes_device;
    cudaMalloc((void**)&dist_between_nodes_device, node_count * node_count * sizeof(int64_t));
    cudaMemcpy(dist_between_nodes_device, dist_between_nodes_host, node_count * node_count * sizeof(int64_t), cudaMemcpyHostToDevice);

    // Allocate the array holding different paths(specimens) on the gpu and initialize (randomly shuffle each path)
    int64_t *paths_device;
    cudaMalloc((void**)&paths_device, population_size * node_count * sizeof(int64_t));
    shuffle<<<blocksPerGrid, threadsPerBlock>>>(paths_device, node_count);

    // Allocate the array holding path lengths on the gpu and initialize
    int64_t *path_lengths_device;
    cudaMalloc((void**)&path_lengths_device, population_size * sizeof(int64_t));
    calculate_path_lengths<<<blocksPerGrid, threadsPerBlock>>>(path_lengths_device, paths_device, dist_between_nodes_device, population_size, node_count);

    // Allocate the array that holds the sorted paths indexes and initialize
    int64_t* sorted_paths_indexes_device;
    cudaMalloc((void**)&sorted_paths_indexes_device, population_size * sizeof(int64_t));

    // Allocate the array that holds the offsprings and initialize
    int64_t* offsprings_device;
    cudaMalloc((void**)&offsprings_device, population_size * node_count * sizeof(int64_t));

    // Run the genetic algorithm
    constexpr auto generations = 1000;
    constexpr auto internal_generations = 10;
    for(auto i = 0u; i < generations; i++) {
        for(auto j = 0u; j < internal_generations; j++) {
            // selection
            sort_by_fitness<<<blocksPerGrid, threadsPerBlock>>>(sorted_paths_indexes_device, path_lengths_device, node_count);
            // NOTE: Before doing crossover and mutation, for population_size=5000 i was getting the shortest path ~300_000 which seems large
            select<<<blocksPerGrid, threadsPerBlock>>>(paths_device, sorted_paths_indexes_device, node_count);
            // crossover, one thread for each offsprings_device element
            // crossover<<<blockPerGrid, threadsPerBlock>>>(offsprings_device, paths_device, sorted_paths_indexes_device, node_count);
            calculate_path_lengths<<<blocksPerGrid, threadsPerBlock>>>(path_lengths_device, paths_device, dist_between_nodes_device, population_size, node_count);
        }
        cudaDeviceSynchronize();
        std::cout << "\r" << i + 1 << "/" << generations << std::flush;
    }
    std::cout << std::endl;

    // sort_by_fitness<<<blocksPerGrid, threadsPerBlock>>>(sorted_paths_indexes_device, path_lengths_device, node_count);
    // select<<<blocksPerGrid, threadsPerBlock>>>(paths_device, sorted_paths_indexes_device, node_count);

    // Get the solution in unified memory
    int64_t* best_length_host;
    int64_t* best_permutation_host;
    cudaMallocManaged(&best_permutation_host, node_count * sizeof(int64_t));
    cudaMallocManaged(&best_length_host, sizeof(int64_t));
    get_best<<<1, 1>>>(best_permutation_host, best_length_host, path_lengths_device, paths_device, population_size, node_count);
    cudaDeviceSynchronize();

    auto paths_host = (int64_t*)malloc(population_size * node_count * sizeof(int64_t));
    cudaMemcpy(paths_host,paths_device, node_count*population_size*sizeof(int64_t),cudaMemcpyDeviceToHost);

    auto path_lengths_host = (int64_t*)malloc(population_size * sizeof(int64_t));
    cudaMemcpy(path_lengths_host,path_lengths_device, population_size*sizeof(int64_t),cudaMemcpyDeviceToHost);

    auto sorted_paths_indexes_host = (int64_t*)malloc(population_size * sizeof(int64_t));
    cudaMemcpy(sorted_paths_indexes_host,sorted_paths_indexes_device, population_size*sizeof(int64_t),cudaMemcpyDeviceToHost);

    // for(auto i = 0u; i < population_size; i++) {
    //     std::cout << sorted_paths_indexes_host[i] << " ";
    //     printf("\n");
    // }
    // for(auto i = 0u; i < population_size; i++) {
    //     if(i%island_size==0) {
    //         std::cout << "\n======================" << std::endl;
    //     }
    //     for(int j = 0u; j < node_count; j++) {
    //         std::cout << paths_host[sorted_paths_indexes_host[i] * node_count + j] << " ";
    //         // printf("paths_host[%zu]=%d\n", i * node_count + j, (int)paths_host[i * node_count + j]);
    //     }
    //     std::cout << "\tlength = " << path_lengths_host[i] << std::endl;
    //     // std::cout << "======================" << std::endl;
    // }

    printf("%ld\n", * best_length_host);

    return 0;
}
