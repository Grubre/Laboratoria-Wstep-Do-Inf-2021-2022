#include "utilities.hpp"
#include <array>
#include <cuda.h>
#include <cuda_runtime.h>
#include <curand_kernel.h>
#include <iostream>
#include <stdio.h>
#include <thrust/device_vector.h>
#include <thrust/execution_policy.h>
#include <thrust/functional.h>
#include <thrust/sort.h>
#include <unordered_map>

__global__ void sort_by_fitness(int64_t *sorted_paths_indexes, int64_t *path_lengths, unsigned int node_count) {
    const auto island_id = blockIdx.x * blockDim.x + threadIdx.x;

    if (island_id >= island_count) {
        return;
    }

    const int start_idx = island_id * island_size;
    const int end_idx = start_idx + island_size;

    // Initializing the indexes
    for (int i = start_idx; i < end_idx; i++) {
        sorted_paths_indexes[i] = i;
    }

    thrust::sort_by_key(thrust::device, path_lengths + start_idx, path_lengths + end_idx,
                        sorted_paths_indexes + start_idx, thrust::less<int64_t>());
}

__global__ void select(int64_t *paths, int64_t *temp, int64_t *sorted_paths_indexes, unsigned int node_count) {
    const auto path_id = blockIdx.x * blockDim.x + threadIdx.x;

    if (path_id >= population_size) {
        return;
    }

    const auto offset_inside_island = path_id % island_size;

    auto selected_path_id = sorted_paths_indexes[path_id - p];
    if (offset_inside_island < p) {
        selected_path_id = sorted_paths_indexes[path_id];
    }

    const auto current_id = path_id;

    // copy paths[selected_path_id] to temp[path_id]
    for (auto i = 0u; i < node_count; i++) {
        temp[current_id * node_count + i] = paths[selected_path_id * node_count + i];
    }
}

void calculate_dist_between_nodes(int64_t *dist_between_nodes, const std::span<const Vec2> node_coords) {
    for (auto i = 0u; i < node_coords.size(); i++) {
        for (auto j = 0u; j < node_coords.size(); j++) {
            if (i == j)
                continue;
            dist_between_nodes[i + j * node_coords.size()] = dist(node_coords[i], node_coords[j]);
        }
    }
}

__global__ void calculate_path_lengths(int64_t *path_lengths, int64_t *paths, int64_t *dist_between_nodes,
                                       unsigned int population_size, unsigned int node_count) {
    const auto starting_id = blockIdx.x * blockDim.x + threadIdx.x;

    if (starting_id >= population_size) {
        return;
    }

    // printf("calculating: %d\n", starting_id);

    int64_t path_length = 0;
    for (int i = 0; i < node_count - 1; i++) {
        const auto from = paths[node_count * starting_id + i];
        const auto to = paths[node_count * starting_id + i + 1];

        path_length += dist_between_nodes[from * node_count + to];
    }

    const auto last_node = paths[node_count * starting_id + node_count - 1];
    const auto first_node = paths[node_count * starting_id + 0];
    path_length += dist_between_nodes[last_node * node_count + first_node];

    // printf("starting_id = %d, path_length=%d\n", (int)starting_id,
    // (int)path_length);
    path_lengths[starting_id] = path_length;
}

__global__ void shuffle(int64_t *path, unsigned int node_count) {
    const auto starting_id = blockIdx.x * blockDim.x + threadIdx.x;

    if (starting_id >= population_size) {
        return;
    }

    for (int i = 0; i < node_count; i++) {
        path[starting_id * node_count + i] = i;
    }

    curandState state;
    curand_init(starting_id + clock(), 0, 0,
                &state); // Unique seed for each thread
    for (int i = node_count - 1; i > 0; i--) {
        int j = curand(&state) % (i + 1);

        int64_t temp = path[starting_id * node_count + i];
        path[starting_id * node_count + i] = path[starting_id * node_count + j];
        path[starting_id * node_count + j] = temp;
    }
}

__global__ void get_best(int64_t *best_permutation_host, int64_t *best_length_host, int64_t *path_lengths,
                         int64_t *sorted_paths_indexes, int64_t *paths, unsigned int population_size,
                         unsigned int node_count) {
    uint64_t best_permutation_index = 0u;
    for (auto i = 0u; i < population_size; i++) {
        if (path_lengths[i] < path_lengths[best_permutation_index]) {
            best_permutation_index = i;
        }
    }

    if (*best_length_host <= path_lengths[best_permutation_index]) {
        return;
    }

    for (auto i = 0u; i < node_count; i++) {
        // printf("%d ", (int)paths[best_permutation_index * node_count + i]);
        best_permutation_host[i] = paths[best_permutation_index * node_count + i];
    }

    *best_length_host = path_lengths[best_permutation_index];
}

__global__ void mutate(int64_t *paths, unsigned int node_count, unsigned int number_of_mutations) {
    const auto path_id = blockIdx.x * blockDim.x + threadIdx.x;

    if (path_id >= population_size) {
        return;
    }

    const auto offset_inside_island = path_id % island_size;
    if (offset_inside_island >= p) {
        return;
    }

    const auto island_id = path_id / island_size;
    const auto mutation_chance = 1.0 - (double)island_id / (double)island_count;

    curandState state;
    curand_init(path_id + clock(), 0, 0, &state);

    // random double between 0 and 1
    const auto random_double = curand_uniform_double(&state) - 0.5;

    if (random_double > mutation_chance) {
        return;
    }

    for (auto i = 0u; i < number_of_mutations; i++) {
        const auto random_node_a = curand(&state) % node_count;
        const auto random_node_b = curand(&state) % node_count;

        const auto temp = paths[path_id * node_count + random_node_a];
        paths[path_id * node_count + random_node_a] = paths[path_id * node_count + random_node_b];
        paths[path_id * node_count + random_node_b] = temp;
    }
}
