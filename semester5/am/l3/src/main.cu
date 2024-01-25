#include "utilities.hpp"
#include <array>
#include <filesystem>
#include <iostream>
#include <span>
#include <stdio.h>
#include <unordered_map>

// taken from https://www.hindawi.com/journals/mpe/2020/1398595/
__device__ void ro(int64_t *a, int64_t *temp, int64_t len, int64_t num, unsigned int node_count) {
    int pos;
    for (int i = 0; i < len; i++) {
        if (a[i] == num) {
            pos = i;
        }
    }
    int j = 0;
    for (int i = pos; i < len; i++) {
        temp[j] = a[i];
        j++;
    }
    for (int k = 0; k < pos; k++) {
        temp[j] = a[k];
        j++;
    }
    for (int i = 0; i < len; i++) {
        a[i] = temp[i];
    }
}

// based on https://www.hindawi.com/journals/mpe/2020/1398595/
__global__ void crossover(int64_t *paths, int64_t *temp_buf, int64_t *dist_between_nodes, unsigned int node_count) {
    const auto pair_id = blockIdx.x * blockDim.x + threadIdx.x;

    if (pair_id >= population_size / 2) {
        return;
    }

    const auto offset_inside_island = (2 * pair_id) % island_size;
    if (offset_inside_island >= p) {
        return;
    }

    curandState state;
    curand_init((unsigned long long)clock() + pair_id, 0, 0, &state);

    const auto parent_a_offset = 2 * pair_id;
    const auto parent_b_offset = 2 * pair_id + 1;
    const auto parent_a = paths + parent_a_offset * node_count;
    const auto parent_b = paths + parent_b_offset * node_count;

    // const auto trial = curand_uniform(&state);
    // if (trial > crossover_chance) {
    //     return;
    // }

    // printf("pair %d: a: %d, b: %d\n", pair_id, parent_a[0], parent_b[0]);
    const auto result = temp_buf + pair_id * node_count;
    const auto temp = temp_buf + (population_size / 2 + pair_id) * node_count;

    const auto start = curand(&state) % node_count;

    result[0] = start;

    ro(parent_a, temp, node_count, start, node_count);
    ro(parent_b, temp, node_count, start, node_count);

    for (auto i = 0u; i < node_count - 1; i++) {
        const auto from_a = parent_a[i];
        const auto from_b = parent_b[i];
        const auto to_a = parent_a[i + 1];
        const auto to_b = parent_b[i + 1];

        const auto dist_from_a = dist_between_nodes[from_a * node_count + to_a];
        const auto dist_from_b = dist_between_nodes[from_b * node_count + to_b];

        if (dist_from_a < dist_from_b) {
            result[i + 1] = to_a;
            ro(paths + parent_b_offset * node_count + i + 1, temp, node_count - i - 1, to_a, node_count);
        } else {
            result[i + 1] = to_b;
            ro(paths + parent_a_offset * node_count + i + 1, temp, node_count - i - 1, to_b, node_count);
        }
    }

    for (auto i = 0u; i < node_count; i++) {
        paths[parent_a_offset * node_count + i] = result[i];
    }

    const auto midpoint = node_count / 2;
    auto j = 0u;

    for (auto i = midpoint; i < node_count; i++) {
        paths[parent_b_offset * node_count + j] = result[i];
        j++;
    }

    for (auto i = 0u; i < midpoint; i++) {
        paths[parent_b_offset * node_count + j] = result[i];
        j++;
    }
}

void print_population(int64_t *paths_device, int64_t *paths_host, int64_t *path_lengths_host,
                      int64_t *path_lengths_device, unsigned int node_count) {
    cudaMemcpy(paths_host, paths_device, node_count * population_size * sizeof(int64_t), cudaMemcpyDeviceToHost);

    cudaMemcpy(path_lengths_host, path_lengths_device, population_size * sizeof(int64_t), cudaMemcpyDeviceToHost);

    cudaDeviceSynchronize();

    for (auto i = 0u; i < population_size; i++) {
        if (i % island_size == 0) {
            std::cout << "\n======================" << std::endl;
        }
        for (int j = 0u; j < node_count; j++) {
            // std::cout
            //     << paths_host[i * node_count + j]
            //     << " ";
            std::cout << paths_host[i * node_count + j] << " ";
            // printf("paths_host[%zu]=%d\n", i * node_count + j,
        }
        std::cout << "\tlength = " << path_lengths_host[i] << std::endl;
        // std::cout << "======================" << std::endl;
    }
}

__global__ void exchange(int64_t *paths, unsigned int node_count) {
    const auto path_id = blockIdx.x * blockDim.x + threadIdx.x;

    if (path_id >= population_size) {
        return;
    }

    const auto island_id = path_id / island_size;
    const auto offset_inside_island = path_id % island_size;

    if (offset_inside_island >= exchange_cutoff_index) {
        return;
    }

    const auto destination_island_id = (island_id + 1) % island_count;
    const auto destination_path_id = destination_island_id * island_size + island_size - offset_inside_island - 1;

    for (auto i = 0u; i < node_count; i++) {
        paths[destination_path_id * node_count + i] = paths[path_id * node_count + i];
    }
}

auto main(int argc, char **argv) -> int {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <input_file> " << std::endl;
        return 1;
    }
    const auto nodes = read_file(argv[1]);

    // const auto nodes = std::vector<Vec2>{Vec2{0, 0},  Vec2{0, 1},  Vec2{0, 2},  Vec2{0, 3},  Vec2{0, 4},
    //                                      Vec2{0, 5},  Vec2{0, 6},  Vec2{0, 7},  Vec2{0, 8},  Vec2{0, 9},
    //                                      Vec2{0, 10}, Vec2{0, 11}, Vec2{0, 12}, Vec2{0, 13}, Vec2{0, 14}};

    const auto node_count = nodes.size();

    // Calculate distances between each node
    auto dist_between_nodes_host = (int64_t *)malloc(node_count * node_count * sizeof(int64_t));
    calculate_dist_between_nodes(dist_between_nodes_host, std::span(nodes));

    // Allocate the array holding distances between nodes on the gpu and
    // initialize
    int64_t *dist_between_nodes_device;
    cudaMalloc((void **)&dist_between_nodes_device, node_count * node_count * sizeof(int64_t));
    cudaMemcpy(dist_between_nodes_device, dist_between_nodes_host, node_count * node_count * sizeof(int64_t),
               cudaMemcpyHostToDevice);

    // Allocate the array holding different paths(specimens) on the gpu and
    // initialize (randomly shuffle each path)
    int64_t *paths_device;
    cudaMalloc((void **)&paths_device, population_size * node_count * sizeof(int64_t));
    shuffle<<<blocksPerGrid, threadsPerBlock>>>(paths_device, node_count);

    // Allocate the array holding path lengths on the gpu and initialize
    int64_t *path_lengths_device;
    cudaMalloc((void **)&path_lengths_device, population_size * sizeof(int64_t));
    calculate_path_lengths<<<blocksPerGrid, threadsPerBlock>>>(path_lengths_device, paths_device,
                                                               dist_between_nodes_device, population_size, node_count);

    // Allocate the array that holds the sorted paths indexes and initialize
    int64_t *sorted_paths_indexes_device;
    cudaMalloc((void **)&sorted_paths_indexes_device, population_size * sizeof(int64_t));

    // Allocate memory for the results on the host and in unified memory
    int64_t *best_length_host;
    int64_t *best_permutation_host;
    auto paths_host = (int64_t *)malloc(population_size * node_count * sizeof(int64_t));
    auto path_lengths_host = (int64_t *)malloc(population_size * sizeof(int64_t));
    cudaMallocManaged(&best_permutation_host, node_count * sizeof(int64_t));
    cudaMallocManaged(&best_length_host, sizeof(int64_t));

    *best_length_host = std::numeric_limits<int64_t>::max();

    // Run the genetic algorithm
    // constexpr auto generations = 1000;
    // constexpr auto internal_generations = 10;

    // NOTE: Before doing crossover and mutation, for
    // population_size=5000 i was getting the shortest path
    // ~300_000
    // which seems large

    int64_t *temp_device;
    cudaMalloc((void **)&temp_device, population_size * node_count * sizeof(int64_t));

    sort_by_fitness<<<blocksPerGrid, threadsPerBlock>>>(sorted_paths_indexes_device, path_lengths_device, node_count);

    constexpr auto generations = 500;
    constexpr auto internal_generations = 10;
    for (auto i = 0u; i < generations; i++) {
        for (auto j = 0u; j < internal_generations; j++) {
            // selection
            select<<<blocksPerGrid, threadsPerBlock>>>(paths_device, temp_device, sorted_paths_indexes_device,
                                                       node_count);
            cudaMemcpy(paths_device, temp_device, population_size * node_count * sizeof(int64_t),
                       cudaMemcpyDeviceToDevice);
            crossover<<<blocksPerGrid, threadsPerBlock>>>(paths_device, temp_device, dist_between_nodes_device,
                                                          node_count);
            const auto number_of_mutations = i * node_count / (4 * generations);
            mutate<<<blocksPerGrid, threadsPerBlock>>>(paths_device, node_count, number_of_mutations);
            calculate_path_lengths<<<blocksPerGrid, threadsPerBlock>>>(
                path_lengths_device, paths_device, dist_between_nodes_device, population_size, node_count);
            sort_by_fitness<<<blocksPerGrid, threadsPerBlock>>>(sorted_paths_indexes_device, path_lengths_device,
                                                                node_count);
        }
        exchange<<<blocksPerGrid, threadsPerBlock>>>(paths_device, node_count);
        calculate_path_lengths<<<blocksPerGrid, threadsPerBlock>>>(
            path_lengths_device, paths_device, dist_between_nodes_device, population_size, node_count);
        sort_by_fitness<<<blocksPerGrid, threadsPerBlock>>>(sorted_paths_indexes_device, path_lengths_device,
                                                            node_count);
        calculate_path_lengths<<<blocksPerGrid, threadsPerBlock>>>(
            path_lengths_device, paths_device, dist_between_nodes_device, population_size, node_count);
        get_best<<<1, 1>>>(best_permutation_host, best_length_host, path_lengths_device, sorted_paths_indexes_device,
                           paths_device, population_size, node_count);
        cudaDeviceSynchronize();
        std::cout << "\r" << i + 1 << "/" << generations << std::flush;
    }
    std::cout << std::endl;

    // sort_by_fitness<<<blocksPerGrid, threadsPerBlock>>>(
    //     sorted_paths_indexes_device, path_lengths_device, node_count);
    // select<<<blocksPerGrid, threadsPerBlock>>>(
    //     paths_device, temp_device, sorted_paths_indexes_device, node_count);
    // cudaMemcpy(paths_device, temp_device,
    //            population_size * node_count * sizeof(int64_t),
    //            cudaMemcpyDeviceToDevice);
    // // printf("After selection:\n");
    // // print_population(paths_device, paths_host, path_lengths_host,
    // //                  path_lengths_device, node_count);
    //
    // crossover<<<blocksPerGrid, threadsPerBlock>>>(
    //     paths_device, temp_device, dist_between_nodes_device, node_count);
    // mutate<<<blocksPerGrid, threadsPerBlock>>>(paths_device, node_count);
    // calculate_path_lengths<<<blocksPerGrid, threadsPerBlock>>>(
    //     path_lengths_device, paths_device, dist_between_nodes_device,
    //     population_size, node_count);
    //
    // printf("%ld\n", *best_length_host);
    // for (auto i = 0u; i < node_count; i++) {
    //     printf("%ld ", best_permutation_host[i]);
    // }
    //
    // auto length = 0llu;
    // for (auto i = 0u; i < node_count; i++) {
    //     length += dist_between_nodes_host[best_permutation_host[i] * node_count + best_permutation_host[i + 1]];
    // }
    // length += dist_between_nodes_host[best_permutation_host[node_count - 1] * node_count + best_permutation_host[0]];
    // printf("\ncalculated length %llu\n", length);


    const auto input_file = std::filesystem::path(argv[1]).filename().string();
    std::cout << "file: " << input_file << " -> best: " << *best_length_host << std::endl;
    auto output = std::ofstream("../output_" + input_file);

    output << *best_length_host << std::endl;

    for (auto i = 0u; i < node_count; i++) {
        output << nodes[best_permutation_host[i]].x << " " << nodes[best_permutation_host[i]].y << std::endl;
    }

    return 0;
}
