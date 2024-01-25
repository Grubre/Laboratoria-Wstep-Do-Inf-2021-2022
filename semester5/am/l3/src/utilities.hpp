#pragma once
#include "common.hpp"
#include <cstdint>
#include <cuda.h>
#include <cuda_runtime.h>
#include <curand_kernel.h>
#include <span>

constexpr auto population_size = 10000;

constexpr auto island_count = 250;
constexpr auto island_size = population_size / island_count;

static_assert(population_size % island_count == 0, "population_size must be divisible by island_count");
static_assert(island_size % 2 == 0, "island_size must be even to allow crossover");

constexpr auto selected_threshold = 0.6;
constexpr auto crossover_chance = 0.8;

constexpr auto exchange_cutoff = 0.2;
constexpr auto exchange_cutoff_index = (int)(exchange_cutoff * island_size);

constexpr auto p = (int)(selected_threshold * island_size);

static_assert(selected_threshold > 0.0 && selected_threshold < 1.0, "selected_threshold must be between 0 and 1");
static_assert(crossover_chance > 0.0 && crossover_chance < 1.0, "crossover_chance must be between 0 and 1");
static_assert((int64_t)(selected_threshold * population_size) % 2 == 0,
              "selected_threshold * population_size must be even to allow "
              "crossover");
static_assert(p % 2 == 0, "p must be even to allow elite retention");
static_assert(exchange_cutoff < 0.5, "exchange_cutoff must be less than 0.5");

constexpr auto threadsPerBlock = 256;
constexpr auto blocksPerGrid = (population_size + threadsPerBlock - 1) / threadsPerBlock;

__global__ void sort_by_fitness(int64_t *sorted_paths_indexes, int64_t *path_lengths, unsigned int node_count);

__global__ void select(int64_t *paths, int64_t *temp, int64_t *sorted_paths_indexes, unsigned int node_count);

void calculate_dist_between_nodes(int64_t *dist_between_nodes, const std::span<const Vec2> node_coords);

__global__ void calculate_path_lengths(int64_t *path_lengths, int64_t *paths, int64_t *dist_between_nodes,
                                       unsigned int population_size, unsigned int node_count);

__global__ void shuffle(int64_t *path, unsigned int node_count);

__global__ void get_best(int64_t *best_permutation_host, int64_t *best_length_host, int64_t *path_lengths,
                         int64_t *sorted_paths_indexes, int64_t *paths, unsigned int population_size,
                         unsigned int node_count);

__global__ void mutate(int64_t *paths, unsigned int node_count, unsigned int number_of_mutations);
