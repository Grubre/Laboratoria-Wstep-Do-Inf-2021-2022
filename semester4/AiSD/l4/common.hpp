#pragma once
#include <cstddef>
struct operation_stats {
    size_t comparisons = 0;
    size_t pointer_reads = 0;
    size_t pointer_substitutions = 0;
};


