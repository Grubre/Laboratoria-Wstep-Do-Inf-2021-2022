add_executable(genetic_tsp main.cu utilities.cu)

set_target_properties(genetic_tsp PROPERTIES CUDA_SEPARABLE_COMPILATION ON)
