cmake_minimum_required(VERSION 3.21)

project(
    ludum_dare_51
    VERSION 0.0.1
    DESCRIPTION ""
    LANGUAGES C
)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS}\
                    -Wall\
                    -Wextra\
                    -Wpedantic\
")

file(GLOB_RECURSE SRC src/*.c)

add_executable(main ${SRC})

include_directories(src)
