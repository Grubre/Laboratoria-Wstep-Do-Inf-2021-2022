#pragma once
#include <chrono>
#include <iostream>

class timer {
public:
    timer() {
        start = std::chrono::steady_clock::now();
    }
    ~timer() {
        auto end = std::chrono::steady_clock::now();
        std::cout << "Elapsed time: " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << " μs" << std::endl;
    }

private:
    std::chrono::steady_clock::time_point start;
};
