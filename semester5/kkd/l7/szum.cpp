#include <random>
#include <iostream>
#include "bitstream.hpp"

auto main(int argc, char** argv) -> int {
    if(argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <p> <input> <output>" << std::endl;
    }

    const auto p = std::stod(argv[1]);
    if(p < 0 || p > 1) {
        std::cerr << "p must be in range [0, 1]" << std::endl;
        return 1;
    }
    auto input = InputBitstream(argv[2]);
    auto output = OutputBitstream(argv[3]);

    std::random_device rd;
    std::mt19937 gen(rd());
    std::bernoulli_distribution d(p);

    while(true) {
        const auto bit = input.next_bit();

        if(!bit) {
            break;
        }

        const auto trial = d(gen) <= p;
        const auto out_bit = trial ? !(bool)*bit : (bool)*bit;
        output.put_bit(out_bit);
    }
    return 0;
}
