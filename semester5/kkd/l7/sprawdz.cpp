#include <random>
#include <iostream>
#include <array>
#include "bitstream.hpp"

auto main(int argc, char** argv) -> int {
    if(argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <in1> <in2>" << std::endl;
        return 1;
    }

    auto input1 = InputBitstream(argv[1]);
    auto input2 = InputBitstream(argv[2]);

    auto differing_blocks_count = 0u;

    auto finished = false;
    while(!finished) {
        bool is_identical = true;
        for(int i = 0u; i < 4; i++) {
            const auto bit1 = input1.next_bit();
            const auto bit2 = input2.next_bit();

            if(!bit1 && !bit2) {
                finished = true;
                break;
            }

            if(!bit1 || !bit2) {
                finished = true;
                is_identical = false;
                break;
            }

            std::cout << *bit1 << " " << *bit2 << std::endl;

            if(*bit1 != *bit2) {
                is_identical = false;
            }
        }
        differing_blocks_count += is_identical ? 0 : 1;
    }

    std::cout << "Differing blocks count: " << differing_blocks_count << std::endl;
}

