#include <random>
#include <iostream>
#include <array>
#include "bitstream.hpp"

auto main(int argc, char** argv) -> int {
    if(argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <in1> <in2>" << std::endl;
    }

    auto input1 = InputBitstream(argv[1]);
    auto input2 = InputBitstream(argv[2]);

    bool identical = true;
    bool finished = false;
    while(!finished) {
        for(int i = 0u; i < 4; i++) {
            const auto bit1 = input1.next_bit();
            const auto bit2 = input2.next_bit();

            if(!bit1 && !bit2) {
                finished = true;
                break;
            }

            if(!bit1 || !bit2) {
                identical = false;
                finished = true;
                break;
            }

            if(*bit1 != *bit2) {
                identical = false;
                finished = true;
                break;
            }
        }
    }
}

