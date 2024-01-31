#include <cstdint>
#include <iostream>
#include <bitset>
#include "bitstream.hpp"
template<size_t T>
void set(std::bitset<T>& bitset, int pos, bool value) {
    bitset[T - pos - 1] = value;
}

template<size_t T>
bool get(const std::bitset<T>& bitset, int pos) {
    return bitset[T - pos - 1];
}

auto encode_hamming(const std::bitset<4>& data) -> uint8_t {
    std::bitset<8> encoded;

    set(encoded, 0, get(data, 0));
    set(encoded, 1, get(data, 0) ^ get(data, 1));
    set(encoded, 2, get(data, 1) ^ get(data, 2));
    set(encoded, 3, get(data, 0) ^ get(data, 2) ^ get(data, 3));
    set(encoded, 4, get(data, 1) ^ get(data, 3));
    set(encoded, 5, get(data, 2));
    set(encoded, 6, get(data, 3));

    auto parity_check = false;
    for (int i = 0; i <= 7; ++i) {
        parity_check = parity_check ^ encoded[i];
    }
    set(encoded, 7, parity_check);

    return encoded.to_ulong();
}

auto main(int argc, char** argv) -> int {
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <input> <output>\n";
        return 1;
    }

    // for(auto i = 0u; i < 16; i++) {
    //     std::cout << std::bitset<8>{encode_hamming({i})} << std::endl;
    // }

    auto input = InputBitstream(argv[1]);
    std::ofstream output(argv[2]);

    bool finished = false;
    while (!finished) {
        std::bitset<4> dataBits;
        for (int i = 3; i >= 0; --i) {
            auto bit = input.next_bit();
            if (!bit) {
                finished = true;
                break;
            }

            dataBits[i] = *bit;
        }

        auto encoded = encode_hamming(dataBits);
        // std::cout << std::bitset<8>{encoded} << std::endl;
        output << encoded;
    }

    return 0;
}
