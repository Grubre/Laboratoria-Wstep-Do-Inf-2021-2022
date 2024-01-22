#include <iostream>
#include <bitset>
#include "bitstream.hpp"

auto calculate_parity_bits(const std::bitset<4>& data) -> std::bitset<3> {
    std::bitset<7> generator{0b1011};
    std::bitset<7> extendedData{data.to_ulong() << 3}; // Append 3 zeros for parity bits

    for (int i = 3; i >= 0; --i) {
        if (extendedData.test(i + 3)) {
            extendedData ^= generator << i;
        }
    }

    return std::bitset<3>(extendedData.to_ulong());
}

auto encode_hamming(const std::bitset<4>& data) -> std::bitset<7> {
    auto parity = calculate_parity_bits(data);
    return std::bitset<7>((data.to_ulong() << 3) | parity.to_ulong());
}

auto main(int argc, char** argv) -> int {
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <input> <output>\n";
        return 1;
    }

    auto input = InputBitstream(argv[1]);
    auto output = OutputBitstream(argv[1]);

    while (true) {
        std::bitset<4> dataBits;
        for (int i = 0; i < 4; ++i) {
            auto bit = input.next_bit();
            if (!bit) break;
            dataBits[i] = *bit;
        }

        if (dataBits.none()) break;

        auto encoded = encode_hamming(dataBits);
        for (int i = 6; i >= 0; --i) {
            output.put_bit(encoded.test(i));
        }
    }

    return 0;
}
