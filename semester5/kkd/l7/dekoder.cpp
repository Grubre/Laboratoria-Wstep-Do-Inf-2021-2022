#include <iostream>
#include <fstream>
#include <cstdint>
#include <bitset>
#include <utility>
#include <optional>

template<size_t T>
void set(std::bitset<T>& bitset, int pos, bool value) {
    bitset[T - pos - 1] = value;
}

template<size_t T>
bool get(const std::bitset<T>& bitset, int pos) {
    return bitset[T - pos - 1];
}

auto check(std::bitset<8> code) -> uint8_t {
    auto ch = std::bitset<8>{};
    // std::cout << "check: " << std::bitset<8>(code) << std::endl;

    set(ch, 0, get(code, 2) ^ get(code, 4) ^ get(code, 5) ^ get(code, 6));
    set(ch, 1, get(code, 1) ^ get(code, 3) ^ get(code, 4) ^ get(code, 5));
    set(ch, 2, get(code, 0) ^ get(code, 2) ^ get(code, 3) ^ get(code, 4));

    
    auto parity_check = false;
    for (int i = 0; i <= 7; ++i) {
        parity_check = parity_check ^ get(code, i);
    }
    set(ch, 3, parity_check);

    return ch.to_ulong();
}

auto correct(uint8_t code) -> std::optional<uint8_t> {
    switch(code) {
        case 48:
            return 0;
        case 80:
            return 1;
        case 176:
            return 2;
        case 112:
            return 3;
        case 240:
            return 4;
        case 208:
            return 5;
        case 144:
            return 6;
        case 16:
            return 7;
        default:
            return std::nullopt;
    }
}

auto decode(std::bitset<8> code) -> std::pair<std::bitset<4>, bool> {
    auto ch = check(code);
    // std::cout << code << " -> " << ch << std::endl;
    auto double_error = false;
    if(ch != 0) {
        auto corrected = correct(ch);
        if(corrected) {
            auto x = get(code, *corrected);
            set(code, *corrected, !x);
        } else {
            double_error = true;
        }
    }
    auto decoded = std::bitset<4>{};
    set(decoded, 0, get(code, 0));
    set(decoded, 1, get(code, 1) ^ get(code, 0));
    set(decoded, 2, get(code, 5));
    set(decoded, 3, get(code, 6));

    return {decoded, double_error};
}

auto main(int argc, char** argv) -> int {
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <input> <output>\n";
        return 1;
    }

    auto input = std::ifstream(argv[1]);
    auto output = std::ofstream(argv[2]);

    uint8_t byte;
    uint8_t out_byte = 0;

    uint64_t errors = 0;
    bool flag = true;
    auto total = 0u;
    while(input >> byte) {
        total++;
        auto code = std::bitset<8>{byte};

        auto [decoded, double_error] = decode(code);
        if(double_error) {
            errors++;
        }
        if(flag) {
            out_byte = decoded.to_ulong();
            // std::cout << std::bitset<8>{out_byte} << std::endl;
        } else {
            out_byte = (out_byte << 4) | decoded.to_ulong();
            output << out_byte;
            // std::cout << std::bitset<8>{out_byte} << std::endl;
            out_byte = 0;
        }
        flag = !flag;
    }
    std::cout << (double)errors / (double)total << std::endl;
    return 0;
}
