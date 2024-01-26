#include "lzw.hpp"
#include <bitset>
#include <cmath>
#include <fstream>
#include <iostream>
#include <optional>

EliasGammaEncoder::EliasGammaEncoder(OutputBitstream& output) : output(output) {}
EliasGammaDecoder::EliasGammaDecoder(InputBitstream& input) : input(input) {}

EliasDeltaEncoder::EliasDeltaEncoder(OutputBitstream& output) : output(output), gamma_encoder(output) {}
EliasDeltaDecoder::EliasDeltaDecoder(InputBitstream& input) : input(input), gamma_decoder(input) {}

EliasOmegaEncoder::EliasOmegaEncoder(OutputBitstream& output) : output(output) {output.is_omega = true;}
EliasOmegaDecoder::EliasOmegaDecoder(InputBitstream& input) : input(input) {}

EliasFibonacciEncoder::EliasFibonacciEncoder(OutputBitstream& output) : output(output) {}
EliasFibonacciDecoder::EliasFibonacciDecoder(InputBitstream& input) : input(input) {}




void EliasGammaEncoder::put(uint64_t value) {
    auto n = uint64_t(std::log2(value));
    auto val = value - (uint64_t(1) << n);

    for(auto i = 0u; i < n; i++) {
        output.put_bit(0);
    }
    output.put_bit(1);
    for(auto i = 0u; i < n; i++) {
        output.put_bit(val % 2);
        val /= 2;
    }
}


std::optional<uint64_t> EliasGammaDecoder::read_next() {
    auto n = 0u;
    while (true) {
        auto bit = input.next_bit();
        if (!bit) {
            return std::nullopt;
        }
        if (bit.value() == 1) {
            break;
        }
        n++;
    }

    auto val = 0u;
    for (auto i = 0u; i < n; i++) {
        auto bit = input.next_bit();
        if (!bit) {
            return std::nullopt;
        }
        val += (bool)bit.value() * (1u << i);
    }

    return (1u << n) + val;
}


void EliasDeltaEncoder::put(uint64_t value) {
    if (value == 0) {
        output.put_bit(0);
    }

    auto n = uint64_t(std::log2(value));
    gamma_encoder.put(n + 1);

    uint64_t mask = uint64_t(1) << (n - 1);
    for (uint64_t i = 0; i < n; ++i) {
        output.put_bit((value & mask) != 0);
        mask >>= 1;
    }
}


std::optional<uint64_t> EliasDeltaDecoder::read_next() {
    auto n = gamma_decoder.read_next();
    if (!n) {
        return std::nullopt;
    }

    if (n.value() == 1) {
        return 0;
    }

    uint64_t val = 1u << (n.value() - 1);

    for (uint64_t i = 1; i < n.value(); ++i) {
        auto bit = input.next_bit();
        if (!bit) {
            return std::nullopt;
        }
        if (bit.value()) {
            val |= (1u << (n.value() - 1 - i));
        }
    }

    return val;
}


void EliasOmegaEncoder::put(uint64_t value) {
    if (value == 0) {
        output.put_bit(0);
        return;
    }


    std::vector<bool> bits;
    bits.push_back(0);
    while (value != 1) {
        uint64_t n = value;
        while (n != 0) {
            // std::cout << n << std::endl;
            bits.push_back(n % 2);
            n /= 2;
        }
        value = uint64_t(std::log2(value));
    }

    for (auto it = bits.rbegin(); it != bits.rend(); ++it) {
        output.put_bit(*it);
    }
}

std::optional<uint64_t> EliasOmegaDecoder::read_next() {
    auto n = 1u;

    while (true) {
        auto next_bit_opt = input.next_bit();
        if(!next_bit_opt) {
            return std::nullopt;
        }
        if(!*next_bit_opt) {
            break;
        }

        auto read_number = 1u;
        auto to_read = n;

        while(to_read > 0) {
            auto bit = input.next_bit();
            if(!bit) {
                return std::nullopt;
            }

            read_number *= 2;
            read_number += *bit ? 1 : 0;
            to_read -= 1;
        }

        n = read_number;
    }

    return n;
}

std::vector<uint64_t> generateFibonacciNumbers(uint64_t limit) {
    std::vector<uint64_t> fibonacci = {1, 2}; // Starting from 1, 2 as per Elias-Fibonacci coding
    while (fibonacci.back() < limit) {
        fibonacci.push_back(fibonacci.end()[-1] + fibonacci.end()[-2]);
    }
    return fibonacci;
}

void EliasFibonacciEncoder::put(uint64_t value) {
    if (value == 0) {
        output.put_bit(0);
        return;
    }

    auto fibonacciNumbers = generateFibonacciNumbers(value);
    std::vector<bool> bits(fibonacciNumbers.size(), false);

    for (auto it = fibonacciNumbers.rbegin(); it != fibonacciNumbers.rend(); ++it) {
        if (*it <= value) {
            bits[it - fibonacciNumbers.rbegin()] = true;
            value -= *it;
        }
    }

    for (auto it = bits.rbegin(); it != bits.rend(); ++it) {
        output.put_bit(*it);
    }

    output.put_bit(1);
}

const auto fibonacciNumbers = generateFibonacciNumbers(UINT32_MAX);

std::optional<uint64_t> EliasFibonacciDecoder::read_next() {
    uint64_t value = 0;
    bool previousBit = false;

    for (uint64_t index = 0; ; ++index) {
        auto bit_opt = input.next_bit();
        if (!bit_opt) {
            return std::nullopt;
        }

        bool bit = *bit_opt;

        if (bit && previousBit) {
            break;
        }

        if (bit) {
            if (index < fibonacciNumbers.size()) {
                value += fibonacciNumbers[index];
            } else {
                return std::nullopt;
            }
        }

        previousBit = bit;

        if (!bit) {
            previousBit = false;
        }
    }

    return value;
}

