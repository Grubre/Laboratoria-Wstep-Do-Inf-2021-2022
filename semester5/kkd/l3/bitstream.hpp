#pragma once
#include <iostream>
#include <fstream>
#include <optional>

struct OutputBitstream {
    OutputBitstream(const std::string& filename) : is_omega(false), file(filename, std::ofstream::binary) {
    }
    OutputBitstream(OutputBitstream&& other) = default;
    ~OutputBitstream() {
        if ( mask != 0b10000000 ) {
            while(mask != 0 && is_omega) {
                byte |= mask;
                mask >>= 1;
            }
            file.put(byte);
        }
    }

    void put_bit( bool val ) {
        if ( val )
            byte |= mask;
        mask >>= 1;
        if ( !mask ) {
            file.put(byte);
            mask = 0x80;
            byte = 0;
        }
    }

    bool is_omega;
private:
    std::ofstream file;
    char byte{0};
    unsigned char mask{0b10000000};
};

class InputBitstream {
public:
    InputBitstream(const std::string& filename) : file(filename, std::ifstream::binary) {}

    auto next_bit() -> std::optional<int> {
        if (mask == 0b00000001) {
            int nextByte = next_byte();
            if (nextByte == EOF) {
                return std::nullopt;
            }
            byte = static_cast<unsigned char>(nextByte);
            mask = 0b10000000;
        } else {
            mask >>= 1;
        }
        return (byte & mask) != 0;
    }

private:
    auto next_byte() -> int {
        return file.get();
    }

    std::ifstream file;
    unsigned char mask{0b00000001};
    unsigned char byte{0};
};
