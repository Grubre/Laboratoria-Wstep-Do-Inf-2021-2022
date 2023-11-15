#include <iostream>
#include <fstream>
#include <vector>
#include <limits>
#include <concepts>
#include <array>
#include <iomanip>

using value_type = unsigned long long;
static constexpr int value_type_bits = (std::numeric_limits<value_type>::digits + 3) / 2;
static constexpr int frequency_bits = std::numeric_limits<value_type>::digits - value_type_bits;
static constexpr int PRECISION            = std::numeric_limits<value_type>::digits;
static constexpr int FREQUENCY_BITS       = frequency_bits;
static constexpr value_type MAX_CODE      = (value_type(1) << value_type_bits) - 1;
static constexpr value_type MAX_FREQ      = (value_type(1) << FREQUENCY_BITS) - 1;
static constexpr value_type ONE_FOURTH    = value_type(1) << (value_type_bits - 2);
static constexpr value_type ONE_HALF      = 2 * ONE_FOURTH;
static constexpr value_type THREE_FOURTHS = 3 * ONE_FOURTH;

template<std::integral T>
struct Prob { 
    T low;
    T high; 
    T denominator;
};

template <typename T, typename V>
concept Model = std::integral<V> && requires(T model, Prob<V> prob, int c) {
    { model.get_char(V{}, c) } -> std::same_as<Prob<V>>;
    { model.get_prob(c) } -> std::same_as<Prob<V>>;
    { model.get_count() } -> std::same_as<V>;
};

struct OutputBitstream {
    OutputBitstream(const std::string& filename) : file(filename, std::ifstream::binary) {}
    OutputBitstream(OutputBitstream&& other) = default;
    ~OutputBitstream() {
        if ( mask != 0b10000000 )
            file.put(byte);
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

private:
    std::ofstream file;
    char byte{0};
    unsigned char mask{0b10000000};
};

struct InputBitstream {
    InputBitstream(const std::string& filename) : file(filename, std::ifstream::binary), bits_left(value_type_bits) {}

    auto next_bit() -> int {
        if (mask == 0b00000001) {
            byte = next_byte();
            if (byte < 0) {
                if ( bits_left <= 0 ) {
                    std::cerr << "EOF on input" << std::endl;
                    exit(1);
                } else {
                    bits_left -= 8;
                } 
            }
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
    unsigned char mask{1};
    int byte{-1};
    int bits_left;
};


template<std::integral T>
struct adaptive_model {
    adaptive_model() {
        for ( auto i = 0u ; i < 258 ; i++ )
            frequencies[i] = i;
        max_freq_reached = false;
    }

    auto update(unsigned int c) {
        for ( auto i = c + 1 ; i < frequencies.size() ; i++ )
            frequencies[i]++;
        denominator++;
        if ( frequencies[257] >= MAX_FREQ ) {
            max_freq_reached = true;
        }
    }

    auto get_char(T value, int& c) -> Prob<T> {
        for ( int i = 0 ; i < 257 ; i++ )
            if ( value < frequencies[i+1] ) {
                c = i;
                auto p = Prob<T>{frequencies[i], frequencies[i+1],frequencies[257]};
                if ( !max_freq_reached)
                    update(c);
                return p;
            }
        exit(1);
    }

    auto get_prob(unsigned int c) -> Prob<T> {
        auto p = Prob<T>{ frequencies[c], frequencies[c+1], frequencies[257] };
        if ( !max_freq_reached ) 
            update(c);
        return p;
    }

    auto get_count() -> T {
        return denominator;
    }

private:
    std::array<T, 257> frequencies;
    T denominator;
    bool max_freq_reached;
};

