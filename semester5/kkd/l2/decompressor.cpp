#include "common.hpp"

template<std::integral T, Model<T> M>
struct Decompressor {
    Decompressor(InputBitstream&& _stream, std::ofstream&& _output, M&& _model) :
        stream(std::move(_stream)),
        output(std::move(_output)),
        model(std::move(_model))
    {}

    auto decompress() {
        T high = MAX_CODE;
        T low = 0;
        T value = 0;
        for (auto i = 0; i < value_type_bits; i++) {
            value <<= 1;
            value += stream.next_bit() ? 1 : 0;
        }
        while (true) {
            T range = high - low + 1;
            T scaled_value = ((value - low + 1) * model.get_count() - 1) / range;
            int c;
            auto p = model.get_char(scaled_value, c);
            if (c == 256) {
                break;
            }

            output.put((char)c);
            high = low + (range * p.high) / p.denominator - 1;
            low = low + (range * p.low) / p.denominator;

            while (true) {
                if (high < ONE_HALF) {}
                else if ( low >= ONE_HALF) {
                    value -= ONE_HALF;
                    low -= ONE_HALF;
                    high -= ONE_HALF;
                } else if ( low >= ONE_FOURTH && high < THREE_FOURTHS) {
                    value -= ONE_FOURTH;
                    low -= ONE_FOURTH;
                    high -= ONE_FOURTH;
                } else {
                    break;
                }
                low <<= 1;
                high = (high << 1) + 1;
                value = (value << 1) + (stream.next_bit() ? 1 : 0);
            }
        }
    }

private:
    InputBitstream stream;
    std::ofstream output;
    M model;
};

auto main(int argc, char** argv) -> int {
    if (argc < 3) {
        std::cerr << "Usage: <inputfile> <outputfile>" << std::endl;
    }
    auto input = InputBitstream(argv[1]);
    auto output = std::ofstream(argv[2], std::ofstream::binary);

    auto model = adaptive_model<value_type>{};

    auto decompressor = Decompressor<value_type, adaptive_model<value_type>>(std::move(input), std::move(output), std::move(model));
    decompressor.decompress();

    return 0;
}
