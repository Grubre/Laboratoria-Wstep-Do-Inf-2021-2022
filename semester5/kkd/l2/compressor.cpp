#include "common.hpp"

template<std::integral T, Model<T> M>
struct Compressor {
    Compressor(std::ifstream&& _stream, OutputBitstream&& _output, M&& _model) :
        stream(std::move(_stream)),
        output(std::move(_output)),
        model(std::move(_model))
    {}

    auto compress() {
        T low = 0;
        T high = MAX_CODE;

        while (true) {
            auto c = stream.get();
            if (c < 0) {
                c = 256;
            }

            auto p = model.get_prob((unsigned int)c);
            T range = high - low + 1;
            high = low + (range * p.high / p.denominator) - 1;
            low = low + (range * p.low / p.denominator);

            while (true) {
                if (high < ONE_HALF) {
                    put(0);
                } else if (low >= ONE_HALF) {
                    put(1);
                } else if ( low >= ONE_FOURTH && high < THREE_FOURTHS ) {
                    pending_bits++;
                    low -= ONE_FOURTH;  
                    high -= ONE_FOURTH;  
                } else {
                    break;
                }
                high <<= 1;
                high++;
                low <<= 1;
                high &= MAX_CODE;
                low &= MAX_CODE;
            }

            if ( c == 256 ) //256 is the special EOF code
                break;
        }

        pending_bits++;
        if (low < ONE_FOURTH) {
            put(0);
        } else {
            put(1);
        }
    }

    auto put(bool bit) {
        output.put_bit(bit);
        for ( int i = 0 ; i < pending_bits ; i++ )
              output.put_bit(!bit);
        pending_bits = 0;
    }

private:
    std::ifstream stream;
    OutputBitstream output;
    M model;
    int pending_bits{0};
};

auto main(int argc, char** argv) -> int {
    if (argc < 3) {
        std::cerr << "Usage: <inputfile> <outputfile>" << std::endl;
    }
    auto input = std::ifstream(argv[1], std::ifstream::binary);
    auto output = OutputBitstream(argv[2]);

    auto model = adaptive_model<value_type>{};

    auto compressor = Compressor<value_type, adaptive_model<value_type>>(std::move(input), std::move(output), std::move(model));
    compressor.compress();
    return 0;
}
