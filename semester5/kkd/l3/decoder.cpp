#include "lzw.hpp"
#include <iostream>
#include <fstream>
#include <memory>

auto main(int argc, char** argv) -> int {
    if (argc < 3) {
        std::cout << "Usage: ./decoder input_file_path output_file_path" << std::endl;
    }

    auto output_file = std::ofstream(argv[2]);

    auto input = InputBitstream(argv[1]);

    auto decoder = std::unique_ptr<UniversalDecoder>();

    if(argv[3] == std::string{"gamma"}) {
        decoder = std::make_unique<EliasGammaDecoder>(input);
    } else if(argv[3] == std::string{"delta"}) {
        decoder = std::make_unique<EliasDeltaDecoder>(input);
    } else if(argv[3] == std::string{"omega"}) {
        decoder = std::make_unique<EliasOmegaDecoder>(input);
    } else if(argv[3] == std::string{"fib"}) {
        decoder = std::make_unique<EliasFibonacciDecoder>(input);
    } else {
        std::cout << "Unknown encoding type" << std::endl;
        return 1;
    }

    decode(decoder.get(), output_file);

    output_file.close();
    return 0;
}
