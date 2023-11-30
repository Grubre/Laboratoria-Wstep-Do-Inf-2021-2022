#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <optional>
#include "bitstream.hpp"

struct UniversalEncoder {
    virtual void put(uint64_t) = 0;
};

struct UniversalDecoder {
    virtual std::optional<uint64_t> read_next() = 0;
};

struct EliasGammaEncoder : public UniversalEncoder {
    EliasGammaEncoder(OutputBitstream& output_file);
    void put(uint64_t) override;

    OutputBitstream& output;
};

struct EliasGammaDecoder : public UniversalDecoder {
    EliasGammaDecoder(InputBitstream& input_file);
    std::optional<uint64_t> read_next() override;

    InputBitstream& input;
};

struct EliasDeltaEncoder : public UniversalEncoder {
    EliasDeltaEncoder(OutputBitstream& output_file);
    void put(uint64_t) override;

    OutputBitstream& output;
    EliasGammaEncoder gamma_encoder;
};

struct EliasDeltaDecoder : public UniversalDecoder {
    EliasDeltaDecoder(InputBitstream& input_file);
    std::optional<uint64_t> read_next() override;

    InputBitstream& input;
    EliasGammaDecoder gamma_decoder;
};

struct EliasOmegaEncoder : public UniversalEncoder {
    EliasOmegaEncoder(OutputBitstream& output_file);
    void put(uint64_t) override;

    OutputBitstream& output;
};

struct EliasOmegaDecoder : public UniversalDecoder {
    EliasOmegaDecoder(InputBitstream& input_file);
    std::optional<uint64_t> read_next() override;

    InputBitstream& input;
};

struct EliasFibonacciEncoder : public UniversalEncoder {
    EliasFibonacciEncoder(OutputBitstream& output_file);
    void put(uint64_t) override;

    OutputBitstream& output;
};

struct EliasFibonacciDecoder : public UniversalDecoder {
    EliasFibonacciDecoder(InputBitstream& input_file);
    std::optional<uint64_t> read_next() override;

    InputBitstream& input;
};




void decode(UniversalDecoder* decoder, std::ostream& output);
void encode(std::istream& input, UniversalEncoder* encoder);

UniversalEncoder* get_encoder(std::string type, OutputBitstream& output);
