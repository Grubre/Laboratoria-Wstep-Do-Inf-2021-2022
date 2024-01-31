#include "lzw.hpp"
#include <bitset>
#include <cmath>
#include <fstream>
#include <iostream>
#include <optional>

EliasGammaEncoder::EliasGammaEncoder(OutputBitstream &output)
    : output(output) {}
EliasGammaDecoder::EliasGammaDecoder(InputBitstream &input) : input(input) {}

EliasDeltaEncoder::EliasDeltaEncoder(OutputBitstream &output)
    : output(output), gamma_encoder(output) {}
EliasDeltaDecoder::EliasDeltaDecoder(InputBitstream &input)
    : input(input), gamma_decoder(input) {}

EliasOmegaEncoder::EliasOmegaEncoder(OutputBitstream &output) : output(output) {
  output.is_omega = true;
}
EliasOmegaDecoder::EliasOmegaDecoder(InputBitstream &input) : input(input) {}

EliasFibonacciEncoder::EliasFibonacciEncoder(OutputBitstream &output)
    : output(output) {}
EliasFibonacciDecoder::EliasFibonacciDecoder(InputBitstream &input)
    : input(input) {}

void EliasGammaEncoder::put(uint64_t value) {
  auto n = uint64_t(std::log2(value));
  auto val = value - (uint64_t(1) << n);

  for (auto i = 0u; i < n; i++) {
    output.put_bit(0);
  }
  output.put_bit(1);
  for (auto i = 0u; i < n; i++) {
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
    if (!next_bit_opt) {
      return std::nullopt;
    }
    if (!*next_bit_opt) {
      break;
    }

    auto read_number = 1u;
    auto to_read = n;

    while (to_read > 0) {
      auto bit = input.next_bit();
      if (!bit) {
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
static std::vector<uint64_t> fibonacci_numbers{0, 1};

void EliasFibonacciEncoder::put(uint64_t value) {
  while (value > fibonacci_numbers.back()) {
    fibonacci_numbers.push_back(
        fibonacci_numbers[fibonacci_numbers.size() - 1] +
        fibonacci_numbers[fibonacci_numbers.size() - 2]);
  }

  auto max_index = 0;
  for (max_index = 0; max_index < fibonacci_numbers.size(); max_index++) {
    if (fibonacci_numbers[max_index] > value) {
      break;
    }
  }

  auto number_tmp = value;
  for (auto i = max_index - 1; i >= 0; i--) {
    if (number_tmp >= fibonacci_numbers[i]) {
      number_tmp -= fibonacci_numbers[i];
      output.put_bit(1);
    } else {
      output.put_bit(0);
    }
  }

  output.put_bit(1);
}

std::optional<uint64_t> EliasFibonacciDecoder::read_next() {
  auto last_bit = false;
  auto input_bit_count = 0u;
  auto number = 0llu;

  auto bit = input.next_bit();

  if (!bit) {
    return std::nullopt;
  }

  while (*bit != last_bit || last_bit == false) {
    if (input_bit_count + 1 >= fibonacci_numbers.size()) {
      fibonacci_numbers.push_back(
          fibonacci_numbers[fibonacci_numbers.size() - 1] +
          fibonacci_numbers[fibonacci_numbers.size() - 2]);
    }

    if (*bit) {
      number += fibonacci_numbers[input_bit_count];
    }

    last_bit = *bit;
    input_bit_count++;

    bit = input.next_bit();

    if (!bit) {
      return std::nullopt;
    }
  }

  return number;
}
