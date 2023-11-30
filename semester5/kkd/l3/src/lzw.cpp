#include "lzw.hpp"
#include <iostream>
#include <map>
#include <fstream>

void decode(UniversalDecoder* decoder, std::ostream& output) {
    auto dict = std::map<uint64_t, std::string>{};
    for(auto i = 0u; i < 256; ++i) {
        dict[i] = std::string{(char)i};
    }

    auto entry = std::string{};
    auto s = entry = dict[*decoder->read_next() - 1];
    output << s;
    while(true) {
        auto current_opt = decoder->read_next();
        if(!current_opt)
            break;

        auto current = *current_opt - 1;

        if (dict.contains(current)) {
            entry = dict[current];
        } else if (current == dict.size()) {
            entry = s + s[0];
        } else {
            std::cerr << "Error: invalid entry" << std::endl;
            return;
        }
        output << entry;
        dict[dict.size()] = s + entry[0];
        s = entry;
    }
}


void encode(std::istream& input, UniversalEncoder* encoder) {
    auto dict = std::map<std::string, uint64_t>{};

    for(auto i = 0u; i < 256; ++i) {
        dict[std::string{(char)i}] = i;
    }

    auto s = std::string{};
    char c;
    while(input.get(c)) {
        if(dict.contains(s + c))
            s += c;
        else {
            encoder->put(dict[s] + 1);
            dict[s + c] = dict.size();
            s = c;
        }
    }

    if(s.size() > 0) {
        encoder->put(dict[s] + 1);
    }
}
