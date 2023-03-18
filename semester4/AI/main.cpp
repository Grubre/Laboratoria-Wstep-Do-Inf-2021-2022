#include <iostream>
#include "fifteen.hpp"
#include "game.hpp"

auto main() -> int {
    game::initialize_lookup();
    game g;
    g.board() = fifteen::get_random();
    for(auto i : g.possible_moves()) {
        std::cout << (int)i.index1 << ", " << (int)i.index2 << std::endl;
    }
    return 0;
}
