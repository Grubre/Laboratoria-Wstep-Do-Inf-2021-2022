#include "dh.hpp"
#include "gf.hpp"
#include <iostream>

auto main() -> int {
    const auto dhsetup = DHSetup<gf>{};
    std::cout << "Generator: " << dhsetup.getGenerator() << '\n';
    return 0;
}
