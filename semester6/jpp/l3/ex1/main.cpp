#include "gf.hpp"
#include <iostream>

auto main() -> int {
  GF<7> a{5}, b{6};
  std::cout << a + 5 << std::endl;
  return 0;
}
