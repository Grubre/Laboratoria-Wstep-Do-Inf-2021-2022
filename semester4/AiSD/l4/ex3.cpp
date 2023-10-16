#include "redblack.hpp"

auto main() -> int {
    RBTree tree;
    tree.insert(0);
    tree.insert(1);
    tree.insert(2);
    tree.insert(3);
    tree.insert(4);
    tree.insert(5);
    tree.printRBT();
    return 0;
}
