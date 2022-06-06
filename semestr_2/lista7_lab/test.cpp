#include <iostream>
#include "binary_tree.hpp"
#include "BinaryTree.hpp"

int main()
{
    BinaryTree<int> tree;
    tree.insert(1);
    tree.insert(2);
    tree.insert(5);

    tree.del(1);

    tree.draw(tree.root);

    return 0;
}