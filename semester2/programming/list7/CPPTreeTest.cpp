#include <iostream>
#include "BinarySearchTree.hpp"


int main()
{
    BinarySearchTree<std::string> tree;
    tree.insert("hter");
    tree.insert("fcgd");
    tree.insert("gdrdr");
    tree.insert("tret");
    tree.insert("asd");

    tree.del("hter");

    tree.print();

    return 0;
}