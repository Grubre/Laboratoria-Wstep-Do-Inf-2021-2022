#pragma once
#include <memory>
#include <vector>

struct Node {
    unsigned int key;
    Node* parent = nullptr;
    Node* child = nullptr;
    Node* sibling = nullptr;
    int degree = 0;

    Node(int key) : key(key) {}
};
