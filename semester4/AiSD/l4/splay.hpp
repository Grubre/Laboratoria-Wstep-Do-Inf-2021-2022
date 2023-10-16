#pragma once
#include <memory>
#include <iostream>
#include "common.hpp"

class Node {
public:
    int value;
    std::unique_ptr<Node> left;
    std::unique_ptr<Node> right;
    explicit Node(int val) : value(val), left(nullptr), right(nullptr) {}
};

class SplayTree {
private:
    std::unique_ptr<Node> root;

    auto rightRotate(std::unique_ptr<Node>& node, operation_stats& stats) -> void {
        stats.pointer_reads += 2;
        auto left_child = std::move(node->left);
        node->left = std::move(left_child->right);
        left_child->right = std::move(node);
        node = std::move(left_child);
        stats.pointer_substitutions += 3;
    }

auto leftRotate(std::unique_ptr<Node>& node, operation_stats& stats) -> void {
    stats.pointer_reads += 2;
    auto right_child = std::move(node->right);
    node->right = std::move(right_child->left);
    right_child->left = std::move(node);
    node = std::move(right_child);
    stats.pointer_substitutions += 3;
}

auto splay(std::unique_ptr<Node>& node, int value, operation_stats& stats) -> void {
    stats.pointer_reads++;
    stats.comparisons++;
    if (!node || node->value == value)
        return;
    
    stats.comparisons++;
    if (node->value > value) {
        stats.pointer_reads++;
        if (!node->left)
            return;
        
        stats.comparisons++;
        if (node->left->value > value) {
            splay(node->left->left, value, stats);
            rightRotate(node, stats);
        } else if (node->left->value < value) {
            splay(node->left->right, value, stats);
            stats.pointer_reads++;
            if (node->left)
                leftRotate(node->left, stats);
        }
        
        stats.pointer_reads++;
        if (node->left)
            rightRotate(node, stats);
    } else {
        stats.pointer_reads++;
        if (!node->right)
            return;
        
        stats.comparisons++;
        if (node->right->value > value) {
            splay(node->right->left, value, stats);
            stats.pointer_reads++;
            if (node->right)
                rightRotate(node->right, stats);
        } else if (node->right->value < value) {
            splay(node->right->right, value, stats);
            leftRotate(node, stats);
        }
        
        stats.pointer_reads++;
        if (node->right)
            leftRotate(node, stats);
    }
}


    auto calculateHeight(const std::unique_ptr<Node>& node) const -> int {
        if(!node) {
            return -1;
        }
        return std::max(calculateHeight(node->left), calculateHeight(node->right)) + 1;
    }

    auto printSplay(const std::string& prefix, const Node* node, bool isLeft) const -> void {
        if(node != nullptr) {
            std::cout << prefix;
            std::cout << (isLeft ? "├──" : "└──" );
            std::cout << node->value << std::endl;

            printSplay(prefix + (isLeft ? "│   " : "    "), node->left.get(), true);
            printSplay(prefix + (isLeft ? "│   " : "    "), node->right.get(), false);
        }
    }

public:
    auto insert(int value) -> operation_stats {
        operation_stats stats;

        if (!root) {
            root = std::make_unique<Node>(value);
            return stats;
        }
        
        stats.pointer_reads++;
        stats.comparisons++;
        splay(root, value, stats);

        stats.comparisons++;
        if (root->value == value)
            return stats;

        auto newNode = std::make_unique<Node>(value);
        stats.comparisons++;
        if (root->value > value) {
            stats.pointer_substitutions+=4;
            newNode->right = std::move(root);
            newNode->left = std::move(newNode->right->left);
            newNode->right->left = nullptr;
            root = std::move(newNode);
        } else {
            stats.pointer_substitutions+=4;
            newNode->left = std::move(root);
            newNode->right = std::move(newNode->left->right);
            newNode->left->right = nullptr;
            root = std::move(newNode);
        }

        return stats;
    }

    auto deleteValue(int value) -> operation_stats {
        operation_stats stats;
        
        stats.pointer_reads++;
        splay(root, value, stats);

        stats.comparisons++;
        if (!root || root->value != value)
            return stats;
        
        stats.pointer_substitutions++;
        auto tmp = std::move(root);
        stats.comparisons++;
        if (!tmp->left) {
            stats.pointer_substitutions++;
            root = std::move(tmp->right);
        } else {
            stats.pointer_substitutions+=3;
            auto x = std::move(tmp->right);
            root = std::move(tmp->left);
            splay(root, value, stats);
            root->right = std::move(x);
        }
        
        return stats;
    }

    auto find(int value) -> bool {
        operation_stats stats;
        splay(root, value, stats);
        return root && root->value == value;
    }

    auto height() const -> int {
        return calculateHeight(root);
    }

    auto printSplay() const -> void {
        printSplay("", root.get(), false);
    }
};

