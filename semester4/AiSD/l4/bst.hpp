#pragma once
#include <memory>
#include <algorithm>
#include <iostream>
#include <queue>


class Node {
public:
    int value;
    std::unique_ptr<Node> left;
    std::unique_ptr<Node> right;
    explicit Node(int val) : value(val), left(nullptr), right(nullptr) {}
};


class BST {
public:
    struct operation_stats {
        size_t comparisons = 0;
        size_t pointer_reads = 0;
        size_t pointer_substitutions = 0;
    };

private:
    std::unique_ptr<Node> root;

    auto insert(std::unique_ptr<Node>& node, int val, operation_stats& stats) -> void {
        stats.pointer_reads++;
        stats.comparisons++;
        if(!node) {
            stats.pointer_substitutions++;
            node = std::make_unique<Node>(val);
            return;
        }
        stats.comparisons++;
        if(val < node->value)
            insert(node->left, val, stats);
        else
            insert(node->right, val, stats);
    }

    auto deleteValue(std::unique_ptr<Node>& node, int val, operation_stats& stats) -> void {
        stats.pointer_reads++;
        stats.comparisons++;
        if (!node)
            return;

        stats.comparisons++;
        stats.pointer_reads++;
        if (val < node->value) {
            deleteValue(node->left, val, stats);
        } else if (val > node->value) {
            stats.comparisons ++;
            stats.pointer_reads++;
            deleteValue(node->right, val, stats);
        } else {
            stats.pointer_reads ++;
            if (!node->left) {
                stats.pointer_substitutions++;
                node = std::move(node->right);
            } else if (!node->right) {
                stats.pointer_reads++;
                stats.pointer_substitutions++;
                stats.comparisons ++;
                node = std::move(node->left);
            } else {
                stats.pointer_reads++;
                auto& successor = minValueNode(node->right);
                stats.comparisons++;
                node->value = successor->value;
                deleteValue(node->right, successor->value, stats);
            }
        }
    }

    auto minValueNode(std::unique_ptr<Node>& node) -> std::unique_ptr<Node>& {
        return node->left ? minValueNode(node->left) : node;
    }    auto calculateHeight(const std::unique_ptr<Node>& node) const -> int {
        if(!node) {
            return -1;
        }
        return std::max(calculateHeight(node->left), calculateHeight(node->right)) + 1;
    }

    void printBT(const std::string& prefix, const Node* node, bool isLeft) const {
        if(node != nullptr) {
            std::cout << prefix;
            std::cout << (isLeft ? "├──" : "└──" );
            std::cout << node->value << std::endl;

            printBT(prefix + (isLeft ? "│   " : "    "), node->left.get(), true);
            printBT(prefix + (isLeft ? "│   " : "    "), node->right.get(), false);
        }
    }

public:
    BST() : root(nullptr) {}

    auto insert(int val) -> operation_stats {
        operation_stats stats;
        insert(root, val, stats);
        return stats;
    }

    auto deleteValue(int val) -> operation_stats {
        operation_stats stats;
        deleteValue(root, val, stats);
        return stats;
    }
    auto height() const -> int {
        return calculateHeight(root);
    }

    auto display() const -> void {
        if(!root) {
            std::cout << "Empty tree" << std::endl;
            return;
        }

        std::queue<const Node*> nodes;
        nodes.push(root.get());

        while(!nodes.empty()) {
            auto levelSize = nodes.size();

            for(auto i = 0u; i < levelSize; ++i) {
                const auto* node = nodes.front();
                nodes.pop();

                std::cout << node->value << ' ';

                if(node->left)
                    nodes.push(node->left.get());
                if(node->right)
                    nodes.push(node->right.get());
            }

            std::cout << std::endl;
        }
    }

    void printBT() const {
        printBT("", root.get(), false);
    }
};
