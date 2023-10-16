#pragma once
#include <memory>
#include <algorithm>
#include <iostream>
#include <queue>
#include "common.hpp"

enum class Color { RED, BLACK };

struct Node {
    int value;
    Node *left, *right, *parent;
    Color color;
};

class RBTree {
private:
    Node *root;

    void fixInsert(Node* &pt, operation_stats& stats) {
        Node *parent_pt = nullptr;
        Node *grand_parent_pt = nullptr;

        while ((pt != root) && (pt->color != Color::BLACK) && (pt->parent->color == Color::RED)) {
            stats.comparisons++;
            parent_pt = pt->parent;
            grand_parent_pt = pt->parent->parent;
            stats.pointer_reads++;
            stats.pointer_substitutions++;
            if (parent_pt == grand_parent_pt->left) {
                stats.comparisons++;
                Node *uncle_pt = grand_parent_pt->right;
                stats.pointer_substitutions++;
                if (uncle_pt != nullptr && uncle_pt->color == Color::RED) {
                    stats.comparisons++;
                    stats.pointer_reads++;
                    grand_parent_pt->color = Color::RED;
                    parent_pt->color = Color::BLACK;
                    uncle_pt->color = Color::BLACK;
                    pt = grand_parent_pt;
                    stats.pointer_substitutions++;
                }
                else {
                    if (pt == parent_pt->right) {
                        stats.pointer_substitutions++;
                        stats.comparisons++;
                        rotateLeft(parent_pt, stats);
                        pt = parent_pt;
                        parent_pt = pt->parent;
                    }

                    rotateRight(grand_parent_pt, stats);
                    stats.pointer_substitutions++;
                    std::swap(parent_pt->color, grand_parent_pt->color);
                    pt = parent_pt;
                }
            }

            else {
                Node *uncle_pt = grand_parent_pt->left;
                stats.pointer_substitutions++;
                if ((uncle_pt != nullptr) && (uncle_pt->color == Color::RED)) {
                    stats.pointer_substitutions++;
                    stats.comparisons++;
                    grand_parent_pt->color = Color::RED;
                    parent_pt->color = Color::BLACK;
                    uncle_pt->color = Color::BLACK;
                    pt = grand_parent_pt;
                    stats.pointer_substitutions++;
                    stats.pointer_reads++;
                    stats.comparisons++;
                }
                else {
                    if (pt == parent_pt->left) {
                        stats.pointer_reads++;
                        stats.comparisons++;
                        rotateRight(parent_pt, stats);
                        pt = parent_pt;
                        parent_pt = pt->parent;
                    }

                    rotateLeft(grand_parent_pt, stats);
                    std::swap(parent_pt->color, grand_parent_pt->color);
                    pt = parent_pt;
                }
            }
        }

        root->color = Color::BLACK;
    }

    void rotateLeft(Node *&pt, operation_stats& stats) {
        Node *pt_right = pt->right;
        stats.pointer_reads++;

        pt->right = pt_right->left;
        stats.pointer_substitutions++;

        stats.comparisons++;
        if (pt->right != nullptr)
            pt->right->parent = pt;

        pt_right->parent = pt->parent;

        if (pt->parent == nullptr)
            root = pt_right;

        else if (pt == pt->parent->left)
            pt->parent->left = pt_right;

        else
            pt->parent->right = pt_right;
        stats.comparisons+=3;

        pt_right->left = pt;
        stats.pointer_substitutions++;
        pt->parent = pt_right;
        stats.pointer_substitutions++;
    }

    void rotateRight(Node *&pt, operation_stats& stats) {
        Node *pt_left = pt->left;
        stats.pointer_reads++;
        
        pt->left = pt_left->right;
        stats.pointer_substitutions++;

        stats.comparisons++;
        if (pt->left != nullptr)
            pt->left->parent = pt;

        pt_left->parent = pt->parent;

        if (pt->parent == nullptr)
            root = pt_left;

        else if (pt == pt->parent->left)
            pt->parent->left = pt_left;

        else
            pt->parent->right = pt_left;
        stats.comparisons+=3;

        pt_left->right = pt;
        stats.pointer_substitutions++;
        pt->parent = pt_left;
        stats.pointer_substitutions++;
    }

    int getBlackHeight(Node *node) {
        int blackHeight = 0;
        while (node != nullptr) {
            if (node->color == Color::BLACK) {
                blackHeight++;
            }
            node = node->left;
        }
        return blackHeight;
    }

    auto calculateHeight(Node* node) const -> int {
        if(!node) {
            return -1;
        }
        return std::max(calculateHeight(node->left), calculateHeight(node->right)) + 1;
    }

public:
    RBTree() { root = nullptr; }

    auto insert(int value) -> operation_stats {
        operation_stats stats;
        Node *node = new Node;
        node->parent = nullptr;
        node->value = value;
        node->left = nullptr;
        node->right = nullptr;
        node->color = Color::RED;

        Node *y = nullptr;
        Node *x = this->root;

        while (x != nullptr) {
            stats.pointer_substitutions++;
            y = x;
            if (node->value < x->value) {
                stats.comparisons++;
                x = x->left;
            }
            else
                x = x->right;
        }

        node->parent = y;
        stats.pointer_substitutions++;
        stats.comparisons++;
        if (y == nullptr) {
            root = node;
            stats.pointer_substitutions++;
        }
        else if (node->value < y->value) {
            y->left = node;
            stats.pointer_substitutions++;
        }
        else {
            y->right = node;
            stats.pointer_substitutions++;
        }
        fixInsert(node, stats);
        return stats;
    }

    auto height() -> int{
        return calculateHeight(root);
    }
    void fixDelete(Node* x, operation_stats& stats) {
        if (x == nullptr)
            return;

        if (x != root && x->color == Color::BLACK) {
            Node* sibling = x->parent->left == x ? x->parent->right : x->parent->left;

            if (sibling->color == Color::RED) {
                sibling->color = Color::BLACK;
                x->parent->color = Color::RED;
                if (sibling == x->parent->left) {
                    rotateRight(x->parent, stats);
                } else {
                    rotateLeft(x->parent, stats);
                }
                fixDelete(x, stats);
            } else {
                if ((sibling->left == nullptr || sibling->left->color == Color::BLACK) &&
                    (sibling->right == nullptr || sibling->right->color == Color::BLACK)) {
                    sibling->color = Color::RED;
                    if (x->parent->color == Color::BLACK)
                        fixDelete(x->parent, stats);
                    else
                        x->parent->color = Color::BLACK;
                } else {
                    if (sibling == x->parent->left && (sibling->left == nullptr || sibling->left->color == Color::BLACK)) {
                        sibling->right->color = Color::BLACK;
                        sibling->color = Color::RED;
                        rotateLeft(sibling, stats);
                        sibling = x->parent->left;
                    } else if (sibling == x->parent->right && (sibling->right == nullptr || sibling->right->color == Color::BLACK)) {
                        sibling->left->color = Color::BLACK;
                        sibling->color = Color::RED;
                        rotateRight(sibling, stats);
                        sibling = x->parent->right;
                    }
                    sibling->color = x->parent->color;
                    x->parent->color = Color::BLACK;
                    if (sibling == x->parent->left) {
                        sibling->left->color = Color::BLACK;
                        rotateRight(x->parent, stats);
                    } else {
                        sibling->right->color = Color::BLACK;
                        rotateLeft(x->parent, stats);
                    }
                }
            }
        } else {
            x->color = Color::BLACK;
        }
    }

    auto deleteValue(int value) -> operation_stats {
        operation_stats stats;
        deleteValue(root, value, stats);
        return stats;
    }

    void deleteValue(Node*& node, int value, operation_stats& stats) {
        stats.comparisons++;
        if (node == nullptr) {
            return;
        } else if (value < node->value) {
            stats.comparisons++;
            deleteValue(node->left, value, stats);
        } else if (value > node->value) {
            stats.comparisons++;
            deleteValue(node->right, value, stats);
        } else {
            stats.comparisons++;
            if (node->left == nullptr || node->right == nullptr) {
                Node* temp = node->left ? node->left : node->right;
                stats.pointer_reads++;
                if (temp == nullptr) {
                    stats.pointer_substitutions++;
                    stats.comparisons++;
                    temp = node;
                    node = nullptr;
                } else {
                    stats.pointer_substitutions++;
                    *node = *temp;
                }

                if (temp->color == Color::BLACK) {
                    stats.comparisons++;
                    fixDelete(node, stats);
                }

                delete temp;
            } else {
                Node* temp = minValueNode(node->right);
                stats.pointer_substitutions++;
                node->value = temp->value;
                stats.pointer_substitutions++;

                deleteValue(node->right, temp->value, stats);
            }
        }
    }

    Node* minValueNode(Node* node) {
        Node* current = node;

        while (current && current->left != nullptr) {
            current = current->left;
        }

        return current;
    }
};

