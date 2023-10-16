#pragma once
#include <iostream>
#include <algorithm>
#include <limits>
#include <vector>
#include <memory>
#include <cmath>
#include <optional>
#include "node.hpp"

class binomial_heap {
public:
    binomial_heap() {}

    auto minimum() -> Node* {
        Node* y = nullptr;
        auto x = head;
        auto min = std::numeric_limits<decltype(Node::key)>::max();
        while(x != nullptr) {
            if(x->key < min) {
                min = x->key;
                y = x;
            }
            x = x->sibling;
        }
        return y;
    }

    auto insert(unsigned int key) -> void{
        auto H = binomial_heap{};
        H.head = new Node(key);
        heap_union(H);
        H.head = nullptr;
    }

    
    auto extract_min() -> Node* {
        if(head == nullptr) {
            return nullptr;
        }

        Node* min_prev = nullptr;
        Node* min = head;

        // Find minimum key root
        Node* curr = head;
        Node* prev = nullptr;
        while (curr != nullptr) {
            if (curr->key < min->key) {
                min = curr;
                min_prev = prev;
            }
            prev = curr;
            curr = curr->sibling;
        }

        // Remove min root from root list of heap
        if (min_prev == nullptr) {
            head = min->sibling;
        } else {
            min_prev->sibling = min->sibling;
        }

        // Create new heap from the children of min
        curr = min->child;
        Node* new_head = nullptr;
        while (curr != nullptr) {
            Node* next = curr->sibling;
            curr->sibling = new_head;
            curr->parent = nullptr;
            new_head = curr;
            curr = next;
        }

        binomial_heap h;
        h.head = new_head;

        // Union the original heap with the new heap
        heap_union(h);

        return min;
    }

    auto print_root() const {
        Node* x = head;
        std::cout << "roots: ";
        while(x != nullptr) {
            std::cout << x->key << ", ";
            x = x->sibling;
        }
        std::cout << std::endl;
    }

    auto heap_union(binomial_heap& H2) -> void {
        heap_merge(H2);

        if(head == nullptr) {
            return;
        }

        Node* prev_x = nullptr;
        auto x = head;
        auto next_x = x->sibling;

        while(next_x != nullptr) {
            if(x->degree != next_x->degree || (next_x->sibling != nullptr && next_x->sibling->degree == x->degree)) {
                prev_x = x;
                x = next_x;
            }
            else if(x->key <= next_x->key) {
                x->sibling = next_x->sibling;
                link(next_x, x);
                x = next_x;  // update x
            }
            else if(prev_x == nullptr) {
                head = next_x;
                link(x, next_x);
                x = next_x;
            }
            else {
                prev_x->sibling = next_x;
                link(x,next_x);
                x = next_x;
            }
            next_x = x->sibling;
        }
    }

    auto heap_merge(binomial_heap& H2) -> void {
        if (head == nullptr && H2.head == nullptr) {
            return;
        } else if (head == nullptr) {
            head = H2.head;
            return;
        } else if (H2.head == nullptr) {
            return;
        }
        // Merge the two heaps into one
        Node* h1 = head;
        Node* h2 = H2.head;
        Node* tail;

        if (h1 == nullptr) {
            head = h2;
            return;
        } else if (h2 == nullptr) {
            head = h1;
            return;
        }

        if (h1->degree <= h2->degree) {
            head = h1;
            tail = h1;
            h1 = h1->sibling;
        } else {
            head = h2;
            tail = h2;
            h2 = h2->sibling;
        }
        tail = head;

        while (h1 != nullptr && h2 != nullptr) {
            if (h1->degree <= h2->degree) {
                tail->sibling = h1;
                h1 = h1->sibling;
            } else {
                tail->sibling = h2;
                h2 = h2->sibling;
            }
            tail = tail->sibling;
        }

        if (h1 != nullptr) {
            tail->sibling = h1;
        } else if (h2 != nullptr) {
            tail->sibling = h2;
        }
    }


public:
    auto link(Node* y, Node* z) -> void {
        y->parent = z;
        y->sibling = z->child;
        z->child = y;
        z->degree++;
    }
    Node* head = nullptr;
};
