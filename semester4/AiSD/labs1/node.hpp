#pragma once

template<typename T>
struct node {
    node() : value(T()), link(nullptr) {}
    node(T val) : value(val), link(nullptr) {}
    T value;
    node* link;
};


