#pragma once


template<typename T>
struct node {
    node() : value(T()), link(nullptr) {}
    node(T val) : value(val), link(nullptr) {}
    T value;
    node* link;
};


template<typename T>
struct denode {
    denode() : value(T()), prev(nullptr), next(nullptr) {}
    denode(T val) : value(val), prev(nullptr), next(nullptr) {}
    T value;
    denode* prev;
    denode* next;
};


