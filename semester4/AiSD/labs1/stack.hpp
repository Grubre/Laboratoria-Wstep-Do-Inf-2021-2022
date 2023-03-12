#pragma once
#include <stdexcept>
#include <utility>
#include <optional>
#include "node.hpp"

template<typename T>
class stack {
public:
    stack();
    stack(const stack&);
    stack(stack&&);
    ~stack();
    auto operator=(const stack&) -> stack&;
    auto operator=(stack&&) -> stack&;
    stack(std::initializer_list<T>);

public:
    auto push(T val = T()) -> void;
    auto pop() -> void;
    auto top() -> T&;
    auto size() const -> unsigned int;
    auto swap(stack<T>& a, stack<T>& b) -> void {
        using std::swap;

        swap(a.m_top, b.m_top);
        swap(a.m_size, b.m_size);
    }
private:
    node<T>* m_top;
    unsigned int m_size;
};


template<typename T>
stack<T>::stack() : m_top(nullptr), m_size(0) {}


template<typename T>
stack<T>::stack(const stack& other) : stack<T>() {
    auto* curr = other.m_top;

    while(m_top != nullptr) {
        push(curr->value);
    }
}


template<typename T>
stack<T>::stack(stack&& other) : stack<T>() {
    swap(*this, other);
}


template<typename T>
stack<T>::~stack() {
    auto* curr = m_top;

    while(curr != nullptr) {
        auto* next = curr->link;
        delete curr;
        curr = next;
    }
}


template<typename T>
auto stack<T>::operator=(const stack& other) -> stack<T>& {
    auto new_stack = other;
    swap(*this, new_stack);
    return *this;
}


template<typename T>
auto stack<T>::operator=(stack&& other) -> stack<T>& {
    swap(*this, other);
    return *this;
}


template<typename T>
stack<T>::stack(std::initializer_list<T> args) : stack<T>() {
    for(const auto t : args) {
        push(t);
    }
}


template<typename T>
auto stack<T>::push(T val) -> void {
    m_size++;
    auto* new_node = new node<T>(val);
    new_node->link = m_top;
    m_top = new_node;
}


template<typename T>
auto stack<T>::pop() -> void {
    m_size--;
    auto* t = m_top;
    m_top = m_top->link;
    delete t;
}


template<typename T>
auto stack<T>::top() -> T& {
    if(!m_top) {
        throw std::range_error("Cant read from empty stack");
    }
    return m_top->value;
}


template<typename T>
auto stack<T>::size() const -> unsigned int {
    return m_size;
}
