#pragma once
#include <utility>
#include <stdexcept>
#include "node.hpp"

template<typename T>
class queue {
public:
    queue();
    queue(const queue&);
    queue(queue&&);
    ~queue();
    auto operator=(const queue&) -> queue&;
    auto operator=(queue&&) -> queue&;
    queue(std::initializer_list<T>);

public:
    auto push(T val = T()) -> void;
    auto pop() -> void;
    auto front() -> T&;
    auto back() -> T&;
    auto size() const -> unsigned int;
    auto swap(queue<T>& a, queue<T>& b) -> void {
        using std::swap;

        swap(a.m_front, b.m_front);
        swap(a.m_rear, b.m_rear);
        swap(a.m_size, b.m_size);
    }

private:
    node<T>* m_rear;
    node<T>* m_front;
    unsigned int m_size;
};


template<typename T>
queue<T>::queue() : m_front(nullptr), m_rear(nullptr), m_size(0) {}


template<typename T>
queue<T>::queue(const queue& other) : queue<T>() {
    auto* curr = other.m_front;

    while(curr != nullptr) {
        push(curr->value);
        curr = curr->link;
    }
}


template<typename T>
queue<T>::queue(queue&& other) : queue<T>() {
    swap(*this, other);
}


template<typename T>
queue<T>::~queue() {
    auto* curr = m_front;

    while(curr != nullptr) {
        auto* next = curr->link;
        delete curr;
        curr = next;
    }
}


template<typename T>
auto queue<T>::operator=(const queue& other) -> queue<T>& {
    auto new_queue = other;
    swap(*this, new_queue);
    return *this;
}


template<typename T>
auto queue<T>::operator=(queue&& other) -> queue<T>& {
    swap(*this, other);
    return *this;
}


template<typename T>
queue<T>::queue(std::initializer_list<T> args) : queue<T>() {
    for(const auto t : args) {
        push(t);
    }
}


template<typename T>
auto queue<T>::push(T val) -> void {
    m_size++;
    auto* new_node = new node<T>(val);
    if(!m_rear) {
        m_front = m_rear = new_node;
        return;
    }

    m_rear->link = new_node;
    m_rear = new_node;
}


template<typename T>
auto queue<T>::pop() -> void {
    m_size--;
    auto* t = m_front;
    m_front = m_front->link;

    if(m_front == nullptr) {
        m_rear = nullptr;
    }

    delete t;
}


template<typename T>
auto queue<T>::front() -> T& {
    if(!m_front) {
        throw std::range_error("Cant read from empty queue");
    }
    return m_front->value;
}


template<typename T>
auto queue<T>::back() -> T& {
    if(!m_rear) {
        throw std::range_error("Cant read from empty queue");
    }
    return m_rear->value;
}


template<typename T>
auto queue<T>::size() const -> unsigned int {
    return m_size;
}
