#pragma once
#include "node.hpp"
#include <iostream>
#include <stdexcept>
#include <utility>


template<typename List>
class ListIterator {
public:
    using T = typename List::ValueType;
public:
    ListIterator();
    ListIterator(node<T>* ptr);
public:
    auto operator++() -> ListIterator<List>&;
    auto operator++(int) -> ListIterator<List>&;
    auto operator*() -> T&;
    auto operator->() -> node<T>*;
    auto operator==(const ListIterator& other) const -> bool;
    auto operator!=(const ListIterator& other) const -> bool;
private:
    node<T>* m_ptr;
};


template<typename T>
class list {
public:
    using ValueType = T;
    using Iterator = ListIterator<list>;
public:
    list();
    list(const list&);
    list(list&&);
    ~list();
    auto operator=(const list&) -> list&;
    auto operator=(list&&) -> list&;
    list(std::initializer_list<T>);

public:
    auto push_front(const T& val = T()) -> void;
    auto push_back(const T& val = T()) -> void;
    auto pop_front() -> void;
    auto front() -> T&;
    auto back() -> T&;
    auto size() const -> unsigned int;
    auto swap(list<T>& a, list<T>& b) -> void {
        using std::swap;

        swap(a.m_head, b.m_head);
        swap(a.m_tail, b.m_tail);
        swap(a.m_size, b.m_size);
    }

public:
    auto merge(const list<T> & other) -> void;
    auto merge(list<T>&& other) -> void;

public:
    auto begin() const -> Iterator;
    auto end() const -> Iterator;
    auto begin() -> Iterator;
    auto end() -> Iterator;


private:
    node<T>* m_head;
    node<T>* m_tail;
    unsigned int m_size;
};


template<typename T>
list<T>::list() : m_head(nullptr), m_tail(nullptr), m_size(0) {}


template<typename T>
list<T>::list(const list& other) : list<T>() {
    auto* curr = other.m_head;

    while(curr != nullptr) {
        push_back(curr->value);
        curr = curr->link;
    }
}


template<typename T>
list<T>::list(list&& other) : list<T>() {
    swap(*this, other);
}


template<typename T>
list<T>::~list() {
    auto* curr = m_head;

    while(curr != nullptr) {
        auto* next = curr->link;
        delete curr;
        curr = next;
    }
}


template<typename T>
auto list<T>::operator=(const list& other) -> list<T>& {
    auto new_list = other;
    swap(*this, new_list);
    return *this;
}


template<typename T>
auto list<T>::operator=(list&& other) -> list<T>& {
    swap(*this, other);
    return *this;
}


template<typename T>
list<T>::list(std::initializer_list<T> args) : list<T>() {
    for(const auto t : args) {
        push_back(t);
    }
}


template<typename T>
auto list<T>::push_front(const T& val) -> void {
    m_size++;
    auto* new_node = new node<T>(val);
    if(!m_head) {
        m_head = m_tail = new_node;
        return;
    }

    new_node->link = m_head;
    m_head = new_node;
}


template<typename T>
auto list<T>::push_back(const T& val) -> void {
    m_size++;
    auto* new_node = new node<T>(val);
    if(!m_tail) {
        m_head = m_tail = new_node;
        return;
    }

    m_tail->link = new_node;
    m_tail = new_node;
}


template<typename T>
auto list<T>::pop_front() -> void {
    m_size--;
    auto* t = m_head;
    m_head = m_head->link;

    if(m_head == nullptr) {
        m_tail = nullptr;
    }

    delete t;
}


template<typename T>
auto list<T>::front() -> T& {
    if(!m_head) {
        throw std::range_error("Cant read from empty list");
    }
    return m_head->value;
}


template<typename T>
auto list<T>::back() -> T& {
    if(!m_tail) {
        throw std::range_error("Cant read from empty list");
    }
    return m_tail->value;
}


template<typename T>
auto list<T>::size() const -> unsigned int {
    return m_size;
}


template<typename T>
auto list<T>::merge(list<T>&& other) -> void {
    m_tail->link = std::exchange(other.m_head, nullptr);
    m_tail = std::exchange(other.m_tail, nullptr);
    m_size = other.m_size + m_size;
}


template<typename T>
auto list<T>::merge(const list<T> & other) -> void {
    for(const auto &i : other) {
        push_back(i);
    }
}


template<typename T>
auto list<T>::begin() -> Iterator {
    return Iterator(m_head);
}


template<typename T>
auto list<T>::end() -> Iterator {
    return Iterator(m_tail->link);
}


template<typename T>
auto list<T>::begin() const -> Iterator {
    return Iterator(m_head);
}


template<typename T>
auto list<T>::end() const -> Iterator {
    return Iterator(m_tail->link);
}



template<typename List>
ListIterator<List>::ListIterator() : m_ptr(nullptr) {}


template<typename List>
ListIterator<List>::ListIterator(node<T>* ptr) : m_ptr(ptr) {}


template<typename List>
auto ListIterator<List>::operator++() -> ListIterator<List>& {
    m_ptr = m_ptr->link;
    return *this;
}


template<typename List>
auto ListIterator<List>::operator++(int) -> ListIterator<List>& {
    ListIterator<List> copy = *this;
    ++(*this);
    return copy;
}


template<typename List>
auto ListIterator<List>::operator*() -> T& {
    return m_ptr->value;
}


template<typename List>
auto ListIterator<List>::operator->() -> node<T>* {
    return m_ptr;
}


template<typename List>
auto ListIterator<List>::operator==(const ListIterator& other) const -> bool {
    return m_ptr == other.m_ptr;
}


template<typename List>
auto ListIterator<List>::operator!=(const ListIterator& other) const -> bool {
    return !(*this == other);
}
