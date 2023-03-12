#pragma once
#include <stdexcept>
#include <utility>
#include "node.hpp"


template<typename Cdeque>
class CdequeIterator {
public:
    using T = typename Cdeque::ValueType;
public:
    CdequeIterator(denode<T>* ptr) : m_ptr(ptr) {}

public:
    auto operator++() -> CdequeIterator&;
    auto operator++(int) -> CdequeIterator&;
    auto operator--() -> CdequeIterator&;
    auto operator--(int) -> CdequeIterator&;
    auto operator*() -> T&;
    auto operator->() -> denode<T>*;
    auto operator==(const CdequeIterator& other) const -> bool;
    auto operator!=(const CdequeIterator& other) const -> bool;

private:
    denode<T>* m_ptr;
};


template<typename T>
class cdeque {
public:
    using ValueType = T;
    using Iterator = CdequeIterator<cdeque<T>>;
public:
    cdeque();
    cdeque(const cdeque&);
    cdeque(cdeque&&);
    ~cdeque();
    auto operator=(const cdeque&) -> cdeque&;
    auto operator=(cdeque&&) -> cdeque&;
    cdeque(std::initializer_list<T>);

public:
    auto insert_front(const T& val = T()) -> void;
    auto insert_back(const T& val = T()) -> void;
    auto pop_front() -> void;
    auto pop_back() -> void;
    auto front() -> T&;
    auto back() -> T&;
    auto size() const -> size_t;
    auto swap(cdeque<T>& a, cdeque<T>& b) -> void {
        using std::swap;

        swap(a.m_head, b.m_head);
        swap(a.m_size, b.m_size);
    }

public:
    auto begin() const -> Iterator;
    auto end() const -> Iterator;
    auto begin() -> Iterator;
    auto end() -> Iterator;

private:
    auto _insert_head_helper(denode<T>* node) -> void;

private:
    denode<T>* m_head;
    size_t m_size;
};


template<typename T>
cdeque<T>::cdeque() : m_head(nullptr), m_size(0) {}


template<typename T>
cdeque<T>::cdeque(const cdeque& other) : cdeque<T>() {
    auto* curr = other.m_head;
    if(!curr) {
        return;
    }
    insert_back(curr->value);
    curr = curr->next;

    while(curr != other.m_head) {
        insert_back(curr->value);
        curr = curr->link;
    }
}


template<typename T>
cdeque<T>::cdeque(cdeque&& other) : cdeque<T>() {
    swap(*this, other);
}


template<typename T>
cdeque<T>::~cdeque() {
    auto* curr = m_head;
    if(!curr) {
        return;
    }
    auto* next = curr->next;
    delete curr;
    curr = next;

    while(curr != m_head) {
        auto* next = curr->next;
        delete curr;
        curr = next;
    }
}


template<typename T>
auto cdeque<T>::operator=(const cdeque& other) -> cdeque<T>& {
    auto new_cdeque = other;
    swap(*this, new_cdeque);
    return *this;
}


template<typename T>
auto cdeque<T>::operator=(cdeque&& other) -> cdeque<T>& {
    swap(*this, other);
    return *this;
}


template<typename T>
cdeque<T>::cdeque(std::initializer_list<T> args) : cdeque<T>() {
    for(const auto t : args) {
        insert_back(t);
    }
}


template<typename T>
auto cdeque<T>::insert_front(const T& val) -> void {
    m_size++;
    auto* new_node = new denode<T>(val);
    if(!m_head) {
        _insert_head_helper(new_node);
        return;
    }

    auto* back = m_head->prev;
    back->next = new_node;
    new_node->next = m_head;
    m_head = new_node;
}


template<typename T>
auto cdeque<T>::insert_back(const T& val) -> void {
    m_size++;
    auto* new_node = new denode<T>(val);
    if(!m_head) {
        _insert_head_helper(new_node);
        return;
    }

    auto* back = m_head->prev;
    m_head->prev = new_node;
    back->next = new_node;
    new_node->prev = back;
    new_node->next = m_head;
}


template<typename T>
auto cdeque<T>::pop_front() -> void
{
    if(!m_head) {
        throw std::range_error("Can't pop from empty list");
    }
    m_size--;

    auto* back = m_head->prev;
    auto* next = m_head->next;

    if(m_head == back) {
        next = nullptr;
    }
    else {
        back->next = next;
    }

    delete m_head;

    m_head = next;
}


template<typename T>
auto cdeque<T>::pop_back() -> void {
    if(!m_head) {
        throw std::range_error("Can't pop from empty list");
    }
    m_size--;

    auto* back = m_head->prev;
    auto* back_prev = back->prev;

    if(back == m_head) {
        delete back;
        m_head = nullptr;
        return;
    }

    back_prev->next = m_head;
    m_head->prev = back_prev;

    delete back;
}


template<typename T>
auto cdeque<T>::front() -> T& {
    return m_head->value;
}


template<typename T>
auto cdeque<T>::back() -> T& {
    return m_head->prev->value;
}


template<typename T>
auto cdeque<T>::size() const -> size_t {
    return m_size;
}


template<typename T>
auto cdeque<T>::_insert_head_helper(denode<T>* node) -> void {
    m_head = node;
    node->next = node;
    node->prev = node;
}


template<typename T>
auto cdeque<T>::begin() -> Iterator {
    return Iterator(m_head);
};


template<typename T>
auto cdeque<T>::end() -> Iterator {
    return Iterator(m_head->prev);
};


template<typename T>
auto cdeque<T>::begin() const -> Iterator {
    return Iterator(m_head);
};


template<typename T>
auto cdeque<T>::end() const -> Iterator {
    return Iterator(m_head->prev);
};


template<typename Cdeque>
auto CdequeIterator<Cdeque>::operator++() -> CdequeIterator& {
    m_ptr = m_ptr->next;
    return *this;
}
template<typename Cdeque>
auto CdequeIterator<Cdeque>::operator++(int) -> CdequeIterator& {
    auto copy = *this;
    m_ptr = m_ptr->next;
    return *this;
}
template<typename Cdeque>
auto CdequeIterator<Cdeque>::operator--() -> CdequeIterator& {
    m_ptr = m_ptr->next;
    return *this;
}
template<typename Cdeque>
auto CdequeIterator<Cdeque>::operator--(int) -> CdequeIterator& {
    auto copy = *this;
    m_ptr = m_ptr->next;
    return copy;
}
template<typename Cdeque>
auto CdequeIterator<Cdeque>::operator*() -> T& {
    return m_ptr->value;
}
template<typename Cdeque>
auto CdequeIterator<Cdeque>::operator->() -> denode<T>* {
    return m_ptr;
}
template<typename Cdeque>
auto CdequeIterator<Cdeque>::operator==(const CdequeIterator& other) const -> bool {
    return m_ptr != other.m_ptr;
}
template<typename Cdeque>
auto CdequeIterator<Cdeque>::operator!=(const CdequeIterator& other) const -> bool {
    return !(m_ptr == other.m_ptr);
}
