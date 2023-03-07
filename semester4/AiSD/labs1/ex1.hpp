#pragma once
#include <algorithm>
#include <cstddef>
#include <cstdlib>
#include <stdexcept>
#include <cstring>

#define BASE_CAPACITY 10


template<typename T>
class FIFO {
public:
    FIFO() : m_data(nullptr), m_size(0), m_capacity(BASE_CAPACITY) {
        realloc(BASE_CAPACITY);
    }
    FIFO(size_t size) {
        realloc(size);
    }
    ~FIFO() {
        m_size = 0;
        m_capacity = 0;
        if(m_data)
            delete[] m_data;
        m_data = nullptr;
    }
    FIFO(const FIFO& other) {
        this->m_size = other.m_size;
        this->m_capacity = other.m_capacity;
        this->m_data = (T*)malloc(sizeof(T) * this->m_capacity);
        std::memcpy(this->m_data, other.m_data, other.m_size * sizeof(T));
    }
    FIFO(FIFO&& other) {
        this->m_size = other.m_size;
        this->m_capacity = other.m_capacity;
        this->m_data = other.m_data;
        other.m_data = nullptr;
        other.m_capacity = 0;
        other.m_size = 0;
    }
public:

    void push(const T& t) {
        m_size++;
        if(m_size > m_capacity) {
            realloc(m_capacity + m_capacity / 2);
        }

        m_data[m_size - 1] = t;
    }

    bool empty() {
        return m_size == 0;
    }

    T pop() {
        if(empty()) {
            throw std::range_error("Trying to pop from empty queue");
        }
        T ret = m_data[0];
        for(int i = 0; i < m_size - 1; i++) {
            m_data[i] = std::move(m_data[i + 1]);
        }
        m_size--;
        return ret;
    }

    T& front() {
        return m_data[0];
    }

    size_t size() const {
        return m_size;
    }
private:
    void realloc(size_t n_capacity) {
        T* new_ptr = (T*)malloc(sizeof(T) * n_capacity);

        m_size = std::min(m_size, n_capacity);

        for(int i = 0; i < m_size; i++) {
            new_ptr[i] = std::move(m_data[i]);
        }

        delete[] m_data;
        m_data = new_ptr;
        m_capacity = n_capacity;
    }
private:
    T* m_data;
    size_t m_capacity;
    size_t m_size;
};
