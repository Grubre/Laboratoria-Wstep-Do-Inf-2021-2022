#pragma once
#include <vector>

class BallMachine
{
public:
    BallMachine(unsigned int size)
    {
        this->size = size;
        bins.resize(size, 0);
    }
    const unsigned int& operator[](std::size_t i) const { return bins[i]; }

    void reset()
    {
        bins.clear();
        bins.resize(size,0);
    }

    void insert(unsigned int bin_num)
    {
        bins[bin_num]++;
    }
private:
    unsigned int size;
    std::vector<unsigned int> bins;
};
