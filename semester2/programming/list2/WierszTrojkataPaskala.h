#ifndef WIERSZ_TROJKATA_PASKALA
#define WIERSZ_TROJKATA_PASKALA

#include <vector>
#include <iostream>

class WierszTrojkataPaskala
{
    private:
    std::vector<int> wiersz;

    public:
    WierszTrojkataPaskala(int n);
    ~WierszTrojkataPaskala();
    int wspolczynnik(int m);
};

#endif