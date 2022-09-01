#ifndef LICZBY_PIERWSZE
#define LICZBY_PIERWSZE

#include <vector>

class LiczbyPierwsze
{
    private:
    std::vector<int> pierwsze;

    public:
    LiczbyPierwsze(int n);
    ~LiczbyPierwsze();
    int liczba(int m);
};

#endif