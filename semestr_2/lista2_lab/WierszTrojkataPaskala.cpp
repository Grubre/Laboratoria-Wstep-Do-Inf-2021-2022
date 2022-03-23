#include "WierszTrojkataPaskala.h"

WierszTrojkataPaskala::WierszTrojkataPaskala(int n)
{
    wiersz.resize(n + 1);

    wiersz[0] = 1;

    for(int i = 1; i <= n; i++)
    {
        wiersz[i] = (wiersz[i-1] * (n - i + 1)) / i;
    }
}
WierszTrojkataPaskala::~WierszTrojkataPaskala()
{
    
}
int WierszTrojkataPaskala::wspolczynnik(int m)
{
    return wiersz.at(m);
}