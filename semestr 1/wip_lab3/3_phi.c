#include <stdio.h>
#include "3_phi.h"

int phi(long int n)
{
    int wynik = 0;
    for(int i = 1; i <= n; i++)
    {
        int suma = 0;
        for(int j = 1; j <= i; j++)
        {
            if(n % j == 0 && i % j == 0)
                suma++;
        }
        if( suma == 1 )
            wynik++;
    }
    return wynik;
}
