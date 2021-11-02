//Jakub Ogrodowczyk 26.10.2021
//zrobione

#include <stdio.h>
#include <stdlib.h>

int main()
{
    double suma = 0;
    int n = 0;
    while(suma < 10)
    {
        n++;
        suma += (1.0 / n);
    }
    printf("%d\n",n);
    return 0;
}
