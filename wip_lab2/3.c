//Jakub Ogrodowczyk 26.10.2021
//zrobione

#include <stdio.h>
#include <stdlib.h>

int main()
{
    double suma = 0, n = 1;
    while(suma < 10)
    {
        suma += (1.0 / n);
        n++;
    }
    printf("%f\n",n);
    return 0;
}
