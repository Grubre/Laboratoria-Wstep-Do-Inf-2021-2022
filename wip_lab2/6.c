//Jakub Ogrodowczyk 27.10.2021
//todo:
//szukanie czy liczba ma pare zaprzyjazniona

#include <stdio.h>
#include <stdlib.h>

int o(int n)
{
    int suma = 0;
    for(int i = 1; i <= n / 2; i++)
    {
        if(n % i == 0)
            suma += i;
    }
    return suma;
}

int main()
{
    int* liczby = (int*)calloc(1000, sizeof(int));
    
    for(int i = 1; i < 1000; i++)
    {
        if(liczby[i] == 0)
        {
            liczby[i] = o(i);
        }
        
        if(liczby[i] == i) //sprawdzanie czy liczba jest doskonała
        {
            printf("%d jest doskonala\n",i);
        }
        else if(liczby[liczby[i]] == 0) //sprawdzanie czy liczba ma pare zaprzyjazniona
        {
            liczby[liczby[i]] = o(liczby[i]);
            if(liczby[liczby[i]] == i)
            {
                printf("%d i %d sa para liczb zaprzyjaznionych\n",i,liczby[i]);
            }
        }
    }
    free(liczby);
    return 0;
}
