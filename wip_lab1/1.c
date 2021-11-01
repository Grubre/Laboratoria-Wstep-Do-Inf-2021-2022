#include <stdio.h>
#include <string.h>

int main()
{
    const char* napis = "ABRAKADABRA";
    size_t dlugosc = strlen(napis);
    for(int i = 0; i < dlugosc; i++)
    {
        for(int j = 0; j < i; j++)
            printf(" ");
        for(int j = 0; j < 2 * (dlugosc - i) - 1; j++)
        {
            j%2 == 0 ? printf("%c",napis[j/2])  : printf(" ");
        }
        printf("\n");
    }
    return 0;
}