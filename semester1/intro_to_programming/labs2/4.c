#include <stdio.h>
#include <math.h>

int main()
{
    double wynik = 1;
    for(int i = 2; i <= 1000; i++)
    {
        wynik = wynik * pow(i,1.0/1000);
    }
    printf("%lf\n",wynik);
    return 0;
}
