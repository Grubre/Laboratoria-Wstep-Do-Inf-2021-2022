//Jakub Ogrodowczyk 27.10.2021
//zrobione

#include <stdio.h>
#include <stdlib.h>

unsigned int nwd(int a, int b)
{
     while(b > 0)
     {
         int c = a;
         a = b;
         b = c % b;
     }
     return a;
}

unsigned int r(int n)
{
    int ilosc = 0;
    for(int i = 1; i < n; i++)
        if(nwd(i,n) == 1)
            ilosc++;
    return 2 * ilosc;
}

int main()
{
    unsigned int* dp = (unsigned int*)malloc(1000*sizeof(int));
    dp[0] = 1;
    FILE* plik;
    plik = fopen("wykres.csv","w");

    printf("%d;%f\n",1,1.0);
    fprintf(plik,"%d;%f\n",1,1.0);
    for(int n = 2; n <= 1000; n++)
    { 
        dp[n - 1] = dp[n - 2] + r(n);
        double rn = (double)dp[n - 1] / (double)(n * n);
        printf("%d;%f\n",n,rn);
        fprintf(plik,"%d;%f\n",n,rn);
    }
    fclose(plik);
    free(dp);
    return 0;
}
