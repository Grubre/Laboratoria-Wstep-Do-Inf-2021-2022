//Jakub Ogrodowczyk 26.10.2021
//implementacja zachłanna (standardowy system monetarny jest kanoniczny,
//wiec implementacja zachlanna jest takze optymalna)
//zrobione

#include <stdlib.h>
#include <stdio.h>

void ObliczIloscPieniedzy(int* ilosc, const int* nominaly, const int wielkoscW , int wartosc)
{
    for(int i = wielkoscW; i >= 0; i--)
    {
        ilosc[i] = wartosc / nominaly[i];
        wartosc = wartosc % nominaly[i];
    }
}

int main()
{
    int zlote, grosze;

    printf("podaj liczbe zlotych: ");
    scanf("%d",&zlote);
    printf("podaj liczbe groszy: ");
    scanf("%d",&grosze);

    const int nominalyZL[8] = {1,2,5,10,20,50,100,200};
    const int nominalyGR[6] = {1,2,5,10,20,50};
    const int iloscWartosciZL = 8, iloscWartosciGR = 6;

    //wyniki
    int* ilosciZL = (int*)calloc(iloscWartosciZL, sizeof(int)); 
    int* ilosciGR = (int*)calloc(iloscWartosciGR, sizeof(int));

    ObliczIloscPieniedzy(ilosciZL, nominalyZL, iloscWartosciZL, zlote);
    ObliczIloscPieniedzy(ilosciGR, nominalyGR, iloscWartosciGR, grosze);

    printf("banknoty:\n");
    for(int i = iloscWartosciZL - 1; i >= 3; i--)
    {
        if(ilosciZL[i] > 0)
            printf("%d x %d zl\n",ilosciZL[i],nominalyZL[i]);
    }
    printf("monety:\n");
    for(int i = 2; i >= 0; i--)
    {
        if(ilosciZL[i] > 0)
            printf("%d x %d zl\n",ilosciZL[i],nominalyZL[i]);
    }
    for(int i = iloscWartosciGR - 1; i >= 0; i--)
    {
        if(ilosciGR[i] > 0)
            printf("%d x %d gr\n",ilosciGR[i],nominalyGR[i]);
    }

    free(ilosciZL);
    free(ilosciGR);
    return 0;
}
