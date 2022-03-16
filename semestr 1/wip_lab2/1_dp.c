//Jakub Ogrodowczyk 26.10.2021
//implementacja dynamiczna
//zrobione

#include <stdlib.h>
#include <stdio.h>

void ObliczIloscPieniedzy(int* ilosc, const int* nominaly, const int wielkoscW , int wartosc)
{
    //tablica pomocnicza przechowujaca poprzednio obliczone wartosci
    int* dp = (int*)malloc((wartosc) * sizeof(int));
    //tablica pomocnicza przechowujaca wartosc nominalu wybranego przy danym i
    int* dp_nominal = (int*)malloc((wartosc) * sizeof(int));

    dp[0] = 0;
    dp_nominal[0] = 0;

    for(int i = 1; i < wartosc; i++)
        dp[i] = __INT_MAX__;

    for (int i = 0; i < wielkoscW; i++)
    {
		for (int j = 0; j < wartosc; j++)
        {
			if ((j + 1) == nominaly[i]) //jesli szukana wartosc jest rowna jakiemus nominalowi to potrzeba tylko jeden banknot (dp[j] = 1)
            {
				dp[j] = 1;
				dp_nominal[j] = i;
			}
			else if (((j + 1) > nominaly[i]) && (dp[j] > dp[j-nominaly[i]])) //w przeciwnym wypadku dp[j] = min(wartosc nominalu + dp[j - wartosc nominalu]) dla wszystkich nominalow
            {
				dp[j] = dp[j - nominaly[i]] + 1;
				dp_nominal[j] = i;
			}
		}
	}

    //dziala to na zasadzie dzialania tego algorytmu
    //czyli dla przykładu 1765 zł, będzie to:
    //(jako &[x] definiujemy wartosc j, dla ktorej nominaly[j] = x)
    //1) dp_nominal[1764] = &[200]:
    //   ilosc[200] += 1
    //   wartosc -= 200
    //2) dp_nominal[1564] = &[200]
    //   ilosc[200] += 1
    //   wartosc -= 200
    //3)      ...
    //        ...
    //9) dp_nominal[164] = &[100]
    //   ilosc[100] += 1
    //   wartosc -= 100
    //10) dp_nominal[64] = &[50]
    //    ilosc[50] += 1
    //    wartosc -= 50
    //11) dp_nominal[14] = &[10]
    //    ilosc[10] += 1
    //    wartosc -= 10
    //12) dp_nominal[4] = &[5]
    //    ilosc[5] += 1
    //    wartosc -= 5
    //13) dp_nominal[0] = 0
    while (wartosc > 0)
    {
		ilosc[dp_nominal[wartosc-1]] += 1;
		wartosc -= nominaly[dp_nominal[wartosc-1]];
	}

    free(dp);
    free(dp_nominal);
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