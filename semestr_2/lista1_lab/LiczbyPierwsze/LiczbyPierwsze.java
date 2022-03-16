package LiczbyPierwsze;

public class LiczbyPierwsze {
    int[] pierwsze;
    int ilosc_pierwszych;

    public LiczbyPierwsze(int n)
    {
        ilosc_pierwszych = n - 1;
        boolean[] sito_eratostenesa = new boolean[n + 1];
        
        for(int i = 0; i <= n; i++)
        {
            sito_eratostenesa[i] = true;
        }

        for(int i = 2; i * i <= n; i++)
        {
            if(sito_eratostenesa[i] == true)
            {
                for(int j = i * i; j <= n; j += i)
                {
                    if(sito_eratostenesa[j] == true)
                        ilosc_pierwszych--;
                    sito_eratostenesa[j] = false;
                }
            }
        }
    
        pierwsze = new int[ilosc_pierwszych];
        int i = 2, j = 0;
        while(i <= n && j < n)
        {
            if(sito_eratostenesa[i] == true)
            {
                pierwsze[j] = i;
                j++;
            }
            i++;
        }
    }


    public int liczba(int m)
    {
        return pierwsze[m];
    }
}