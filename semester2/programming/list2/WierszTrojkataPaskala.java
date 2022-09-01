public class WierszTrojkataPaskala {
    int[] wiersz;

    public WierszTrojkataPaskala(int n)
    {
        wiersz = new int[n+1];

        wiersz[0] = 1;

        for(int i = 1; i <= n; i++)
        {
            wiersz[i] = (wiersz[i-1] * (n - i + 1)) / i;
        }
    }


    public int wspolczynnik(int m)
    {
        return wiersz[m];
    }
}