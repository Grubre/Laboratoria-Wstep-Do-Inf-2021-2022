import java.lang.Math;

// kat wewnetrzny = 108


public class Pieciokat implements Figura {
    public double obliczObwod()
    {
        return 5 * bok;
    }
    public double obliczPole()
    {
        return bok * bok * Math.sqrt(25 + 10 * Math.sqrt(5)) / 4;
    }

    public Pieciokat(double _bok)
    {
        bok = _bok;
    }

    private double bok;
}
