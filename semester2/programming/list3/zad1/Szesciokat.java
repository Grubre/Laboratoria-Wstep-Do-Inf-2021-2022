import java.lang.Math;  

public class Szesciokat implements Figura {
    public double obliczObwod()
    {
        return 6 * bok;
    }
    public double obliczPole()
    {
        return 3 * bok * bok * Math.sqrt(3) / 2;
    }

    public Szesciokat(double _bok)
    {
        bok = _bok;
    }

    private double bok;
}
