public class Prostokat extends Czworokat {
    public double obliczObwod()
    {
        return bok1 + bok2 + bok3 + bok4;
    }
    public double obliczPole()
    {
        return bok1 * bok2;
    }

    public Prostokat(double _bok1, double _bok2, double _bok3, double _bok4, double _kat)
    {
        super(_bok1, _bok2, _bok3, _bok4, _kat);
    }
}
