public class Kwadrat extends Czworokat{
    public double obliczObwod()
    {
        return 4 * bok1;
    }
    public double obliczPole()
    {
        return bok1 * bok1;
    }

    public Kwadrat(double _bok1, double _bok2, double _bok3, double _bok4, double _kat)
    {
        super(_bok1, _bok2, _bok3, _bok4, _kat);
    }
}
