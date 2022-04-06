import java.lang.Math;

public class Kolo implements Figura {
    public double obliczObwod()
    {
        return 2 * Math.PI * radius;
    }
    public double obliczPole()
    {
        return Math.PI * radius * radius;
    }
    public Kolo(double _radius)
    {
        radius = _radius;
    }

    private double radius;
}
