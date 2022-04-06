import java.lang.Math;
import java.lang.reflect.Parameter;

interface SingleParameterFigure{
    public double obliczObwod(double parameter);
    public double obliczPole(double parameter);
}

interface TwoParameterFigure{
    public double obliczObwod(double parameter1, double parameter2);
    public double obliczPole(double parameter1, double parameter2);
}


public class Figury
{
    public enum SingleParameter implements SingleParameterFigure{
        Kolo {
            public double obliczObwod(double parameter)
            {
                return 2 * Math.PI * parameter;
            }
            public double obliczPole(double parameter)
            {
                return Math.PI * parameter * parameter;
            }
        },
        Kwadrat {
            public double obliczObwod(double parameter)
            {
                return 4 * parameter;
            }
            public double obliczPole(double parameter)
            {
                return parameter * parameter;
            }
        },
        Pieciokat {
            public double obliczObwod(double parameter)
            {
                return 5 * parameter;
            }
            public double obliczPole(double parameter)
            {
                return parameter * parameter * Math.sqrt(25 + 10 * Math.sqrt(5)) / 4;
            }
        },
        Szesciokat {
            public double obliczObwod(double parameter)
            {
                return 6 * parameter;
            }
            public double obliczPole(double parameter)
            {
                return 3 * parameter * parameter * Math.sqrt(3) / 2;
            }
        }
    }
    public enum TwoParameters implements TwoParameterFigure{
        Prostokat {
            public double obliczObwod(double parameter1, double parameter2)
            {
                return 2 * parameter1 + 2 * parameter2;
            }
            public double obliczPole(double parameter1, double parameter2)
            {
                return parameter1 * parameter2; 
            }
        },
        Romb {
            public double obliczObwod(double parameter1, double parameter2)
            {
                return 4 * parameter1;
            }
            public double obliczPole(double parameter1, double parameter2)
            {
                return parameter1 * parameter2 * Math.sin(Math.toRadians(parameter1));
            }
        };
    }
}