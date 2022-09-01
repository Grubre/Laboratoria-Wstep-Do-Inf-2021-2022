#include <stdio.h>
#include "2_f.h"

double f(double x)
{
    return x * x - 4;
}

double rozwiazanie(double a, double b, double eps)
{
    double L = a, R = b;
    int rosnaca = f(a) < f(b) ? 1 : -1;
    while( L <= R )
    {
        double x = L + (R - L) / 2;
        double y1 = f(x - eps), y2 = f(x + eps);
        if(y1 * y2 < 0)
            return x;
        if(rosnaca * y1 > 0)
            R = x - eps;
        else
            L = x + eps;
    }
    return -1;
}
