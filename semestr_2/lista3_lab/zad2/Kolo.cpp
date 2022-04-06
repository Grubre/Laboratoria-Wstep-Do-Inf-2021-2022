#define _USE_MATH_DEFINES
#include "Kolo.hpp"


Kolo::Kolo(double _radius) : radius(_radius) {}


double Kolo::obliczPole()
{
    return M_PI * radius * radius;
}
double Kolo::obliczObwod()
{
    return 2 * M_PI * radius;
}