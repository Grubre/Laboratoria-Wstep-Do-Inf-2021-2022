#define _USE_MATH_DEFINES
#include "Romb.hpp"

double Romb::obliczPole()
{
    return bok1 * bok1 * std::sin(kat * (M_PI / 180));
}