#include "Szesciokat.hpp"

double Szesciokat::obliczPole()
{
    return 3 * bok * bok * std::sqrt(3) / 2;
}
double Szesciokat::obliczObwod()
{
    return 6 * bok;
}
