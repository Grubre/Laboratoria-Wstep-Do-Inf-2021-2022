#include "Pieciokat.hpp"

double Pieciokat::obliczPole()
{
    return bok * bok * std::sqrt(25 + 10 * std::sqrt(5)) / 4;
}
double Pieciokat::obliczObwod()
{
    return 5 * bok;
}
