#include "Czworokat.hpp"


Czworokat::Czworokat(double _bok1, double _bok2, double _bok3, double _bok4, double _kat) :
bok1(_bok1), bok2(_bok2), bok3(_bok3), bok4(_bok4), kat(_kat)
{}


double Czworokat::obliczObwod() 
{
    return bok1 + bok2 + bok3 + bok4;
}