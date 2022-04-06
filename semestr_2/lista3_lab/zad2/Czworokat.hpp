#pragma once
#include "Figura.hpp"

class Czworokat : public Figura
{
public:
    virtual double obliczPole() = 0;
    double obliczObwod() override;

    Czworokat(double _bok1, double _bok2, double _bok3, double _bok4, double _kat);

protected:
    double bok1, bok2, bok3, bok4, kat;
};
