#pragma once
#include "Figura.hpp"

class Szesciokat : public Figura
{
public:
    double obliczPole() override;
    double obliczObwod() override;

    Szesciokat(double _bok) : bok(_bok){};
private:
    double bok;
};
