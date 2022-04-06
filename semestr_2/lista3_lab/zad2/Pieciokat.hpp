#pragma once
#include "Figura.hpp"

class Pieciokat : public Figura
{
public:
    double obliczPole() override;
    double obliczObwod() override;

    Pieciokat(double _bok) : bok(_bok){};
private:
    double bok;
};
