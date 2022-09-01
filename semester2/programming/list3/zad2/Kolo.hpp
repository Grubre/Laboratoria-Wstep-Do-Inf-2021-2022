#pragma once
#include "Figura.hpp"

class Kolo : public Figura
{
public:
    double obliczPole() override;
    double obliczObwod() override;

    Kolo(double _radius);

private:
    double radius;
};
