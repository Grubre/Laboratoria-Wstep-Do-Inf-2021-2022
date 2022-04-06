#include <iostream>
#include <vector>
#include <stdexcept>
#include <memory>
#include "Kolo.hpp"
#include "Kwadrat.hpp"
#include "Prostokat.hpp"
#include "Romb.hpp"
#include "Pieciokat.hpp"
#include "Szesciokat.hpp"

int main(int argc, char *argv[])
{
    std::string rodzaje;
    try{
        if(argc < 2)
            throw std::invalid_argument("");
        rodzaje = argv[1];
    }
    catch(std::exception e){
        std::cerr << "Nieprawidlowo podane rodzaje figur" << std::endl;
        return 1;
    };
    std::vector<std::unique_ptr<Figura>> figury;

    int j = 2;
    for(int i = 0; i < rodzaje.size(); i++)
    {
        switch(rodzaje[i])
        {
            case 'o':
            {
                double radius = 0;
                try{
                    radius = std::stoi(argv[j]);
                    if(radius <= 0)
                    {
                        throw std::invalid_argument("");
                    }
                    figury.push_back(std::make_unique<Kolo>(Kolo(radius)));
                }
                catch(std::exception e){
                    std::cerr << "blednie podany promien kola nr " << i << std::endl;
                    return 1;
                }
                j++;
                break;
            }
            case 'c':
            {
                double bok1 = 0, bok2 = 0, bok3 = 0, bok4 = 0, kat = 0;
                try{
                    bok1 = std::stoi(argv[j]);
                    bok2 = std::stoi(argv[j + 1]);
                    bok3 = std::stoi(argv[j + 2]);
                    bok4 = std::stoi(argv[j + 3]);
                    kat  = std::stoi(argv[j + 4]);
                    if(bok1 <= 0 || bok2 <= 0 || bok3 <= 0 || bok4 <= 0 || kat <= 0)
                    {
                        throw std::invalid_argument("");
                    }
                    if(bok1 == bok2 && bok2 == bok3 && bok3 == bok4 && kat == 90)
                    {
                        figury.push_back(std::make_unique<Kwadrat>(Kwadrat(bok1, bok2, bok3, bok4, kat)));
                    }
                    else if(bok1 == bok3 && bok1 != bok2 && bok2 == bok4 && kat == 90)
                    {
                        figury.push_back(std::make_unique<Prostokat>(Prostokat(bok1, bok2, bok3, bok4, kat)));
                    }
                    else if(bok1 == bok2 && bok2 == bok3 && bok3 == bok4 && kat < 90)
                    {
                        figury.push_back(std::make_unique<Romb>(Romb(bok1, bok2, bok3, bok4, kat)));
                    }
                    else if(bok1 == bok2 && bok2 == bok3 && bok3 == bok4 && kat < 180)
                    {
                        figury.push_back(std::make_unique<Romb>(Romb(bok1, bok2, bok3, bok4, 180 - kat)));
                    }
                    else
                    {
                        throw std::invalid_argument("");
                    }
                }
                catch(std::exception e){
                    std::cerr << "blednie podane parametry czworokata nr " << i << std::endl;
                    return 1;
                }
                j+=5;
                break;
            }
            case 'p':
            {
                double bok = 0;
                try{
                    bok = std::stoi(argv[j]);
                    if(bok <= 0)
                    {
                        throw std::invalid_argument("");
                    }
                    figury.push_back(std::make_unique<Pieciokat>(Pieciokat(bok)));
                }
                catch(std::exception e){
                    std::cerr << "blednie podane parametry pieciokata nr " << i << std::endl;
                    return 1;
                }
                j++;
                break;
            }
            case 's':
            {
                double bok = 0;
                try{
                    bok = std::stoi(argv[j]);
                    if(bok <= 0)
                    {
                        throw std::invalid_argument("");
                    }
                    figury.push_back(std::make_unique<Szesciokat>(Szesciokat(bok)));
                }
                catch(std::exception e){
                    std::cerr << "blednie podane parametry szesciokata nr " << i << std::endl;
                    return 1;
                }
                j++;
                break;
            }
            default:
            {
                std::cerr << "Nieprawidlowy typ figury nr " << i << std::endl;
                return 1;
                break;
            }
        }
    }
    for(int i = 0; i < rodzaje.size(); i++)
    {
        try{
            std::cout << "Rodzaj figury: " << rodzaje[i] << std::endl;
            std::cout << "Obwod = " << figury[i]->obliczObwod() << std::endl;
            std::cout << "Pole = " << figury[i]->obliczPole() << std::endl;
        }
        catch(std::exception e){
            std::cerr << "Blad wczytywania figury nr " << i << std::endl;
            return 1;
        };
    }

    return 0;
}