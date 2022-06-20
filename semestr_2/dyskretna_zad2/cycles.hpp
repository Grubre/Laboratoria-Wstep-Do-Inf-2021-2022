#pragma once
#include <vector>
#include <algorithm>

// ten template sprawia ze funkcja akceptuje tylko i wylacznie
// vectory przechowuajce liczby naturalne czyli zmienne typow takich jak
// unsigned long long, unsigned int
//
// funkcja zwraca vector vectorow
// gdzie kazdy vector jest oddzielnym cyklem
template<typename T, typename TEnabled = typename std::enable_if_t<std::is_unsigned<T>::value, T>>
std::vector<std::vector<T>> split_cycles(const std::vector<T> &permutation)
{
    int n = permutation.size();
    std::vector<std::vector<T>> cycles;

    // do podzielenia na cykle korzystamy z metody znanej z przeszukiwania grafow
    // czyli tworzymy tablice ktora zapamietuje ktore indeksy naszej permutacji juz odwiedzilismy
    std::vector<bool> visited(n,false);

    for(int i = 0; i < n; i++)
    {
        if(!visited[i])
        {
            std::vector<T> cycle;
            int j = i;
            while(!visited[j])
            {
                cycle.push_back(j + 1);
                visited[j] = true;
                j = permutation[j] - 1;
            }
            cycles.push_back(cycle);
        }
    }

    return cycles;
}

template<typename T, typename TEnabled = typename std::enable_if_t<std::is_unsigned<T>::value, T>>
std::vector<T> generate_n_sequence(unsigned int n)
{
    std::vector<T> sequence(n,0);
    for(int i = 1; i <= n; i++)
    {
        sequence[i-1] = i;
    }
    return sequence;
}

// generuje n-ta liczbe harmoniczna
// w kontekscie naszego programu mozna to zrobic efektywniej
// poprzez zapamietywanie poprzednich wynikow i dodawanie n-tej liczby,
// ale poniewaz zmniejszyloby to nieco czytelnosc kodu,
// a nasz n jest stosunkowo maly (100) wiec lepiej pozostawic to w takiej formie
double harmonic(unsigned int n)
{
    double sum = 0;
    for(int i = 1; i <= n; i++)
    {
        sum += 1.0/i;
    }
    return sum;
}