//C++17
#include <iostream>
#include <vector>
#include <optional>
#include <cassert>


/*
krótkie objasnienie użytych struktur:
std::vector<int> - tablica o dynamicznej wielkości
std::optional - pozwala nam na opcjonalne przechowywanie danych
w tym wypadku gdy next_tuple nie jest w stanie znaleźć następnego
elementu porządku zwraca on pusty obiekt
*/


inline void print_tuple(const std::vector<int> &tuple)
{
    std::cout << "(";
    for(int i = 0; i < tuple.size() - 1; i++)
    {
        std::cout << tuple[i] << ", ";
    }
    std::cout << tuple[tuple.size()-1] << ")\n";
}


std::optional<std::vector<int>> next_tuple(std::vector<int> L, int n, int k)
{
    int i = k - 1;
    // szukamy ostatniego elementu w ciągu, który
    // możemy zinkrementować tak żeby ciąg
    // nadal był elementem M(n,k)
    while(i >= 0 && !(L[i] < n - (k - i - 1)))
    {
        i--;
    }
    // jeżeli nie znaleźliśmy żadnego elementu,
    // który możemy zinkrementować to znaczy, że
    // podany do funkcji ciąg jest maksymalnym
    // elementem naszego porządku
    if(i < 0)
    {
        return {};
    }
    // w przeciwnym wypadku inkrementujemy ten element,
    // a wszystkie pozostałe elementu ciągu,
    // które występują po nim, zmieniamy na ich
    // minimalną możliwą wartość (wartość poprzedniego elementu + 1)
    L[i]++;
    i++;
    while(i < k)
    {
        L[i] = L[i-1] + 1;
        i++;
    }
    return L;
}


void gen_tuples(int n, int k)
{
    assert(("n nie moze byc mniejsze od k", n >= k));
    assert(("n i k nie moga byc mniejsze lub rowne 0", n > 0 && k > 0));

    std::optional<std::vector<int>> tuple(k);
    for(int i = 0; i < k; i++)
        tuple.value()[i] = i + 1;

    while(tuple.has_value())
    {
        print_tuple(tuple.value());
        tuple = next_tuple(tuple.value(), n, k);
    }
}


int main()
{
    int n,k;
    n = 5;
    k = 3;
    gen_tuples(n,k);
    return 0;
}