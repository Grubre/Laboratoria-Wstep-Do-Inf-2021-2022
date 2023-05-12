# Wyniki

Heurystyki dopuszczalne dla losowego cofania o k = 80:
|Heurystyka                |Średnia liczba ruchów|Średnia liczba stanów|
|--------------------------|---------------------|---------------------|
|Manhattan Distance        |39.96                |2864772.69           |
|Linear Conflict           |40.08                |1850999.16           |
|Walking Distance          |40.32                |814513.35            |

Heurystyki dopuszczalne dla losowych permutacji:
|Heurystyka                |Średnia liczba ruchów|Średnia liczba stanów|
|--------------------------|---------------------|---------------------|
|Walking Distance          |51.728               |10509990.16          |

Ponadto próbowałem też różne heurystyki niedopuszczalne dla losowych permutacji:
|Heurystyka                              |Średnia liczba ruchów     |Średnia liczba stanów  |
|----------------------------------------|--------------------------|-----------------------|
|Walking Distance (bez brania odległości)| 481.381                  |56234.12               |
|2 * Manhattan Distance                  | 54.981                   |9861091.6              |
|3 * Manhattan Distance                  | 57.266                   |669160.1               |
