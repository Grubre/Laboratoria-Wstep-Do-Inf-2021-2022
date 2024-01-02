# Wyniki

| Plik     | Local Search - avg | Annealing - avg | Annealing - min | Tabu Search - avg | Tabu Search - min |
|----------|--------------------|-----------------|-----------------|-------------------|-------------------|
| xqf131   | 655                | 624.4           | 590             | 641.1             | 602               |
| xqg237   | 1215.4             | 1158.3          | 1095            | 1189.2            | 1118              |
| pma343   | 1584.7             | 1510.1          | 1427            | 1550.4            | 1457              |
| pka379   | 1540.1             | 1468            | 1388            | 1507.4            | 1376              |
| bcl380   | 1964.2             | 1872.4          | 1770            | 1922.4            | 1807              |
| pbl395   | 1541.6             | 1469.1          | 1389            | 1508.3            | 1418              |
| pbk411   | 1615               | 1539.6          | 1455            | 1580.8            | 1443              |
| pbn423   | 1645.4             | 1568.2          | 1482            | 1610.1            | 1513              |
| pbm436   | 1737.5             | 1656            | 1565            | 1700.2            | 1598              |
| xql662   | 3047               | 2904.9          | 2746            | 2982.5            | 2804              |
| xit1083  | 4342.6             | 4139.5          | 3914            | 4250.1            | 3882              |
| icw1483  | 5393.6             | 5141.5          | 4861            | 5278.8            | 4962              |
| djc1785  | 7442.9             | 7094.9          | 6708            | 7284.4            | 6848              |
| dcb2086  | 8070.3             | 7693.6          | 7274            | 7899.2            | 7215              |
| pds2566  | 9406.1             | 8967.3          | 8479            | 9206.9            | 8655              |

# Parametry
## Wyżarzanie
>   Początkowa wartość temperatury $(T)\leftarrow 0.5$ \
    Współczynnik obniżania $\leftarrow 0.97$ \
    Długość epoki $\leftarrow 0.15T$ \
    Max iteracji od ostatniej poprawy $\leftarrow 0.2N$

## Tabu search
>   Długość listy tabu $\leftarrow 0.2N$ \
    Max iteracji od ostatniej poprawy $\leftarrow 0.3N$