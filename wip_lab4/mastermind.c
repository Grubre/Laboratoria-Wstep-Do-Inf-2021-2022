#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_ROUNDS 100

int rounds_num;

void guesses_init(int* values, bool* bitmask)
{
    int j = 0;
    for(int a = 1; a <= 6; a++)
    {
        for(int b = 1; b <= 6; b++)
        {
            for(int c = 1; c <= 6; c++)
            {
                for(int d = 1; d <= 6; d++)
                {
                    values[j + 0] = a;
                    values[j + 1] = b;
                    values[j + 2] = c;
                    values[j + 3] = d;
                    j+=4;
                }
            }
        }
    }
    for(int i = 0; i < 6 * 6 * 6 * 6; i++)
        bitmask[i] = true;
}

bool guess(int* kombinacje, bool* bitmask, int red, int white)
{
    

    for(int i = 0; i < 4 * 6 * 6 * 6 * 6; i += 4)
    {
        if(bitmask[i] == true)
        {
            printf("[%d] [%d] [%d] [%d]\n",
            kombinacje[i + 0], kombinacje[i + 1], kombinacje[i + 2], kombinacje[i + 3]);
            return false; // they didn't cheat
        }
    }
    
    return true; // they cheated!
}

int main()
{
    int* kombinacje = (int*)malloc(4 * sizeof(int) * 6 * 6 * 6 * 6);
    bool* bitmask = (bool*)malloc(sizeof(bool) * 6 * 6 * 6 * 6);

    rounds_num = 1;
    int red = 0, white = 0;

    bool cheated = 0;

    guesses_init(kombinacje, bitmask);

    while(rounds_num < MAX_ROUNDS)
    {
        printf("runda numer: %d\n", rounds_num);
        cheated = guess(kombinacje, bitmask, red, white);
        printf("red: "); scanf("%d", &red);
        printf("white: "); scanf("%d", &white);

        if(cheated)
        {
            printf("Oszukujesz!\n");
            break;
        }
        if(white == 4)
        {
            printf("Wygralem w %d rundach!", rounds_num);
            break;
        }
        rounds_num++;
    }

    free(kombinacje);
    free(bitmask);
    return 0;
}