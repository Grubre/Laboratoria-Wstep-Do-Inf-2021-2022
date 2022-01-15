#include <stdio.h>

#define MAX_ROUNDS 100

int* previous_guesses;
int g_num;

void guess(int red, int white)
{
    int g[4] = {1,1,1,1};
    printf("[%d] [%d] [%d] [%d]\n", g[0], g[1], g[2], g[3]);
}

int main()
{
    g_num = 1;
    int red = 0, white = 0;
    previous_guesses = (int*)malloc(sizeof(int) * MAX_ROUNDS * 4);

    while(g_num < MAX_ROUNDS)
    {
        printf("runda numer: %d\n", g_num);
        guess(red, white);
        printf("red: "); scanf("%d", &red);
        printf("white: "); scanf("%d", &white);

        if(red == 4)
        {
            printf("Wygrałem w %d rundach!", g_num);
            break;
        }
        g_num++;
    }
    return 0;
}