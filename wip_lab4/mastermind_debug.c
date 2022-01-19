#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>


#define MAX_ROUNDS 100


int rounds_num;
FILE* out;


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


void flush_guesses(int* kombinacje, bool* bitmask, bool hide)
{
    for(int i = 0; i < 4 * 6 * 6 * 6 * 6; i+=4)
    {
        if(hide)
        {
            if(bitmask[i/4] == true)
            {
                printf("i = %d: [%d] [%d] [%d] [%d]\n",
                i/4, kombinacje[i + 0], kombinacje[i + 1],
                kombinacje[i + 2], kombinacje[i + 3]);
                fprintf(out, "i = %d: [%d] [%d] [%d] [%d]\n",
                i/4, kombinacje[i + 0], kombinacje[i + 1],
                kombinacje[i + 2], kombinacje[i + 3]);
            }
        } 
        else
        {
            fprintf(out, "i = %d: %d [%d] [%d] [%d] [%d]\n",
            i/4, bitmask[i/4], kombinacje[i + 0], kombinacje[i + 1],
            kombinacje[i + 2], kombinacje[i + 3]);
        }
            
    }
}


void print_guess_debug(int* kombinacje, int* prev_guess, 
int red, int white, int amount_red, int amount_white, int i)
{
    fprintf(out, "========================================\n");
    fprintf(out, "prev guess:\n");
    fprintf(out, "[%d] [%d] [%d] [%d]\n", prev_guess[0], prev_guess[1],
    prev_guess[2], prev_guess[3]);
    fprintf(out, "curr guess:\n");
    fprintf(out, "[%d] [%d] [%d] [%d]\n", kombinacje[i + 0], kombinacje[i + 1],
    kombinacje[i + 2], kombinacje[i + 3]);
    fprintf(out, "ilosc czerwonych = %d\n",amount_red);
    fprintf(out, "red: (%d == %d)\n",red,amount_red);
    fprintf(out, "ilosc bialych = %d\n",amount_white);
    fprintf(out, "white: (%d == %d)\n",white,amount_white);
    fprintf(out, "%s\n", ((amount_white == white && amount_red == red) ? "valid" : "invalid"));
}


void guess(int* kombinacje, bool* bitmask, int* prev_guess, int red, int white)
{
    if(prev_guess[0] == 0) // sprwadzamy czy nie jest to pierwsza iteracja
        return; 
    for(int i = 0; i < 4 * 6 * 6 * 6 * 6; i += 4) // sprawdzamy wszystkie permutacje
    {
        if(bitmask[i / 4] == false) continue;
        int amount_red = 0; // ilosc kolorow ktore sie zgadzaja
        int amount_white = 0; // ilosc kolorow na pozycji ktore sie zgadzaja

        for(int j = 0; j < 4; j++)
        {
            if(prev_guess[j] == kombinacje[i + j]) // sprawdzamy ilosc czerwonych
            {
                amount_red++;
            }
            else
            for(int y = 0; y < 4; y++)
            {
                if(y != j && prev_guess[j] == kombinacje[i + y] && prev_guess[j] != prev_guess[y])
                {
                    amount_white++;
                    break;
                }
            }
        }
        print_guess_debug(kombinacje,prev_guess,red,white,amount_red,amount_white,i);
        

        bitmask[i / 4] = (amount_white == white && amount_red == red);
    }
}


bool guess_and_print(int* kombinacje, bool* bitmask, int* prev_guess, int red, int white)
{
    // to do
    guess(kombinacje, bitmask, prev_guess, red, white);

    flush_guesses(kombinacje, bitmask, true);

    // wypisujemy pierwsza mozliwa permutajce
    for(int i = 0; i < 4 * 6 * 6 * 6 * 6; i += 4)
    {
        if(bitmask[i / 4] == true)
        {
            printf("[%d] [%d] [%d] [%d]\n",
            kombinacje[i + 0], kombinacje[i + 1], kombinacje[i + 2], kombinacje[i + 3]);
            prev_guess[0] = kombinacje[i + 0];
            prev_guess[1] = kombinacje[i + 1];
            prev_guess[2] = kombinacje[i + 2];
            prev_guess[3] = kombinacje[i + 3];
            
            return false; // they didn't cheat
        }
    }
    
    return true; // they cheated!
}


int main()
{
    out = fopen("out.txt","w");

    int* kombinacje = (int*)malloc(4 * sizeof(int) * 6 * 6 * 6 * 6);
    bool* bitmask = (bool*)malloc(sizeof(bool) * 6 * 6 * 6 * 6);

    int* prev_guess = (int*)malloc(4 * sizeof(int));
    prev_guess[0] = 0; // wskazuje na to ze jest to pierwsza iteracja

    rounds_num = 1;
    int red = 0, white = 0;

    bool cheated = 0;

    guesses_init(kombinacje, bitmask);

    while(rounds_num < MAX_ROUNDS)
    {
        cheated = guess_and_print(kombinacje, bitmask, prev_guess, red, white);
        if(cheated)
        {
            printf("Oszukujesz!\n");
            break;
        }
        printf("red: "); scanf("%d", &red);
        printf("white: "); scanf("%d", &white);

        if(red == 4)
        {
            printf("Wygralem po %d zgadnieciach!", rounds_num);
            break;
        }
        rounds_num++;
    }

    fclose(out);
    free(kombinacje);
    free(bitmask);
    free(prev_guess);
    return 0;
}