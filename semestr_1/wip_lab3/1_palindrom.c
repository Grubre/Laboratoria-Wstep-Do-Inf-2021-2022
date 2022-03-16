#include <stdio.h>
#include "1_palindrom.h"

bool palindrom(const char* input)
{
    int dlugosc = strlen(input), i = 0;
    while(i < dlugosc)
    {
        if(input[i] != input[dlugosc - 1 - i])
            return 0;
        i++;
    }
    return 1;
}
