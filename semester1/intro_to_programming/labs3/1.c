#include <stdio.h>
#include "1_palindrom.h"

int main(void)
{
    char* input;
    scanf("%s", input);
    printf("%s\n", palindrom(input) ? "true" : "false");
    return 0;
}
