#include <stdio.h>

int main()
{
    const char* str = "Hello World!\n";

    for(int i = 30; i < 38; i++)
    {
        printf("\x1b[%dm%s", i, str);
    }
    return 0;
}
