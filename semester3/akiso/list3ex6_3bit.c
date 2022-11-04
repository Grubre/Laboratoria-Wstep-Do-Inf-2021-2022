#include <stdio.h>

int main()
{
    const char* str = "Hello World!\n";

    for(int i = 0; i < 256; i++)
    {
// "\x1b[30m"
        printf("\x1b[38;5;%dm%s",i , str);

    }
    return 0;
}
