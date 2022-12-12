// Najpierw root musi nadac uprawnienia:
// sudo chown root a.out
// sudo chmod +s a.out

#include <stddef.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
    if(setuid(0)==-1)
    {
        fprintf(stderr, "You first have to set super user privileges!\n");
        return 1;
    }
    system("/bin/bash");
    return 0;
}

