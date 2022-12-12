#include <signal.h>
#include <stdio.h>

int main()
{
    int success = kill(1, SIGKILL);
    printf("kill(1, SIGKILL) returned %d which means %s\n", 
            success,
            success == -1 ? "fail" : "success");
    return 0;
}
