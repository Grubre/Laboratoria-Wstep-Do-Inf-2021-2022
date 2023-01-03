#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>

volatile int nr_of_sigs = 0;

void sig_handler(int sig)
{
    nr_of_sigs++;
    printf("%d\n",nr_of_sigs);
}

int main()
{
    signal(SIGUSR1, sig_handler);
    
    int pid = fork();

    if(pid == 0)
    {
        sleep(2);
    }
    else
    {
        for(int i = 0; i < 1000; i++)
        {
            kill(pid, SIGUSR1);
        }
        wait(NULL);
    }

    return 0;
}
