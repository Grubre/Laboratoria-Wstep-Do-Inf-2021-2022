#include <signal.h>
#include <stdio.h>

int main()
{
    int signals[] = {
        SIGHUP, SIGINT, SIGQUIT,
        SIGILL, SIGTRAP, SIGABRT,
        SIGBUS, SIGFPE, SIGKILL,
        SIGUSR1, SIGSEGV, SIGUSR2,
        SIGPIPE, SIGALRM, SIGTERM,
        SIGCHLD, SIGCONT, SIGSTOP,
        SIGTSTP, SIGTTIN, SIGTTOU,
        SIGURG, SIGXCPU, SIGXFSZ,
        SIGVTALRM, SIGPROF, SIGWINCH,
        SIGIO, SIGPWR, SIGSYS
    };

    char* signal_names[] = {
        "SIGHUP", "SIGINT", "SIGQUIT",
        "SIGILL", "SIGTRAP", "SIGABRT",
        "SIGBUS", "SIGFPE", "SIGKILL",
        "SIGUSR1", "SIGSEGV", "SIGUSR2",
        "SIGPIPE", "SIGALRM", "SIGTERM",
        "SIGCHLD", "SIGCONT", "SIGSTOP",
        "SIGTSTP", "SIGTTIN", "SIGTTOU",
        "SIGURG", "SIGXCPU", "SIGXFSZ",
        "SIGVTALRM", "SIGPROF", "SIGWINCH",
        "SIGIO", "SIGPWR", "SIGSYS"
    };

    for(int i = 0; i < 30; i++)
    {
        if(kill(1, signals[i]) != 0)
            printf("Couldnt send signal[%d] = %s to init\n", i, signal_names[i]);
    }
    return 0;
}
