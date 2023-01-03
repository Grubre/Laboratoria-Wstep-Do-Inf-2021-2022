#include <stdio.h>
#include <signal.h>
#include <unistd.h>

void sig_handler(int sig)
{

}

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
        if(signal(signals[i], sig_handler) != 0)
            printf("Couldnt change behaviour for signal[%d] = %s\n", i, signal_names[i]);
    }

    return 0;
}
