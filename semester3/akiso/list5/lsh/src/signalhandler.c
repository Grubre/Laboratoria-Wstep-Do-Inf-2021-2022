#include "signalhandler.h"

sigjmp_buf main_jmp_buf;
void signal_handler(int sig)
{
    if(sig == SIGINT)
    {
        printf("\n");
        siglongjmp(main_jmp_buf,LOOP_BEGIN_JMP_POINT);
    }
    if(sig == SIGTSTP)
    {
        printf("STOPIMG!\n");
    }
}

void fork_sig_handler()
{
    signal(SIGINT, SIG_DFL);
    signal(SIGTSTP, SIG_DFL);
}

void bg_process_sig_handler()
{
    signal(SIGINT, SIG_IGN);
}
