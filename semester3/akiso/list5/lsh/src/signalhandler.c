#include <stdbool.h>
#include <sys/wait.h>
#include <unistd.h>
#include "signalhandler.h"
#include "jobs.h"

sigjmp_buf main_jmp_buf;

extern volatile bool waiting_for_proc;

void reset_loop()
{
    siglongjmp(main_jmp_buf,LOOP_BEGIN_JMP_POINT);
}

void signal_handler(int sig)
{
    if(sig == SIGINT)
    {
        printf("\n");
        reset_loop();
    }
    else if(sig == SIGTSTP)
    {
        printf("SIGTSTP\n");
        int pgid = tcgetpgrp(0);
        add_new_job(pgid);
        tcsetpgrp(0, getpgrp());
        reset_loop();
    }
    else if(sig == SIGCHLD)
    {
        int ret = -1;
        int pid = waitpid(-1, &ret, WNOHANG);
        for(size_t i = 0; i < jobs_size; i++)
        {
            if(last_job)
            {
                if(last_job->pgid == pid)
                    tcsetpgrp(0, getpgrp());
                last_job = NULL;
            }
            if(jobs[i].pgid == pid)
            {
                printf("Process [%d] returned %d\n", pid, ret);
                remove_job(pid);
            }
        }
        if(!waiting_for_proc)
            reset_loop();
    }
}

void fork_sigtstp(int sig)
{
    kill(getppid(), SIGTSTP);
    printf("mainpgid: %d\n", getppid());
    signal(SIGTSTP, SIG_DFL);
    raise(SIGTSTP);
}

void fork_sig_handler()
{
    signal(SIGINT, SIG_DFL);
    signal(SIGTSTP, fork_sigtstp);
}

void bg_process_sig_handler()
{
    // signal(SIGINT, SIG_IGN);
}
