#include "pipechain.h"
#include "errorhandler.h"
#include "signalhandler.h"
#include <stdlib.h>
#include <unistd.h>

void push_comm(PipeChain *pipeChain, Command comm)
{
    pipeChain->size++;
    pipeChain->commands = (Command*)realloc(pipeChain->commands, pipeChain->size * sizeof(Command));
    pipeChain->commands[pipeChain->size - 1] = comm;
}

PipeChain create_pipechain()
{
    PipeChain pipeChain;
    pipeChain.commands = (Command*)malloc(sizeof(Command));
    pipeChain.size = 0;
    pipeChain.logic = END;
    return pipeChain;
}

bool builtin_func(PipeChain *pipeChain)
{
    if(!strcmp(pipeChain[0].commands->cmd, "cd"))
    {
        // char cur[500];
        // getcwd(cur, 500);
        if(!pipeChain[0].commands->args[1])
            chdir("/");
        else if(chdir(pipeChain->commands->args[1]) == -1)
            err("could not change directory");
        return true;
    }
    else if(!strcmp(pipeChain[0].commands->cmd,"exit"))
    {
        exit(EXIT_SUCCESS);
        return true;
    }
    else if(pipeChain[0].commands->args[1])
    {
        if(!strcmp(pipeChain[0].commands->cmd,"echo") &&
           !strcmp(pipeChain[0].commands->args[1],"$?"))
        {
            extern short last_proc_ret_val;
            printf("%d\n",last_proc_ret_val);
            return true;
        }
    }
    return false;
}

/*
 * @returns PID of the started process
 */
int start_proc(size_t proc_num, PipeChain* pipeChain, int* fd, int* fdin, int* fdout)
{
    int pid = fork();
    if(pid < 0)
        err("could not fork!");
    if(pid == 0)
    {
        fork_sig_handler();
        if(pipeChain->logic == AMPERSAND)
            bg_process_sig_handler();
        for(size_t j = 0; j < (pipeChain->size - 1) * 2; j++)
        {
            if(!(j == (proc_num - 1) * 2 || j == proc_num * 2 + 1))
                {
                    close(fd[j]);
                }
        }
        fflush(stdout);
        execute_cmd(&pipeChain->commands[proc_num],
                    fdin,
                    fdout);
        err("could not execute command!");
        exit(EXIT_FAILURE);
    }


    return pid;
}

/*
 * @returns PID of the last process in the pipechain
 */
int execute_pipechain(PipeChain* pipeChain)
{
    if(builtin_func(pipeChain))
        return -1;
    int pid = -1;
    int* fd = (int*)malloc((pipeChain->size - 1) * 2 * sizeof(int));

    for(size_t i = 0; i < pipeChain->size - 1; i++)
        pipe(fd + (i * 2));

    for(size_t i = 0; i < pipeChain->size; i++)
    {
        int* fdin = ( i == 0 ) ? NULL : &fd[(i - 1) * 2];
        int* fdout = (i == pipeChain->size - 1) ? NULL : &fd[i * 2 + 1];

        pid = start_proc(i, pipeChain, fd, fdin, fdout);
    }

    for(size_t j = 0; j < (pipeChain->size - 1) * 2; j++)
    {
        close(fd[j]);
    }
    return pid;
}
