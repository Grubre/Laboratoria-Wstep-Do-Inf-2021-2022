#include "pipechain.h"
#include "errorhandler.h"
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
    return pipeChain;
}

bool builtin_func(PipeChain *pipeChain)
{
    if(!strcmp(pipeChain[0].commands->cmd, "cd"))
    {
        // char cur[500];
        // getcwd(cur, 500);
        if(chdir(pipeChain->commands->args[1]) == -1)
            err("could not change directory");
        return true;
    }
    else if(!strcmp(pipeChain[0].commands->cmd,"exit"))
    {
        exit(EXIT_SUCCESS);
        return true;
    }
    return false;
}

int execute_pipechain(PipeChain* pipeChain)
{
    if(builtin_func(pipeChain))
        return -1;
    int pid;
    int* fd = (int*)malloc((pipeChain->size - 1) * 2 * sizeof(int));

    for(size_t i = 0; i < pipeChain->size - 1; i++)
        pipe(fd + (i * 2));

    // first process
    pid = fork();
    if(pid < 0)
        err("could not fork!");
    if(pid == 0)
    {
        for(size_t j = 0; j < (pipeChain->size - 1) * 2; j++)
        {
            if(!(j == 1))
            {
                close(fd[j]);
            }
        }
        fflush(stdout);
        execute_cmd(&pipeChain->commands[0],
                    NULL,
                    &fd[1]);
    }

    // middle processes
    for(size_t i = 1; i < pipeChain->size - 1; i++)
    {
        int* fdin = &fd[(i - 1) * 2];
        int* fdout = &fd[i * 2 + 1];

        pid = fork();
        if(pid < 0)
            err("could not fork!");
        if(pid == 0)
        {
            for(size_t j = 0; j < (pipeChain->size - 1) * 2; j++)
            {
                if(!(j == (i - 1) * 2 || j == i * 2 + 1))
                {
                    close(fd[j]);
                }
            }
            fflush(stdout);
            execute_cmd(&pipeChain->commands[i - 1],
                    fdin,
                    fdout);
        }
    }

    // last process
    if(pipeChain->size == 1)
    {
        return pid;
    }
    pid = fork();
    if(pid < 0)
        err("could not fork!");
    if(pid == 0)
    {
        for(size_t j = 0; j < (pipeChain->size - 1) * 2; j++)
        {
            if(!(j == (pipeChain->size - 2) * 2))
            {
                close(fd[j]);
            }
        }
        fflush(stdout);
        execute_cmd(&pipeChain->commands[pipeChain->size - 1],
        &fd[(pipeChain->size - 2) * 2],
        NULL);
    }

    for(size_t j = 0; j < (pipeChain->size - 1) * 2; j++)
    {
        close(fd[j]);
    }
    return pid;
}
