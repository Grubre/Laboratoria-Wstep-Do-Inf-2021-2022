#include "command.h"
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>

Command create_comm(char** args, char* _stdin, char* _stdout, char* _stderr)
{
    Command comm;
    comm.cmd = args[0];
    comm.args = args;
    comm.stdin = _stdin;
    comm.stderr = _stderr;
    comm.stdout = _stdout;
    return comm;
}

int execute_cmd(Command *cmd, int* fdin, int* fdout)
{
    if(cmd->stdin)
    {
        int out = open(cmd->stdin, O_RDWR|O_CREAT|O_APPEND, 0600);
        dup2(out, 0);
    }

    if(cmd->stderr)
    {
        int out = open(cmd->stderr, O_RDWR|O_CREAT|O_APPEND, 0600);
        dup2(out, 2);
    }

    if(cmd->stdout)
    {
        int out = open(cmd->stdout, O_RDWR|O_CREAT|O_APPEND, 0600);
        dup2(out, 1);
    }

    if(fdin)
    {
        dup2(*(fdin), 0);
    }

    if(fdout)
    {
        dup2(*(fdout), 1);
    }
    fflush(stdout);

    execvp(cmd->cmd, cmd->args);
    return 0;
}

