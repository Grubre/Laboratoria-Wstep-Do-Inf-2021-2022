#include "command.h"
#include <stdio.h>
#include <unistd.h>

Command create_comm(char** args)
{
    Command comm;
    comm.cmd = args[0];
    comm.args = args;
    return comm;
}

int execute_cmd(Command *cmd, int* fdin, int* fdout)
{
    if(fdin)
    {
        dup2(*(fdin), 0);
    }

    if(fdout)
    {
        dup2(*(fdout), 1);
    }

    execvp(cmd->cmd, cmd->args);
    return 0;
}

