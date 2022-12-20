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

int execute_cmd(Command *cmd, int* fdin, int* fdout, bool close_fdin, bool close_fdout)
{
    // if(cmd->fdin)
    // {
    //     dup2(*(cmd->fdin), 0);
    // }
    // if(cmd->fdout)
    // {
    //     dup2(*(cmd->fdout), 1);
    // }
    // for(int i = 0; i < 2; i++)
    // {
    //     if(cmd->close_fd[i])
    //         close(i);
    // }

    // printf("cmd: %s, args[0]: %s", cmd->cmd, cmd->args[0]);
    //
    execvp(cmd->cmd, cmd->args);
    return 0;
}

