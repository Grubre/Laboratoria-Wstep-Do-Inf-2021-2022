#ifndef COMMAND_H
#define COMMAND_H

#include <stdlib.h>
#include <stdbool.h>
struct Command
{
    char* cmd;
    char** args;
    int* fdin;
    int* fdout;
    bool close_fd[2];
};
typedef struct Command Command;

Command create_comm(char** args, int* fdin, int* fdout, bool close_fdin, bool close_fdout);
int execute_cmd(Command* comm);

#endif // !COMMAND_H
