#ifndef COMMAND_H
#define COMMAND_H

#include <stdlib.h>
#include <stdbool.h>
struct Command
{
    char* cmd;
    char** args;
};
typedef struct Command Command;

Command create_comm(char** args);
int execute_cmd(Command* comm, int* fdin, int* fdout, bool close_fdin, bool close_fdout);

#endif // !COMMAND_H
