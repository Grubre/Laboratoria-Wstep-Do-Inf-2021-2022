#ifndef COMMAND_H
#define COMMAND_H

#include <stdlib.h>
#include <stdbool.h>
struct Command
{
    char* cmd;
    char** args;
    char* stdin;
    char* stderr;
    char* stdout;
};
typedef struct Command Command;

Command create_comm(char** args, char* _stdin, char* _stdout, char* _stderr);
int execute_cmd(Command* comm, int* fdin, int* fdout);

#endif // !COMMAND_H
