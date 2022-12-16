#ifndef COMMAND_H
#define COMMAND_H

#include <stdlib.h>
struct Command
{
    char* cmd;
    char** args;
    int* fd;
};
typedef struct Command Command;

Command create_comm(char** args);
// {
//     Command comm;
//     comm.cmd = args[0];
//     comm.args = args + 1;
//
//     return comm;
// }

#endif // !COMMAND_H
