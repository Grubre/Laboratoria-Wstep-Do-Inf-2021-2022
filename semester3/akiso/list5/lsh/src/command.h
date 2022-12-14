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

Command create_comm(const char* line)
{
    Command comm;

    return comm;
}

#endif // !COMMAND_H
