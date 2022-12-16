#include "command.h"

Command create_comm(char** args)
{
    Command comm;
    comm.cmd = args[0];
    comm.args = args + 1;
    return comm;
}

