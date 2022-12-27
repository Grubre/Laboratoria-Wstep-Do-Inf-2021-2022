#include "errorhandler.h"

void err(const char* message)
{
    fprintf(stderr, "[ERROR]: %s\n", message);
}

