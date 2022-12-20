#ifndef WHOLELINE
#define WHOLELINE
#include "pipechain.h"

struct WholeLine
{
    PipeChain* pipeChains;
    size_t size;
};
typedef struct WholeLine WholeLine;

WholeLine create_wholeline();

void push_pipechain();

#endif // !WHOLELINE
