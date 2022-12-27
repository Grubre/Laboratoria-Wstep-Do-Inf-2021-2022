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
int execute_wholeline(WholeLine* wholeLine);

void push_pipechain();

#endif // !WHOLELINE
