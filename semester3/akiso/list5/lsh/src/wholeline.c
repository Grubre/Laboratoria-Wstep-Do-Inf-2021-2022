#include "wholeline.h"

WholeLine create_wholeline()
{
    WholeLine wholeLine;
    wholeLine.pipeChains = (PipeChain*)malloc(sizeof(PipeChain));
    wholeLine.size = 0;
    return wholeLine;
}

void push_pipechain(WholeLine* wholeLine, PipeChain pipeChain)
{
    wholeLine->size++;
    wholeLine->pipeChains = (PipeChain*)realloc(wholeLine->pipeChains, wholeLine->size * sizeof(PipeChain));
    wholeLine->pipeChains[wholeLine->size - 1] = pipeChain;
}

