#include "wholeline.h"
#include "errorhandler.h"
#include <stdlib.h>

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

int execute_wholeline(WholeLine* wholeLine)
{
    int ret;
    for(size_t i = 0; i < wholeLine->size; i++)
    {
        PipeChain pipeChain = wholeLine->pipeChains[i];

        int last_proc_pid = execute_pipechain(&pipeChain);

        if(last_proc_pid < 0)
            continue;

        if(pipeChain.logic == AMPERSAND)
            continue;

        waitpid(last_proc_pid, &ret, 0);
        for(size_t i = 0; i < pipeChain.size - 1; i++)
        {
            wait(NULL);
        }

        if (pipeChain.logic == AND)
        {
            if(ret != 0)
                break;
        }
        else if(pipeChain.logic == OR)
        {
            if(ret == 0)
                break;
        }
        else if(pipeChain.logic == SEMICOLON)
        {
            continue;
        }
    }
    return ret;
}
