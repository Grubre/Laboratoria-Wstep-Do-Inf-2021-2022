#include "wholeline.h"
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
    for(size_t i = 0; i < wholeLine->size; i++)
    {
        PipeChain pipeChain = wholeLine->pipeChains[0];

        int last_proc_pid = execute_pipechain(&pipeChain);

        if(last_proc_pid < 0)
            continue;

        if(pipeChain.logic == AMPERSAND)
            continue;

        int t;
        waitpid(last_proc_pid, &t, 0);
        for(size_t i = 0; i < pipeChain.size - 1; i++)
        {
            wait(NULL);
        }
        int ret = WIFEXITED(t);
        printf("exited with %d\n",t);
        switch (pipeChain.logic) {
            case AND:
                printf("AND\n");
                if(ret != 0)
                    break;
                break;
            case OR:
                printf("OR\n");
                if(ret == 0)
                    break;
                break;
            case SEMICOLON:
                printf("COLON\n");
                continue;
                break;
            default:
                break;
        }
    }
    return 0;
}
