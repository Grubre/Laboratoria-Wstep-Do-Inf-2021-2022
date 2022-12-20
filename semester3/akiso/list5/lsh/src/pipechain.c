#include "pipechain.h"

void push_comm(PipeChain *pipeChain, Command comm)
{
    pipeChain->size++;
    pipeChain->commands = (Command*)realloc(pipeChain->commands, pipeChain->size * sizeof(Command));
    pipeChain->commands[pipeChain->size - 1] = comm;
}

PipeChain create_pipechain()
{
    PipeChain pipeChain;
    pipeChain.commands = (Command*)malloc(sizeof(Command));
    pipeChain.size = 0;
    return pipeChain;
}

void execute_pipechain(PipeChain* pipeChain)
{
    for(size_t i = 0; i < pipeChain->size; i++)
    {
        // execute_cmd(&pipeChain->commands[i]);
    }
}
