#include "parser.h"
#include "pipechain.h"
#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>


void push_str(char*** acc_tokens, size_t* arr_size, char* str)
{
    (*arr_size)++;
    (*acc_tokens) = (char**)realloc(*acc_tokens, (*arr_size) * sizeof(char*));
    (*acc_tokens)[(*arr_size) - 1] = str;
}


int parse(char** tokens, const size_t token_cnt)
{
    size_t comm_size = 0;
    char** current_comm_tokens = (char**)malloc(sizeof(char*));
    PipeChain pipeChain = create_pipechain();
    for(size_t i = 0; i < token_cnt; i++)
    {
        if(!strcmp(tokens[i], "&&"))
        {
            push_str(&current_comm_tokens, &comm_size, NULL);
            Command cmd = create_comm(current_comm_tokens, NULL, NULL, false, false);
            if(fork())
                execute_cmd(&cmd);
            wait(NULL);
            comm_size = 0;
        }
        else if(!strcmp(tokens[i], "||"))
        {
            push_str(&current_comm_tokens, &comm_size, NULL);
            comm_size = 0;
        }
        else if(!strcmp(tokens[i], ";"))
        {
            push_str(&current_comm_tokens, &comm_size, NULL);
            comm_size = 0;
        }
        else if(!strcmp(tokens[i], "|"))
        {
            push_str(&current_comm_tokens, &comm_size, NULL);
            push_comm(&pipeChain, create_comm(current_comm_tokens, NULL, NULL, false, false));

            // reset the pipechain
            pipeChain = create_pipechain();
            comm_size = 0;
        }
        else
        {
            push_str(&current_comm_tokens, &comm_size, tokens[i]);
        }
    }
    if(comm_size > 0)
    {
        push_str(&current_comm_tokens, &comm_size, NULL);
        Command cmd = create_comm(current_comm_tokens, NULL, NULL, false, false);
        if(fork())
            execute_cmd(&cmd);
    }
    return 0;
}


