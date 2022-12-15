#include "parser.h"
#include "command.h"

int parse(char** tokens, const size_t token_cnt)
{
    int comm_size = 0;
    char** acc_tokens;
    for(size_t i = 0; i < token_cnt; i++)
    {
        if(strcmp(tokens[i], "&&"))
        {
            Command command;
            comm_size = 0;
        }
        else if(strcmp(tokens[i], "||"))
        {
            comm_size = 0;
        }
        else if(strcmp(tokens[i], ";"))
        {
            comm_size = 0;
        }
        else if(strcmp(tokens[i], "|"))
        {
            comm_size = 0;
        }
        else
        {
            comm_size = 0;
        }

    }
    return 0;
}


