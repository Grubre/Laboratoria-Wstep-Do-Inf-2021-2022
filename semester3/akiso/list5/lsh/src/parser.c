#include "parser.h"
#include "command.h"
#include <stdio.h>


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
    for(size_t i = 0; i < token_cnt; i++)
    {
        printf("token = %s\n", tokens[i]);
        if(!strcmp(tokens[i], "&&"))
        {
            printf("your token is: %s\n", "&&");
            for(size_t j = 0; j < comm_size; j++)
            {
                printf("[%s]\n", current_comm_tokens[j]);
            }
            comm_size = 0;
        }
        else if(!strcmp(tokens[i], "||"))
        {
            printf("your token is: %s\n", "||");
            comm_size = 0;
        }
        else if(!strcmp(tokens[i], ";"))
        {
            printf("your token is: %s\n", ";");
            comm_size = 0;
        }
        else if(!strcmp(tokens[i], "|"))
        {
            printf("your token is: %s\n", "|");
            comm_size = 0;
        }
        else
        {
            printf("your token is: %s\n","else");
            push_str(&current_comm_tokens, &comm_size, tokens[i]);
        }

    }
    return 0;
}


