#include "parser.h"
#include "pipechain.h"
#include "wholeline.h"

struct TokenArr
{
    char** comm_tokens;
    size_t size;
};
typedef struct TokenArr TokenArr;


TokenArr create_token_arr()
{
    TokenArr tokenArr;
    tokenArr.comm_tokens = (char**)malloc(sizeof(char*));
    tokenArr.size = 0;
    return tokenArr;
}


void push_str(TokenArr* tokenArr, char* str)
{
    tokenArr->size++;
    tokenArr->comm_tokens = (char**)realloc(tokenArr->comm_tokens, tokenArr->size * sizeof(char*));
    tokenArr->comm_tokens[tokenArr->size - 1] = str;
}


void reset_tokens(TokenArr* tokenArr)
{
    // for(size_t i = 0; i < tokenArr->size; i++)
    //     free(tokenArr->comm_tokens[i]);
    tokenArr->size = 0;
}


void add_new_pipechain(WholeLine* wholeLine, TokenArr* tokenArr, PipeChain pipeChain, LogicDelim logic)
{
    // We need to push NULL because execvp has to
    // have its args array end with NULL
    push_str(tokenArr, NULL);
    push_comm(&pipeChain, create_comm(tokenArr->comm_tokens));
    pipeChain.logic = logic;
    // reset_tokens(tokenArr);

    push_pipechain(wholeLine, pipeChain);
}


WholeLine parse(char** tokens, const size_t token_cnt)
{
    WholeLine wholeLine = create_wholeline();
    TokenArr tokenArr = create_token_arr();
    PipeChain pipeChain = create_pipechain();

    for(size_t i = 0; i < token_cnt; i++)
    {
        if(!strcmp(tokens[i], "&&"))
        {
            add_new_pipechain(&wholeLine, &tokenArr, pipeChain, AND);
            // reset the pipechain
            pipeChain = create_pipechain();
        }
        else if(!strcmp(tokens[i], "||"))
        {
            add_new_pipechain(&wholeLine, &tokenArr, pipeChain, OR);
            // reset the pipechain
            pipeChain = create_pipechain();
        }
        else if(!strcmp(tokens[i], ";"))
        {
            add_new_pipechain(&wholeLine, &tokenArr, pipeChain, SEMICOLON);
            // reset the pipechain
            pipeChain = create_pipechain();
        }
        if(!strcmp(tokens[i], "&"))
        {
            add_new_pipechain(&wholeLine, &tokenArr, pipeChain, AMPERSAND);
            // reset the pipechain
            pipeChain = create_pipechain();
        }
        else if(!strcmp(tokens[i], "|"))
        {
            // We need to push NULL because execvp has to
            // have its args array end with NULL
            push_str(&tokenArr, NULL);

            push_comm(&pipeChain, create_comm(tokenArr.comm_tokens));
            tokenArr = create_token_arr();
        }
        else
        {
            push_str(&tokenArr, tokens[i]);
        }
    }
    
    if(tokenArr.size > 0)
    {
        add_new_pipechain(&wholeLine, &tokenArr, pipeChain, END);
    }

    return wholeLine;
}


