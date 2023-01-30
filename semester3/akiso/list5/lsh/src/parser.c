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


void add_new_pipechain(WholeLine* wholeLine, TokenArr* tokenArr, PipeChain pipeChain, LogicDelim logic,
        char* _stdin, char* _stdout, char* _stderr)
{
    // We need to push NULL because execvp has to
    // have its args array end with NULL
    push_str(tokenArr, NULL);
    push_comm(&pipeChain, create_comm(tokenArr->comm_tokens, _stdin, _stdout, _stderr));
    pipeChain.logic = logic;
    // reset_tokens(tokenArr);

    // printf("comm: %s %s\n",pipeChain.commands[0].args[0], pipeChain.commands[0].args[1]);
    push_pipechain(wholeLine, pipeChain);
}

void reset_fds(char**  _stdin, char** _stdout, char** _stderr)
{
    *_stdin = NULL;
    *_stdout = NULL;
    *_stderr = NULL;
}


WholeLine parse(char** tokens, const size_t token_cnt)
{
    WholeLine wholeLine = create_wholeline();
    TokenArr tokenArr = create_token_arr();
    PipeChain pipeChain = create_pipechain();

    char* _stdin = NULL;
    char* _stdout = NULL;
    char* _stderr = NULL;

    for(size_t i = 0; i < token_cnt; i++)
    {
        if(!strcmp(tokens[i], "&&"))
        {
            add_new_pipechain(&wholeLine, &tokenArr, pipeChain, AND, _stdin, _stdout, _stderr);
            // reset the pipechain
            pipeChain = create_pipechain();
            tokenArr = create_token_arr();
            reset_fds(&_stdin, &_stdout, &_stderr);
        }
        else if(!strcmp(tokens[i], "||"))
        {
            add_new_pipechain(&wholeLine, &tokenArr, pipeChain, OR, _stdin, _stdout, _stderr);
            // reset the pipechain
            pipeChain = create_pipechain();
            tokenArr = create_token_arr();
            reset_fds(&_stdin, &_stdout, &_stderr);
        }
        else if(!strcmp(tokens[i], ";"))
        {
            add_new_pipechain(&wholeLine, &tokenArr, pipeChain, SEMICOLON, _stdin, _stdout, _stderr);
            // reset the pipechain
            pipeChain = create_pipechain();
            tokenArr = create_token_arr();
            reset_fds(&_stdin, &_stdout, &_stderr);
        }
        else if(!strcmp(tokens[i], "&"))
        {
            add_new_pipechain(&wholeLine, &tokenArr, pipeChain, AMPERSAND, _stdin, _stdout, _stderr);
            // reset the pipechain
            pipeChain = create_pipechain();
            tokenArr = create_token_arr();
            reset_fds(&_stdin, &_stdout, &_stderr);
        }
        else if(!strcmp(tokens[i], "|"))
        {
            // We need to push NULL because execvp has to
            // have its args array end with NULL
            push_str(&tokenArr, NULL);
            push_comm(&pipeChain, create_comm(tokenArr.comm_tokens, _stdin, _stdout, _stderr));

            tokenArr = create_token_arr();
            reset_fds(&_stdin, &_stdout, &_stderr);
        }
        else if(!strcmp(tokens[i], ">"))
        {
            if(i >= token_cnt - 1)
                continue;
            _stdout = tokens[i+1];

            i += 1;
        }
        else if(!strcmp(tokens[i], "2>"))
        {
            if(i >= token_cnt - 1)
                continue;
            _stderr = tokens[i+1];

            i += 1;
        }
        else if(!strcmp(tokens[i], "<"))
        {
            if(i < token_cnt - 1)
                continue;
            _stdin = tokens[i+1];

            i += 1;
        }
        else
        {
            push_str(&tokenArr, tokens[i]);
        }
    }
    
    if(tokenArr.size > 0)
    {
            add_new_pipechain(&wholeLine, &tokenArr, pipeChain, END, _stdin, _stdout, _stderr);
    }
    else if(pipeChain.size > 0)
    {
        pipeChain.logic = END;
        push_pipechain(&wholeLine, pipeChain);
    }

    return wholeLine;
}


