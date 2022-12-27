#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/wait.h>
#include "command.h"
#include "pipechain.h"
#include "lexer.h"
#include "parser.h"
#include "wholeline.h"


int main()
{
    
    while(true)
    {
        printf(">>> ");
        char* line = (char*)malloc(sizeof(char));
        size_t bufsize;
        if(getline(&line, &bufsize, stdin) == -1)
            exit(EXIT_SUCCESS);
        line[strlen(line) - 1] = '\0';
        line = trimwhitespace(line);

        size_t arrsize;
        char** arr = tokenize(line, &arrsize);

        WholeLine wholeLine = parse(arr, arrsize);

        printf("comm: [%s]\n",wholeLine.pipeChains[0].commands->cmd);
        printf("comm: [%s]\n",wholeLine.pipeChains[0].commands->args[1]);

        execute_wholeline(&wholeLine);

        free(line);
    }


    return 0;
}
