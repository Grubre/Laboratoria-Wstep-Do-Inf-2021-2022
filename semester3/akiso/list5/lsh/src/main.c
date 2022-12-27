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
#include "signalhandler.h"

extern sigjmp_buf main_jmp_buf;
int main()
{
    signal(SIGINT, signal_handler);
    while(true)
    {
        if (sigsetjmp(main_jmp_buf,1) == LOOP_BEGIN_JMP_POINT)
        {
        }
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

        execute_wholeline(&wholeLine);

        free(line);
    }


    return 0;
}
