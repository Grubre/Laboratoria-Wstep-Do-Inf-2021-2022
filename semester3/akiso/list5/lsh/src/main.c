#include <ctype.h>
#include <stddef.h>
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
short last_proc_ret_val;

int main()
{
    signal(SIGINT, signal_handler);
    // signal(SIGSTOP, signal_handler);
    signal(SIGTSTP, signal_handler);
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

        line = trimwhitespace(line);

        size_t arrsize;
        char** arr = tokenize(line, &arrsize);

        WholeLine wholeLine = parse(arr, arrsize);

        last_proc_ret_val = execute_wholeline(&wholeLine);

        // Free the resources
        // for(size_t i = 0; i < arrsize; i++)
        //     free(arr[i]);
        // free(arr);
        //
        // for(size_t i = 0; i < wholeLine.size; i++)
        // {
        //     for(size_t j = 0; j < wholeLine.pipeChains[i].size; j++)
        //     {
        //         free(wholeLine.pipeChains[i].commands[j].args);
        //     }
        // }
        // free(wholeLine.pipeChains);
        //
        free(line);
    }


    return 0;
}
