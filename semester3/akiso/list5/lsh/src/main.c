#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <stdbool.h>
#include <unistd.h>
#include "command.h"
#include "pipechain.h"
#include <sys/wait.h>


int readinput(char* line, size_t* bufsize)
{
    int ret = getline(&line, bufsize, stdin);
    line[sizeof(line) - 1] = '\0';
    return ret;
}


void split_by_pipe(int* arglist_size, char* line)
{
    bool isInQuotes = false;
}


int main()
{
    char* line = "echo \"abc\" | cat -et | cat ; echo a";
    int pipechaincount = 0;
    PipeChain* pipeChain = split_pipechains(&pipechaincount, line);

    for(int i = 0; i < pipechaincount; i++)
    {
        printf("%s ", pipeChain[i].comm);
        if(pipeChain[i].logic)
            printf("%c", pipeChain[i].logic);
        printf("\n");
    }

    





    // char* argument_list[] = {"ls", "-l", NULL};
    // execvp("ls", argument_list);
    // while(true)
    // {
    //     char *line = NULL;
    //     size_t bufsize = 0; // have getline allocate a buffer for us
    //     
    //     printf(">>> ");
    //     int eof_ind = readinput(line, &bufsize);
    //     printf("%s",line);
    //
    //     if (eof_ind == -1)
    //         exit(EXIT_SUCCESS);
    //
    //     Command comm = create_comm(line);
    //
    //     free(line);
    // }
    return 0;
}
