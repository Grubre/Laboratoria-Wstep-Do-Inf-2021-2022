#ifndef PIPECHAIN_H
#define PIPECHAIN_H

#include "command.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct PipeChain
{
    Command* commands;
    size_t size;
};
typedef struct PipeChain PipeChain;

void push_comm(PipeChain *pipeChain, Command comm);

PipeChain create_pipechain();

void execute_pipechain(PipeChain* pipeChain);

// This function takes a shell line and splits it into chains of pipes
// pipechaincount is the size of returned array, ex.
// echo a | cat && sleep 1 && false || echo b
// gets turned to an array of four PipeChains
// PipeChain* split_pipechains(int* pipechaincount, const char* line)
// {
//     PipeChain *pipeChain = (PipeChain*)malloc(sizeof(PipeChain));
//     bool isInQuotes = false;
//     int linesize = strlen(line);
//     *pipechaincount = 0;
//     int last_delimiter = 0;
//     int i;
//     for(i = 0; i < linesize; i++)
//     {
//         if(line[i] == '"')
//             isInQuotes = !isInQuotes;
//         if(i < linesize - 1 && !isInQuotes)
//         {
//             for(int j = 0; j < 3; j++)
//             {
//                 if(line[i] == delimiters[j][0] && line[i + 1] == delimiters[j][1])
//                 {
//                     (*pipechaincount)++;
//                     pipeChain = (PipeChain*)realloc(pipeChain, *pipechaincount * sizeof(PipeChain));
//                     pipeChain[*pipechaincount - 1].logic = line[i];
//                     pipeChain[*pipechaincount - 1].comm = (char*)malloc(sizeof(char) * (i - last_delimiter));
//                     memcpy(pipeChain[*pipechaincount - 1].comm, &line[last_delimiter], i - last_delimiter - 1);
//                     last_delimiter = i + 2 + (delimiters[j][1] != ' ');
//                 }
//             }
//         }
//     }
//
//     // The last pipe chain isnt followed by any delimiter
//     // therefore we add it manually and
//     // it's delimiter is left as null
//     (*pipechaincount)++;
//     pipeChain = (PipeChain*)realloc(pipeChain, *pipechaincount * sizeof(PipeChain));
//     pipeChain[*pipechaincount - 1].comm = (char*)malloc(sizeof(char) * (i - last_delimiter));
//     memcpy(pipeChain[*pipechaincount - 1].comm, &line[last_delimiter], i - last_delimiter - 1);
//
//
//     return pipeChain;
// }

// example of getting exit the exit status of a process
// int t;
//     int pid = fork();
//     if(pid < 0)
//         return 1;
//     if(pid == 0)
//     {
//         char* arg[] = { "false", NULL};
//         execvp("false", arg);
//         return 1;
//     }
//     wait(&t);
//     printf("%d\n", WEXITSTATUS(t));



#endif // !PIPECHAIN_H
