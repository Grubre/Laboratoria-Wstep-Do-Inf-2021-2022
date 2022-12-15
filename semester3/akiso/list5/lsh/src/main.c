#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <stdbool.h>
#include <unistd.h>
#include "command.h"
#include "pipechain.h"
#include "lexer.h"
#include <sys/wait.h>


int readinput(char* line, size_t* bufsize)
{
    int ret = getline(&line, bufsize, stdin);
    line[sizeof(line) - 1] = '\0';
    return ret;
}


int main()
{
    char* mystr = "echo    \"abc\" | cat -et | cat ; echo a && false";
    char* line = (char*)malloc(sizeof(char) * strlen(mystr));
    memcpy(line, mystr, strlen(mystr));
    printf("%s\n",line);
    line = trimwhitespace(line);
    // getline(&line, &bufsize, stdin);

    size_t arrsize;
    char** arr = tokenize(line, &arrsize);
    for(size_t i = 0; i < arrsize; i++)
    {
        printf("(%s)\n",arr[i]);
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
