#include "lexer.h"

char *trimwhitespace(char *str)
{
    char *end;

    // Trim leading space
    while(isspace((unsigned char)*str)) str++;

    if(*str == 0)  // All spaces?
        return str;

    // Trim trailing space
    end = str + strlen(str) - 1;
    while(end > str && isspace((unsigned char)*end)) end--;

    // Write new null terminator character
    end[1] = '\0';

    return str;
}


char** tokenize(char* line, size_t* arr_size)
{
    // char** arr = (char**)malloc(sizeof(char*) * 100);
    char** arr = NULL;
    (*arr_size) = 0;
    size_t len = strlen(line);
    size_t token_begin = 0;
    size_t i;
    for(i = 0; i < len; i++)
    {
        if(line[i] == ' ' && i == token_begin)
        {
            token_begin++;
        }
        else if(line[i] == ' ')
        {
            (*arr_size)++;
            arr = (char**)realloc(arr, (*arr_size) * sizeof(char*));
            arr[(*arr_size) - 1] = (char*)malloc(sizeof(char) * (i - token_begin + 1));
            memcpy(arr[(*arr_size) - 1], line + token_begin, i - token_begin);
            arr[(*arr_size) - 1][i - token_begin] = '\0';
            // printf("Allocating string (%s) of size %lu at %d\n",arr[(*arr_size) - 1], i - token_begin, (*arr_size) - 1);
            token_begin = i + 1;
        }
        else if(line[i] == '"')
        {
            i++;
            while(i < len && line[i] != '"')
            {
                i++;
            }
            i++;
            (*arr_size)++;
            arr = (char**)realloc(arr, (*arr_size) * sizeof(char*));
            arr[(*arr_size) - 1] = (char*)malloc(sizeof(char) * (i - token_begin + 1));
            memcpy(arr[(*arr_size) - 1], line + token_begin, i - token_begin);
            arr[(*arr_size) - 1][i - token_begin] = '\0';
            // printf("Allocating string (%s) of size %lu at %d\n",arr[(*arr_size) - 1], i - token_begin, (*arr_size) - 1);
            token_begin = i + 1;
        }
    }
    
    (*arr_size)++;
    arr = (char**)realloc(arr, (*arr_size) * sizeof(char*));
    arr[(*arr_size) - 1] = (char*)malloc(sizeof(char) * (i - token_begin + 1));
    memcpy(arr[(*arr_size) - 1], line + token_begin, i - token_begin);
    arr[(*arr_size) - 1][i - token_begin] = '\0';
    // printf("Allocating string (%s) of size %lu at %d\n",arr[(*arr_size) - 1], i - token_begin, (*arr_size) - 1);

    return arr;
}



