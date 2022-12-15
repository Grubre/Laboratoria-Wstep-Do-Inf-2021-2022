#ifndef LEXER_H
#define LEXER_H
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/wait.h>

char *trimwhitespace(char *str);

char** tokenize(char* line, size_t* token_cnt);

#endif // !LEXER_H
