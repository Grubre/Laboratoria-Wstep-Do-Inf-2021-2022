#ifndef PARSER_H
#define PARSER_H

#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>
#include "wholeline.h"

WholeLine parse(char** tokens, const size_t token_cnt);

#endif // !PARSER_H
