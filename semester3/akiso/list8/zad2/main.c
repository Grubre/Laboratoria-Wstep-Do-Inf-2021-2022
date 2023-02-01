#include "assembler.h"
#include <ctype.h>
#include <stdio.h>
#include <err.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

/*
Krok pierwszy to znalezienie wszystkich labeli i stokenizowanie
calego programu
potem zassemblowanie linijek po kolei
*/

int parse_instruction(char* token_str) {
    const char* instructions[] = {"jns", "load", "store", "add", "subt",
                                "input", "output", "halt", "skipcond",
                                "jump", "clear", "addi", "jumpi", "loadi", "storei"};
    // tolower
    for ( ; *token_str; ++token_str) *token_str = tolower(* token_str);

    for(int i = 0; i < INSTRUCTIONS_NUM; i++) {
        if(!strcmp(token_str, instructions[i]))
            return i;
    }
    return -1;
}


int parse_memory_alloc(char* token_str) {
    const char* systems[] = {"dec", "hex"};

    for(int i = 0; i < SYSTEMS_NUM; i++) {
        if(!strcmp(token_str, systems[i]))
            return i;
    }
    return -1;
}


Token parse_token(char* token_str, token_type previous_token) {
    Token tok;
    tok.type = NONE;
    const int len = strlen(token_str);

    if(len > 1 && token_str[0] == '/' && token_str[1] == '/') {
        tok.type = COMMENT;
        return tok;
    }

    if(token_str[len - 1] == ',' && len > 1 && previous_token == NONE) {
        tok.type = LABEL;
        return tok;
    }

    int instruction = parse_instruction(token_str);
    if(instruction >= 0) {
        tok.type = INSTRUCTION;
        tok.val = instruction;
        return tok;
    }

    int memory_alloc = parse_memory_alloc(token_str);
    if(memory_alloc >= 0) {
        tok.type = MEMORY;
        tok.val = memory_alloc;
        return tok;
    }

    if(previous_token == INSTRUCTION || previous_token == MEMORY) {
        tok.type = ARGUMENT;
        tok.val = (size_t)token_str;
    }
    
    return tok;

}

int main(int argc, char** argv) {
    //
    // if(argc != 2) {
    //     errno = 22;
    //     err(1, NULL);
    // }
    //
    // FILE* ptr = fopen(argv[1], "r");
    FILE* fptr = fopen("/home/jabuk/Studia/semester3/akiso/list8/zad2/test.txt", "r");

    if(fptr == NULL) {
        errno = 22;
        err(1, "\"%s\" %s", argv[1], " file doesn't exist: ");
    }

    char* line = NULL;
    size_t line_size = 0;

    fseek(fptr, 0L, SEEK_END);
    int sz = ftell(fptr);

    Line* file_contents = (Line*)malloc(sz * sizeof(Line));

    rewind(fptr);

    int k = 0;
    while(getline(&line, &line_size, fptr) != -1) {
        char* token0 = strtok(line, ' ');
        printf("%s", line);
        k++;
    }


    fclose(fptr);

    return 0;
}
