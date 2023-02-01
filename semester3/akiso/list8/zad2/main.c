#include "assembler.h"
#include <ctype.h>
#include <stdio.h>
#include <err.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>


split_line split(char* line) {
    split_line tokens;
    tokens.strs[0] = NULL;
    tokens.strs[1] = NULL;
    tokens.strs[2] = NULL;
    tokens.num_of_tokens = 0;
    char* token = strtok(line, " \t");
    while(token && tokens.num_of_tokens < 3) {
        if(token[strlen(token) - 1] == '\n') {
            token[strlen(token) - 1] = '\0';
        }
        tokens.strs[tokens.num_of_tokens] = token;
        tokens.num_of_tokens++;
        token = strtok(NULL, " \t");
    }
    return tokens;
}


int parse_instruction(char* token_str) {
    const char* instructions[] = {"jns", "load", "store", "add", "subt",
                                "input", "output", "halt", "skipcond",
                                "jump", "clear", "addi", "jumpi", "loadi", "storei"};

    for(int i = 0; i < INSTRUCTIONS_NUM; i++) {
        if(!strcasecmp(token_str, instructions[i]))
            return i;
    }

    return -1;
}


bool is_comment(char* token_str) {
    if(token_str == NULL || strlen(token_str) < 2)
        return 0;
    return token_str[0] == '/' && token_str[1] == '/';
}


int parse_memory_alloc(char* token_str) {
    const char* systems[] = {"hex", "dec"};

    for(int i = 0; i < SYSTEMS_NUM; i++) {
        if(!strcasecmp(token_str, systems[i]))
            return i;
    }
    return -1;
}

line_tokens tokenize_line(int line_nr, split_line line) {
    line_tokens tokens;
    tokens.tokens[0] = NULL;
    tokens.tokens[1] = NULL;
    tokens.types[0] = NONE;
    tokens.types[1] = NONE;
    if(line.num_of_tokens < 1 || strlen(line.strs[0]) < 2) {
        // errno = 5;
        // errx(2, "Parsing error at line: %d\n", line_nr);
        return tokens;
    }

    // variable marking the first token (in case of label it is 1)
    int current_token = 0;

    // Check if the line is commented out
    if(is_comment(line.strs[current_token])) {
        tokens.types[0] = COMMENT;
        return tokens;
    }

    // Check if the line has a label and add it if so
    if(line.strs[current_token][strlen(line.strs[current_token]) - 1] == ',') {
        if(line.num_of_tokens < 2) {
            errno = 5;
            errx(2, "Parsing error at line: %d [Label has to be followed by an instruction]\n", line_nr);
        }
        line.strs[current_token][strlen(line.strs[current_token]) - 1] = '\0';
        // printf("Adding label: [%s] at %d\n", line.strs[current_token], line_nr);
        push_label(line.strs[current_token], line_nr);
        current_token++;
    }

    tokens.tokens[0] = strdup(line.strs[current_token]);
    int inst = parse_instruction(tokens.tokens[0]);
    // if it is not an instruction
    if(inst < 0) {
        inst = parse_memory_alloc(tokens.tokens[0]);
        if(inst >= 0) {
            tokens.types[0] = MEMORY;
        }
    }
    else {
        tokens.types[0] = INSTRUCTION;
    }

    current_token++;

    // If its neither instruction nor memory alloc - throw error
    if(inst < 0) {
        errno = 5;
        errx(2, "Parsing error at line: %d\n", line_nr);
    }

    if(current_token == line.num_of_tokens || is_comment(line.strs[current_token])) {
        return tokens;
    }

    tokens.types[1] = ARGUMENT;
    tokens.tokens[1] = strdup(line.strs[current_token]);

    return tokens;
}


char* to_binary(unsigned int bit_cnt, int value) {
    char* ret = (char*)malloc((bit_cnt + 1) * sizeof(char));
    ret += bit_cnt + 1;

    for (int i = bit_cnt - 1; i >= 0; i--) {
        *ret = (value & 1) + '0';
        ret--;

        value >>= 1;
    }
    ret++;

    ret[bit_cnt] = '\0';

    return ret;
}


char* assemble(int line_nr, line_tokens tokens) {
    char* ret = NULL;

    char* op = tokens.tokens[0];
    char* arg = tokens.tokens[1];

    if(tokens.types[0] == MEMORY) {
        if(op == NULL) {
            errno = 5;
            errx(2, "Parsing error at line: %d [No value to put in memory]\n", line_nr);
        }
        number_system system = parse_memory_alloc(op);
        switch (system) {
            case HEX:
                ret = to_binary(BITS, strtol(arg, NULL, 16));
            break;
            case DEC:
                ret = to_binary(BITS, atoi(arg));
            break;
        }
    }
    else if(tokens.types[0] == INSTRUCTION) {
        Instruction instruction = parse_instruction(op);

        ret = to_binary(4, instruction);

        // no operand
        if(instruction == INPUT || instruction == OUTPUT || instruction == CLEAR || instruction == HALT) {
            ret = strcat(ret, "000000000000");
            return ret;
        }

        if(arg == NULL) {
            errno = 5;
            errx(2, "Parsing error at line: %d [No argument found for %s]\n", line_nr, tokens.tokens[0]);
        }

        int label_address = get_address_from_label_name(arg);

        char* argument_str = NULL;
        if(label_address >= 0) {
            argument_str = to_binary(12, label_address);
        }
        else {
            errno = 0;
            char *endptr = NULL;
            int argval = strtol(arg, &endptr, 10);
            if (endptr == arg) {
                errno = 5;
                errx(2, "Parsing error at line: %d [Wrong argument: \"%s\"]\n", line_nr, arg);
            }
            argument_str = to_binary(12, argval);
        }

        ret = strcat(ret, argument_str);
    }

    return ret;
}


int main(int argc, char** argv) {

    if(argc != 3) {
        errno = 22;
        err(1, NULL);
    }

    FILE* fptr = fopen(argv[1], "r");

    if(fptr == NULL) {
        errno = 2;
        err(1, "\"%s\" ", argv[1]);
    }

    char* line = NULL;
    size_t line_size = 0;

    int line_tokens_cnt = 1;
    line_tokens* file_contents = (line_tokens*)malloc(line_tokens_cnt * sizeof(Line));

    int k = 0;
    while(getline(&line, &line_size, fptr) != -1) {
        if(k >= line_tokens_cnt) {
            line_tokens_cnt *= 2;
            file_contents = (line_tokens*)realloc(file_contents, line_tokens_cnt * sizeof(line_tokens));
        }
        split_line spl = split(line);
        file_contents[k] = tokenize_line(k, spl);
        k++;
    }

    fclose(fptr);

    fptr = fopen(argv[2], "w");

    for(int i = 0; i < k; i++) {
        char* str = assemble(i + 1, file_contents[i]);
        if(str != NULL) {
            fprintf(fptr, "%s\n", str);
        }
    }

    fclose(fptr);

    return 0;
}
