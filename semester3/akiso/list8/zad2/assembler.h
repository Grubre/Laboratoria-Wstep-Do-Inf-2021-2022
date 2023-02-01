#ifndef ASSEMBLER
#define ASSEMBLER
#include <stddef.h>

#define BITS 16


void push_label(char* name, unsigned int mem_loc);
int get_address_from_label_name(const char* label_name);


typedef enum Instruction {
    JNS,
    LOAD,
    STORE,
    ADD,
    SUBT,
    INPUT,
    OUTPUT,
    HALT,
    SKIPCOND,
    JUMP,
    CLEAR,
    ADDI,
    JUMPI,
    LOADI,
    STOREI
} Instruction;


#define INSTRUCTIONS_NUM STOREI+1


typedef enum token_type {
    COMMENT,
    INSTRUCTION,
    ARGUMENT,
    MEMORY,
    NONE
} token_type;


typedef struct split_line {
    int num_of_tokens;
    char* strs[3];
} split_line;


#define TOKENSPERLINE 2
typedef struct line_tokens {
    token_type types[TOKENSPERLINE];
    char* tokens[TOKENSPERLINE];
} line_tokens;


typedef enum number_system {
    HEX,
    DEC
} number_system;


#define SYSTEMS_NUM DEC+1
#endif
