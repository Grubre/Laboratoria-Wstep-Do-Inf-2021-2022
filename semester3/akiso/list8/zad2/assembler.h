#pragma once

#include <stddef.h>
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

const int INSTRUCTIONS_NUM = STOREI + 1;

typedef enum token_type {
    COMMENT,
    LABEL,
    INSTRUCTION,
    ARGUMENT,
    MEMORY,
    NONE
} token_type;

typedef enum number_system {
    HEX,
    DEC
} number_system;

const int SYSTEMS_NUM = DEC + 1;

typedef struct Token {
    token_type type;
    size_t val;
} Token;

const char* encode_instruction(Instruction instr, int parsed_arg);

typedef struct Line {
    Instruction instruction;
    const char* arg;
} Line;
