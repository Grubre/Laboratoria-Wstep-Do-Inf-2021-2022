%{
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <math.h>

#define INITIAL_STACK_SIZE 2
#define GROW_FACTOR 2

int yylex(); 

int* stack;
int stack_pointer = 0;
int stack_capacity = 0;

bool error_occured = false;

void push(int value);
int pop();
bool empty();
void reset();
int size();
void print_stack();

void err(const char* message);

void m_add();
void m_sub();
void m_mult();
void m_div();
void m_mod();
void m_pow();

%}

%%

\-?[[:digit:]]+ { if(!error_occured) push(atoi(yytext)); }
\+							{ if(!error_occured) m_add(); }
\-							{ if(!error_occured) m_sub(); }
\*							{ if(!error_occured) m_mult(); }
\/							{ if(!error_occured) m_div(); }
%							{ if(!error_occured) m_mod(); }
\^							{ if(!error_occured) m_pow(); }
\n { if(!error_occured) print_stack(); reset(); }
[[:blank:]]*                { }
. {
    fprintf(stderr, "Error: Unknown token: \"%s\".\n", yytext);
    error_occured = true;
}
%%

void err(const char* message) {
    fprintf(stderr, "Error: %s", message);
    error_occured = true;
}

void reset() {
    error_occured = false;
    stack_capacity = 0;
    stack_pointer = 0;
    free(stack);
    stack = NULL;
}

void print_stack() {
    if(size() <= 0) {
        return;
    }
    printf("= ");
    while(!empty()) {
        printf("%d ", pop());
    }
    printf("\n");
}

void grow() {
    if (stack_capacity == 0) {
        stack_capacity = INITIAL_STACK_SIZE;
    } else {
        stack_capacity *= GROW_FACTOR;
    }
    stack = (int *)realloc(stack, stack_capacity * sizeof(int));
}

void push(int value) {
    if (stack_pointer >= stack_capacity) {
        grow();
    }

    stack[stack_pointer] = value;
    stack_pointer++;
}

int pop() {
    stack_pointer--;
    if(stack_pointer < 0) 
        err("Tried popping from empty stack.\n");

    return stack[stack_pointer];
}

bool empty() {
    return stack_pointer <= 0;
}

int size() {
    return stack_pointer;
}

void m_add() {
    if (size() < 2) {
        err("Not enough numbers to add.\n");
        return;
    }
    int a = pop();
    int b = pop();
    push(b + a);
}

void m_sub() {
    if (size() < 2) {
        err("Not enough numbers to subtract.\n");
        return;
    }
    int a = pop();
    int b = pop();
    push(b - a);
}

void m_mult() {
    if (size() < 2) {
        err("Not enough numbers to multiply.\n");
        return;
    }
    int a = pop();
    int b = pop();
    push(b * a);
}

void m_div() {
    if (size() < 2) {
        err("Not enough numbers to divide.\n");
        return;
    }
    int a = pop();
    int b = pop();

    if (a == 0) {
        err("Can't divide by 0.\n");
        return;
    }
    push(b / a);
}

void m_mod() {
    if (size() < 2) {
        err("Not enough numbers to divide.\n");
        return;
    }

    int a = pop();
    int b = pop();

    if (a == 0) {
        err("Can't do mod 0.\n");
        return;
    }
    push(b % a);
}

void m_pow() {
    if (size() < 2) {
        err("Not enough numbers to divide.\n");
        return;
    }
    int a = pop();
    int b = pop();
    push(pow(b, a));
}




int main() {
    stack = (int*)malloc(sizeof(int) * INITIAL_STACK_SIZE);
    stack_capacity = INITIAL_STACK_SIZE;


	yylex();

	return 0;
}
