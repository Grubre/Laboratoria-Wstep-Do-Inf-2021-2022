#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "assembler.h"

typedef struct Label
{
    char* name;
    unsigned int mem_loc;
} Label;

Label* labels = NULL;
int labels_size = 0;

void push_label(char* name, unsigned int mem_loc) {
    labels_size++;
    labels = (Label*)realloc(labels, labels_size * sizeof(Label));
    Label label;
    label.name = strdup(name);
    label.mem_loc = mem_loc;
    labels[labels_size - 1] = label;
}

int get_address_from_label_name(const char* label_name) {
    for(int i = 0; i < labels_size; i++) {
        if(!strcmp(labels[i].name, label_name)) {
            return labels[i].mem_loc;
        }
    }
    return -1;
}

