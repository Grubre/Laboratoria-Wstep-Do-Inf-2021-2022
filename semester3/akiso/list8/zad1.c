#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

int numlen(unsigned x) {
    if(x == 0) {
        return 1;
    }
    int len = 0;
    while(x > 0) {
        x /= 10;
        len++;
    }
    return len;
}

char* get_whole_part(unsigned long long number) {
    int curr_size = 16;
    int* digits = (int*)malloc(curr_size * sizeof(int));
    int i = 0;
    while(number > 0) {
        digits[i] = number % 2 + 48;
        number = number / 2;
        i++;
        if( i >= curr_size ) {
            curr_size *= 2;
            digits = (int*)realloc(digits, curr_size * sizeof(int));
        }
    }

    char* num = (char*)malloc((i + 1) * sizeof(char));
    for(int j = 0; j < i; j++) {
        num[j] = digits[i - 1 - j];
    }
    return num;
}

char* get_frac_part(unsigned long long number, const int precision) {
    char* num = (char*)malloc((precision + 1) * sizeof(char));
    const int len = numlen(number);
    const int tenpowlen = pow((unsigned long long)10,len);

    for(int i = 0; i < precision; i++) {
        number = 2 * number;
        if(numlen(number) > len) {
            number = number % tenpowlen;
            num[i] = '1';
        }
        else {
            num[i] = '0';
        }
    }

    return num;
}

int main(int argc, char** argv) {
    if(argc < 3)
        return 1;
    char * input_num_str = strtok(argv[1], ".");
    unsigned long long whole = atoi(input_num_str);
    unsigned long long frac = atoi(strtok(NULL, "."));
    int precision = atoi(argv[2]);
    //
    char* wholepart = get_whole_part(whole);
    char* numpart = get_frac_part(frac, precision);
    //
    char* output = strcat(wholepart, ".");
    output = strcat(output, numpart);
    //
    printf("%s\n", output);
    //     
    // float input = atof()
    return 0;
}
