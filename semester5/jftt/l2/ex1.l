%{
#include<stdio.h>

int num_lines = 0;
int num_words = 0; 

int yylex(); 
%}

%%
^[[:blank:]]*\n	 { };
[[:blank:]]+$    { };
^[[:blank:]]+    { };
[[:blank:]]{1,}  { printf(" "); num_words++; }
\n               { printf("\n"); num_lines++; num_words++; }
%%

int main() {
	yylex();

	fprintf(stderr, "counted lines: %d\ncounted words: %d\n", num_lines, num_words);
	return 0;
}
