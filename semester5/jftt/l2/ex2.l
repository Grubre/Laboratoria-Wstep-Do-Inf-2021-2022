%{
#include <stdio.h>

int yylex(); 
%}

%s BASE
%s SINGLE_QUOTE
%s DOUBLE_QUOTE
%s CDATA

%%
    BEGIN(BASE);
<BASE>\'           					    { ECHO; BEGIN(SINGLE_QUOTE); }
<BASE>\"           					    { ECHO; BEGIN(DOUBLE_QUOTE); }
<BASE>\<!\[CDATA\[					    { ECHO; BEGIN(CDATA); }

<SINGLE_QUOTE>\'					    { ECHO; BEGIN(BASE); }
<DOUBLE_QUOTE>\"					    { ECHO; BEGIN(BASE); }
<CDATA>\]\]\>							{ ECHO; BEGIN(BASE); }

<BASE>\.								{ ECHO; }
<BASE><!--((-?[^-])*)-?-->  		    { }
%%

int main() {
	yylex();

	return 0;
}
