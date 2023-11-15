%{
#include <stdio.h>
#include <stdbool.h>

bool keep_docs = false;

int yylex(); 
%} 

ESCAPE      (\\\n.*)*

%s STRING
%s DOCS
%s MULTILINE
%s INCLUDE

%%
<INITIAL>#include[[:blank:]]*                                                                                   { ECHO; BEGIN(INCLUDE); }
<INCLUDE>(\/\*).*(\*\/)                                                                                         { }
<INCLUDE>>                                                                                                      { ECHO; BEGIN(INITIAL); }

<INITIAL>#include[[:blank:]]*\"                                                                                 { ECHO; BEGIN(STRING); }

<INITIAL>\"                                                                                                     { ECHO; BEGIN(STRING); }
<STRING>\\\\\"                                                                                                  {ECHO; BEGIN(INITIAL);}
<STRING>[^\\](\\\\)*\"                                                                                       { ECHO; BEGIN(INITIAL); }

<INITIAL>(\/{ESCAPE}\*{ESCAPE}\*)|(\/{ESCAPE}\*{ESCAPE}!)				                                        { if(keep_docs) { ECHO;} BEGIN(DOCS); };
<DOCS>{ESCAPE}\*{ESCAPE}\/						                                                                { if(keep_docs) { ECHO;} BEGIN(INITIAL); };
<DOCS>.							                                                                                { if(keep_docs) { ECHO;} };

<INITIAL>\/{ESCAPE}\*							                                                                { BEGIN(MULTILINE); };
<MULTILINE>{ESCAPE}\*{ESCAPE}\/				                                                                    { BEGIN(INITIAL); };
<MULTILINE>.                                                                                                    { }

<INITIAL>((\/{ESCAPE}\/{ESCAPE}\/)|(\/{ESCAPE}\/{ESCAPE}!)).*{ESCAPE}.*$                                        { if(keep_docs) { ECHO;} }
<INITIAL>\/{ESCAPE}\/.*{ESCAPE}.*$				                                                                { };
%%

int main(int argc, char** argv) {
    if (argc > 1 && strcmp(argv[1], "-docs") == 0) {
        keep_docs = true;
    }
    
	yylex();

	return 0;
}
