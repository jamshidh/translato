
%{

#include <string>

extern "C"
{
    int yylex(void);
    void yyerror(const char *);
}

#include "parseGrammar.tab.cc"
%}

%option nounput

%%

. { yylval = strdup(yytext); return yytext[0]; }

\n { yylval = strdup(yytext); return yytext[0]; }

%%

/*void yyerror(char *s) {
  fprintf(stderr, "%s\n", s);
}*/
