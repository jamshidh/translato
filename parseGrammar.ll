
%{

#include <string>

extern "C"
{
    int yylex(void);
    void yyerror(const char *);
}

#include "parseGrammar.tab.cc"
%}

%%

a { yylval = "a"; return 'a'; }

b { yylval = yytext; return 'b'; }

c { yylval = yytext; return 'c'; }

d { yylval = yytext; return 'd'; }

e { yylval = yytext; return 'e'; }

f { yylval = yytext; return 'f'; }

g { yylval = yytext; return 'g'; }

h { yylval = yytext; return 'h'; }

i { yylval = yytext; return 'i'; }

j { yylval = yytext; return 'j'; }

k { yylval = yytext; return 'k'; }

l { yylval = yytext; return 'l'; }

m { yylval = yytext; return 'm'; }

n { yylval = yytext; return 'n'; }

o { yylval = yytext; return 'o'; }

p { yylval = yytext; return 'p'; }

q { yylval = yytext; return 'q'; }

r { yylval = yytext; return 'r'; }

s { yylval = yytext; return 's'; }

t { yylval = yytext; return 't'; }

u { yylval = yytext; return 'u'; }

v { yylval = yytext; return 'v'; }

w { yylval = yytext; return 'w'; }

x { yylval = yytext; return 'x'; }

y { yylval = yytext; return 'y'; }

z { yylval = yytext; return 'z'; }

A { yylval = yytext; return 'A'; }

B { yylval = yytext; return 'B'; }

C { yylval = yytext; return 'C'; }

D { yylval = yytext; return 'D'; }

E { yylval = yytext; return 'E'; }

F { yylval = yytext; return 'F'; }

G { yylval = yytext; return 'G'; }

H { yylval = yytext; return 'H'; }

I { yylval = yytext; return 'I'; }

J { yylval = yytext; return 'J'; }

K { yylval = yytext; return 'K'; }

L { yylval = yytext; return 'L'; }

M { yylval = yytext; return 'M'; }

N { yylval = yytext; return 'N'; }

O { yylval = yytext; return 'O'; }

P { yylval = yytext; return 'P'; }

Q { yylval = yytext; return 'Q'; }

R { yylval = yytext; return 'R'; }

S { yylval = yytext; return 'S'; }

T { yylval = yytext; return 'T'; }

U { yylval = yytext; return 'U'; }

V { yylval = yytext; return 'V'; }

W { yylval = yytext; return 'W'; }

X { yylval = yytext; return 'X'; }

Y { yylval = yytext; return 'Y'; }

Z { yylval = yytext; return 'Z'; }

1 { yylval = yytext; return '1'; }

2 { yylval = yytext; return '2'; }

3 { yylval = yytext; return '3'; }

4 { yylval = yytext; return '4'; }

5 { yylval = yytext; return '5'; }

6 { yylval = yytext; return '6'; }

7 { yylval = yytext; return '7'; }

8 { yylval = yytext; return '8'; }

9 { yylval = yytext; return '9'; }

! { yylval = yytext; return '!'; }

@ { yylval = yytext; return '@'; }

\# { yylval = yytext; return '#'; }

\$ { yylval = yytext; return '$'; }

% { yylval = yytext; return '%'; }

\^ { yylval = yytext; return '^'; }

& { yylval = yytext; return '&'; }

\* { yylval = yytext; return '*'; }

\( { yylval = yytext; return '('; }

\) { yylval = yytext; return ')'; }

- { yylval = yytext; return '-'; }

= { yylval = yytext; return '='; }

_ { yylval = yytext; return '_'; }

\+ { yylval = yytext; return '+'; }

\[ { yylval = yytext; return '['; }

\] { yylval = yytext; return ']'; }

\{ { yylval = yytext; return '{'; }

\} { yylval = yytext; return '}'; }

\\ { yylval = yytext; return '\\'; }

\| { yylval = yytext; return '|'; }

\; { yylval = yytext; return ';'; }

' { yylval = yytext; return '\''; }

: { yylval = yytext; return ':'; }

\" { yylval = yytext; return '"'; }

\, { yylval = yytext; return ','; }

\. { yylval = yytext; return '.'; }

\/ { yylval = yytext; return '/'; }

\< { yylval = yytext; return '<'; }

\> { yylval = yytext; return '>'; }

\? { yylval = yytext; return '?'; }

` { yylval = yytext; return '`'; }

~ { yylval = yytext; return '~'; }

[ ] { yylval = yytext; return ' '; }

\n { yylval = yytext; return '\n'; }

\r { yylval = yytext; return '\r'; }

\t { yylval = yytext; return '\t'; }


%%

/*void yyerror(char *s) {
  fprintf(stderr, "%s\n", s);
}*/
