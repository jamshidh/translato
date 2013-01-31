
%option nounput

%%

\<text\>[^\<]*\<\/text> { 
  bool inEscapeCode = false;
  for(char *p = yytext + 6; *p != '<'; p++) {
    if (!inEscapeCode) 
      printf("<text>"); 
    printf("%c", *p); 
    if (*p == ';') inEscapeCode = false;
    if (*p == '&') inEscapeCode = true;
    if (!inEscapeCode) printf("</text>"); 
  }
}

%%

int main() {

  yylex();

  return 0;

}

