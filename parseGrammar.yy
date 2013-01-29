%{

#include <string>

#include <iostream>

using namespace std;

#define YYSTYPE string

   int yylex(void);
   void yyerror(const char *);

  %}

%right LOW

%right 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' 'p' 'q' 'r' 's' 't' 'u' 'v' 'w' 'x' 'y' 'z' 'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' 'O' 'P' 'Q' 'R' 'S' 'T' 'U' 'V' 'W' 'X' 'Y' 'Z' ' ' '\t' '\n' '\r' '<' '>' '(' ')' '\\' '{' '}' '0' '1' '2' '3' '4' '5' '6' '7' '8' '9' 

%left '+'


%%

rulesFile: { cout << "<?xml version='1.0' encoding='utf-8' ?>" << endl << endl << "<rules>" << endl; } rules possibleWhitespace { cout << "</rules>" << endl; };

rules : 
       |
       rules rule ';'
       ;

       rule : possibleWhitespace ident possibleWhitespace '=' '>' { cout << "<rule tag='" << $2 << "'>"; } conversionText { cout << "</rule>" << endl << endl; };

lowercase_letter: 'a' |'b' |'c' |'d' |'e' |'f' |'g' |'h' |'i' |'j' |'k' |'l' |'m' |'n' |'o' |'p' |'q' |'r' |'s' |'t' |'u' |'v' |'w' |'x' |'y' |'z';

uppercase_letter: 'A' |'B' |'C' |'D' |'E' |'F' |'G' |'H' |'I' |'J' |'K' |'L' |'M' |'N' |'O' |'P' |'Q' |'R' |'S' |'T' |'U' |'V' |'W' |'X' |'Y' |'Z';

letter: lowercase_letter | uppercase_letter;

digit: '1'|'2'|'3' |'4' |'5' |'6' |'7' |'8' |'9' |'0' ;

alphanum: letter | digit ;

number: digit | number digit;

whitespaceChar:' '|'\n'|'\r'|'\t';

whitespace: whitespaceChar
          |
          whitespace whitespaceChar;

possibleWhitespace: | whitespace;

blankWhitespace: '<' 'w' 's' '>';

conversionTextStringChar: '\\' ';' { $$ = ";"; } | '\\' '<' { $$ = "&lt;"; } | '\\' '>' { $$ = "&gt;"; } | '/' | '?' | '(' | ')' | '\\' '{' { $$ = "{"; } | '\\' '}' { $$ = "}"; } | letter | digit ;

conversionTextString : conversionTextStringChar | conversionTextString conversionTextStringChar { $$ = $1 + $2; };

ident: letter | ident alphanum { $$ = $1 + $2; };

//nonNullConversionText: conversionTextString { cout << "<text>" << $1 << "</text>"; } | '{' expression '}' { cout << "<expression>" << $2 << "</expression>"; }  | nonNullConversionText '{' expression '}' { cout << "<expression>" << $3 << "</expression>"; }  | nonNullConversionText '{' expression '}' conversionTextString { cout << "<expression>" << $3 << "</expression><text>" << $5 << "</text>"; } ;

//conversionText: | nonNullConversionText;

conversionText: 
  | 
  conversionText conversionTextString %prec LOW 
                   { cout << "<text>" << $2 << "</text>"; } 
  | 
  conversionText whitespace %prec LOW 
                   { cout << "<whitespace>" << $2 << "</whitespace>"; } 
  | 
  conversionText blankWhitespace %prec LOW 
                   { cout << "<whitespace></whitespace>"; } 
  | 
  conversionText attribute
                   { cout << "<attribute>" << $2 << "</attribute>"; }
  | 
  conversionText '{' expression '}' 
                   { cout << "<expression>" << $3 << "</expression>"; }
  ;

attribute: 
         '@' ident { $$ = $2; } 


expression: 
          'l' 'i' 's' 't' '(' expression ')' { $$ = "<list>" + $6 + "</list>"; }
          | 
	  number 
	  |
	  ident { $$ = "<lrule>" + $1 + "</lrule>"; }
	  | 
	  expression '|' expression { $$ = "<or>" + $1 + $3 + "</or>"; };

%%

int main() {
  yyparse();
}

void yyerror(const char *s) {
  cerr << endl << "Error in line: " << yylineno << ": " << s << endl;
  exit(1);
}

