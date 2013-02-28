%{

#include <string>

#include <iostream>

using namespace std;

#define YYSTYPE string

   int yylex(void);
   void yyerror(const char *);

  %}



%error-verbose

%define parse.lac full


%right LOW 

%right 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' 'p' 'q' 'r' 's' 't' 'u' 'v' 'w' 'x' 'y' 'z' 'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' 'O' 'P' 'Q' 'R' 'S' 'T' 'U' 'V' 'W' 'X' 'Y' 'Z' ' ' '\t' '\n' '\r' '<' '>' '(' ')' '\\' '{' '}' '0' '1' '2' '3' '4' '5' '6' '7' '8' '9' '?' '=' ',' '|' '/'

%left '+'


%%

rulesFile: { cout << "<?xml version='1.0' encoding='utf-8' ?>" << endl << endl << "<rules>" << endl; } rules possibleWhitespace { cout << "</rules>" << endl; };

rules : 
       |
       rules elementRule ';'
       |
       rules assignment ';'
       |
       rules operatorDefinition ';'
       ;

elementRule: possibleWhitespace ident possibleWhitespace '=' '>' { cout << "<rule tag='" << $2 << "'>"; } conversionText { cout << "</rule>" << endl << endl; };

assignment: possibleWhitespace ident possibleWhitespace '=' { cout << "<assignment tag='" << $2 << "'>"; } conversionText { cout << "</assignment>" << endl << endl; };

operatorDefinition: possibleWhitespace ident ':' 'o' 'p' 'e' 'r' 'a' 't' 'o' 'r' 's' possibleWhitespace '=' '>' { cout << "<operatorDefinition tag='" << $2 << "'>"; } possibleWhitespace operatorList[operatorList] possibleWhitespace { cout << $operatorList << "</operatorDefinition>" << endl << endl; };

operatorChar: '+' | '-' | '*' | '/';

L_operatorChar_L: operatorChar | L_operatorChar_L operatorChar { $$ = $1 + $2; };

operator: '\'' L_operatorChar_L '\'' { $$ = "<operator>" + $2 + "</operator>"; }

operatorList: operator | operatorList whitespace operator { $$ = $1 + $3; };

lowercase_letter: 'a' |'b' |'c' |'d' |'e' |'f' |'g' |'h' |'i' |'j' |'k' |'l' |'m' |'n' |'o' |'p' |'q' |'r' |'s' |'t' |'u' |'v' |'w' |'x' |'y' |'z';

uppercase_letter: 'A' |'B' |'C' |'D' |'E' |'F' |'G' |'H' |'I' |'J' |'K' |'L' |'M' |'N' |'O' |'P' |'Q' |'R' |'S' |'T' |'U' |'V' |'W' |'X' |'Y' |'Z';

letter: lowercase_letter | uppercase_letter;

digit: '1'|'2'|'3' |'4' |'5' |'6' |'7' |'8' |'9' |'0' ;

alphanum: letter | digit ;

number: digit | number digit;

whitespaceChar:' '|'\n'|'\r'|'\t';

whitespace: whitespaceChar
          |
          whitespace whitespaceChar { $$ = $1 + $2; };

possibleWhitespace: | whitespace;

blankWhitespace: '<' 'w' 's' '>';

conversionTextStringChar: '\\' ';' { $$ = ";"; } 
			  | 
			  '\\' '<' { $$ = "&lt;"; } 
			  | 
			  '\\' '>' { $$ = "&gt;"; } 
			  | 
			  '\\' '{' { $$ = "{"; } 
			  | 
			  '\\' '}' { $$ = "}"; } 
			  | 
			  ':' | '+' | '-' | '*' | '^' | '#' | '.' | '"' | ',' | '=' | '/' | '?' | '(' | ')' 
			  | 
			  letter | digit ;

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
  conversionText binding
  ;

binding: attribute condition 
            { cout << "<attribute name='" << $1 << "'>" << $2 << "</attribute>"; }
       | 
       attribute 
            { cout << "<attribute name='" << $1 << "'></attribute>"; }
       | 
       element condition 
            { cout << "<element name='" << $1 << "'>" << $2 << "</element>"; }
       | 
       condition
            { cout << "<element>" << $1 << "</element>"; }
       ;

element: ident;

condition: '{' expression '}' { $$ = $2; };

textOrWhitespace:
  { $$ = ""; }
  |
  textOrWhitespace conversionTextString %prec LOW 
                   { $$ = $1 + "<text>" + $2 + "</text>"; } 
  | 
  textOrWhitespace whitespace %prec LOW 
                   { $$ = $1 + "<whitespace>" + $2 + "</whitespace>"; } 
  |
  textOrWhitespace blankWhitespace %prec LOW 
                   { $$ = $1 + "<whitespace></whitespace>"; } 
  ;

attribute: 
         '@' ident %prec LOW { $$ = $2; } 


expression: 
          'l' 'i' 's' 't' '(' possibleWhitespace expression[exp] possibleWhitespace ',' possibleWhitespace '\'' textOrWhitespace[separator] '\'' possibleWhitespace  ')' { $$ = "<list><expression>" + $exp + "</expression><separator>" + $separator + "</separator></list>"; }
          | 
          'l' 'i' 's' 't' '(' possibleWhitespace expression possibleWhitespace ')' { yyerror("You need to supply two arguments to the 'list' function, the expression and the separator."); }
	  |
          'a' 'n' 'y' 'C' 'h' 'a' 'r' 'B' 'u' 't' '(' possibleWhitespace '\'' textOrWhitespace[char] '\'' possibleWhitespace ')' { $$ = "<anyCharBut><expression>" + $char + "</expression></anyCharBut>"; }
          | 
	  number 
	  |
	  ident { $$ = "<link>" + $1 + "</link>"; }
	  | 
	  expression '|' expression { $$ = "<or>" + $1 + $3 + "</or>"; };

%%

int main() {
  yyparse();
}

void yyerror(const char *s) {
  cerr << endl << endl << ">>>> Error in line: " << yylineno << ": " << s << endl << endl;
  exit(1);
}

