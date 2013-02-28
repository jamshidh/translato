<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text" />

  <xsl:template match="text" mode="output"> addTextToOutputStream("<xsl:value-of select="text()" />"); </xsl:template>

  <xsl:template match="attribute" mode="output"> addAttributeToOutputStream("<xsl:value-of select="text()" />"); </xsl:template>

  <xsl:template match="*" mode="output" />






  <xsl:template match="text">'<xsl:value-of select="text()" />' </xsl:template>

  <xsl:template match="text" mode="separator">'<xsl:value-of select="text()" />' </xsl:template>

  <xsl:template match="expression"> <xsl:apply-templates select="*" /></xsl:template>

  <xsl:template match="link">
    <xsl:value-of select="text()" />
  </xsl:template>

  <xsl:template match="list">
    <xsl:apply-templates select="*" />
  </xsl:template>

  <xsl:template match="or">
    <xsl:apply-templates select="." mode="ruleName" />
  </xsl:template>

  <xsl:template match="list">
    <xsl:apply-templates select="." mode="ruleName" />
    <xsl:text>-or-NULL</xsl:text>
  </xsl:template>

  <xsl:template match="attribute"> 
    <xsl:choose>
      <xsl:when test="link/text() != ''">
	<xsl:value-of select="link/text()" />
      </xsl:when>
      <xsl:otherwise>ident</xsl:otherwise>
  </xsl:choose>[<xsl:value-of select="@name" /><xsl:value-of select="position()" />] { setVar("<xsl:value-of select="@name" />", $<xsl:value-of select="@name" /><xsl:value-of select="position()" />, $context); } </xsl:template>


  <xsl:template mode="attribute" match="*"></xsl:template>

  <xsl:template match="whitespace"> 
    <xsl:choose>
      <xsl:when test="(name(./following-sibling::*[1]) = 'attribute') and (name(./preceding-sibling::*[1]) = 'attribute')"> whitespace </xsl:when>
      <xsl:otherwise> possibleWhitespace </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="whitespace" mode="separator"> 
    <xsl:choose>
      <xsl:when test="(name(./following-sibling::*[1]) = 'attribute') and (name(./preceding-sibling::*[1]) = 'attribute')"> whitespace </xsl:when>
      <xsl:otherwise> possibleWhitespace </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="whitespace[count(./following-sibling::*) = 0 and count(../preceding-sibling::*) > 0]"> 
  </xsl:template>

  <xsl:template match="whitespace[count(./preceding-sibling::*) = 0 and count(../preceding-sibling::*) > 0]">
  </xsl:template>

  <xsl:template mode="ruleName" match="or">
    <xsl:for-each select="*">
      <xsl:if test="position() &gt; 1">-or-</xsl:if>
      <xsl:apply-templates select="." />
    </xsl:for-each>
  </xsl:template>

  <xsl:template mode="ruleName" match="list">
    <xsl:text>L_</xsl:text>
    <xsl:apply-templates select="expression" />
    <xsl:text>_L</xsl:text>
  </xsl:template>

  <xsl:template match="or" mode="orRules">
    <xsl:apply-templates select="." mode="ruleName" />
    <xsl:text>: </xsl:text>
    <xsl:for-each select="*">
      <xsl:if test="position() &gt; 1"> | </xsl:if>
      <xsl:apply-templates select="." />
    </xsl:for-each>
    ;

  </xsl:template>

  <xsl:template match="list" mode="listRules">
    <xsl:apply-templates select="." mode="ruleName" />
    <xsl:text>: </xsl:text>
    <xsl:for-each select="expression">
      <xsl:if test="position() &gt; 1"> | </xsl:if>
      <xsl:apply-templates select="." />
    </xsl:for-each>
    <xsl:text> | </xsl:text>
    <xsl:apply-templates select="." mode="ruleName" />
    <xsl:text> </xsl:text>
    <xsl:apply-templates select="separator" mode="separator"/>
    <xsl:text> </xsl:text>
    <xsl:for-each select="expression">
      <xsl:if test="position() &gt; 1"> | </xsl:if>
      <xsl:apply-templates select="." />
    </xsl:for-each>
    ;

    <xsl:apply-templates select="." mode="ruleName" />
    <xsl:text>-or-NULL: | </xsl:text>    
    <xsl:apply-templates select="." mode="ruleName" /> 
    ;

  </xsl:template>

  <xsl:template match="rule">
    <xsl:value-of select="@tag" />
    <xsl:text>: { $context = getNewContext(); bcout &lt;&lt; "&lt;</xsl:text>
    <xsl:value-of select="@tag" />
    <xsl:for-each select=".//attribute">
      <xsl:text> </xsl:text>
      <xsl:value-of select="@name" />='" &lt;&lt; var("<xsl:value-of select="@name" />
      <xsl:text>", $context) &lt;&lt; "'</xsl:text>
    </xsl:for-each>
    <xsl:text>&gt;"; }[context] </xsl:text>

    <xsl:apply-templates select="*" /> 

    <xsl:text> { bcout &lt;&lt; "&lt;/</xsl:text>
    <xsl:value-of select="@tag" />
    <xsl:text>&gt;"; } </xsl:text>
    ;

  </xsl:template>

  <xsl:template match="rules">
%{
 //#include &lt;iostream&gt;

 #include "tokenStreamer.h"

 using namespace std;

#define YYSTYPE char *

#define YYMAXDEPTH    100000
#define YYINITDEPTH   100000

 int yylex(void);
 void yyerror(const char *);

 char *concatAndFreeOriginal(char *, char *);

 BufferedOutput bcout;

%}


%glr-parser

%error-verbose

%right LOW

%right 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' 'p' 'q' 'r' 's' 't' 'u' 'v' 'w' 'x' 'y' 'z' 'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' 'O' 'P' 'Q' 'R' 'S' 'T' 'U' 'V' 'W' 'X' 'Y' 'Z' '1''2''3' '4' '5' '6' '7' '8' '9' '0' ' ' '\t' '\n' '\r' '&lt;' '&gt;' '(' ')' '\\' '{' '}' '=' ',' '|' '/'



%%

<xsl:apply-templates select="rule" />

<xsl:apply-templates select="//or" mode="orRules" />

<xsl:apply-templates select="//list" mode="listRules" />


lowercase_letter: 'a' |'b' |'c' |'d' |'e' |'f' |'g' |'h' |'i' |'j' |'k' |'l' |'m' |'n' |'o' |'p' |'q' |'r' |'s' |'t' |'u' |'v' |'w' |'x' |'y' |'z';

uppercase_letter: 'A' |'B' |'C' |'D' |'E' |'F' |'G' |'H' |'I' |'J' |'K' |'L' |'M' |'N' |'O' |'P' |'Q' |'R' |'S' |'T' |'U' |'V' |'W' |'X' |'Y' |'Z';

letter: lowercase_letter | uppercase_letter;

digit: '1'|'2'|'3' |'4' |'5' |'6' |'7' |'8' |'9' |'0' ;

alphanum: letter | digit ;
<xsl:if test="count(//link[text()='number'])&gt;0">
number: integer 
      | 
      integer '.' integer { $$ = concatAndFreeOriginal(concatAndFreeOriginal($1, $2), $3); };
      | 
      '.' integer { $$ = concatAndFreeOriginal($1, $2); };
</xsl:if>

<xsl:if test="count(//link[text()='integer'])&gt;0">
integer: digit | integer digit { $$ = concatAndFreeOriginal($1, $2); };
</xsl:if>

whitespaceChar:' '|'\n'|'\r'|'\t';

whitespace: whitespaceChar
          |
          whitespace whitespaceChar;

possibleWhitespace: %prec LOW | whitespace %prec LOW;

ident: letter | ident alphanum { $$ = concatAndFreeOriginal($1, $2); };

chars : letter | digit | '.' | '-' ;

eIdent: chars | eIdent chars { $$ = concatAndFreeOriginal($1,  $2); };

%%

int main() {
  yyparse();
}

char *concatAndFreeOriginal(char *s1, char *s2) {
  char * ret = (char *)malloc(strlen(s1) + strlen(s2) + 1);
  strcpy(ret, s1);
  strcpy(ret+strlen(s1), s2);
  free(s1);
  free(s2);
  return ret;
}

void yyerror(const char *s) {
  fprintf(stderr, "\n\n&gt;&gt;&gt;&gt; Error in line: %d: %s\n\n", yylineno, s);
  exit(1);
}

  </xsl:template>

</xsl:stylesheet>
