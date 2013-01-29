<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text" />

  <xsl:template match="text">'<xsl:value-of select="text()" />' </xsl:template>

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
  </xsl:template>

  <xsl:template match="attribute"> ident </xsl:template>

  <xsl:template mode="attribute" match="attribute"><xsl:text> </xsl:text><xsl:value-of select="text()" />='" &lt;&lt; $<xsl:value-of select="position()" /> &lt;&lt; "'</xsl:template>

  <xsl:template mode="attribute" match="*"></xsl:template>

  <xsl:template match="whitespace"> possibleWhitespace </xsl:template>

  <xsl:template mode="ruleName" match="or">
    <xsl:for-each select="*">
      <xsl:if test="position() &gt; 1">-or-</xsl:if>
      <xsl:apply-templates select="." />
    </xsl:for-each>
  </xsl:template>

  <xsl:template mode="ruleName" match="list">
    <xsl:text>L_</xsl:text>
    <xsl:apply-templates select="*" />
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
    <xsl:text>: | </xsl:text>
    <xsl:apply-templates select="." mode="ruleName" />
    <xsl:text> </xsl:text>
    <xsl:for-each select="*">
      <xsl:if test="position() &gt; 1"> | </xsl:if>
      <xsl:apply-templates select="." />
    </xsl:for-each> { $$ = $1 + $2; }
    ;

  </xsl:template>

  <xsl:template match="rule">
    
    <xsl:value-of select="@tag" />: <xsl:apply-templates select="*" /> { cout &lt;&lt; "&lt;<xsl:value-of select="@tag" /> <xsl:apply-templates select="*" mode="attribute" />&gt;&lt;/<xsl:value-of select="@tag" />&gt;"; } ;

  </xsl:template>

  <!-- Ugh! We can't have attributes in the first tag and streaming output.
       I will choose to keep streaming output for now, but may change my mind 
       later. -->
  <xsl:template match="rule[1]">
    
    <xsl:value-of select="@tag" />: 
      { cout &lt;&lt; "&lt;?xml version='1.0' encoding='utf-8'?&gt;" &lt;&lt; endl &lt;&lt; "&lt;<xsl:value-of select="@tag" />&gt;"; } 
      <xsl:apply-templates select="*" /> { cout &lt;&lt; "&lt;/<xsl:value-of select="@tag" />&gt;"; } ;

  </xsl:template>

  <xsl:template match="rules">
%{
 #include &lt;string&gt;
 #include &lt;iostream&gt;

 using namespace std;

 #define YYSTYPE string

 int yylex(void);
 void yyerror(const char *);

%}

%token CHAR

%left '+'

%%

<xsl:apply-templates select="rule" />

<xsl:apply-templates select="//or" mode="orRules" />

<xsl:apply-templates select="//list" mode="listRules" />


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

ident: letter | ident alphanum { $$ = $1 + $2; };

%%

int main() {
  yyparse();
}

void yyerror(const char *s) {
  fprintf(stderr, "%s\n", s);
}

  </xsl:template>

</xsl:stylesheet>
