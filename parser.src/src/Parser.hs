{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Parser (
    createParser
) where

import Data.Maybe
import Data.Text
import Text.XML.Light as L
import Text.XML
import Text.XML.Cursor
import Data.List

import Quoter(here)

{-  <xsl:template match="text" mode="output"> addTextToOutputStream("<xsl:value-of select="text()" />"); </xsl:template>

  <xsl:template match="attribute" mode="output"> addAttributeToOutputStream("<xsl:value-of select="text()" />"); </xsl:template>

  <xsl:template match="*" mode="output" />

  <xsl:template match="whitespace">
    <xsl:choose>
      <xsl:when test="(name(./following-sibling::*[1]) = 'attribute') and (name(./preceding-sibling::*[1]) = 'attribute')"> whitespace </xsl:when>
      <xsl:otherwise> possibleWhitespace </xsl:otherwise>
    </xsl:choose>
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

  </xsl:template> -}


--Splits strings into chars, suitable for Bison input
splito::[Char]->[Char]
splito x = Data.List.intercalate " " (Data.List.map escapo x)

--Like splito, but or's the chars
oreo::[Char]->[Char]
oreo x = Data.List.intercalate " | " (Data.List.map escapo x)

--Escapes chars for splito and oreo
escapo::Char->String
escapo '\''= "'\\''"
escapo '\\' = "'\\\\' "
escapo x = "'" ++ [x] ++ "' "

isTextNode::Text.XML.Node->Bool
isTextNode (NodeContent x) = True
isTextNode x = False

cursorIsAtTextNode::Cursor->Bool
cursorIsAtTextNode c = isTextNode $ Text.XML.Cursor.node c

getElement::Text.XML.Node->Text.XML.Element
getElement (Text.XML.NodeElement x) = x

tagName::Cursor->String
tagName cursor = unpack (nameLocalName (elementName (getElement $ Text.XML.Cursor.node cursor)))

rules::Cursor->String
rules c | cursorIsAtTextNode c = ""
rules c | tagName c == "text" = splito (contentString c)

rules c | tagName c == "attribute" = (if ((Data.List.length $ child c) > 0) then
        (getRuleName $ Data.List.head (child c)) else "ident")
    ++ "[" ++ getAttribute c "name" ++ (show $ position c) ++ "]"
    ++ "{ setVar(\"" ++ getAttribute c "name" ++"\", escape($" ++ getAttribute c "name" ++ (show $ position c) ++ "), $context); }"

--The whitespace rule is complex....  We ignore whitespace surrounding an item, unless it is the first rule
rules c | tagName c == "whitespace" &&
    (
        (((Data.List.length $ precedingSibling c) > 0) && ((Data.List.length $ followingSibling c) > 0))
        ||
        (Data.List.length (parent c >>= precedingSibling >>= element (Name (pack "rule") Nothing Nothing)) == 0)
    )
        = " possibleWhitespace "
rules c | tagName c == "whitespace" = ""
rules c | tagName c == "element" = applyTemplates rules (child c)
rules c | tagName c == "or" = getRuleName c
rules c | tagName c == "list" = getRuleName c ++ "-or-NULL"
rules c | tagName c == "link" = contentString c
rules c | tagName c == "rules" = applyTemplates rules (child c)

rules c | tagName c == "rule" = "\n\n" ++ getAttribute c "tag" ++
    ": { $context = getNewContext(); bcout << \"<" ++ getAttribute c "tag"
    ++ Data.List.intercalate " " (Data.List.map att2AttOut (nubBy namesAreEqual (descendant c >>= element (Name (pack "attribute") Nothing Nothing))))
    ++ ">\\n\";}[context] "
    ++ applyTemplates rules (child c)
    ++ " { bcout << \"</" ++ getAttribute c "tag" ++ ">\\n\"; }"
    ++ "\n    ;"

rules c | tagName c == "assignment" = "\n\n" ++ getAttribute c "tag" ++ ": "
    ++ applyTemplates rules (child c) ++ "\n    ;"

rules c | tagName c == "operatorDefinition" = "\n\n"
    ++ Data.List.intercalate "\n\n" (Data.List.map (operatorRules (getAttribute c "tag")) (child c)) ++ "\n    ;"

rules c | tagName c == "anyCharBut" = charsetName c
rules c = error ("--Unknown element in rules: [" ++ tagName c ++ "]\n")


namesAreEqual::Cursor->Cursor->Bool
namesAreEqual a b = getAttribute a "name" == getAttribute b "name"

att2AttOut::Cursor->String
att2AttOut c = " " ++ n ++ "='\" << var(\"" ++ n  ++ "\", $context) << \"'"
    where n=unpack $ Data.Text.concat (attribute (Name (pack "name") Nothing Nothing) c)


op2Name::String->String
op2Name "+" = "plus"
op2Name "-" = "minus"
op2Name "*" = "times"
op2Name "/" = "divide"
op2Name x = error ("Unknown operator in op2Name: \'" ++ x ++ "'")

operatorRules::String->Cursor->String
--operatorRules tag c = tag ++ ": {  bcout << \"<" ++ (op2Name $ contentString c) ++ ">\"; } "
operatorRules tag c = tag ++ ": "
    ++ tag ++ " " ++ (splito $ contentString c) ++ " " ++ tag
    ++ " {  bcout << \"</" ++ (op2Name $ contentString c) ++ ">\"; } "
    ++ ";"




cursor2NameCursorPair::Cursor->(String, Cursor)
cursor2NameCursorPair c = (getRuleName c, c)

pairEqual::(String, a)->(String, a)->Bool
pairEqual a b = fst a == fst b

getUniqueListPatterns::Cursor->[(String, Cursor)]
getUniqueListPatterns c = nubBy pairEqual (Data.List.map cursor2NameCursorPair lists)
    where lists = descendant c >>= element (Name (pack "list") Nothing Nothing)

getRuleName::Cursor->String
getRuleName c | tagName c == "list" = "L_" ++
    getRuleName (Data.List.head (child c >>= element (Name (pack "expression") Nothing Nothing) >>= child)) ++
    "_L"
getRuleName c | tagName c == "or" =
    Data.List.intercalate "-or-" (Data.List.map getRuleName (child c))
getRuleName c | tagName c == "link" = contentString c
getRuleName c | tagName c == "anyCharBut" = charsetName c
getRuleName c = error ("Unknown tagName in getRuleName: " ++ tagName c)

listRules::Cursor->String
listRules c | cursorIsAtTextNode c = ""
listRules c | tagName c == "list" = getRuleName c ++ ": "
    ++ getRuleName (Data.List.head (child c >>= element (Name (pack "expression") Nothing Nothing) >>= child))
    ++ " | "
    ++ getRuleName c ++ "[first] "
    ++ (getSeparatorText (child c >>= element (Name (pack "separator") Nothing Nothing) >>= child))
    ++ getRuleName (Data.List.head (child c >>= element (Name (pack "expression") Nothing Nothing) >>= child))
    ++ "[second] "
    ++ " { $$ = concatAndFreeOriginal($first, $second); };\n\n"
    ++ getRuleName c ++ "-or-NULL: | "
    ++ getRuleName c
    ++ ";\n\n"
listRules c = applyTemplates listRules (child c)

orRules::Cursor->String
orRules c | cursorIsAtTextNode c = ""
orRules c | tagName c == "or" = getRuleName c ++ ": "
    ++ Data.List.intercalate " | " (Data.List.map getRuleName (child c))
    ++ ";\n\n"
orRules c = applyTemplates listRules (child c)

--TODO- Make this work for all chars, including unicode (this is easier said than done)
charsetRules::Cursor->String
charsetRules c = charsetName c ++ ": "
    ++ oreo (Data.List.filter (\x -> not $ elem x (contentString c)) "abcdefghijklmnopqrstuvwxyzABCDEFGHOIJKLMNOPQRSTUVWXYZ0123456789`~!@#$%^&*()-_=+[]\\{}|;':\",./<>? ")
    ++ ";\n\n"

charsetName::Cursor->String
charsetName c = "charset" ++ show (Data.List.length (precedingSibling c) + 1)



position::Cursor->Int
position c = Data.List.length $ precedingSibling c

contentString::Cursor->String
--contentString c = content2ContentString $ content c
contentString c = unpack $ Data.Text.concat $ (descendant c >>= content)

content2ContentString::[Text]->String
content2ContentString (x:xs) = unpack x ++ content2ContentString xs
content2ContentString [] = []

--nameIs::Cursor->String->Bool
--nameIs cursor name = (elementName (getElement $ Text.XML.Cursor.node cursor)) ==(Name (pack name) Nothing Nothing)

getSeparatorText::[Cursor]->String
getSeparatorText (c:cs) | cursorIsAtTextNode c = getSeparatorText cs
getSeparatorText (c:cs) | tagName c == "whitespace" = " whitespace " ++ getSeparatorText cs
getSeparatorText (c:cs) | tagName c == "text" = splito (contentString c) ++ getSeparatorText cs
getSeparatorText (c:cs) = error ("unknown tag in getSeparatorText: " ++ tagName c)
getSeparatorText [] = ""

getText::Cursor->String
getText cursor = unpack (getTextContent $ Text.XML.Cursor.node cursor)


getTextContent::Text.XML.Node->Text
getTextContent (Text.XML.NodeContent x) = x

applyTemplates::(Cursor->[Char])->[Cursor]->String
applyTemplates template (x:xs) = template x ++ applyTemplates template xs
applyTemplates template [] = ""

name::L.Element->[Char]
name L.Element { elName=QName{qName=name}} = name

getAttribute::Cursor->String->String
getAttribute c attName = content2ContentString $ attribute (Name (pack attName) Nothing Nothing) c

ruleNamesAreEqual::Cursor->Cursor->Bool
ruleNamesAreEqual a b = getRuleName a == getRuleName b

createParser::Cursor->String
createParser c =
    [here|
%{
 #include <iostream>

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

%right 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' 'p' 'q' 'r' 's' 't' 'u' 'v' 'w' 'x' 'y' 'z' 'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' 'O' 'P' 'Q' 'R' 'S' 'T' 'U' 'V' 'W' 'X' 'Y' 'Z' '1''2''3' '4' '5' '6' '7' '8' '9' '0' ' ' '\t' '\n' '\r' '<' '>' '(' ')' '\\' '{' '}' '=' ',' '|' '/'



%%

//===================Before

|] ++

    rules c ++

    "\n\n" ++

    (applyTemplates listRules (Data.List.map snd (getUniqueListPatterns c))) ++

    "\n\n" ++

    (applyTemplates orRules (nubBy ruleNamesAreEqual (descendant c >>= element (Name (pack "or") Nothing Nothing)))) ++

    (applyTemplates charsetRules (descendant c >>= element (Name (pack "anyCharBut") Nothing Nothing))) ++


    [here|

//==================After

lowercase_letter: 'a' |'b' |'c' |'d' |'e' |'f' |'g' |'h' |'i' |'j' |'k' |'l' |'m' |'n' |'o' |'p' |'q' |'r' |'s' |'t' |'u' |'v' |'w' |'x' |'y' |'z';

uppercase_letter: 'A' |'B' |'C' |'D' |'E' |'F' |'G' |'H' |'I' |'J' |'K' |'L' |'M' |'N' |'O' |'P' |'Q' |'R' |'S' |'T' |'U' |'V' |'W' |'X' |'Y' |'Z';

letter: lowercase_letter | uppercase_letter;

digit: '1'|'2'|'3' |'4' |'5' |'6' |'7' |'8' |'9' |'0' ;

alphanum: letter | digit ;
//<xsl:if test="count(//link[text()='number'])&gt;0">
number: integer
      |
      integer '.' integer { $$ = concatAndFreeOriginal(concatAndFreeOriginal($1, $2), $3); };
      |
      '.' integer { $$ = concatAndFreeOriginal($1, $2); };
//</xsl:if>

//<xsl:if test="count(//link[text()='integer'])&gt;0">
integer: digit | integer digit { $$ = concatAndFreeOriginal($1, $2); };
//</xsl:if>

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

|]
