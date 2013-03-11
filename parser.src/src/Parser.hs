{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
    createParser
) where

import Data.Maybe
import Data.Text as DT hiding (map, length, foldl, foldl1, empty, head, filter)
import Text.XML
import Data.List as L hiding (union)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Map hiding (map, foldl, filter)

import Debug.Trace

import GrammarParser

type Attribute = (String, String)

(-!-)::Map String b->String->b
a -!- b = (trace b) $ c
    where c=a!b

whitespace::Parser [Element]
whitespace = do
    many $ oneOf " \n\r\t"
    return []

(<&>)::Parser ([Element], [Attribute])->Parser ([Element], [Attribute])->Parser ([Element], [Attribute])
(<&>) a b = do
    r1<-a
    r2<-b
    return (fst r1 ++ fst r2, snd r1 ++ snd r2)

(<||>)::Parser a->Parser a->Parser a
x <||> y = try x <|> try y

abcd::Parser ([Element], [Attribute])->Parser ([Element], [Attribute])
abcd a = do
    r1<-many a
    return (L.concat (map fst r1), L.concat (map snd r1))

e2eList::Parser Element->Parser [Element]
e2eList a = do
    r1<-a
    return [r1]

ident::Parser String
ident = do
    c<-letter
    cs<-many (alphaNum <|> char '_')
    return (c:cs)

eIdent = do
    c<-letter
    cs<-many (alphaNum <|> char '_' <|> char '.')
    return (c:cs)

number::Parser String
number = many1 digit

expression2StringParser::Expression->Parser String
expression2StringParser Ident  = ident
expression2StringParser EIdent = eIdent
expression2StringParser Number = number
expression2StringParser (List e s) = many (expression2CharParser e)
expression2StringParser x = error ("Unknown parameter given to expression2StringParser: " ++ show x)

expression2CharParser::Expression->Parser Char
expression2CharParser (AnyCharBut x) = noneOf x

expression2Parser::Grammar->Expression->Parser ([Element], [Attribute])
expression2Parser g (TextMatch matchString) =
        do
            string matchString
            return ([], [])

--The whitespace rule is complex....  We ignore whitespace surrounding an item, unless it is the first rule
expression2Parser g (WhiteSpace defaultString)
--    |
--    (
--        (((length $ precedingSibling c) > 0) && ((length $ followingSibling c) > 0))
--        ||
--        (length (parent c >>= precedingSibling >>= element "rule") == 0)
--    )
        =
        do
            whitespace
            return ([], [])

--cursor2Parser m c | tagName c == "whitespace" = do string "" >> return ([], [])


expression2Parser g (Attribute name e) =
    do
        r1<-(expression2StringParser e)
        return ([], [(name, r1)])

--cursor2Parser m c | tagName c == "element" = cursors2Parser m (child c)

expression2Parser g (Or x) = foldl1 (\x -> \y -> try x <|> try y) (map (expression2Parser g) x)

expression2Parser g (List exp separator) =
    do
        list<-(sepBy (expression2Parser g exp) (expression2Parser g separator))
        return (foldl (\a -> \b -> (fst a ++ fst b, snd a ++ snd b)) ([], []) list)

expression2Parser g (Link name) =
    do
        r1<-((ruleParsers g) ! name)
        return (r1, [])

expression2Parser g (AnyCharBut chars) =
    do
        r1<-noneOf chars
        return ([Element {elementName="text",
                elementAttributes=[],
                elementNodes=[NodeContent (pack [r1])]}], [])

expression2Parser g (Sequence expressions) = foldl1 (<&>) (map (expression2Parser g) expressions)


--------------------------------

attribute2NameText::Attribute->(Name, Text)
attribute2NameText (s1, s2) =
                ((Name (pack s1) Nothing Nothing), pack $ s2)

-------------------------

addElementUsingAttributeValues::String->Parser ([Element], [Attribute])->Parser [Element]

addElementUsingAttributeValues tagName parser =
    do
        r1<-parser
        return [Element {
            elementName=Name (pack tagName) Nothing Nothing,
            elementAttributes=map attribute2NameText (snd r1),
            elementNodes=(map (\x->NodeElement x) (fst r1))}]

dropAttributes::Parser ([Element], [Attribute])->Parser [Element]
dropAttributes parser =
    do
        r1<-parser
        return (fst r1)

----------------------------------

ruleParsers::Grammar->Map String (Parser [Element])
ruleParsers g = mapWithKey (addOperators) (terminalParsers g)
    where addOperators name parser =
            if (member name (operatorDefinitions g))
                then buildExpressionParser (operatorSymbols2OpTable $ (operatorDefinitions g) ! name) parser
                else parser

operatorSymbols2OpTable::[OperatorSymbol]->OperatorTable Char () [Element]
operatorSymbols2OpTable operators = map (\x -> [Infix (do { string x; return (op2Xml x)}) AssocLeft]) operators

--cursor2OpTable c = map (\x -> [Infix (do { string (unpack x); return (op2Xml $ op2Name x)}) AssocLeft]) ["+", "*"]

op2Xml::String->[Element]->[Element]->[Element]
op2Xml opName left right =
    [Element {
                elementName=Name (pack $ op2Name opName) Nothing Nothing,
                elementAttributes=[],
                elementNodes=map NodeElement (left ++ right)
            }]


{--ruleList::Grammar->[(Text, Parser [Element])]
ruleList g =
    terminalRules ++ operatorRules
        where terminalRules =
                filter ((not . (hasOperator g)) .fst ) (terminalRules (ruleMap g) g); operatorRules =
                filter ((hasOperator g) .fst) (operatorRuleList (ruleMap g) g)


ruleMap::Grammar->Map Text (Parser [Element])
ruleMap g = fromListWith (<||>) (ruleList g) --}

terminalParsers::Grammar->Map String (Parser [Element])
terminalParsers g = union
    (mapWithKey (\name -> \p -> (addElementUsingAttributeValues name) $ expression2Parser g p) (elementRules g))
    (mapWithKey (\name -> \p -> dropAttributes $ expression2Parser g p) (assignments g))

createParser::Grammar->Parser Document
createParser g =
    do
        r1<-(ruleParsers strippedGrammar) ! startSymbol strippedGrammar
        return Document {
            documentPrologue=Prologue {prologueBefore=[], prologueDoctype=Nothing, prologueAfter=[]},
            documentRoot=head r1,
            documentEpilogue=[]
            }
        where strippedGrammar = stripWhitespaceFromGrammar g


op2Name::String->String
op2Name "+" = "plus"
op2Name "-" = "minus"
op2Name "*" = "times"
op2Name "/" = "divide"
op2Name "." = "dot"
op2Name x = error ("Unknown operator in op2Name: \'" ++ x ++ "'")

--operatorRules::String->Cursor->String
--operatorRules tag c = tag ++ ": "
--    ++ tag ++ " " ++ (splito $ contentString c) ++ " " ++ tag
--    ++ " {  bcout << \"</" ++ (op2Name $ contentString c) ++ ">\"; } "
--    ++ ";"




