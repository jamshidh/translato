{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
    createParser
) where

import Data.Maybe
import Data.Text as DT hiding (map, length, foldl, foldl1, empty, head, filter)
import Text.XML
import Text.XML.Cursor
import Data.List as L
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Map hiding (map, foldl, filter)

import Debug.Trace

isTextNode::Node->Bool
isTextNode (NodeContent x) = True
isTextNode x = False

cursorIsAtTextNode::Cursor->Bool
cursorIsAtTextNode c = isTextNode $ node c

getElement::Node->Element
getElement (NodeElement x) = x

tagName::Cursor->Text
tagName cursor = nameLocalName (elementName (getElement $ node cursor))

whitespace::Parser [Element]
whitespace = do
    many $ oneOf " \n\r\t"
    return []

(<&>)::Parser ([Element], [(Text, String)])->Parser ([Element], [(Text, String)])->Parser ([Element], [(Text, String)])
(<&>) a b = do
    r1<-a
    r2<-b
    return (fst r1 ++ fst r2, snd r1 ++ snd r2)

(<||>)::Parser a->Parser a->Parser a
x <||> y = try x <|> try y

abcd::Parser ([Element], [(Text, String)])->Parser ([Element], [(Text, String)])
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

number::Parser String
number = many digit

cursor2StringParser::Cursor->Parser String
cursor2StringParser c | tagName c == "ident" = ident
cursor2StringParser c | tagName c == "number" = number


cursor2Parser::Map Text (Parser [Element])->Cursor->Parser ([Element], [(Text, String)])
cursor2Parser m c | tagName c == "text" =
        do
            string (unpack $ contentString c)
            return ([], [])

--The whitespace rule is complex....  We ignore whitespace surrounding an item, unless it is the first rule
cursor2Parser m c | tagName c == "whitespace" &&
    (
        (((length $ precedingSibling c) > 0) && ((length $ followingSibling c) > 0))
        ||
        (length (parent c >>= precedingSibling >>= element "rule") == 0)
    )
        =
        do
            whitespace
            return ([], [])

cursor2Parser m c | tagName c == "whitespace" = do string "" >> return ([], [])


cursor2Parser m c | tagName c == "attribute" =
    do
        r1<-parser
        return ([], [(getAttribute c "name", r1)])
            where parser = if ((length $ child c) > 0) then (cursor2StringParser c) else ident

--cursor2Parser m c | tagName c == "element" = cursors2Parser m (child c)

cursor2Parser m c | tagName c == "or" = foldl1 (\x -> \y -> try x <|> try y) (map (cursor2Parser m) (child c))

cursor2Parser m c | tagName c == "list" = list <|> (do string ""; return ([], []))
    where list=do
                r1<-e
                r2<-many $ try (do
                    getSeparatorText m c
                    r3<-e
                    return r3
                    )
                return (fst r1 ++ (foldl (\x -> \y -> x ++ fst y) [] r2),
                    snd r1 ++ (foldl (\x -> \y -> x ++ snd y) [] r2))

            where e = foldl1 (<&>) (map (cursor2Parser m) (child c >>= element "expression" >>= child))

cursor2Parser m c | tagName c == "link" =
    do
        r1<-(m ! ((trace $ show $ contentString c) (contentString c)))
        return (r1, [])

cursor2Parser m c | tagName c == "anyCharBut" =
    do
        r1<-noneOf (unpack $ contentString c)
        return ([Element {elementName="text",
                elementAttributes=empty,
                elementNodes=[NodeContent (pack [r1])]}], [])

cursor2Parser m c | cursorIsAtTextNode c = error ("cursor2Parser is at text node: [" ++ (unpack $ contentString c) ++ "]")
cursor2Parser m c = error ("cursor2Parser is at unknown element: [" ++ (unpack $ tagName c) ++ "]")

-------------------

cursors2Parser::Map Text (Parser [Element])->[Cursor]->Parser ([Element], [(Text, String)])
cursors2Parser m cs = foldl1 (<&>) (map (cursor2Parser m) cs)


--------------------------------

textString2NameText::(Text, String)->(Name, Text)
textString2NameText (s1, s2) =
                ((Name s1 Nothing Nothing), pack $ s2)

-------------------------

rules::Map Text (Parser [Element])->Cursor->Parser [Element]

rules m c | tagName c == "rule" =
    do
        r1<-cursors2Parser m (child c )
        return [Element {
            elementName=Name (getAttribute c "tag") Nothing Nothing,
            elementAttributes=fromList (map textString2NameText (snd r1)),
            elementNodes=(map (\x->NodeElement x) (fst r1))}]

rules m c | tagName c == "assignment" =
    do
        r1<-cursors2Parser m (child c )
        return (fst r1)

rules m c | tagName c == "operatorDefinition" = string "" >> return []

rules m c = error ("--Unknown element in rules: [" ++ (unpack $ tagName c) ++ "]\n")

----------------------------------

terminalRuleList::Map Text (Parser [Element])->Cursor->[(Text, Parser [Element])]
terminalRuleList ruleMap c = map (\c1 -> (getAttribute c1 "tag", rules ruleMap c1))
    (child c >>= checkElement (\e -> "assignment" == elementName e || "rule" == elementName e))

terminalRuleMap m c = fromListWith (<||>) (terminalRuleList m c)

operatorRuleList::Map Text (Parser [Element])->Cursor->[(Text, Parser [Element])]
operatorRuleList ruleMap root =
    map (\c1 -> (getAttribute c1 "tag",
            buildExpressionParser (cursor2OpTable c1) ((terminalRuleMap ruleMap root) ! (getAttribute c1 "tag"))))
        (child root >>= element "operatorDefinition")

cursor2OpTable::Cursor->OperatorTable Char () [Element]
cursor2OpTable c = map (\x -> [Infix (do { string (unpack x); return (op2Xml x)}) AssocLeft]) operators
    where operators = (child c >>= element "operator" >>= child >>= content)

--cursor2OpTable c = map (\x -> [Infix (do { string (unpack x); return (op2Xml $ op2Name x)}) AssocLeft]) ["+", "*"]

op2Xml::Text->[Element]->[Element]->[Element]
op2Xml opName left right =
    [Element {
                elementName=Name opName Nothing Nothing,
                elementAttributes=empty,
                elementNodes=map NodeElement (left ++ right)
            }]


ruleList::Cursor->[(Text, Parser [Element])]
ruleList root =
    terminalRules ++ operatorRules
        where terminalRules =
                filter ((not . (hasOperator root)) .fst ) (terminalRuleList (ruleMap root) root); operatorRules =
                filter ((hasOperator root) .fst) (operatorRuleList (ruleMap root) root)


ruleMap::Cursor->Map Text (Parser [Element])
ruleMap c = fromListWith (<||>) (ruleList c)


hasOperator::Cursor->Text->Bool
hasOperator root tag = 0 /= length (child root >>= element "operatorDefinition" >>= checkElement (\c -> tag == elementAttributes c ! "tag"))

root::Cursor->Parser Element
root c =
    do
        r1<-(ruleMap c) ! rootName
        return (head r1)
            where rootName = (getAttribute (head $ (child c >>= anyElement)) "tag")

createParser::Cursor->Parser Document
createParser c =
    do
        r1<-root c
        return Document {
            documentPrologue=Prologue {prologueBefore=[], prologueDoctype=Nothing, prologueAfter=[]},
            documentRoot=r1,
            documentEpilogue=[]
            }


op2Name::String->String
op2Name "+" = "plus"
op2Name "-" = "minus"
op2Name "*" = "times"
op2Name "/" = "divide"
op2Name x = error ("Unknown operator in op2Name: \'" ++ x ++ "'")

--operatorRules::String->Cursor->String
--operatorRules tag c = tag ++ ": "
--    ++ tag ++ " " ++ (splito $ contentString c) ++ " " ++ tag
--    ++ " {  bcout << \"</" ++ (op2Name $ contentString c) ++ ">\"; } "
--    ++ ";"




contentString::Cursor->Text
contentString c = DT.concat $ (descendant c >>= content)

getSeparatorText::Map Text (Parser [Element])->Cursor->Parser ([Element], [(Text, String)])
getSeparatorText m c = (cursors2Parser m) (child c >>= element "separator" >>= child)

getAttribute::Cursor->Name->Text
getAttribute c attName = DT.concat $ attribute attName c

