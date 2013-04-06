{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
    createParser,
    modifyGrammar,
    element2Document
) where

import Prelude hiding (lookup)
import Data.Maybe
import Data.Text as DT hiding (map, length, foldl, foldl1, empty, head, filter)
import Text.XML
import Data.List as L hiding (union, lookup)
import Data.Map hiding (map, foldl, filter)

--import Debug.Trace

import GrammarParser
import GrammarTools

import ManyWorldsParser

type Attribute = (String, String)

abcd::[State ([Element], [Attribute])]->[State ([Element], [Attribute])]
abcd a = combine (\(x1, y1) (x2, y2) -> (x1 ++ x2, y1 ++ y2)) a (abcd a)

expression2CharParser::Expression->[State Char]
expression2CharParser (AnyCharBut forbiddenChars) = noneOf forbiddenChars

expression2StringParser::Expression->[State String]
expression2StringParser Ident  = ident
expression2StringParser EIdent = eIdent
expression2StringParser Number = number
expression2StringParser (StringOf e) = string1Of (expression2CharParser e)
expression2StringParser (List e) = many (expression2CharParser e)
expression2StringParser x = error ("Unknown parameter given to expression2StringParser: " ++ show x)

returnBlank::[State a]->[State ([Element], [Attribute])]
returnBlank = sMap (\x -> ([], []))

combino::([Element], [Attribute])->([Element], [Attribute])->([Element], [Attribute])
combino (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)

(<++>)::[State ([Element], [Attribute])]->[State ([Element], [Attribute])]->[State ([Element], [Attribute])]
(<++>) = combine combino

returnAttributeNamed::String->[State String]->[State ([Element], [Attribute])]
returnAttributeNamed name = sMap2 (\value -> if (value /= "script") then ([Done ([], [(name, value)])]) else [err "script tag not allowed"])
--returnAttributeNamed name = sMap2 (\value -> ([Done ([], [(name, value)])]))
--returnAttributeNamed name states = sMap (\value -> ([], [(name, value)])) states


expression2Parser::Grammar->Expression->[State ([Element], [Attribute])]
expression2Parser g (TextMatch matchString) = returnBlank (string matchString)

expression2Parser g (Attribute name e) = returnAttributeNamed name (expression2StringParser e)

expression2Parser g (Or x) = foldl1 (|||) (map (expression2Parser g) x)

expression2Parser g (ReturnBlank e) = returnBlank (expression2Parser g e)

expression2Parser g Blank = blank ([], [])

expression2Parser g (List exp) = sMap (foldl combino ([], [])) (many (expression2Parser g exp))

expression2Parser g (SepBy exp separator) = blank ([], []) |||
    ((expression2Parser g exp) <++>
    sMap (foldl combino ([], [])) (many (expression2Parser g (Sequence [separator, exp]))))

--expression2Parser g (InfixElement string) = expression2Parser g e

expression2Parser g (Link name) =
        sMap (\elements -> (elements, [])) lookupRule
            where lookupRule = case lookup name (grammar2Rules g) of
                    Just x -> x
                    Nothing -> error ("The grammar links to a non-existant rule named " ++ name)

expression2Parser g (Sequence ((WhiteSpace _):e:rest)) =
    ignorableWhitespacePrefix(expression2Parser g e <++> expression2Parser g (Sequence rest))
expression2Parser g (Sequence (e:rest)) = expression2Parser g e
    <++> expression2Parser g (Sequence rest)
expression2Parser g (Sequence []) = blank ([], [])

expression2Parser g EOF = eof ([], [])

expression2Parser g x = error ("Missing case in expression2Parser: " ++ show x)

--------------------------------

attribute2NameText::Attribute->(Name, Text)
attribute2NameText (s1, s2) =
                ((Name (pack s1) Nothing Nothing), pack $ s2)

-------------------------

addElementUsingAttributeValues::String->[State ([Element], [Attribute])]->[State [Element]]

addElementUsingAttributeValues tagName parser =
        sMap makeElement parser
            where makeElement (elements, attributes) = [Element {
                elementName=Name (pack tagName) Nothing Nothing,
                elementAttributes=map attribute2NameText attributes,
                elementNodes=(map (\x->NodeElement x) elements)}]

dropAttributes::[State ([Element], [Attribute])]->[State [Element]]
dropAttributes parser =
        sMap fst parser

----------------------------------

grammar2Rules::Grammar->Map String ([State [Element]])
grammar2Rules g = terminalParsers g

addElement::String->[Element]->[Element]->[Element]
addElement opName elements1 elements2 = [Element {
        elementName=Name (pack $ opName) Nothing Nothing,
            elementAttributes=[],
            elementNodes=map NodeElement (elements1 ++ elements2)
            }]

terminalParsers::Grammar->Map String ([State [Element]])
terminalParsers g = union
    (mapWithKey (\name e -> (addElementUsingAttributeValues name) $ expression2Parser g e) (elementRules g))
    (mapWithKey (\name e -> dropAttributes $ expression2Parser g e) (assignments g))

modifyGrammar::Grammar->Grammar
--modifyGrammar g = fullySimplifyGrammar $ removeLeftRecursionFromGrammar $ fullySimplifyGrammar $ expandOperators $ stripWhitespaceFromGrammar $ addEOFToGrammar g
modifyGrammar g = fullySimplifyGrammar $ expandOperators $ stripWhitespaceFromGrammar $ addEOFToGrammar g


createParser::Grammar->[State [Element]]
createParser g =
        (modifiedRules ! startSymbol g)
            where modifiedRules = grammar2Rules $ modifyGrammar g

element2Document::Element->Document
element2Document element = Document {
    documentPrologue=Prologue {prologueBefore=[], prologueDoctype=Nothing, prologueAfter=[]},
    documentRoot=element,
    documentEpilogue=[]
    }




--operatorRules::String->Cursor->String
--operatorRules tag c = tag ++ ": "
--    ++ tag ++ " " ++ (splito $ contentString c) ++ " " ++ tag
--    ++ " {  bcout << \"</" ++ (op2Name $ contentString c) ++ ">\"; } "
--    ++ ";"




