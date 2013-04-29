{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
    createParser,
    createParserWithStartRule,
    modifyGrammar,
    element2Document
) where

import Prelude hiding (lookup)
import Data.Maybe
import Data.Text as DT hiding (map, length, foldl, foldl1, empty, head, filter, intercalate)
import Text.XML
import Data.List as L hiding (union, lookup)
import Data.Map hiding (map, foldl, filter)

import Debug.Trace

import GrammarParser
import GrammarTools
import ManyWorldsParser
import OperatorNames

type Attribute = (String, String)

sequence2CharParser::Grammar->Sequence->[State Char]
sequence2CharParser _ [AnyCharBut forbiddenChars] = noneOf forbiddenChars
sequence2CharParser g [Link name] =
        sequence2CharParser g lookupRule
            where lookupRule = case lookup name (ruleMap g) of
                    Just x -> x
                    Nothing -> error ("The grammar links to a non-existant rule named " ++ name)
sequence2CharParser g e = error ("sequence2CharParser can't handle " ++ show e)

returnBlank::[State a]->[State ([Node], [Attribute])]
returnBlank = sMap (\x -> ([], []))

combino::([Node], [Attribute])->([Node], [Attribute])->([Node], [Attribute])
combino (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)

(<++>)::[State ([Node], [Attribute])]->[State ([Node], [Attribute])]->[State ([Node], [Attribute])]
(<++>) = combine combino

returnAttributeNamed::String->[State String]->[State ([Node], [Attribute])]
returnAttributeNamed name = sMap2 (\value -> if (value /= "script") then ([Done ([], [(name, value)])]) else [err "script tag not allowed"])

getTextFromSingleTextNode::([Node], [Attribute])->String
getTextFromSingleTextNode ([NodeContent text], []) = unpack text
getTextFromSingleTextNode ([], []) = "" --(error "getTextFromSingleTextNode is empty")
getTextFromSingleTextNode x = error ("getTextFromSingleTextNode called with nodes that aren't text [\n\n" ++ show x ++ "]")

textNode::[State String]->[State ([Node], [Attribute])]
textNode = sMap (\string -> ([NodeContent (pack string)], []))

reparse::Grammar->Sequence->String->([Node], [Attribute])
reparse g second s = case parse "file" (sequence2Parser g (second ++ [EOF])) s of
    Right [val] -> val
    Right val -> error ("Reparse value is not unique: " ++ (intercalate "\n\n----\n\n" (map show val)))
    Left err -> error "reparse failed"


sequence2Parser::Grammar->Sequence->[State ([Node], [Attribute])]
sequence2Parser g ((WhiteSpace _):e:rest) =
    ignorableWhitespacePrefix(expression2Parser g e <++> sequence2Parser g rest)
sequence2Parser g (e:rest) = expression2Parser g e <++> sequence2Parser g rest
sequence2Parser g [] = blank ([], [])








expression2Parser::Grammar->Expression->[State ([Node], [Attribute])]
expression2Parser g (TextMatch matchString) = returnBlank (string matchString)

expression2Parser _ Ident  = textNode ident
expression2Parser _ EIdent = textNode eIdent
expression2Parser _ Number = textNode number
expression2Parser g (StringOf e) = textNode $ string1Of (sequence2CharParser g e)

expression2Parser g (Attribute name seq) = returnAttributeNamed name (sMap getTextFromSingleTextNode (sequence2Parser g seq))

expression2Parser g (Or x) = foldl1 (|||) (map (sequence2Parser g) x)

expression2Parser g Blank = blank ([], [])
expression2Parser g (Tab _ e) = sequence2Parser g e

expression2Parser g (MultiElementWrapper name e) =
    sMap
        (\x@(nodes, attributes) -> if (length nodes > 1) then (addElementIfMultiple name x, []) else x)
        (sequence2Parser g e)
            where
                addElementIfMultiple tagName (nodes, attributes) =
                    [NodeElement $ Element {
                        elementName=Name (pack tagName) Nothing Nothing,
                        elementAttributes=map attribute2NameText attributes,
                        elementNodes=nodes}]


expression2Parser g (List exp) = sMap (foldl combino ([], [])) (many (sequence2Parser g exp))

{--expression2Parser g (SepBy exp separator) = blank ([], []) |||
    ((expression2Parser g exp) <++> expression2Parser g (List (Sequence [separator, exp])))

expression2Parser g (SepBy1 exp separator) =
    expression2Parser g (Sequence [exp, List (Sequence [separator, exp])])--}

expression2Parser g (Reparse second first) = sMap ((reparse g second) . getTextFromSingleTextNode) (sequence2Parser g first)





--expression2Parser g (InfixElement string) = expression2Parser g e

expression2Parser g (Link name) =
        sMap (\elements -> (elements, [])) lookupRule
            where lookupRule = case lookup name (grammar2Rules g) of
                    Just x -> x
                    Nothing -> error ("The grammar links to a non-existant rule named " ++ name)

expression2Parser g EOF = eof ([], [])

expression2Parser g x = error ("Missing case in expression2Parser: " ++ show x)

--------------------------------

attribute2NameText::Attribute->(Name, Text)
attribute2NameText (s1, s2) =
                ((Name (pack s1) Nothing Nothing), pack $ s2)

-------------------------

addElementUsingAttributeValues::String->[State ([Node], [Attribute])]->[State [Node]]

addElementUsingAttributeValues tagName parser =
        sMap makeElement parser
            where makeElement (elements, attributes) = [NodeElement $ Element {
                elementName=Name (pack tagName) Nothing Nothing,
                elementAttributes=map attribute2NameText attributes,
                elementNodes=elements}]

dropAttributes::[State ([Node], [Attribute])]->[State [Node]]
dropAttributes parser = sMap fst parser

----------------------------------

grammar2Rules::Grammar->Map String ([State [Node]])
grammar2Rules g = terminalParsers g

terminalParsers::Grammar->Map String ([State [Node]])
terminalParsers g = union
    (mapWithKey (\name e -> (addElementUsingAttributeValues name) $ sequence2Parser g e) (ruleMap g))
    (mapWithKey (\name e -> dropAttributes $ sequence2Parser g e) (assignmentMap g))

modifyGrammar::Grammar->Grammar
--modifyGrammar g = fullySimplifyGrammar $ removeLeftRecursionFromGrammar $ fullySimplifyGrammar $ expandOperators $ stripWhitespaceFromGrammar $ addEOFToGrammar g
modifyGrammar g = fullySimplifyGrammar $ expandOperators $ stripWhitespaceFromGrammar $ addEOFToGrammar g

createParserWithStartRule::String->Grammar->Maybe [State [Node]]
createParserWithStartRule startRule g =
        lookup startRule modifiedRules
            where modifiedRules = grammar2Rules $ modifyGrammar g


createParser::Grammar->[State [Node]]
createParser g = case createParserWithStartRule (startSymbol g) g of
    Just states -> states

element2Document::Element->Document
element2Document element = Document {
    documentPrologue=Prologue {prologueBefore=[], prologueDoctype=Nothing, prologueAfter=[]},
    documentRoot=element,
    documentEpilogue=[]
    }

