-----------------------------------------------------------------------------
--
-- traceModule      :  GrammarParser
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module GrammarParser (
    parseGrammar,
    Grammar(Grammar),
    RuleName,
    Sequence
) where

import Data.Char hiding (Space)
import Data.List
import Data.Map as M hiding (filter, map, foldl)
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Colors hiding (reverse)
import CharSet
import XPath
import Grammar

import JDebug

--import Debug.Trace



---------- Convert Lists to Maps (ie- create the Grammar from the parsed data)

parseGrammar =
    do
        spaces
        classes<-endBy parseClass spaces
        spaces
        eof
        return Grammar {
                            main = (assert (length classes > 0) "Grammar contains no classes")
                                (className $ head classes),
                            classes = classes
                        }

parseClass = parseFullClass <|> parseSimpleClass

parseSimpleClass =
    do
        RuleItem rule<-parseRule
        spaces
        return Class {
                    className=fst rule,
                    parentNames=[],
                    rawRules=[rule],
                    operators=[],
                    left=[],
                    right=[],
                    separator=[WhiteSpace " "]
               }

data RuleOrParameter =
    RuleItem RawRule | Comment | OperatorsItem [Sequence] | SeparatorItem Sequence | LeftItem Sequence | RightItem Sequence

parseFullClass =
    do
        string "====["
        name<-ident
        parents <- option [] (char ':' >> sepBy ident (char ','))
        string "]"
        many (char '=')
        spaces
        items<-endBy (
            parseSeparator
            <|> parseLeft
            <|> parseRight
            <|> parseOperators
            <|> parseRule
            <|> parseComment) spaces
        spaces
        string "====[/"
        name<-ident
        char ']'
        many (char '=')
        return Class {
                className=name,
                parentNames=parents,
                rawRules=[rule|RuleItem rule<-items],
                operators=concat [operators|OperatorsItem operators<-items],
                left=case [separator|LeftItem separator<-items] of
                    [] -> [TextMatch " "]
                    [x] -> x
                    _ -> error "Only one separator allowed for a class",
                right=case [separator|RightItem separator<-items] of
                    [] -> [TextMatch " "]
                    [x] -> x
                    _ -> error "Only one separator allowed for a class",
                separator=case [separator|SeparatorItem separator<-items] of
                    [] -> [TextMatch " "]
                    [x] -> x
                    _ -> error "Only one separator allowed for a class"
            }

parseComment =
    do
        char '#'
        many (noneOf "\n")
        char '\n'
        return Comment

parseRule =
    do
        name<-ident
        spaces
        condition <- option CnTrue parseBracketedCondition
        spaces
        string "=>"
        sequence<-parseSequence
        spaces
        string ";"
        many (char '-')
        spaces
        return (RuleItem (name, (condition, sequence)))

parseOperators =
    do
        string "operators:"
        spaces
        operators<- endBy parseQuote spaces
        return (OperatorsItem operators)

parseLeft =
    do
        try (string "left:")
        spaces
        sequence<-parseSequence
        spaces
        string ";"
        return (RightItem sequence)

parseRight =
    do
        try (string "right:")
        spaces
        sequence<-parseSequence
        spaces
        string ";"
        spaces
        return (RightItem sequence)

parseSeparator =
    do
        try (string "separator:")
        spaces
        separator<- parseQuote
        return (SeparatorItem separator)

-----------------------------

parseBracketedCondition =
    do
        char '['
        condition <- parseCondition
        char ']'
        return condition

ident =
    do
        c<-letter
        cs<-many alphaNum
        return (c:cs)

parseQuote::Parser Sequence
parseQuote =
    do
        char '\''
        quotedString<-many (
            noneOf "'\\\n"
                <|> try (string "\\'" >> return '\'')
                <|> try (string "\\n" >> return '\n')
                <|> try (string "\\r" >> return '\r')
                <|> try (string "\\t" >> return '\t'))
        char '\''
        return (string2Sequence quotedString)

string2Sequence::String->Sequence
string2Sequence [] = []
string2Sequence ('_':rest) = WhiteSpace "":string2Sequence rest
string2Sequence s | isSpace (head s) = WhiteSpace first:string2Sequence rest
    where (first, rest) = break (not . isSpaceOrUnderscore) s
string2Sequence s = TextMatch first:string2Sequence rest
    where (first, rest) = break isSpaceOrUnderscore s

isSpaceOrUnderscore::Char->Bool
isSpaceOrUnderscore c = isSpace c || c == '_'

many1WithSeparator::Parser a->Parser b->Parser [a]
many1WithSeparator x matchSeparator =
    do
        first<-x
        rest<-many $ try (do matchSeparator; val<-x; return val)
        return (first:rest)

data Count = One | ZeroOrMore | OneOrMore

parseSequence::Parser Sequence
parseSequence =
    do
        sequences <- many matchSequenceItem
        return (concat sequences)

data SequenceItem = TextItem String | SequenceItem Sequence | ExpressionItem Expression

matchSequenceItem =
    do
        item <- (
            exp2 ExpressionItem matchAttribute
            <|> exp2 ExpressionItem matchLink
            <|> exp2 SequenceItem matchParen
            <|> exp2 ExpressionItem matchCharsetChar
            <|> exp2 ExpressionItem matchSimpleCharsetChar
            <|> exp2 TextItem matchText)
        count <- option "" (string "*" <|> string "+")
        return (case (count, item) of
            ("", ExpressionItem exp) -> [exp]
            ("", SequenceItem seq) -> seq
            ("", TextItem text) -> string2Sequence text
            ("*", ExpressionItem exp) -> [SepBy 0 [exp]]
            ("*", SequenceItem seq) -> [SepBy 0 seq]
            ("*", TextItem text) -> string2Sequence (init text) ++ [SepBy 0 [TextMatch [last text]]]
            ("+", ExpressionItem exp) -> [SepBy 1 [exp]]
            ("+", SequenceItem seq) -> [SepBy 1 seq]
            ("+", TextItem text) -> string2Sequence (init text) ++ [SepBy 1 [TextMatch [last text]]])



exp2Seq::Parser Expression->Parser Sequence
exp2Seq x =
    do
        ret<-x
        return [ret]

exp2::(a->b)->Parser a->Parser b
exp2 f x =
    do
        ret<-x
        return (f ret)

matchAttribute =
    do
        char '@'
        name<-ident
        parseType<-option [Ident] matchParen
        return (Attribute name parseType)

matchParen =
    do
        char '('
        sequence <- parseSequence
        char ')'
        return sequence

matchCharsetChar =
    do
        char '['
        isInverse <- option False (char '^' >> return True)
        charset <- many (noneOf "]")
        char ']'
        return (Character (CharSet isInverse (string2CharTypes charset)))

matchSimpleCharsetChar =
    do
        chartype <- (
            try (string "\\s" >> return Space)
            <|> try (string "\\d" >> return Digit)
            <|> try (string "\\w" >> return WordChar)
            )
        return (Character (CharSet False [chartype]))

matchLink =
    do
        char '{'
        val<-ident
        char '}'
        return (Link val)

matchText =
    do
        text<-many1 (noneOf ";*()[]-{}@\\"
            <|> (try (string "\\@") >> return '@')
            <|> (try (string "\\*") >> return '*')
            <|> (try (string "\\+") >> return '+')
            <|> (try (string "\\;") >> return ';')
            <|> (try (string "\\(") >> return '(')
            <|> (try (string "\\)") >> return ')')
            <|> (try (string "\\[") >> return '[')
            <|> (try (string "\\]") >> return ']')
            <|> (try (string "\\{") >> return '{')
            <|> (try (string "\\}") >> return '}'))
        return text

string2CharTypes::String->[CharType]
string2CharTypes ('\\':'s':rest) = Space:string2CharTypes rest
string2CharTypes ('\\':'d':rest) = Digit:string2CharTypes rest
string2CharTypes ('\\':'n':rest) = SingleChar '\n':string2CharTypes rest
string2CharTypes ('\\':'r':rest) = SingleChar '\r':string2CharTypes rest
string2CharTypes ('\\':'t':rest) = SingleChar '\t':string2CharTypes rest
string2CharTypes ('\\':c:rest) = error ("Missing escape char in string2CharTypes " ++ [c])
string2CharTypes (c:rest) = SingleChar c:string2CharTypes rest
string2CharTypes [] = []


{--matchWhiteSpaceAndBracket::Parser Sequence
matchWhiteSpaceAndBracket =
    do
        whitespace <- try (many1 space) <|> (string "_")
        val<-matchBracket
        return (
            let (leftWhitespace, rightWhitespace) = breakRight '\n' whitespace in
                if (elem '\n' whitespace) &&
                    (rightWhitespace /= "") &&
                    isOnlyMadeOfSpaces rightWhitespace then [WhiteSpace leftWhitespace, Tab rightWhitespace val]
                        else (WhiteSpace whitespace):val)--}

isOnlyMadeOfSpaces::String->Bool
isOnlyMadeOfSpaces [] = True
isOnlyMadeOfSpaces (' ':rest) = isOnlyMadeOfSpaces rest
isOnlyMadeOfSpaces (c:rest) = False

breakRight::(Eq a)=>a->[a]->([a], [a])
breakRight char string = (\(x, y) -> (reverse y, reverse x)) (break (char ==) (reverse string))

{--matchBracketedSequence::Parser Sequence
matchBracketedSequence =
            try matchPreDefinedRule <|>
            try matchList1 <|>
            try matchList <|>
            try matchAnyCharBut <|>
            try matchReparse <|>
            try matchLink <|>
            try parseQuote--}

{--matchList::Parser Sequence
matchList =
    do
        string "("
        spaces
        e<-matchBracketedSequence
        spaces
        string ")*"
        return ([List 0 e])

matchList1::Parser Sequence
matchList1 =
    do
        string "("
        spaces
        e<-matchBracketedSequence
        spaces
        string ")+"
        return ([List 1 e])--}

{--matchAnyCharBut =
    do
        string "[^"
        e<-many1 (noneOf "]")
        string "]"
        return (AnyCharBut e)--}

{--matchReparse::Parser Sequence
matchReparse =
    do
        string "reparse("
        spaces
        secondExpression<-matchBracketedSequence
        spaces
        char ','
        spaces
        firstExpression<-matchBracketedSequence
        spaces
        string ")"
        return ([Reparse secondExpression firstExpression])--}

matchPreDefinedRule::Parser Sequence
matchPreDefinedRule =
    do
        rule<-(
            (string "ident" >> return Ident) <|>
            (string "number" >> return Number)
            )
        return [rule]

matchConversionText =
    do
        text<-many1 (noneOf "-{}@\\ \t\n\r_" <|>
            try (string "\\\\" >> return '\\') <|>
            try (string "\\{" >> return '{') <|>
            try (string "\\}" >> return '}') <|>
            try (string "\\@" >> return '@'))
        return (TextMatch text)

{--matchWhiteSpace =
    do
        formatDefault<-(many1 space) <|> (string "_")
        return (WhiteSpace formatDefault)--}

