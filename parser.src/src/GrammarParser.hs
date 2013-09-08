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
import Data.Functor
import Data.List
import Data.Map as M hiding (filter, map, foldl)
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr hiding (Operator)
import qualified Data.Set as Set

import Colors hiding (reverse)
import CharSet
import EnhancedString
import XPath
import Grammar

import JDebug

--import Debug.Trace



---------- Convert Lists to Maps (ie- create the Grammar from the parsed data)

parseGrammar =
    do
        spaces
        classList<-endBy parseClass spaces
        spaces
        eof
        return Grammar {
                            main = (assert (length classList > 0) "Grammar contains no classes")
                                (className $ head classList),
                            classes = M.fromList ((\cl -> (className cl, cl)) <$> classList)
                        }

parseClass = parseFullClass <|> parseSimpleClass

parseSimpleClass =
    do
        RuleItem rule<-parseRule
        spaces
        return Class {
            className=name rule,
            rules=[rule],
            suffixSeqs=[],
            operators=[],
            separator=[WhiteSpace " "],
            left=[],
            right=[],
            parentNames=[]
        }

data ClassItem =
    RuleItem Rule
    | Comment
    | OperatorsItem [Operator]
    | SeparatorItem Sequence
    | LeftItem Sequence
    | RightItem Sequence

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
        return (classWithPriorities items name parents)
        where
            classWithPriorities items name parents = Class {
                className=name,
                rules=[rule|RuleItem rule<-itemsWithPriority],
                suffixSeqs=[],
                operators=concat [operators|OperatorsItem operators<-itemsWithPriority],
                separator=case [separator|SeparatorItem separator<-itemsWithPriority] of
                    [] -> [TextMatch " " Nothing]
                    [x] -> x
                    _ -> error "Only one separator allowed for a class",
                left=case [left|LeftItem left<-itemsWithPriority] of
                    [] -> []
                    [x] -> x
                    _ -> error "Only one left allowed for a class",
                right=case [right|RightItem right<-itemsWithPriority] of
                    [] -> []
                    [x] -> x
                    _ -> error "Only one right allowed for a class",
                parentNames=parents
                }
                where itemsWithPriority = addPriorityToItems 0 items

            addPriorityToItems::Int->[ClassItem]->[ClassItem]
            addPriorityToItems p (RuleItem rule:rest) = RuleItem (rule{rulePriority=p}):addPriorityToItems (p+1) rest
            addPriorityToItems p (Comment:rest) = Comment:addPriorityToItems p rest
            addPriorityToItems p (OperatorsItem ops:rest) =
                        OperatorsItem (addPriority p <$> ops):addPriorityToItems (p+(length ops)) rest
                            where addPriority p op = op{priority=p+priority op}
            addPriorityToItems p (SeparatorItem separator:rest) = SeparatorItem separator:addPriorityToItems p rest
            addPriorityToItems p (LeftItem seq:rest) = LeftItem seq:addPriorityToItems p rest
            addPriorityToItems p (RightItem seq:rest) = RightItem seq:addPriorityToItems p rest
            addPriorityToItems _ _ = []

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
        string "=>"
        sequence<-parseSequence
        spaces
        string ";"
        many (char '-')
        spaces
        return (RuleItem Rule{name=name, rawSequence=sequence, rulePriority=0}) -- I will reset the rulePriority using the order of rules later

parseOperators =
    do
        string "operators:"
        spaces
        operatorSequences <- endBy parseOperator spaces
        return (OperatorsItem ((\(priority, associativity, symbol) -> Operator{symbol=symbol,priority=priority,associativity=associativity}) <$> addPriority 0 operatorSequences))
    where
        addPriority::Int->[(Associativity, Sequence)]->[(Int, Associativity, Sequence)]
        addPriority p ((assoc, seq):rest) = (p, assoc, seq):addPriority (p+1) rest
        addPriority _ [] = []

parseOperator =
    do
        associativity <- option "l:" (string "r:")
        operator <- parseQuote
        return (if associativity == "l:" then LeftAssoc else RightAssoc, operator)

parseSeparator =
    do
        try (string "separator:")
        spaces
        separator<- parseQuote
        return (SeparatorItem separator)

parseLeft =
    do
        try (string "left:")
        spaces
        left<- parseSequence
        spaces
        string ";"
        return (LeftItem left)

parseRight =
    do
        try (string "right:")
        spaces
        right<- parseSequence
        spaces
        string ";"
        return (RightItem right)

-----------------------------

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
string2Sequence s = TextMatch first Nothing:string2Sequence rest
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
            exp2 SequenceItem matchAttribute
            <|> exp2 ExpressionItem matchLink
            <|> exp2 SequenceItem matchParen
            <|> exp2 ExpressionItem matchCharset
            <|> exp2 ExpressionItem matchSimpleCharsetChar
            <|> exp2 TextItem matchText)
        count <- option "" (string "*" <|> string "+" <|> string "?")
        return (case (count, item) of
            ("", ExpressionItem exp) -> [exp]
            ("", SequenceItem seq) -> seq
            ("", TextItem text) -> string2Sequence text
            ("*", ExpressionItem exp) -> [EQuote 0 [exp]]
            ("*", SequenceItem seq) -> [EQuote 0 seq]
            ("*", TextItem text) -> string2Sequence (init text) ++ [EQuote 0 [TextMatch [last text] Nothing]]
            ("+", ExpressionItem exp) -> [EQuote 1 [exp]]
            ("+", SequenceItem seq) -> [EQuote 1 seq]
            ("+", TextItem text) -> string2Sequence (init text) ++ [EQuote 1 [TextMatch [last text] Nothing]]
            ("?", ExpressionItem exp) -> [Option [exp]]
            ("?", SequenceItem seq) -> [Option seq]
            ("?", TextItem text) -> string2Sequence (init text) ++ [Option [TextMatch [last text] Nothing]])



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
        parseType<-option [Link "ident"] matchParen
        return ([Out [VStart name Nothing]] ++ parseType ++ [Out [VEnd]])

matchParen =
    do
        char '('
        sequence <- parseSequence
        char ')'
        return sequence

matchCharset =
    do
        char '['
        isInverse <- option False (char '^' >> return True)
        charTypes <- many (matchEscapedChar <|> try matchCharsetRange <|> matchCharsetChar)
        char ']'
        return (Character (CharSet isInverse charTypes) Nothing)

matchEscapedChar =
    do
        char '\\'
        escapedChar <- noneOf "]"
        return (unescape escapedChar)

matchCharsetChar =
    do
        char <- noneOf "]"
        return (SingleChar char)

matchCharsetRange =
    do
        char1 <- noneOf "]"
        char '-'
        char2 <- noneOf "]"
        return (CharRange char1 char2)

matchSimpleCharsetChar =
    do
        chartype <- (
            try (string "\\s" >> return Space)
            <|> try (string "\\d" >> return Digit)
            <|> try (string "\\w" >> return WordChar)
            )
        return (Character (CharSet False [chartype]) Nothing)

matchLink =
    do
        char '{'
        val<-ident
        char '}'
        return (Link val)

matchText =
    do
        text<-many1 (noneOf ";*()[]-+{}@\\"
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

unescape::Char->CharType
unescape 's' = Space
unescape 'd' = Digit
unescape 'n' = SingleChar '\n'
unescape 'r' = SingleChar '\r'
unescape 't' = SingleChar '\t'
unescape c = error ("Missing escape char in string2CharTypes " ++ [c])


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

