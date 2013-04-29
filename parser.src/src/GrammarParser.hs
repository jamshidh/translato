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
    grammarParser,
    Grammar(Grammar),
    RuleName,
    Rule,
    ruleMap,
    sShow,
    assignmentMap,
    startSymbol,
    elementRules,
    assignments,
    operatorDefinitions,
    OperatorSymbol,
    Expression(
        AnyCharBut,
        Attribute,
        Blank,
        EIdent,
        EOF,
        Ident,
        InfixElement,
        Link,
        List,
        MultiElementWrapper,
        NestedElement,
        Number,
        Or,
        Reparse,
        SepBy,
        SepBy1,
        SepBy2,
        StringOf,
        Tab,
        TextMatch,
        Variable,
        WhiteSpace),
    Sequence
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Map as M hiding (filter, map, foldl)
import Data.List
import Data.Maybe

import Colors hiding (reverse)

import JDebug

type OperatorSymbol = String

type Sequence = [Expression]

data Expression = TextMatch String | Attribute String Sequence |
    SepBy Sequence Sequence | SepBy1 Sequence Sequence | SepBy2 Sequence Sequence |
    Ident | EIdent | Number | WhiteSpace String |
    List Sequence | Blank | EOF | InfixElement String |
    Variable | NestedElement String Sequence | MultiElementWrapper String Sequence |
    AnyCharBut String | Or [Sequence] | Link String |  StringOf Sequence |
    Reparse Sequence Sequence |
    Tab String Sequence deriving (Eq, Show)

{--instance Show Expression where
    show (Or list) = intercalate "\n    |\n    " (map sShow list)
    show x = iShow x--}

sShow::Sequence->String
sShow seq = intercalate " " (map show seq)

iShow::Expression->String
iShow (TextMatch text) = show text
iShow (Attribute name [Ident]) = "@" ++ name
iShow (Attribute name theType) = "@" ++ name ++ "{" ++ sShow theType ++ "}"
iShow (NestedElement name theType) = name ++ "{" ++ sShow theType ++ "}"
iShow (MultiElementWrapper name items) = "multiElementWrapper(" ++ name ++ ", " ++ sShow items ++ ")"
iShow (InfixElement name) = name ++ "{<-->}"
iShow (SepBy e separator) = "sepBy(" ++ sShow e ++ ", " ++ sShow separator ++ ")"
iShow (SepBy1 e separator) = "sepBy1(" ++ sShow e ++ ", " ++ sShow separator ++ ")"
iShow (List e) = "list(" ++ sShow e ++ ")"
iShow (StringOf e) = "stringOf(" ++ sShow e ++ ")"
iShow (Reparse second first) = "reparse(" ++ sShow second ++ ", " ++ sShow first ++ ")"
iShow Variable = blue "Variable"
iShow Blank = "blank"
iShow Ident = underline "ident"
iShow EIdent = underline "eIdent"
iShow Number = underline "number"
iShow (WhiteSpace defaultValue) = "_"
iShow (AnyCharBut chars) = "anyCharBut(" ++ show chars ++ ")"
iShow (Or list) = "(" ++ intercalate " | " (map sShow list) ++ ")"
iShow (Link name) = underline $ magenta name
iShow (Tab tabString e) = "(" ++ show tabString ++ ")==>(" ++ sShow e ++ ")"
iShow EOF = "EOF"


type RuleName = String

type Rule = (RuleName, Sequence)

data GrammarItem = ElementRule (RuleName, Sequence) |
                Assignment (RuleName, Sequence) |
                OperatorDefinition (RuleName, [OperatorSymbol]) deriving (Show)

data Grammar = Grammar { startSymbol::String,
                        elementRules::[Rule],
                        assignments::[Rule],
                        operatorDefinitions::Map String [OperatorSymbol] } deriving (Eq)

ruleMap::Grammar->Map RuleName Sequence
ruleMap g = fromListWith (\a b -> [Or [a, b]]) (elementRules g)

assignmentMap::Grammar->Map RuleName Sequence
assignmentMap g = fromListWith (\a b -> [Or [a, b]]) (assignments g)


instance Show Grammar where
    show g = "Start Symbol = " ++ (startSymbol g) ++ "\n\n------------\n\n" ++
        (intercalate "\n\n" (map formatRule (elementRules g))) ++ "\n\n" ++
        (intercalate "\n\n" (map formatAssignment (assignments g))) ++ "\n\n" ++
        (intercalate "\n\n" (map formatOperator (toList $ operatorDefinitions g)))

formatRule::(String, Sequence)->String
formatRule (name, e) = (cyan name) ++ (green " => ") ++ show e

formatAssignment::(String, Sequence)->String
formatAssignment (name, e) = (cyan name) ++ (yellow " = ") ++ show e

formatOperator::(String, [OperatorSymbol])->String
formatOperator (name, operators) = (cyan name) ++ (yellow " has operators ") ++ show operators

---------- Convert Lists to Maps (ie- create the Grammar from the parsed data)

getElementRules::[GrammarItem]->[(RuleName, Sequence)]
getElementRules ((ElementRule rulePair):rest) = rulePair:(getElementRules rest)
getElementRules (x:rest) = getElementRules rest
getElementRules [] = []

getAssignments::[GrammarItem]->[(RuleName, Sequence)]
getAssignments ((Assignment rulePair):rest) = rulePair:(getAssignments rest)
getAssignments (x:rest) = getAssignments rest
getAssignments [] = []

getOperatorDefinitions::[GrammarItem]->[(RuleName, [OperatorSymbol])]
getOperatorDefinitions ((OperatorDefinition rulePair):rest) = rulePair:(getOperatorDefinitions rest)
getOperatorDefinitions (x:rest) = getOperatorDefinitions rest
getOperatorDefinitions [] = []

grammarParser::Parser Grammar
{--grammarParser =
    do
        grammarItems<-grammarItemParser
        return Grammar {
                            startSymbol = startSymbol grammarItems,
                            elementRules = fromListWithKey
                                (\name -> \a -> \b -> if (name == startSymbol grammarItems) then Or [a, b] else Or [strip a, strip b])
                                (getElementRules grammarItems),
                            assignments = fromList (getAssignments grammarItems),
                            operatorDefinitions = fromList (getOperatorDefinitions grammarItems)
                        }
            where startSymbol items = fst $ head $ getElementRules items --}

grammarParser =
    do
        grammarItems<-grammarItemParser
        return Grammar {
                            startSymbol = startSymbol grammarItems,
                            elementRules = getElementRules grammarItems,
                            assignments = getAssignments grammarItems,
                            operatorDefinitions = fromList (getOperatorDefinitions grammarItems)
                        }
            where startSymbol items = fst $ head $ getElementRules items

-----------------------------

grammarItemParser::Parser [GrammarItem]
grammarItemParser =
    do
        spaces
        rules<-matchRules
        spaces
        eof
        return rules

matchRules::Parser [GrammarItem]
matchRules = many1WithSeparator (try matchElementRule <|> try matchAssignment <|> try matchOperatorDefinition) spaces

ident =
    do
        c<-letter
        cs<-many alphaNum
        return (c:cs)

matchElementRule::Parser GrammarItem
matchElementRule =
    do
        name<-ident
        spaces
        string "=>"
        sequences<-many1 matchSequence
        spaces
        char ';'
        return (ElementRule (name, concat sequences))

matchAssignment::Parser GrammarItem
matchAssignment =
    do
        name<-ident
        spaces
        string "="
        sequences<-many1 matchSequence
        spaces
        char ';'
        return (Assignment (name, concat sequences))

matchOperatorDefinition::Parser GrammarItem
matchOperatorDefinition =
    do
        name<-ident
        string ":operators"
        spaces
        string "=>"
        spaces
        operators<-many1WithSeparator matchSimpleQuote spaces
        spaces
        char ';'
        return (OperatorDefinition (name, operators))

matchQuotedStringWithWhitespace::Parser Sequence
matchQuotedStringWithWhitespace =
    do
        string "'"
        val<-many (
            try (do text <- many1 (noneOf "' \t\r\n_\\" <|> (string "\\'" >> return '\'')); return (TextMatch text)) <|>
            (do defaultSpace <- many1(
                    try space
                    <|> try (char '_')
                    <|> try (string "\\n" >> return '\n')
                    <|> try (string "\\r" >> return '\r')
                    <|> (string "\\t" >> return '\t')
                    ); return (WhiteSpace defaultSpace)))
        string "'"
        return val

matchSimpleQuote::Parser String
matchSimpleQuote =
    do
        string "'"
        val<-many (try (noneOf "'\\") <|> try (string "\\\\" >> return '\\')
            <|> try (string "\\'" >> return '\'')
            <|> try (string "\\n" >> return '\n')
            <|> try (string "\\r" >> return '\r')
            <|> (string "\\t" >> return '\t'))
        string "'"
        return val


many1WithSeparator::Parser a->Parser b->Parser [a]
many1WithSeparator x matchSeparator =
    do
        first<-x
        rest<-many $ try (do matchSeparator; val<-x; return val)
        return (first:rest)

matchSequence::Parser Sequence
matchSequence = try matchAttribute
    <|> try matchWhiteSpaceAndBracket
    <|> try matchNestedElement <|> try matchInlineElement <|> try matchConversionText
    <|> try matchBracket <|> try matchBlank <|> matchWhiteSpace

matchAttribute::Parser Sequence
matchAttribute =
    do
        string "@"
        name<-ident
        parseType<-option [Ident] matchBracket
        return ([Attribute name parseType])

matchNestedElement::Parser Sequence
matchNestedElement =
    do
        name<-ident
        parseType<-matchBracket
        return ([NestedElement name parseType])

matchInlineElement::Parser Sequence
matchInlineElement =
    do
        name<-ident
        string "{<-->}"
        return ([InfixElement name])

matchBracket::Parser Sequence
matchBracket =
    do
        char '{'
        val<-matchBracketedSequence
        char '}'
        return val

matchWhiteSpaceAndBracket::Parser Sequence
matchWhiteSpaceAndBracket =
    do
        whitespace <- try (many1 space) <|> (string "_")
        val<-matchBracket
        return (
            let (leftWhitespace, rightWhitespace) = breakRight '\n' whitespace in
                if (elem '\n' whitespace) &&
                    (rightWhitespace /= "") &&
                    isOnlyMadeOfSpaces rightWhitespace then [WhiteSpace leftWhitespace, Tab rightWhitespace val]
                        else (WhiteSpace whitespace):val)

isOnlyMadeOfSpaces::String->Bool
isOnlyMadeOfSpaces [] = True
isOnlyMadeOfSpaces (' ':rest) = isOnlyMadeOfSpaces rest
isOnlyMadeOfSpaces (c:rest) = False

breakRight::(Eq a)=>a->[a]->([a], [a])
breakRight char string = (\(x, y) -> (reverse y, reverse x)) (break (char ==) (reverse string))

matchBracketedSequence::Parser Sequence
matchBracketedSequence =
    buildExpressionParser

        [[Infix (do {string "|"; return (\a -> \b -> [Or [a, b]])}) AssocLeft]]

        (
            try matchPreDefinedRule <|>
            try matchList1 <|>
            try matchList <|>
            try matchAnyCharBut <|>
            try matchStringOf <|>
            try matchReparse <|>
            try matchLink <|>
            try matchQuotedStringWithWhitespace
        )


matchLink::Parser Sequence
matchLink =
    do
        name<-ident
        return ([Link name])

matchList::Parser Sequence
matchList =
    do
        string "list("
        spaces
        e<-matchBracketedSequence
        spaces
        char ','
        spaces
        separator<-matchBracketedSequence
        spaces
        string ")"
        return ([SepBy e separator])

matchList1::Parser Sequence
matchList1 =
    do
        string "list1("
        spaces
        e<-matchBracketedSequence
        spaces
        char ','
        spaces
        separator<-matchBracketedSequence
        spaces
        string ")"
        return ([SepBy1 e separator])

matchMultiElementWrapper::Parser Sequence
matchMultiElementWrapper =
    do
        string "multiElementWrapper("
        spaces
        name<-ident
        spaces
        char ','
        spaces
        items<-matchBracketedSequence
        spaces
        string ")"
        return ([MultiElementWrapper name items])

matchStringOf::Parser Sequence
matchStringOf =
    do
        string "stringOf("
        spaces
        e<-matchBracketedSequence
        spaces
        string ")"
        return ([StringOf e])

matchAnyCharBut::Parser Sequence
matchAnyCharBut =
    do
        string "anyCharBut("
        spaces
        e<-matchSimpleQuote
        spaces
        string ")"
        return ([AnyCharBut e])

matchReparse::Parser Sequence
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
        return ([Reparse secondExpression firstExpression])

matchPreDefinedRule::Parser Sequence
matchPreDefinedRule =
    do
        rule<-(
            (string "ident" >> return Ident) <|>
            (string "eIdent" >> return EIdent) <|>
            (string "number" >> return Number)
            )
        return [rule]

matchConversionText::Parser Sequence
matchConversionText =
    do
        text<-many1 (noneOf "{}@;\\ \t\n\r_" <|>
            try (string "\\\\" >> return '\\') <|>
            try (string "\\{" >> return '{') <|>
            try (string "\\}" >> return '}') <|>
            try (string "\\;" >> return ';') <|>
            try (string "\\@" >> return '@'))
        return ([TextMatch text])

matchWhiteSpace::Parser Sequence
matchWhiteSpace =
    do
        formatDefault<-try (many1 space) <|> (string "_")
        return ([WhiteSpace formatDefault])

matchBlank::Parser Sequence
matchBlank =
    do
        string "<blank>"
        return [Blank]



--------------------------
