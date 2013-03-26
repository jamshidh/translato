-----------------------------------------------------------------------------
--
-- Module      :  GrammarParser
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
    startSymbol,
    elementRules,
    assignments,
    operatorDefinitions,
    OperatorSymbol,
    Expression(Sequence, Blank, TextMatch, Attribute,
        StringOf, List, Ident, EIdent,
        Number, SepBy,
        WhiteSpace, NestedElement,
        AnyCharBut, Or, Link, ReturnBlank, Variable,
        EOF)
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Map as M hiding (filter, map, foldl)
import Data.List
import Data.Maybe

import Colors

data Expression = ReturnBlank Expression | TextMatch String | Attribute String Expression |
    SepBy Expression Expression | Ident | EIdent | Number | WhiteSpace String |
    List Expression | Blank | EOF |
    Variable | NestedElement String Expression |
    AnyCharBut String | Or [Expression] | Link String | StringOf Expression |
    Sequence [Expression] deriving (Eq)

instance Show Expression where
    show (Or list) = intercalate "\n    |\n    " (map iShow list)
    show (Sequence list) = intercalate " " (map iShow list)
    show x = iShow x

iShow::Expression->String
iShow (TextMatch text) = show text
iShow (Attribute name Ident) = "@" ++ name
iShow (Attribute name theType) = "@" ++ name ++ "{" ++ iShow theType ++ "}"
iShow (NestedElement name theType) = name ++ "{" ++ iShow theType ++ "}"
iShow (SepBy e separator) = "sepBy(" ++ iShow e ++ ", " ++ iShow separator ++ ")"
iShow (List e) = "list(" ++ iShow e ++ ")"
iShow (ReturnBlank e) = "returnBlank(" ++ iShow e ++ ")"
iShow (StringOf e) = "stringOf(" ++ iShow e ++ ")"
iShow Blank = "blank"
iShow Ident = underscore "ident"
iShow EIdent = underscore "eIdent"
iShow Number = underscore "number"
iShow (WhiteSpace defaultValue) = "_"
iShow (AnyCharBut chars) = "anyCharBut(" ++ show chars ++ ")"
iShow (Or list) = "(" ++ intercalate " | " (map iShow list) ++ ")"
iShow (Link name) = underscore $ magenta name
iShow (Sequence list) = "(" ++ intercalate " " (map iShow list) ++ ")"
iShow EOF = "EOF"


type RuleName = String
type OperatorSymbol = String

data GrammarItem = ElementRule (RuleName, Expression) |
                Assignment (RuleName, Expression) |
                OperatorDefinition (RuleName, [OperatorSymbol]) deriving (Show)

data Grammar = Grammar { startSymbol::String,
                        elementRules::Map RuleName Expression,
                        assignments::Map RuleName Expression,
                        operatorDefinitions::Map String [OperatorSymbol] } deriving (Eq)



instance Show Grammar where
    show g = "Start Symbol = " ++ (startSymbol g) ++ "\n\n------------\n\n" ++
        (intercalate "\n\n" (map formatRule (toList $ elementRules g))) ++ "\n\n" ++
        (intercalate "\n\n" (map formatAssignment (toList $ assignments g))) ++ "\n\n" ++
        (intercalate "\n\n" (map formatOperator (toList $ operatorDefinitions g)))

formatRule::(String, Expression)->String
formatRule (name, e) = (cyan name) ++ (green " => ") ++ show e

formatAssignment::(String, Expression)->String
formatAssignment (name, e) = (cyan name) ++ (yellow " = ") ++ show e

formatOperator::(String, [OperatorSymbol])->String
formatOperator (name, operators) = (cyan name) ++ (yellow " has operators ") ++ show operators

---------- Convert Lists to Maps (ie- create the Grammar from the parsed data)

getElementRules::[GrammarItem]->[(RuleName, Expression)]
getElementRules ((ElementRule rulePair):rest) = rulePair:(getElementRules rest)
getElementRules (x:rest) = getElementRules rest
getElementRules [] = []

getAssignments::[GrammarItem]->[(RuleName, Expression)]
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

eOr::Expression->Expression->Expression
eOr x y = Or [x, y]

grammarParser =
    do
        grammarItems<-grammarItemParser
        return Grammar {
                            startSymbol = startSymbol grammarItems,
                            elementRules = fromListWith eOr (getElementRules grammarItems),
                            assignments = fromList (getAssignments grammarItems),
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

addSequenceIfMultiple::[Expression]->Expression
addSequenceIfMultiple [] = TextMatch ""
addSequenceIfMultiple [x] = x
addSequenceIfMultiple x = Sequence x

matchElementRule::Parser GrammarItem
matchElementRule =
    do
        name<-ident
        spaces
        string "=>"
        expressions<-many1 matchExpression
        spaces
        char ';'
        return (ElementRule (name, addSequenceIfMultiple expressions))

matchAssignment::Parser GrammarItem
matchAssignment =
    do
        name<-ident
        spaces
        string "="
        expressions<-many1 matchExpression
        spaces
        char ';'
        return (Assignment (name, addSequenceIfMultiple expressions))

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

matchQuotedStringWithWhitespace::Parser Expression
matchQuotedStringWithWhitespace =
    do
        string "'"
        val<-many (
            (do text <- many1 (noneOf "' \t\r\n" <|> (string "\\'" >> return '\'')); return (TextMatch text)) <|>
            (do defaultSpace <- many1(space); return (WhiteSpace defaultSpace)))
        string "'"
        return (addSequenceIfMultiple val)

matchSimpleQuote::Parser String
matchSimpleQuote =
    do
        string "'"
        val<-many (noneOf "'" <|> (string "\\'" >> return '\''))
        string "'"
        return val


many1WithSeparator::Parser a->Parser b->Parser [a]
many1WithSeparator x matchSeparator =
    do
        first<-x
        rest<-many $ try (do matchSeparator; val<-x; return val)
        return (first:rest)

matchExpression::Parser Expression
matchExpression = try matchConversionText <|> try matchAttribute <|> try matchNestedElement
    <|> try matchBracket <|> try matchBlank <|> matchWhiteSpace

matchAttribute::Parser Expression
matchAttribute =
    do
        string "@"
        name<-ident
        parseType<-option Ident matchBracket
        return (Attribute name parseType)

matchNestedElement::Parser Expression
matchNestedElement =
    do
        name<-ident
        parseType<-option Ident matchBracket
        return (NestedElement name parseType)

matchBracket::Parser Expression
matchBracket =
    do
        char '{'
        val<-matchBracketedExpression
        char '}'
        return val

matchBracketedExpression::Parser Expression
matchBracketedExpression =
    buildExpressionParser

        [[Infix (do {string "|"; return (\a -> \b -> Or [a, b])}) AssocLeft]]

        (
            try matchPreDefinedRule <|>
            try matchList <|>
            try matchAnyCharBut <|>
            try matchStringOf <|>
            try matchLink <|>
            try matchQuotedStringWithWhitespace
        )


matchLink::Parser Expression
matchLink =
    do
        name<-ident
        return (Link name)

matchList::Parser Expression
matchList =
    do
        string "list("
        spaces
        e<-matchBracketedExpression
        spaces
        char ','
        spaces
        separator<-matchBracketedExpression
        spaces
        string ")"
        return (SepBy e separator)

matchStringOf::Parser Expression
matchStringOf =
    do
        string "stringOf("
        spaces
        e<-matchBracketedExpression
        spaces
        string ")"
        return (StringOf e)

matchAnyCharBut::Parser Expression
matchAnyCharBut =
    do
        string "anyCharBut("
        spaces
        e<-matchSimpleQuote
        spaces
        string ")"
        return (AnyCharBut e)


matchPreDefinedRule::Parser Expression
matchPreDefinedRule =
    do
        rule<-(
            (string "ident" >> return Ident) <|>
            (string "eIdent" >> return EIdent) <|>
            (string "number" >> return Number)
            )
        return rule

matchConversionText::Parser Expression
matchConversionText =
    do
        text<-many1 (noneOf "<>{}@;\\ \t\n\r" <|>
            try (string "\\\\" >> return '\\') <|>
            try (string "\\<" >> return '<') <|>
            try (string "\\>" >> return '>') <|>
            try (string "\\{" >> return '{') <|>
            try (string "\\}" >> return '}') <|>
            try (string "\\;" >> return ';') <|>
            try (string "\\@" >> return '@'))
        return (TextMatch text)

matchWhiteSpace::Parser Expression
matchWhiteSpace =
    do
        formatDefault<-try (many1 space) <|> (string "<ws>" >> return "")
        return (WhiteSpace formatDefault)

matchBlank::Parser Expression
matchBlank =
    do
        string "<blank>"
        return Blank

matchOr::Parser Expression
matchOr =
    do
        first<-matchBracketedExpression
        -- matchSeparator
        rest<-string "abcd" >> return ([TextMatch "qqqq"]) -- many1WithSeparator matchBracketedExpression matchSeparator
        return (Or (first:rest))
        --    where matchSeparator = (do many space; string "|"; many space; return "")


--------------------------


