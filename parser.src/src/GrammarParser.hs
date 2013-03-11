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
    Grammar,
    startSymbol,
    elementRules,
    assignments,
    operatorDefinitions,
    OperatorSymbol,
    Expression(Sequence, TextMatch, Attribute, List, Ident, EIdent, Number, WhiteSpace, AnyCharBut, Or, Link),
    stripWhitespaceFromGrammar
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Map as M hiding (filter, map, foldl)
import Data.List
import Data.Maybe

import Colors

data Expression = TextMatch String | Attribute String Expression |
    List Expression Expression | Ident | EIdent | Number | WhiteSpace String |
    AnyCharBut String | Or [Expression] | Link String |
    Sequence [Expression] deriving (Eq)

instance Show Expression where
    show (Or list) = intercalate "\n    |\n    " (map show list)
    show x = iShow x

iShow::Expression->String
iShow (TextMatch text) = show text
iShow (Attribute name Ident) = "@" ++ name
iShow (Attribute name theType) = "@" ++ name ++ "{" ++ iShow theType ++ "}"
iShow (List e separator) = "list(" ++ iShow e ++ ", " ++ iShow separator ++ ")"
iShow Ident = underscore "ident"
iShow EIdent = underscore "eIdent"
iShow Number = underscore "number"
iShow (WhiteSpace defaultValue) = "_"
iShow (AnyCharBut chars) = "anyCharBut(" ++ chars ++ ")"
iShow (Or list) = intercalate " | " (map iShow list)
iShow (Link name) = underscore $ magenta name
iShow (Sequence list) = intercalate " " (map iShow list)


type Name = String
type OperatorSymbol = String

data GrammarItem = ElementRule (Name, Expression) |
                Assignment (Name, Expression) |
                OperatorDefinition (Name, [OperatorSymbol]) deriving (Show)

data Grammar = Grammar { startSymbol::String,
                        elementRules::Map Name Expression,
                        assignments::Map Name Expression,
                        operatorDefinitions::Map String [OperatorSymbol] } deriving (Eq)



instance Show Grammar where
    show g = "Start Symbol = " ++ (startSymbol g) ++ "\n\n------------\n\n" ++
        (intercalate "\n\n" (map formatRule (toList $ elementRules g))) ++ "\n\n" ++
        (intercalate "\n\n" (map formatAssignment (toList $ assignments g)))

formatRule::(String, Expression)->String
formatRule (name, e) = (cyan name) ++ (green " => ") ++ show e

formatAssignment::(String, Expression)->String
formatAssignment (name, e) = (cyan name) ++ (yellow " = ") ++ show e

---------- Convert Lists to Maps (ie- create the Grammar from the parsed data)

getElementRules::[GrammarItem]->[(Name, Expression)]
getElementRules ((ElementRule rulePair):rest) = rulePair:(getElementRules rest)
getElementRules (x:rest) = getElementRules rest
getElementRules [] = []

getAssignments::[GrammarItem]->[(Name, Expression)]
getAssignments ((Assignment rulePair):rest) = rulePair:(getAssignments rest)
getAssignments (x:rest) = getAssignments rest
getAssignments [] = []

getOperatorDefinitions::[GrammarItem]->[(Name, [OperatorSymbol])]
getOperatorDefinitions ((OperatorDefinition rulePair):rest) = rulePair:(getOperatorDefinitions rest)
getOperatorDefinitions (x:rest) = getOperatorDefinitions rest
getOperatorDefinitions [] = []

grammarParser::Parser Grammar
grammarParser =
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
matchExpression = try matchConversionText <|> try matchAttribute <|> try matchBracket <|> matchWhiteSpace

matchAttribute::Parser Expression
matchAttribute =
    do
        string "@"
        name<-ident
        parseType<-option Ident matchBracket
        return (Attribute name parseType)

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
        return (List e separator)

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

matchOr::Parser Expression
matchOr =
    do
        first<-matchBracketedExpression
        -- matchSeparator
        rest<-string "abcd" >> return ([TextMatch "qqqq"]) -- many1WithSeparator matchBracketedExpression matchSeparator
        return (Or (first:rest))
        --    where matchSeparator = (do many space; string "|"; many space; return "")


--------------------------

stripWhitespaceFromGrammar::Grammar->Grammar
stripWhitespaceFromGrammar g = Grammar
    {
        startSymbol = startSymbol g,
        elementRules = mapWithKey (\name -> \e -> if (name == startSymbol g) then e else strip e) (elementRules g),
        assignments = mapWithKey (\name -> \e -> if (name == startSymbol g) then e else strip e) (assignments g),
        operatorDefinitions = operatorDefinitions g

    }

strip::Expression->Expression
strip (Sequence ((WhiteSpace defaultText):rest)) = Sequence (removeLastWhitespace rest)
strip x = x

removeLastWhitespace::[Expression]->[Expression]
removeLastWhitespace (e:[WhiteSpace defaultText]) = [e]
removeLastWhitespace (e:rest) = e:(removeLastWhitespace rest)
removeLastWhitespace [] = []

---------------------

fullySimplify::Grammar->Grammar
fullySimplify g = fst $ fromJust $ find (\(g1, g2) -> g1 == g2) (zip simplifiedProgression (tail simplifiedProgression))
    where
        simplifiedProgression = g:(map simplifyGrammar  simplifiedProgression)

simplifyGrammar::Grammar->Grammar
simplifyGrammar g = g
    {
        elementRules = mapWithKey simplifyKeyAndExpression (elementRules g),
        assignments = mapWithKey simplifyKeyAndExpression (assignments g)
    }

simplifyKeyAndExpression::String->Expression->Expression
simplifyKeyAndExpression key e = simplify e

simplify::Expression->Expression
----
simplify (Or ((Or x):y)) = Or (x++y)
---- The remainder are a recursive identity
simplify (Attribute s e) = Attribute s (simplify e)
simplify (List e1 e2) = List (simplify e1) (simplify e2)
simplify (Or x) = Or (map simplify x)
simplify (Sequence x) = Sequence (simplifySequence (map simplify x))
simplify x = x

simplifySequence::[Expression]->[Expression]
simplifySequence (List e separator:xs) = simplifySequence xs
simplifySequence (x:xs) = x:xs
