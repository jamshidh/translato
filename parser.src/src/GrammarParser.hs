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
    grammarParser
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

data Expression = TextMatch String | Attribute String Expression |
    List Expression Expression | Ident | Number | WhiteSpace String |
    AnyCharBut String | Or [Expression] | Link String deriving (Show)

data GrammarRule = ElementRule String [Expression] |
                Assignment String [Expression] |
                OperatorDefinition String [String] deriving (Show)

type Grammar = [GrammarRule]

grammarParser::Parser Grammar
grammarParser =
    do
        spaces
        rules<-matchRules
        spaces
        eof
        return rules

matchRules = many1WithSeparator (try matchElementRule <|> try matchAssignment <|> try matchOperatorDefinition) spaces

ident =
    do
        c<-letter
        cs<-many alphaNum
        return (c:cs)

matchElementRule::Parser GrammarRule
matchElementRule =
    do
        name<-ident
        spaces
        string "=>"
        expressions<-many matchExpression
        spaces
        char ';'
        return (ElementRule name expressions)

matchAssignment::Parser GrammarRule
matchAssignment =
    do
        name<-ident
        spaces
        string "="
        expressions<-many matchExpression
        spaces
        char ';'
        return (Assignment name expressions)

matchOperatorDefinition::Parser GrammarRule
matchOperatorDefinition =
    do
        name<-ident
        string ":operators"
        spaces
        string "=>"
        spaces
        operators<-many1WithSeparator matchQuotedString spaces
        spaces
        char ';'
        return (OperatorDefinition name operators)

matchQuotedString::Parser String
matchQuotedString =
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
        spaces
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
            try (do s<-matchQuotedString; return (TextMatch s))
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
        e<-matchQuotedString
        spaces
        string ")"
        return (AnyCharBut e)


matchPreDefinedRule::Parser Expression
matchPreDefinedRule =
    do
        rule<-(
            (string "ident" >> return Ident) <|>
            (string "number" >> return Number)
            )
        return rule

matchConversionText::Parser Expression
matchConversionText =
    do
        text<-many1 (noneOf "<>{}@;\\ " <|>
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

