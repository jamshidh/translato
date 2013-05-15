-----------------------------------------------------------------------------
--
-- Module      :  XPath
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

module XPath (
    Condition (
        CnAnd,
        CnOr,
        CnTrue,
        CnFalse,
        CnAtt,
        CnString,
        CnEq),
    eval,
    parseCondition
) where

import Prelude hiding (lookup)
import Data.Map
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

data Condition = CnAnd Condition Condition
    | CnOr Condition Condition
    | CnTrue
    | CnFalse
    | CnAtt String
    | CnString String
    | CnNeq Condition Condition
    | CnEq Condition Condition deriving (Eq, Ord)

instance Show Condition where
    show (CnEq first second) = "(" ++ show first ++ " = " ++ show second ++ ")"
    show (CnNeq first second) = "(" ++ show first ++ " != " ++ show second ++ ")"
    show (CnAtt name) = "@" ++ name
    show (CnString s) = show s
    show CnTrue = "true"
    show CnFalse = "false"

eval::Map String String->Condition->Maybe Condition
eval atts (CnAtt name) = case lookup name atts of
    Nothing -> Nothing
    Just s -> Just (CnString s)
eval atts (CnString s) = Just $ CnString s
eval atts CnTrue = Just CnTrue
eval atts CnFalse = Just CnFalse
eval atts (CnEq first second) = case (eval atts first, eval atts second) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
eval atts (CnNeq first second) = case (eval atts first, eval atts second) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just res1, Just res2) -> Just (if res1 /= res2 then CnTrue else CnFalse)
eval _ cn = error ("missing case in eval: " ++ show cn)


ident::Parser String
ident =
    do
        c<-letter
        cs<-many alphaNum
        return (c:cs)

parseCondition::Parser Condition
parseCondition = buildExpressionParser operators parseTerminals

operators = [
        [Infix (do try (char '='); spaces >> return CnEq) AssocNone],
        [Infix (do try (string "!="); spaces >> return CnNeq) AssocNone],
        [Infix (do string "and" >> return CnAnd) AssocNone],
        [Infix (do string "or" >> return CnOr) AssocNone]
    ]

parseTerminals =
    do
        condition <- (parseCnAtt <|> parseQuoteString)
        spaces
        return condition

parseCnAtt =
    do
        char '@'
        name <- ident
        return (CnAtt name)

parseDoubleQuoteString =
    do
        char '"'
        theString <- many (noneOf "\"")
        char '"'
        return (CnString theString)

parseSingleQuoteString =
    do
        char '\''
        theString <- many (noneOf "'")
        char '\''
        return (CnString theString)

parseQuoteString = parseSingleQuoteString <|> parseDoubleQuoteString


