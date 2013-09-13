{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
--
-- Module      :  Grammar
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

module Grammar (
    Sequence,
    Expression(..),
    Operator(..),
    Class(..),
    parents,
    RuleName,
    Name,
    Rule(..),
    Grammar(..),
    formatExpression,
    formatSequence,
    formatGrammar,
    Separator,
    safeDrawEForest,
    safeDrawETree
) where

import Prelude hiding (lookup)

import Data.Functor
import Data.List hiding (lookup)
import Data.Map hiding (filter, map, null, union)
import Data.Maybe
import Data.Tree

import CharSet
import Colors
import EnhancedString
import TreeTools

--import JDebug

data Operator =
    Operator {
        symbol::Sequence,
        priority::Int,
        associativity::Associativity
    } deriving (Eq, Show)

formatOperator::Operator->String
formatOperator op = show (priority op) ++ ":" ++ formatSequence (symbol op)

type Sequence = [Expression]

data Expression =
    TextMatch String (Maybe String) --The 'Maybe String' is only used for error reporting
    | WhiteSpace DefaultWS
    | Character CharSet (Maybe String) --The 'Maybe String' is only used for error reporting
    | EOF
    | Or [Sequence]
    | List Int Sequence
    | SepBy Int Sequence Sequence
    | EQuote Int Sequence
    | Option Sequence
    | Link String
    | FallBack
    | Out EString
--    | Reparse Sequence Sequence

    deriving (Eq, Ord, Show)

formatSequence::Sequence->String
formatSequence = formatSequence' 0

formatSequence'::Int->Sequence->String
formatSequence' level sq = intercalate " " ((formatExpression' level) <$> sq)

formatExpression::Expression->String
formatExpression = formatExpression' 0

formatExpression'::Int->Expression->String
formatExpression' _ (Character charset _) = formatCharSet charset
formatExpression' _ EOF = "EOF"
formatExpression' _ FallBack = "FallBack"
formatExpression' level (List minCount expr) = "list" ++ (if (minCount > 0) then show minCount else "") ++ "(" ++ formatSequence' level expr ++ ")"
formatExpression' _ (Link linkName) = underline $ magenta linkName
formatExpression' level (Or sequences) =
    case level of
        0 -> "{\n    " ++ intercalate "\n    |\n    " (formatSequence' (level+1) <$> sequences) ++ "\n}"
        _ -> "{" ++ intercalate " | " (formatSequence' (level+1) <$> sequences) ++ "}"
formatExpression' _ (Out estring) = blue "Out(" ++ show estring ++ blue ")"
formatExpression' level (SepBy minCount expr sep) = "SepBy" ++ (if (minCount > 0) then show minCount else "") ++ "(" ++ formatSequence' level expr ++ "," ++ formatSequence' level sep ++ ")"
formatExpression' level (EQuote minCount expr) = "EQuote" ++ (if (minCount > 0) then show minCount else "") ++ "(" ++ formatSequence' level expr ++ ")"
formatExpression' level (Option expr) = "Option" ++ "(" ++ formatSequence' level expr ++ ")"
formatExpression' _ (TextMatch text _) = show text
formatExpression' _ (WhiteSpace FutureWS) = "_??_"
formatExpression' _ (WhiteSpace _) = "_"

safeDrawETree::Tree Expression->String
safeDrawETree = safeDrawTree . (fmap formatExpression)

safeDrawEForest::Forest Expression->String
safeDrawEForest = safeDrawForest. (fmap formatExpression <$>)

type RuleName = String


data Rule = Rule {
    rulePriority::Int,
    name::String,
    rawSequence::Sequence
    } deriving (Eq, Show)


formatRule::Rule->String
formatRule Rule{name=ruleName,rawSequence=sq,rulePriority=p} = show p ++ ":" ++ blue (ruleName) ++ " => " ++ formatSequence sq ++ "\n"

type ClassName=String

type Name = String

type Separator = Sequence

data Class = Class {
    rules::[Rule],
    suffixSeqs::[Sequence], --Left recursion is removed from a grammar, and replaced with suffix sequences. IE- exp->exp '+' exp can be rewritten as suffix ('+' exp)*
    operators::[Operator],
    separator::Separator,
    className::ClassName,
    left::Sequence,
    right::Sequence,
    parentNames::[String]
    } deriving (Eq, Show)

formatClass::Class->String
formatClass c = "====[" ++ className c
        ++ (if null (parentNames c) then "" else ":" ++ intercalate "," (parentNames c))
        ++ "]====\n  "
        ++ intercalate "  " (formatRule <$> rules c)
        ++ concat (("\n  suffix: " ++) <$>  (map formatSequence (suffixSeqs c)))
        ++ "  separator: " ++ formatSequence (separator c) ++ "\n"
        ++ "  left: " ++ formatSequence (left c) ++ "\n"
        ++ "  right: " ++ formatSequence (right c) ++ "\n"
        ++ (if (length (operators c) > 0)
            then "  operators: " ++ intercalate ", " (formatOperator <$> operators c) ++ "\n" else "")
        ++ "====[/" ++ className c ++ "]===="

parents::Grammar->Class->[Class]
parents g cl = fromJust <$> (`lookup` classes g) <$> parentNames cl

data Grammar = Grammar { main::String,
                        classes::Map ClassName Class } deriving (Show)

formatGrammar::Grammar->String
formatGrammar g =
        "-----------" ++ replicate (length $ main g) '-' ++ "\n"
        ++ "| main = " ++ main g ++ " |\n"
        ++ "-----------" ++ replicate (length $ main g) '-' ++ "\n\n"
        ++ (intercalate "\n\n" ((formatClass . snd) <$> (toList (classes g)))) ++ "\n\n"

data ParseType = Block | Stream deriving (Eq)
