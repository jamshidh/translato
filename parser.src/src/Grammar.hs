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
    CharSet (..),
    CharType (..),
    OperatorSymbol,
    Class (..),
    parents,
    allRules,
    RuleName,
    Name,
    Rule (..),
    Grammar (..),
    formatExpression,
    formatSequence,
    formatGrammar,
    ParseType (..),
    Separator,
    --ruleShow,
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
import EnhancedString hiding (VEnd, InfixTag, EStart, EmptyEStart, EInfo, EEnd)
import TreeTools
import XPath

import JDebug

type OperatorSymbol = Sequence

type Sequence = [Expression]

data Expression =
    TextMatch String
    | WhiteSpace String
    | Character CharSet
    | EOF
    | Or [Sequence]
    | List Int Sequence
    | SepBy Int Sequence
    | Option Sequence
    | Link String
    | FallBack
    | Out EString
--    | Reparse Sequence Sequence

    deriving (Eq, Ord, Show)

formatSequence::Sequence->String
formatSequence seq = intercalate " " (map formatExpression seq)

formatExpression::Expression->String
formatExpression (Character charset) = formatCharSet charset
formatExpression EOF = "EOF"
formatExpression FallBack = "FallBack"
formatExpression (List min e) = "list" ++ (if (min > 0) then show min else "") ++ "(" ++ formatSequence e ++ ")"
formatExpression (Link name) = underline $ magenta name
formatExpression (Or sequences) = "{" ++ intercalate " |\n         " (formatSequence <$> sequences) ++ "}"
formatExpression (Out estring) = blue "Out(" ++ show estring ++ blue ")"
formatExpression (SepBy min e) = "SepBy" ++ (if (min > 0) then show min else "") ++ "(" ++ formatSequence e ++ ")"
formatExpression (Option e) = "Option" ++ "(" ++ formatSequence e ++ ")"
formatExpression (TextMatch text) = show text
formatExpression (WhiteSpace defaultValue) = "_"

safeDrawETree::Tree Expression->String
safeDrawETree = safeDrawTree . (fmap formatExpression)

safeDrawEForest::Forest Expression->String
safeDrawEForest = safeDrawForest. (fmap formatExpression <$>)

type RuleName = String


data Rule = Rule {
    name::String,
    rawSequence::Sequence
    } deriving (Eq, Show)


ruleShow::Rule->String
ruleShow Rule{name=name,rawSequence=sequence} = blue (name) ++ " => " ++ formatSequence sequence ++ "\n"

type ClassName=String

type Name = String

type Separator = Sequence

data Class = Class {
    rules::[Rule],
    suffixSeqs::[Sequence],
    --extensions::[Sequence],
    operators::[OperatorSymbol],
    separator::Separator,
    className::ClassName,
    left::Sequence,
    right::Sequence,
    parentNames::[String]
    } deriving (Eq, Show)

formatClass c = "====[" ++ className c
        ++ (if null (parentNames c) then "" else ":" ++ intercalate "," (parentNames c))
        ++ "]====\n  "
        ++ intercalate "  " (map ruleShow (rules c))
        ++ concat (("\n  suffix: " ++) <$>  (map formatSequence (suffixSeqs c)))
        ++ "  separator: " ++ formatSequence (separator c) ++ "\n"
        ++ "  left: " ++ formatSequence (left c) ++ "\n"
        ++ "  right: " ++ formatSequence (right c) ++ "\n"
        ++ (if (length (operators c) > 0)
            then "  operators: " ++ intercalate ", " (map formatSequence (operators c)) ++ "\n" else "")
        ++ "====[/" ++ className c ++ "]===="

parents::Grammar->Class->[Class]
parents g cl = fromJust <$> (`lookup` (classes g)) <$> parentNames cl

allRules::Grammar->Class->[Rule]
allRules g cl = nub (rules cl ++ ((parents g cl) >>= allRules g))


data Grammar = Grammar { main::String,
                        classes::Map ClassName Class } deriving (Show)

formatGrammar g =
        "-----------" ++ replicate (length $ main g) '-' ++ "\n"
        ++ "| main = " ++ main g ++ " |\n"
        ++ "-----------" ++ replicate (length $ main g) '-' ++ "\n\n"
        ++ (intercalate "\n\n" ((formatClass . snd) <$> (toList (classes g)))) ++ "\n\n"

data ParseType = Block | Stream deriving (Eq)
