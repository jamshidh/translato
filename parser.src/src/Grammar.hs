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
    Associativity(..),
    Operator(..),
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

data Associativity = LeftAssoc | RightAssoc | UseEndCap deriving (Eq, Show)

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
formatSequence = formatSequence' 0

formatSequence'::Int->Sequence->String
formatSequence' level seq = intercalate " " ((formatExpression' level) <$> seq)

formatExpression = formatExpression' 0

formatExpression'::Int->Expression->String
formatExpression' level (Character charset) = formatCharSet charset
formatExpression' level EOF = "EOF"
formatExpression' level FallBack = "FallBack"
formatExpression' level (List min e) = "list" ++ (if (min > 0) then show min else "") ++ "(" ++ formatSequence' level e ++ ")"
formatExpression' level (Link name) = underline $ magenta name
formatExpression' level (Or sequences) =
    case level of
        0 -> "{\n    " ++ intercalate "\n    |\n    " (formatSequence' (level+1) <$> sequences) ++ "\n}"
        _ -> "{" ++ intercalate " | " (formatSequence' (level+1) <$> sequences) ++ "}"
formatExpression' level (Out estring) = blue "Out(" ++ show estring ++ blue ")"
formatExpression' level (SepBy min e) = "SepBy" ++ (if (min > 0) then show min else "") ++ "(" ++ formatSequence' level e ++ ")"
formatExpression' level (Option e) = "Option" ++ "(" ++ formatSequence' level e ++ ")"
formatExpression' level (TextMatch text) = show text
formatExpression' level (WhiteSpace defaultValue) = "_"

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
formatRule Rule{name=name,rawSequence=sequence,rulePriority=p} = show p ++ ":" ++ blue (name) ++ " => " ++ formatSequence sequence ++ "\n"

type ClassName=String

type Name = String

type Separator = Sequence

data Class = Class {
    rules::[Rule],
    suffixSeqs::[Sequence],
    --extensions::[Sequence],
    operators::[Operator],
    separator::Separator,
    className::ClassName,
    left::Sequence,
    right::Sequence,
    parentNames::[String]
    } deriving (Eq, Show)

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
