{-# LANGUAGE TemplateHaskell #-}
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
    Class(Class),
    HasClass(..),
    parents,
    RuleName,
    Name,
    Rule(..),
    Grammar(Grammar),
    main,
    classes,
    formatExpression,
    formatSequence,
    formatGrammar,
    Separator,
    safeDrawEForest,
    safeDrawETree
) where

import Prelude hiding (lookup)

import Control.Lens
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
formatOperator opr = show (priority opr) ++ ":" ++ formatSequence (symbol opr)

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
    _rules::[Rule],
    _suffixSeqs::[Sequence], --Left recursion is removed from a grammar, and replaced with suffix sequences. IE- exp->exp '+' exp can be rewritten as suffix ('+' exp)*
    _operators::[Operator],
    _separator::Separator,
    _className::ClassName,
    _left::Sequence,
    _right::Sequence,
    _parentNames::[String]
    } deriving (Eq, Show)
$(makeClassy ''Class)

formatClass::Class->String
formatClass c = "====[" ++ c^.className
        ++ (if null (c^.parentNames) then "" else ":" ++ intercalate "," (c^.parentNames))
        ++ "]====\n  "
        ++ intercalate "  " (formatRule <$> c^.rules)
        ++ concat (("\n  suffix: " ++) <$>  (formatSequence <$> c^.suffixSeqs))
        ++ "  separator: " ++ formatSequence (c^.separator) ++ "\n"
        ++ "  left: " ++ formatSequence (c^.left) ++ "\n"
        ++ "  right: " ++ formatSequence (c^.right) ++ "\n"
        ++ (if (length (c^.operators) > 0)
            then "  operators: " ++ intercalate ", " (formatOperator <$> c^.operators) ++ "\n" else "")
        ++ "====[/" ++ c^.className ++ "]===="


--classes = 1 --lens _classes (\g v -> g { _classes = v })

data Grammar = Grammar { _main::String
                       , _classes::Map ClassName Class
                       } deriving (Show)

$(makeLenses ''Grammar)

parents::Grammar->Class->[Class]
parents g cl = fromJust <$> (`lookup` (g^.classes)) <$> cl^.parentNames


formatGrammar::Grammar->String
formatGrammar g =
        "-----------" ++ replicate (length $ g^.main) '-' ++ "\n"
        ++ "| main = " ++ g^.main ++ " |\n"
        ++ "-----------" ++ replicate (length $ g^.main) '-' ++ "\n\n"
        ++ (intercalate "\n\n" ((formatClass . snd) <$> (toList (g^.classes)))) ++ "\n\n"

data ParseType = Block | Stream deriving (Eq)
