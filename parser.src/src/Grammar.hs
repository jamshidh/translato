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
    RuleName,
    Name,
    --RawRule,
    Rule (..),
    Grammar (..),
    formatExpression,
    formatSequence,
    formatGrammar,
    ParseType (..),
    Separator,
    ruleShow,
    safeDrawEForest,
    safeDrawETree
    --ruleMapShow
) where

import Prelude hiding (lookup)

import Data.Functor
import Data.List hiding (lookup)
import Data.Map hiding (filter, map, null, union)
import Data.Tree

import CharSet
import Colors
import EnhancedString hiding (VEnd, InfixTag, EStart, EEnd)
import TreeTools
import XPath

import JDebug

type OperatorSymbol = Sequence

type Sequence = [Expression]

data Expression = TextMatch String
    | Or [Sequence]
    | List Int Sequence | SepBy Int Sequence
    | WhiteSpace String | Character CharSet
    | EOF
    | FallBack
    | Link String | LinkStream String
    | Reparse Sequence Sequence
    -- | JustOutput EString
    | InfixTag Int String
    | AStart String
    | AEnd
    | EStart String [String]
    | EEnd String
    | TabStart String
    | TabEnd deriving (Eq, Ord, Show)

formatSequence::Sequence->String
formatSequence seq = intercalate " " (map formatExpression seq)

formatExpression::Expression->String
formatExpression (AStart name) = "@" ++ name ++ "("
formatExpression AEnd = ")"
formatExpression (Character charset) = formatCharSet charset
formatExpression (EStart tagName attributes) = cyan ("<" ++ tagName ++ concat (map (" " ++) attributes) ++ ">")
formatExpression (EEnd tagName) = cyan ("</" ++ tagName ++ ">")
formatExpression EOF = "EOF"
formatExpression FallBack = "FallBack"
formatExpression (InfixTag priority tagName) = cyan ("<-" ++ tagName ++ ":" ++ show priority ++ "->")
formatExpression (List min e) = "list" ++ (if (min > 0) then show min else "") ++ "(" ++ formatSequence e ++ ")"
formatExpression (LinkStream name) = underline $ magenta ("LS(" ++ name ++ ")")
formatExpression (Link name) = underline $ magenta name
formatExpression (Or sequences) = "{" ++ intercalate " |\n         " (formatSequence <$> sequences) ++ "}"
formatExpression (SepBy min e) = "SepBy" ++ (if (min > 0) then show min else "") ++ "(" ++ formatSequence e ++ ")"
formatExpression (TabStart tabString) = "(" ++ show tabString ++ ")==>("
formatExpression TabEnd = ")"
formatExpression (TextMatch text) = show text
formatExpression (WhiteSpace defaultValue) = "_"

safeDrawETree::Tree Expression->String
safeDrawETree = safeDrawTree . (fmap formatExpression)

safeDrawEForest::Forest Expression->String
safeDrawEForest = safeDrawForest. (fmap formatExpression <$>)

type RuleName = String


--type RawRule = (RuleName, Sequence)
data Rule = Rule {
    name::String,
    --tagName::RuleName,
    --theClass::Class,
    rawSequence::Sequence
    } deriving (Eq, Show)


ruleShow::Rule->String
ruleShow Rule{name=name,rawSequence=sequence} = blue (name) ++ " => " ++ formatSequence sequence ++ "\n"

{--formatNameSequenceMap::Map RuleName [Rule]->String
formatNameSequenceMap rm =
    intercalate "\n" (map ruleShow (concat (toList rm))) ++ "\n"--}

type ClassName=String

type Name = String

type Separator = Sequence

data Class = Class {
    rules::[Rule],
    suffixRules::[Rule],
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
        ++ concat (("\n  suffix: " ++) <$>  (map ruleShow (suffixRules c)))
        ++ "  separator: " ++ formatSequence (separator c) ++ "\n"
        ++ "  left: " ++ formatSequence (left c) ++ "\n"
        ++ "  right: " ++ formatSequence (right c) ++ "\n"
        ++ (if (length (operators c) > 0)
            then "  operators: " ++ intercalate ", " (map formatSequence (operators c)) ++ "\n" else "")
        ++ "====[/" ++ className c ++ "]===="

data Grammar = Grammar { main::String,
                        classes::Map ClassName Class } deriving (Show)

formatGrammar g =
        "-----------" ++ replicate (length $ main g) '-' ++ "\n"
        ++ "| main = " ++ main g ++ " |\n"
        ++ "-----------" ++ replicate (length $ main g) '-' ++ "\n\n"
        ++ (intercalate "\n\n" ((formatClass . snd) <$> (toList (classes g)))) ++ "\n\n"

data ParseType = Block | Stream deriving (Eq)
