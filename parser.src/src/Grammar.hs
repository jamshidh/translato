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
    RawRule,
    Rule (..),
    Grammar (..),
    sShow,
    Separator,
    ruleShow,
    ruleMapShow
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

type SequenceForest = Forest Expression

data Expression = TextMatch String | Attribute String Sequence | VEnd
    | Or [Sequence]
    | Bind | List Int Sequence | SepBy Int Sequence
    | Loop Sequence
    | Exit
    | Ident | Number | WhiteSpace String | Character CharSet
    | EOF
    -- | AnyCharBut String
    | Link String | LinkStream String
    | Reparse Sequence Sequence
    -- | JustOutput EString
    | InfixTag Int String
    | EStart String [String] Condition
    | EEnd String
    | Tab String Sequence deriving (Eq, Ord)

instance Show Expression where show = iShow

sequence2SequenceForest::Sequence->SequenceForest
sequence2SequenceForest [Or seqs] = seqs >>= sequence2SequenceForest
sequence2SequenceForest (Or seqs:rest) = (++rest) <$> seqs >>= sequence2SequenceForest --error "An 'Or' is not at the end of a sequence"
sequence2SequenceForest (first:rest) = [Node {rootLabel=first,subForest=sequence2SequenceForest rest}]
sequence2SequenceForest [] = []

tabRight::String->String
tabRight ('\n':'|':rest) = "\n                |" ++ tabRight rest
tabRight ('\n':'+':rest) = "\n                +" ++ tabRight rest
tabRight ('\n':'`':rest) = "\n                `" ++ tabRight rest
tabRight ('\n':rest) = "\n            " ++ tabRight rest
tabRight (c:rest) = c:tabRight rest
tabRight [] = []

sShow::Sequence->String
sShow seq = tabRight ((if length stringForest > 1 then "\n" else "") ++ concat stringForest)
    where stringForest = drawTree <$> (((sFlatShow `fmap`) <$> (cleanForest (sequence2SequenceForest seq))))

sFlatShow::Sequence->String
sFlatShow seq = intercalate " " (map show seq)

iShow::Expression->String
iShow (TextMatch text) = show text
iShow (Attribute name [Ident]) = "@" ++ name
iShow (Attribute name theType) = "@" ++ name ++ "(" ++ sFlatShow theType ++ ")"
iShow VEnd = green "VEnd"
iShow Bind = "Bind"
iShow (SepBy min e) = "SepBy" ++ (if (min > 0) then show min else "") ++ "(" ++ sFlatShow e ++ ")"
iShow (List min e) = "list" ++ (if (min > 0) then show min else "") ++ "(" ++ sFlatShow e ++ ")"
--iShow (Reparse second first) = "reparse(" ++ sShow second ++ ", " ++ sShow first ++ ")"
iShow Ident = "Ident"
iShow Number = underline "number"
iShow (InfixTag priority tagName) =
    "InfixTag("
        ++ show priority ++ "," ++ tagName
        ++ ")"
iShow (EStart tagName attributes condition) = cyan ("<" ++ tagName ++ concat (map (" " ++) attributes) ++ ">")
iShow (EEnd tagName) = cyan ("</" ++ tagName ++ ">")
iShow (WhiteSpace defaultValue) = "_"
--iShow (AnyCharBut chars) = "anyCharBut(" ++ show chars ++ ")"
iShow (Link name) = underline $ magenta name
iShow (Tab tabString e) = "(" ++ show tabString ++ ")==>(" ++ sFlatShow e ++ ")"
iShow (Character charset) = show charset
--iShow (JustOutput eString) = green ("-->(" ++ show eString ++ ")")
iShow (Or sequences) = intercalate " |\n         " (show <$> sequences)
iShow EOF = "EOF"


type RuleName = String


type RawRule = (RuleName, (Condition, Sequence))
data Rule = Rule {
    name::String,
    tagName::RuleName,
    --theClass::Class,
    rawSequence::Sequence,
    fullSequence::Sequence,
    condition::Condition,
    isLRecursive::Bool
    } deriving (Eq)


rawRuleShow::RawRule->String
rawRuleShow (name, (condition, sequence)) =
    blue (name) ++ "[" ++ show condition ++ "] => " ++ sShow sequence ++ "\n"

ruleShow::(String,Rule)->String
ruleShow (className, Rule {tagName=name,condition=condition,fullSequence=sequence, isLRecursive=isLRecursive}) =
    (if isLRecursive then magenta "lRecurse: " else "")
    ++ (if (className == name)
            then blue (name)
            else blue ("(" ++ className ++ "," ++ name ++ ")"))
        ++ (if condition /= CnTrue then "[" ++ show condition ++ "]" else "")
        ++ " => " ++ sShow sequence ++ "\n"

ruleMapShow::Map RuleName [Rule]->String
ruleMapShow rm = intercalate "\n" (map ruleShow (concat (map expand (toList rm)))) ++ "\n"

expand::(a, [b])->[(a, b)]
expand (x, []) = []
expand (x, (y:rest)) = (x, y):expand (x, rest)

type ClassName=String

data Name = ClassName | RuleName

type Separator = Sequence

data Class = Class {
    rawRules::[RawRule],
    operators::[OperatorSymbol],
    left::Sequence,
    right::Sequence,
    separator::Separator,
    className::ClassName,
    parentNames::[String]
    } deriving (Eq)

instance Show Class where
    show c = "====[" ++ className c
        ++ (if null (parentNames c) then ":" ++ intercalate "," (parentNames c) else "")
        ++ "]====\n  "
        ++ intercalate "  " (map rawRuleShow (rawRules c))
        ++ "  separator: " ++ sShow (separator c) ++ "\n"
        ++ (if (length (operators c) > 0)
            then "  operators: " ++ intercalate ", " (map sShow (operators c)) ++ "\n" else "")
        ++ "====[/" ++ className c ++ "]===="

data Grammar = Grammar { main::String,
                        classes::[Class] }

instance Show Grammar where
    show g =
        "-----------" ++ replicate (length $ main g) '-' ++ "\n"
        ++ "| main = " ++ main g ++ " |\n"
        ++ "-----------" ++ replicate (length $ main g) '-' ++ "\n\n"
        ++ (intercalate "\n\n" (map show (classes g))) ++ "\n\n"
