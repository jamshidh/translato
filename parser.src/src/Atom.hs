-----------------------------------------------------------------------------
--
-- Module      :  Atom
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

module Atom (

) where

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

data Atom =
    Ch String
    | ChType CharSet
    | WhiteSpace String
    | EOF
    | Attribute String Sequence
    | VEnd
    | Bind
    | InfixTag Int String
    | EStart String [String] Condition
    | EEnd String
    | Tab String Sequence deriving (Eq, Ord)

instance Show Atom where show = iShow

sequence2SequenceForest::Sequence->SequenceForest
sequence2SequenceForest [Or seqs] = seqs >>= sequence2SequenceForest
sequence2SequenceForest (Or _:_) = error "An 'Or' is not at the end of a sequence"
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

