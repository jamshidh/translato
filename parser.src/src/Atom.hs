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
    Exp,
    Atom(..),
    expShow,
    safeExpShow
) where

import Prelude hiding (lookup)

import Data.Functor
import Data.List hiding (lookup)
import Data.Map hiding (filter, map, null, union)
import Data.Tree

import CharSet
import Colors
import EnhancedString hiding (VEnd, InfixTag, EStart, EEnd, Ch)
import TreeTools
import XPath

import JDebug

type Exp = Tree Atom

data Atom =
    Ch Char
    | ChType CharSet
    | WhiteSpace String
    | EOF
    | Out EChar
    {--| Attribute String
    | VEnd
    | Bind
    | InfixTag Int String
    | EStart String [String] Condition
    | EEnd String
    | Tab String--}
        deriving (Eq, Ord)

instance Show Atom where show = atomShow

tabRight::String->String
tabRight ('\n':'|':rest) = "\n                |" ++ tabRight rest
tabRight ('\n':'+':rest) = "\n                +" ++ tabRight rest
tabRight ('\n':'`':rest) = "\n                `" ++ tabRight rest
tabRight ('\n':rest) = "\n            " ++ tabRight rest
tabRight (c:rest) = c:tabRight rest
tabRight [] = []

expShow::Exp->String
--expShow 0 _ = ""
--expShow count item =
--    jtrace ("abcd: " ++ show (rootLabel item) ++ " " ++ show (length (subForest item))) $
--    atomShow (rootLabel item) ++ " " ++ (subForest item >>= expShow (count - 1))
expShow e = tabRight ((if length stringTree > 1 then "\n" else "") ++ stringTree)
    where stringTree = drawTree ((show `fmap` (cleanTree e)))

safeExpShow::Int->Exp->String
safeExpShow num = expShow . treeTake num

atomShow::Atom->String
atomShow (Ch c) = green ("'" ++ [c] ++ "'")
atomShow (ChType charset) = show charset
atomShow (Out ec) = "Out(" ++ show ec ++ ")"
atomShow (WhiteSpace defaultValue) = "_"
atomShow EOF = "EOF"

