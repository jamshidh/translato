-----------------------------------------------------------------------------
--
-- Module      :  Lookahead
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

module Lookahead (
    chooseOne
) where

import Prelude hiding (lookup)

import Data.Char
import Data.Functor
import Data.List hiding (lookup)
import Data.Map hiding (filter, map)
import Data.Tree

import CharSet
import Context
import Grammar
import LeftFactoring
import qualified LString as LS
import LString (LString)

import JDebug

fst3 (a, _, _) = a
snd3 (_, b, _) = b

firstMatcher::Tree Expression->[(Expression, Bool)]
firstMatcher Node{rootLabel=TextMatch text} = [(TextMatch text, False)]
firstMatcher Node{rootLabel=AStart _, subForest=[next]} = firstMatcher next
firstMatcher Node{rootLabel=WhiteSpace _, subForest=[next]} =
    (\item -> (fst item, True)) <$> result
        where result = firstMatcher next
firstMatcher Node{rootLabel=EStart _ _, subForest=[next]} = firstMatcher next
firstMatcher Node{rootLabel=EEnd _, subForest=[next]} = firstMatcher next
firstMatcher tree = error ("Missing case in firstMatcher: " ++ show tree)

check::LString->(Expression, Bool)->Bool
check s x@(_, True) | isSpace (LS.head s) = check (LS.tail s) x
check s (TextMatch text, _) = text `LS.isPrefixOf` s
check s (Character charset, _) = LS.head s `isIn` charset

checkList::LString->[(Expression, Bool)]->Bool
checkList s items = or (check s <$> items)

chooseOne::Forest Expression->LString->Tree Expression
chooseOne [tree] s = tree
chooseOne trees s = --jtrace ("Choice: " ++ show (length sequences)) $
    case filter (checkList s . firstMatcher) trees of
        [] -> error "Nothing matched in chooseOne"
        [sequence] -> sequence
        _ -> error (
                    "multiple things matched in chooseOne:"
                        ++ concat (("\n--------------\n" ++) <$> (show <$> trees))
                        ++ "\ns = " ++ LS.string s)

maximumsUsing::Ord b=>(a->b)->[a]->[a]
maximumsUsing f list = filter (\x -> f x == max) list
    where max = maximum (f <$> list)
