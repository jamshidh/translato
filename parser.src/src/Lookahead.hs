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
import Data.Function
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
import TreeTools

import JDebug

fst3 (a, _, _) = a
snd3 (_, b, _) = b

firstMatcher::Tree Expression->(Expression, Bool)
firstMatcher Node{rootLabel=TextMatch text} = (TextMatch text, False)
firstMatcher Node{rootLabel=Character charset} = (Character charset, False)
firstMatcher Node{rootLabel=AStart _, subForest=[next]} = firstMatcher next
firstMatcher Node{rootLabel=AEnd, subForest=[next]} = firstMatcher next
firstMatcher Node{rootLabel=WhiteSpace _, subForest=[next]} =
    (\item -> (fst item, True)) result
        where result = firstMatcher next
firstMatcher Node{rootLabel=EStart _ _, subForest=[next]} = firstMatcher next
firstMatcher Node{rootLabel=EEnd _, subForest=[next]} = firstMatcher next
firstMatcher tree =
    error ("Missing case in firstMatcher: " ++ safeDrawETree tree)

check::LString->(Expression, Bool)->Bool
check s x@(_, True) | isSpace (LS.head s) = check (LS.tail s) x
check s (TextMatch text, _) = text `LS.isPrefixOf` s
check s (Character charset, _) = LS.head s `isIn` charset

chooseOne::Forest Expression->LString->Tree Expression
chooseOne [tree] s = tree
chooseOne trees s = --jtrace ("Choice: " ++ show (length sequences)) $
    case removeTextMatchSize <$>
            (maximumsBy fst ((filter ((/= 0) . fst)) (addTextMatchSize s <$> trees))) of
        [] -> case filter (check s . firstMatcher) trees of
                [] -> error "Nothing matched in chooseOne"
                [sequence] -> sequence
                items ->  error (
                            "multiple things matched in chooseOne:"
                                ++ (safeDrawForest (map (fmap show) trees))
                                ++ "\ns = " ++ LS.string s)
        [item] -> item
        items -> error ("multiple TextMatches matched in chooseOne:"
                            ++ (safeDrawForest (map (fmap show) trees))
                            ++ "\ns = " ++ LS.string s)
            where theMaxTextMatchSize = trees

maximumsBy::(Eq a)=>(Ord b)=>(a->b)->[a]->[a]
maximumsBy _ [] = []
maximumsBy f list = filter ((== theMaximum) . f) list
        where theMaximum = maximum (f <$> list)

addTextMatchSize::LS.LString->Tree Expression->(Int, Tree Expression)
addTextMatchSize s tree = (textMatchMatchSize (fst (firstMatcher tree)) s, tree)

removeTextMatchSize::(Int, Tree Expression)->Tree Expression
removeTextMatchSize = snd


textMatchMatchSize::Expression->LS.LString->Int
textMatchMatchSize (TextMatch text) s | text `LS.isPrefixOf` s = length text
textMatchMatchSize _ _ = 0


maximumsUsing::Ord b=>(a->b)->[a]->[a]
maximumsUsing f list = filter (\x -> f x == max) list
    where max = maximum (f <$> list)
