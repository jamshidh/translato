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
    chooseOne,
    firstMatcher,
    removeFirstMatcher
) where

import Prelude hiding (lookup)

import Data.Char
import Data.Functor
import Data.List hiding (lookup)
import Data.Map hiding (filter, map)
import Data.Maybe
import Data.Tree

import CharSet
import Context
import Atom
--import Grammar
import LeftFactoring
import qualified LString as LS
import LString (LString)
import TreeTools

import JDebug

fst3 (a, _, _) = a
snd3 (_, b, _) = b

firstMatcher::Exp->(Atom, Bool)
firstMatcher Node{rootLabel=Ch c} = (Ch c, False)
firstMatcher Node{rootLabel=Out _, subForest=[rest]} = firstMatcher rest
firstMatcher exp@Node{rootLabel=Out _, subForest=[]} = error ("The tree has a choice of 'Out', but no 'Ch' before it ends: " ++ show (treeTake 4 exp))
firstMatcher exp = error ("Missing case in firstMatcher: " ++ expShow (treeTake 4 exp))

removeFirstMatcher::Exp->[Exp]
removeFirstMatcher = removeFirstMatcher' 2

removeFirstMatcher'::Int->Exp->[Exp]
removeFirstMatcher' 0 _ = error "Dug too deep into removeFirstMatcher'"
removeFirstMatcher' count Node{rootLabel=Ch charset, subForest=rest} =
    rest
removeFirstMatcher' count node@Node{rootLabel=Out _, subForest=[rest]} =
    [node{subForest=removeFirstMatcher' (count-1) rest}]
--removeFirstMatcher' Node{rootLabel=Out _, subForest=[]} = (EOF, False)
--removeFirstMatcher' Node{rootLabel=WhiteSpace _, subForest=[rest]} = (\item -> (fst item, True)) result
--    where result = removeFirstMatcher' rest
removeFirstMatcher' count exp = error ("Missing case in removeFirstMatcher': " ++ show (treeTake 4 exp))


check::LString->(Atom, Bool)->Bool
--check s _ | LS.null s = False
--check s x@(_, True) | isSpace (LS.head s) = check (LS.tail s) x
check s (Ch charset, _) = listToMaybe (LS.string s) `isIn` charset

chooseOne::[Exp]->LString->Exp
chooseOne [exp] s = exp
chooseOne seqs s =
    case filter (check s . firstMatcher) seqs of
        [] -> error ("Nothing matched in chooseOne, s=" ++ show (LS.string s) ++ ", exp = " ++ ((forestTake 4 seqs) >>= expShow))
        [singleSeq] -> singleSeq
        seqs -> error (
                    "multiple things matched in chooseOne:"
                         ++ (seqs >>= safeExpShow 4)
                         ++ "\ns = " ++ LS.string s
                        )

maximumsUsing::Ord b=>(a->b)->[a]->[a]
maximumsUsing f list = filter (\x -> f x == max) list
    where max = maximum (f <$> list)
