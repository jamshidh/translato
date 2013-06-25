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
firstMatcher Node{rootLabel=ChType charset} = (ChType charset, False)
firstMatcher Node{rootLabel=Out _, subForest=[rest]} = firstMatcher rest
firstMatcher Node{rootLabel=Out _, subForest=[]} = (EOF, False)
firstMatcher Node{rootLabel=WhiteSpace _, subForest=[rest]} = (\item -> (fst item, True)) result
    where result = firstMatcher rest
firstMatcher exp = error ("Missing case in firstMatcher: " ++ show exp)

removeFirstMatcher::Exp->[Exp]
removeFirstMatcher = removeFirstMatcher' 2

removeFirstMatcher'::Int->Exp->[Exp]
removeFirstMatcher' 0 _ = error "Dug too deep into removeFirstMatcher'"
removeFirstMatcher' count Node{rootLabel=Ch c, subForest=rest} = jtrace "a" $
    rest
removeFirstMatcher' count Node{rootLabel=ChType charset, subForest=rest} = jtrace "b" $
    rest
removeFirstMatcher' count node@Node{rootLabel=Out _, subForest=[rest]} = jtrace "c" $
    [node{subForest=removeFirstMatcher' (count-1) rest}]
--removeFirstMatcher' Node{rootLabel=Out _, subForest=[]} = (EOF, False)
--removeFirstMatcher' Node{rootLabel=WhiteSpace _, subForest=[rest]} = (\item -> (fst item, True)) result
--    where result = removeFirstMatcher' rest
removeFirstMatcher' count exp = error ("Missing case in removeFirstMatcher': " ++ show exp)


check::LString->(Atom, Bool)->Bool
check s (EOF, _) = LS.null s
check s _ | LS.null s = False
check s x@(_, True) | isSpace (LS.head s) = check (LS.tail s) x
check s (Ch c, _) = c == LS.head s
check s (ChType charset, _) = LS.head s `isIn` charset

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
