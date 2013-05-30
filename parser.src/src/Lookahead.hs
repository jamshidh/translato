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

import CharSet
import Context
import Grammar
import LeftFactoring
import qualified LString as LS
import LString (LString)

import JDebug

fst3 (a, _, _) = a
snd3 (_, b, _) = b

firstMatcher::Context->Sequence->[(Expression, Bool)]
firstMatcher cx seq@(TextMatch text:_) = [(TextMatch text, False)]
firstMatcher cx seq@(Ident:_) = [(Ident, False)]
firstMatcher cx(Attribute _ seq:rest) = firstMatcher cx (seq ++ rest)
firstMatcher cx (Link name:rest) = case lookup name (rules cx) of
    Nothing -> error "qqqq"
    Just [rule] -> firstMatcher cx (fullSequence rule ++ rest)
    Just rules -> firstMatcher cx (leftFactor (Or ((\item -> (1, fullSequence item)) <$> rules):rest))
firstMatcher cx seq@(WhiteSpace _:rest) = (\item -> (fst item, True)) <$> result
    where result = firstMatcher cx rest
firstMatcher cx (EStart _ _ _:rest) = firstMatcher cx rest
firstMatcher cx (EEnd _:rest) = firstMatcher cx rest
firstMatcher cx (SepBy count seq:rest) | count > 0 = firstMatcher cx seq
firstMatcher cx (List count seq:rest) | count > 0 = firstMatcher cx seq
firstMatcher cx (List count seq:rest)  = firstMatcher cx [Or [(1, seq), (2, rest)]]
firstMatcher cx seq@(Character charset:rest) = [(Character charset, False)]
firstMatcher cx (Or items:rest) = items >>= (\(p, seq) -> (firstMatcher cx (seq ++ rest)))
    where changePriority p (a, b, c) = (p, b, c)
firstMatcher cx seq = error ("Missing case in firstMatcher: " ++ show (head seq))

check::LString->(Expression, Bool)->Bool
check s x@(_, True) | isSpace (LS.head s) = check (LS.tail s) x
check s (TextMatch text, _) = text `LS.isPrefixOf` s
check s (Character charset, _) = LS.head s `isIn` charset
check s (Ident, _) = isAlpha $ LS.head s

checkList::LString->[(Expression, Bool)]->Bool
checkList s items = or (check s <$> items)

chooseOne::Context->[(Int, Sequence)]->LString->Sequence
chooseOne cx [(_, seq)] s = seq
chooseOne cx items s = --jtrace ("Choice: " ++ show (length sequences)) $
    case filter (checkList s . firstMatcher cx . snd) items of
        [] -> error "Nothing matched in chooseOne"
        [(_, sequence)] -> sequence
        items -> case maximumsUsing fst items of
            [(_, sequence)] -> sequence
            _ -> error (
                    "multiple things matched in chooseOne:"
                        ++ concat (("\n--------------\n" ++) <$> (show <$> items))
                        ++ "\ns = " ++ LS.string s)

maximumsUsing::Ord b=>(a->b)->[a]->[a]
maximumsUsing f list = filter (\x -> f x == max) list
    where max = maximum (f <$> list)
