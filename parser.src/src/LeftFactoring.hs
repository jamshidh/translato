-----------------------------------------------------------------------------
--
-- Module      :  LeftFactoring
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

module LeftFactoring (
    leftFactor
) where

import Data.Functor
import Data.List
import Data.Map hiding (map, null)

import Grammar
import GrammarTools

import JDebug

verifyHead x = if null x then error "leftFactor called with empty sequence" else head x

normalizedEHead::[Expression]->Maybe Expression
normalizedEHead [] = Nothing
normalizedEHead (WhiteSpace _:_) = Just (WhiteSpace " ")
normalizedEHead x = Just (head x)

safeTail::[a]->[a]
safeTail [] = []
safeTail x = tail x


leftFactor::Sequence->Sequence
leftFactor (Or []:rest) = leftFactor rest
leftFactor (Or [seq]:rest) = leftFactor (seq ++ rest)

leftFactor (Or items:rest) = --jtrace ("Left factoring: " ++ intercalate "\n  " (map show items)) $
    (addOrIfNecessary (makeSeq <$> theMap)) ++ rest
    where
        theMap = toList (fromListWith (++) ((\seq -> (normalizedEHead seq, [safeTail seq])) <$> items))
        makeSeq (Just first, rest2) = first:(leftFactor (addOrIfNecessary rest2))
        makeSeq (Nothing, rest2) = addOrIfNecessary rest2

leftFactor (x:rest) = x:leftFactor rest
leftFactor [] = []




