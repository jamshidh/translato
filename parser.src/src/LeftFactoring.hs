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

--import JDebug

verifyHead x = if null x then error "leftFactor called with empty sequence" else head x

leftFactor::Sequence->Sequence
leftFactor (Or []:rest) = leftFactor rest
leftFactor (Or [seq]:rest) = leftFactor (seq ++ rest)

leftFactor (Or items:rest) = --jtrace ("Left factoring: " ++ intercalate "\n  " (map show sequences)) $
    [Or (makeSeq <$> theMap)] ++ rest
    where
        theMap = toList (fromListWith (++) ((\seq -> (verifyHead seq, [tail seq])) <$> items))
        makeSeq (first, rest2) = first:(leftFactor [Or rest2])

leftFactor (x:rest) = x:leftFactor rest
leftFactor [] = []




