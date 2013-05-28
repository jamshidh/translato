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

import Data.List
import Data.Map hiding (map, null)

import Grammar

--import JDebug

verifyHead x = if null x then error "leftFactor called with empty sequence" else head x

leftFactor::Sequence->Sequence
leftFactor (Or []:rest) = leftFactor rest
leftFactor (Or [x]:rest) = leftFactor (x ++ rest)
leftFactor (Or sequences:rest) = --jtrace ("Left factoring: " ++ intercalate "\n  " (map show sequences)) $
    [Or (map (\(first, rest2) -> first:(leftFactor [Or rest2])) (toList theMap))] ++ rest
    where theMap = fromListWith (++) (map (\x -> (verifyHead x, [tail x])) sequences)
leftFactor (x:rest) = x:leftFactor rest
leftFactor [] = []




