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
import Data.Map hiding (map, null, filter)
import Data.Ord

import Grammar

--import JDebug

verifyHead x = if null x then error "leftFactor called with empty sequence" else head x

leftFactor'::Sequence->Sequence
leftFactor' (Or []:rest) = leftFactor rest
leftFactor' (Or [seq]:rest) = leftFactor (seq ++ rest)

leftFactor' (Or items:rest) = --jtrace ("Left factoring: " ++ intercalate "\n  " (map show sequences)) $
    [Or (makeSeq <$> theMap)] ++ rest
    where
        theMap = toList (fromListWith (++) ((\seq -> (verifyHead seq, [tail seq])) <$> items))
        makeSeq (first, rest2) = first:(leftFactor [Or rest2])

leftFactor' (x:rest) = x:leftFactor rest
leftFactor' [] = []

leftFactor = splitFirstTextMatch . leftFactor'

splitFirstTextMatch::Sequence->Sequence
splitFirstTextMatch (Or seqs:rest) = Or ((\seq -> splitMatchUsingPrefixes firstTexts (head seq) ++ tail seq) <$> seqs):rest
    where
        firstTexts = [text|(TextMatch text:_)<-seqs]
        splitMatchUsingPrefixes prefixes (TextMatch text) =
            case filter (`isPrefixOf` text) (filter (/= text) prefixes) of
                [] -> [TextMatch text]
                matchingPrefixes -> [TextMatch minPrefix, TextMatch (drop (length minPrefix) text)]
                    where minPrefix = minimumBy (comparing length) matchingPrefixes
        splitMatchUsingPrefixes prefixes exp = [exp]
splitFirstTextMatch seq = seq



