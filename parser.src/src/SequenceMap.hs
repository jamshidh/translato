{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
--
-- Module      :  SequenceMap
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

module SequenceMap (
    SequenceMap,
    sequenceMap,
    formatSequenceMap
) where

import Data.Functor
import Data.List hiding (union)
import Data.Map as M hiding (filter)

import EnhancedString
import Grammar
import GrammarTools

--import JDebug

type SequenceMap = Map Name Sequence

formatSequenceMap::SequenceMap->String
formatSequenceMap sMap = concat ((++ "\n\n") <$> formatSubstitution <$> (toList sMap))

formatSubstitution::(String, Sequence)->String
formatSubstitution (ruleName, sq) = ruleName ++ " => " ++ formatSequence sq

sequenceMap::Grammar->SequenceMap
sequenceMap g =
    union
        (fromList (fmap (classSequence g) <$> (M.toList $ classes g)))
        (fmap orIfy (fromListWith (++) ((\rule -> (name rule, [rawSequence rule])) <$> allRules)))
            where
                allRules = elems (classes g) >>= rules

classSequence::Grammar->Class->Sequence
classSequence g cl =
    (if classIsBlocking then [Out [StartBlock]] else [])
        ++ prefix ++ (if length suffix == 0 then [] else [List 0 suffix])
    ++ (if classIsBlocking then [Out [EndBlock]] else [])
    where
        classIsBlocking = isBlockClass g cl
        prefix = orIfy (nonSelfRule ++ selfRule)
        selfRule::[Sequence]
        selfRule = rawSequence <$> filter ((== className cl) . name) (rules cl)
        nonSelfRule::[Sequence]
        nonSelfRule = replicate 1 <$> Link <$>
                            ((filter (/= className cl) (nub $ name <$> rules cl)) ++ (parentNames cl))
        suffix = orIfy (suffixSeqs cl)
