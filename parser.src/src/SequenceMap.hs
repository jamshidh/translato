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

import Control.Lens
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
        (fromList (fmap (classSequence g) <$> (M.toList $ g^.classes)))
        (fmap orify (fromListWith (++) ((\rule -> (name rule, [rawSequence rule])) <$> allRules)))
            where
                allRules = elems (g^.classes) >>= (^.rules)

classSequence::Grammar->Class->Sequence
classSequence g cl =
    (if classIsBlocking then [Out [StartBlock]] else [])
        ++ prefix ++ (if length suffix == 0 then [] else [List 0 suffix])
    ++ (if classIsBlocking then [Out [EndBlock]] else [])
    where
        classIsBlocking = isBlockClass g cl
        prefix = orify (nonSelfRule ++ selfRule)
        selfRule::[Sequence]
        selfRule = rawSequence <$> filter ((== cl^.className ) . name) (cl^.rules)
        nonSelfRule::[Sequence]
        nonSelfRule = replicate 1 <$> Link <$>
                            ((filter (/= cl^.className) (nub $ name <$> cl^.rules)) ++ cl^.parentNames)
        suffix = orify (cl^.suffixSeqs)
