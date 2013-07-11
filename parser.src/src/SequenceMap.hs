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
import Data.Map as M

import Grammar

type SequenceMap = Map Name Sequence

formatSequenceMap::SequenceMap->String
formatSequenceMap sMap = concat ((++ "\n\n") <$> formatSubstitution <$> (toList sMap))

formatSubstitution::(String, Sequence)->String
formatSubstitution (name, seq) = name ++ " => " ++ formatSequence seq

sequenceMap::Grammar->SequenceMap
sequenceMap g =
    union
        (fromList (fmap classSequence <$> M.toList (classes g)))
        (fromList ((\rule -> (name rule, fullSequence rule)) <$> allRules))
            where
                allRules = elems (classes g) >>= rules

classSequence::Class->Sequence
classSequence cl =
    case classRules of
        [rule] -> fullSequence rule
        _ -> [Or (fullSequence <$> classRules)]
    where
        classRules = rules cl

fullSequence::Rule->Sequence
fullSequence rule =
    [EStart (name rule) (nub (seq2AttNames (rawSequence rule)))]
    ++ (rawSequence rule)
    ++ [EEnd (name rule)]
        where
            seq2AttNames::Sequence->[String]
            seq2AttNames (AStart name:rest) = name:seq2AttNames rest
            seq2AttNames (_:rest) = seq2AttNames rest
            seq2AttNames [] = []


removeQuote::Grammar->Sequence->Sequence
removeQuote g (SepBy 0 seq:rest) =
    [Or [seq ++ [List 0 (separator ++ seq)] ++ removeQuote g rest, removeQuote g rest]]
    where separator = (seq2Separator g) seq;
removeQuote g (SepBy count seq:rest) =
    seq ++ [List (count -1) (separator++seq)] ++ rest
    where separator = (seq2Separator g) seq

seq2Separator::Grammar->Sequence->Sequence
seq2Separator g [Link name] = case M.lookup name (classes g) of
    Nothing -> error "qqqq"
    Just cl -> separator cl
