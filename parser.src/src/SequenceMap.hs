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
    formatSequenceMap,
    removeSepBy,
    removeSepByFromSeq
) where

import Data.Functor
import Data.List hiding (union)
import Data.Map as M hiding (filter)

import Grammar
import GrammarTools

import JDebug

type SequenceMap = Map Name Sequence

formatSequenceMap::SequenceMap->String
formatSequenceMap sMap = concat ((++ "\n\n") <$> formatSubstitution <$> (toList sMap))

formatSubstitution::(String, Sequence)->String
formatSubstitution (name, seq) = name ++ " => " ++ formatSequence seq

sequenceMap::Grammar->SequenceMap
sequenceMap g =
    union
        (fromList (fmap (classSequence g) <$> M.toList (classes fixedG)))
        (fmap orIfy (fromListWith (++) ((\rule -> (name rule, [fullSequence rule])) <$> allRules)))
            where
                allRules = elems (classes fixedG) >>= rules
                fixedG = removeSepBy g

classSequence::Grammar->Class->Sequence
classSequence g cl = prefix ++ (if length suffix == 0 then [] else [List 0 suffix])
    where
        prefix = orIfy
                    (nonSelfRule ++ selfRule)
        selfRule::[Sequence]
        selfRule = fullSequence <$> (filter ((== className cl) . name) (rules cl))
        nonSelfRule::[Sequence]
        nonSelfRule = replicate 1 <$> Link <$>
                            ((filter (/= className cl) (nub $ name <$> rules cl)) ++ (parentNames cl))
        suffix = orIfy (suffixSeqs cl)

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

removeSepBy::Grammar->Grammar
removeSepBy g =
    g {
        classes= fmap (removeSepByFromClass g) (classes g)
    }

removeSepByFromClass::Grammar->Class->Class
removeSepByFromClass g cl =
    cl {
        rules = (\rule -> rule{rawSequence = removeSepByFromSeq g (rawSequence rule)})
                            <$> rules cl,
        suffixSeqs = removeSepByFromSeq g <$> suffixSeqs cl,
        separator = [], --removeSepByFromSeq g (separator cl),
        left = [], --removeSepByFromSeq g (left cl),
        right = [] --removeSepByFromSeq g (right cl)
    }

removeSepByFromSeq::Grammar->Sequence->Sequence
removeSepByFromSeq g (SepBy count seq:rest) =
    removeSepByFromSeq g (seq2Left g (removeSepByFromSeq g seq))
    ++ repeatWithSeparator g count seq (removeSepByFromSeq g (seq2Separator g seq))
    ++ removeSepByFromSeq g rest
    ++ removeSepByFromSeq g (seq2Right g (removeSepByFromSeq g seq))
removeSepByFromSeq g (List count seq:rest) = List count (removeSepByFromSeq g seq):removeSepByFromSeq g rest
removeSepByFromSeq g (Or seqs:rest) = Or (removeSepByFromSeq g <$> seqs):removeSepByFromSeq g rest
removeSepByFromSeq g (x:rest) = x:removeSepByFromSeq g rest
removeSepByFromSeq _ [] = []

repeatWithSeparator::Grammar->Int->Sequence->Sequence->Sequence
repeatWithSeparator g 0 seq separator =
    [Or [seq ++ [List 0 (separator ++ seq)],
            [FallBack]]]
repeatWithSeparator g count seq separator =
    seq ++ [List (count -1) (removeSepByFromSeq g (separator++seq))]
--    ++ repeatWithSeparator (count-1) seq separator

seq2Separator::Grammar->Sequence->Sequence
seq2Separator g [Link name] = case M.lookup name (classes g) of
    Nothing -> error ("Missing link name in seq2Separator: " ++ name)
    Just cl -> separator cl
seq2Separator g [Character charset] = []
seq2Separator g [TextMatch _] = []
seq2Separator _ seq = error ("Missing case in seq2Separator: " ++ formatSequence seq)

seq2Left::Grammar->Sequence->Sequence
seq2Left g [Link name] = case M.lookup name (classes g) of
    Nothing -> error ("Missing link name in seq2Left: " ++ name)
    Just cl -> left cl
seq2Left g [Character charset] = []
seq2Left g [TextMatch _] = []
seq2Left _ seq = error ("Missing case in seq2Left: " ++ show seq)

seq2Right::Grammar->Sequence->Sequence
seq2Right g [Link name] = case M.lookup name (classes g) of
    Nothing -> error ("Missing link name in seq2Right: " ++ name)
    Just cl -> right cl
seq2Right g [Character charset] = []
seq2Right g [TextMatch _] = []
seq2Right _ seq = error ("Missing case in seq2Separator: " ++ formatSequence seq)
