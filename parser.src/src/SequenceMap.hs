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
import Data.Map as M

import Grammar
import LeftFactoring

import JDebug

type SequenceMap = Map Name Sequence

formatSequenceMap::SequenceMap->String
formatSequenceMap sMap = concat ((++ "\n\n") <$> formatSubstitution <$> (toList sMap))

formatSubstitution::(String, Sequence)->String
formatSubstitution (name, seq) = name ++ " => " ++ formatSequence seq

addOrIfNecessary::[Sequence]->Sequence
addOrIfNecessary [seq] = seq
addOrIfNecessary [] = []
addOrIfNecessary seqs = [Or seqs]

sequenceMap::Grammar->SequenceMap
sequenceMap g =
    union
        (fromList (fmap classSequence <$> M.toList (classes fixedG)))
        (fmap addOrIfNecessary (fromListWith (++) ((\rule -> (name rule, [fullSequence rule])) <$> allRules)))
            where
                allRules = elems (classes fixedG) >>= rules
                fixedG = removeSepBy g

classSequence::Class->Sequence
classSequence cl = prefix ++ (if length suffix == 0 then [] else [List 0 suffix])
    where
        prefix = case rules cl of
                    [rule] -> fullSequence rule
                    rules -> leftFactor [Or (fullSequence <$> rules)]
        suffix = case suffixRules cl of
                    [rule] -> fullSuffixSequence rule
                    rules -> leftFactor [Or (fullSequence <$> rules)]

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

fullSuffixSequence::Rule->Sequence
fullSuffixSequence rule =
    [InfixTag 0 (name rule)]
    ++ (rawSequence rule)

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
        suffixRules = (\rule -> rule{rawSequence = removeSepByFromSeq g (rawSequence rule)})
                            <$> suffixRules cl,
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
            []]]
repeatWithSeparator g count seq separator =
    seq ++ [List (count -1) (removeSepByFromSeq g (separator++seq))]
--    ++ repeatWithSeparator (count-1) seq separator

seq2Separator::Grammar->Sequence->Sequence
seq2Separator g [Link name] = case M.lookup name (classes g) of
    Nothing -> error "qqqq"
    Just cl -> separator cl
seq2Separator g [Character charset] = []
seq2Separator g [TextMatch _] = []
seq2Separator _ seq = error ("Missing case in seq2Separator: " ++ formatSequence seq)

seq2Left::Grammar->Sequence->Sequence
seq2Left g [Link name] = case M.lookup name (classes g) of
    Nothing -> error "qqqq"
    Just cl -> left cl
seq2Left g [Character charset] = []
seq2Left g [TextMatch _] = []
seq2Left _ seq = error ("Missing case in seq2Left: " ++ show seq)

seq2Right::Grammar->Sequence->Sequence
seq2Right g [Link name] = case M.lookup name (classes g) of
    Nothing -> error "qqqq"
    Just cl -> FallBack:right cl
seq2Right g [Character charset] = [FallBack]
seq2Right g [TextMatch _] = []
seq2Right _ seq = error ("Missing case in seq2Separator: " ++ formatSequence seq)
