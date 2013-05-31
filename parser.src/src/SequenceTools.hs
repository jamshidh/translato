-----------------------------------------------------------------------------
--
-- Module      :  SequenceTools
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

module SequenceTools (
    expandSepBy,
    expandList
) where

import Data.Functor

import Grammar

removeBind::Sequence->Sequence
removeBind (Bind:rest)=removeBind rest
removeBind (x:rest) = x:removeBind rest
removeBind [] = []

expandSepBy::(Sequence->Sequence)->Sequence->Sequence
expandSepBy seq2Sep (SepBy 0 seq:rest) = [Or [seq ++ [Bind] ++ [List 0 (separator ++ seq)] ++ cleanedRest, cleanedRest]]
    where
        separator = seq2Sep seq
        cleanedRest = removeBind rest
expandSepBy seq2Sep (SepBy count seq:rest) = seq ++ [List (count -1) (separator++seq)] ++ rest
    where separator = seq2Sep seq
expandSepBy seq2Sep (Or seqs:rest) = (Or (expandSepBy seq2Sep <$> seqs)):expandSepBy seq2Sep rest
expandSepBy seq2Sep (first:rest) = first:expandSepBy seq2Sep rest
expandSepBy _ [] = []

expandList::Sequence->Sequence
expandList (List 0 seq:rest) = expanded
    where
        expanded = [Or [seq ++ [Bind] ++ expanded, cleanedRest]]
        cleanedRest = removeBind rest

expandList (List count seq:rest) = seq ++ [List (count -1) seq] ++ rest
expandList (Or seqs:rest) = (Or (expandList <$> seqs)):expandList rest
expandList (first:rest) = first:expandList rest
expandList [] = []

expandFirstLink::Sequence->Sequence
expandFirstLink (Or seqs:rest) = (Or (expandList <$> seqs)):expandList rest
expandFirstLink (first:rest) = first:expandList rest
expandFirstLink [] = []
