-----------------------------------------------------------------------------
--
-- Module      :  ETreeTools
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

module ETreeTools (
    name2Exp
) where

import Prelude hiding (lookup)

import Data.Functor
import Data.Map
import Data.Tree

import Atom as A
import Context
import EnhancedString as E
import Grammar as G
import Lookahead
import TreeTools

import JDebug

name2Exp::Grammar->String->[Exp]
--name2Exp g name = seq2Exp cx [Link name]
name2Exp g name = leftFactor (seq2Exp cx [Link name])
            where cx = grammar2Context g


seq2Exp::Context->Sequence->[Exp]
seq2Exp cx (val@(G.EStart a b c):rest) = [Node{rootLabel=Out (E.EStart a b c), subForest=seq2Exp cx rest}]
seq2Exp cx (val@(G.EEnd name):rest) = [Node{rootLabel=Out (E.EEnd name), subForest=seq2Exp cx rest}]
seq2Exp cx (val@(Attribute name seq):rest) = seq2Exp cx (seq++rest)
--seq2Exp cx (val@(Attribute a b):rest) = [Node{rootLabel=Out (E.VStart a b), subForest=seq2Exp cx rest}]
seq2Exp cx (G.WhiteSpace _:rest) = exps
    where
        exps = [Node{rootLabel=A.Ch (CharSet False [Space]), subForest=exps}] ++ seq2Exp cx rest
seq2Exp cx (Ident:rest) = exps
    where
        exps = [Node{rootLabel=A.Ch (CharSet False [Alphanum]), subForest=exps}] ++ seq2Exp cx rest
seq2Exp cx (Bind:rest) = seq2Exp cx rest
seq2Exp cx (Link name:rest) =
    case lookup name (sequences cx) of
        Nothing -> error ("No sequence named '" ++ name ++ "'")
        Just seq -> seq2Exp cx (seq ++ rest)

seq2Exp cx (TextMatch []:rest) = seq2Exp cx rest
seq2Exp cx (TextMatch (c:cs):rest) = [Node{rootLabel=A.Ch (CharSet False [SingleChar c]), subForest=seq2Exp cx (TextMatch cs:rest)}]
--seq2Exp cx (Link name:rest) = [Node{rootLabel=A.Ch 'q', subForest=seq2Exp cx rest}]

seq2Exp cx (G.SepBy count seq:rest) | count > 0 = jtrace ("count = " ++ show count) $ seq2Exp cx (seq ++ [G.SepBy (count-1) seq] ++ rest)
seq2Exp cx (G.SepBy 0 seq:rest) = seq2Exp cx seq1 ++ seq2Exp cx rest
    where
--    seq2Exp cx (seq ++ [Bind] ++ [List 0 (separator ++ seq)] ++ cleanedRest)
        seq1 = seq ++ [Bind] ++ [SepBy 0 seq] ++ rest
--    where
--        separator = seq2Sep seq
--        cleanedRest = removeBind rest

seq2Exp cx (Or seqs:rest) = (++rest) <$> seqs >>= seq2Exp cx

seq2Exp cx (Character charset:rest) = [Node{rootLabel=A.Ch charset, subForest=seq2Exp cx rest}]

seq2Exp cx [] = [Node{rootLabel=A.Ch (CharSet False [NoChar]), subForest=[]}]
seq2Exp cx seq = error ("Missing case in seq2Exp: " ++ sShow seq)

{--expandSepBy::(Sequence->Sequence)->Sequence->Sequence
expandSepBy seq2Sep (SepBy 0 seq:rest) = [Or [seq ++ [Bind] ++ [List 0 (separator ++ seq)] ++ cleanedRest, cleanedRest]]
    where
        separator = seq2Sep seq
        cleanedRest = removeBind rest
expandSepBy seq2Sep (SepBy count seq:rest) = seq ++ [List (count -1) (separator++seq)] ++ rest
    where separator = seq2Sep seq
expandSepBy seq2Sep (Or seqs:rest) = (Or (expandSepBy seq2Sep <$> seqs)):expandSepBy seq2Sep rest
expandSepBy seq2Sep (first:rest) = first:expandSepBy seq2Sep rest
expandSepBy _ [] = []--}

listMapWith::Ord k=>(a->k)->[a]->Map k [a]
listMapWith f items = fromListWith (++) ((\x -> (f x, [x])) <$> items)

leftFactor::[Exp]->[Exp]
leftFactor [item] = [item{subForest=leftFactor (subForest item)}]
leftFactor items  = addPrefix <$> toList (listMapWith firstMatcher items)
    where
        addPrefix prefix =
            case prefix of
                (_, [item]) -> item{subForest=leftFactor (subForest item)}
                ((x, False), items') ->
                    Node{rootLabel=x, subForest=leftFactor (items' >>= removeFirstMatcher)}
