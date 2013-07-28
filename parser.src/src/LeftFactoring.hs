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

import Prelude hiding (lookup)

import Data.Functor
import Data.List hiding (lookup)
import Data.Map hiding (map, null, foldl')
import Data.Maybe

import Grammar
import GrammarTools
import SequenceMap

import JDebug

verifyHead x = if null x then error "leftFactor called with empty sequence" else head x

normalizedEHead::[Expression]->Maybe Expression
normalizedEHead [] = Nothing
normalizedEHead (WhiteSpace _:_) = Just (WhiteSpace " ")
normalizedEHead x = Just (head x)

safeTail::[a]->[a]
safeTail [] = []
safeTail x = tail x


leftFactor::Sequence->Sequence
leftFactor (Or []:rest) = leftFactor rest
leftFactor (Or [seq]:rest) = leftFactor (seq ++ rest)
leftFactor (Or items:rest) = --jtrace ("Left factoring: " ++ intercalate "\n  " (map show items)) $
    (addOrIfNecessary (makeSeq <$> theMap)) ++ rest
    where
        theMap = toList (fromListWith (++) ((\seq -> (normalizedEHead seq, [safeTail seq])) <$> items))
        makeSeq (Just first, rest2) = first:(leftFactor (addOrIfNecessary rest2))
        makeSeq (Nothing, rest2) = addOrIfNecessary rest2
leftFactor (x:rest) = x:leftFactor rest
leftFactor [] = []




isTokenSeq::Sequence->Bool
isTokenSeq (Link "ident":rest) = isTokenSeq rest
isTokenSeq (Link _:rest) = False
isTokenSeq (Or seqs:rest) = and (isTokenSeq <$> seqs)
isTokenSeq (_:rest) = isTokenSeq rest
isTokenSeq [] = True

isToken::SequenceMap->String->Bool
isToken sMap name =
    case lookup name sMap of
        Nothing -> error ("Unknown link name in isTokens: " ++ name)
        Just seq -> isTokenSeq seq

getTokens::SequenceMap->Sequence->[String]
getTokens sMap (Link name:rest) =
    case lookup name sMap of
        Nothing -> error ("Unknown link name in getTokens: " ++ name)
        Just seq -> if isTokenSeq seq then [name] else getTokens sMap seq
getTokens sMap (Or seqs:rest) = nub (concat $ getTokens sMap <$> seqs)
getTokens sMap (EStart _ _:rest) = getTokens sMap rest
getTokens sMap (AStart _:rest) = getTokens sMap rest
getTokens _ seq = error ("Missing case in getTokens: " ++ formatSequence seq)

derivative::SequenceMap->String->Sequence->Maybe Sequence
derivative _ name2 (Link name1:rest) | name1 == name2 = Just rest
derivative sMap name2 (Link name1:rest) | isToken sMap name1 = Nothing
derivative sMap name2 (Link name1:rest) =
    case lookup name1 sMap of
        Nothing -> error ("Unknown link name in derivative: " ++ name1)
        Just seq -> derivative sMap name2 (seq ++ rest)
derivative sMap name (EStart tagName _:rest) =
    case derivative sMap name rest of
        Nothing -> Nothing
        Just seq -> Just (EStart "qqqq" []:seq)
derivative sMap name (Or seqs:rest) = Just (addOrIfNecessary [justSeq|Just justSeq<-derivative sMap name <$> seqs])
derivative _ name seq = error ("Missing case in derivative: D[" ++ formatSequence seq ++ ", " ++ show name ++ "]")

remainder::SequenceMap->String->Sequence->Maybe Sequence
remainder _ name2 (Link name1:_) | name1 == name2 = Nothing
remainder sMap name2 (Link name1:rest)
    | getTokens sMap [Link name1] `intersect` getTokens sMap [Link name2] == [] = Just (Link name1:rest)
remainder sMap name2 seq@(Link name1:_) | isToken sMap name1 = Just seq
remainder sMap name2 (Link name1:rest) =
    case lookup name1 sMap of
        Nothing -> error ("Unknown link name in derivative: " ++ name1)
        Just seq -> remainder sMap name2 (seq ++ rest)
remainder sMap name (EStart tagName attributes:rest) =
    case remainder sMap name rest of
        Nothing -> Nothing
        Just seq -> Just (EStart tagName attributes:seq)
remainder sMap name (Or seqs:rest) = Just (addOrIfNecessary [justSeq|Just justSeq<-(remainder sMap name) <$> seqs])
remainder _ name seq = error ("Missing case in remainder: seq=" ++ formatSequence seq ++ ", name=" ++ show name)

leftFactor2::SequenceMap->Sequence->Sequence
leftFactor2 sMap (Or seqs:rest) = addOrIfNecessary (derivatives ++ [item|Just item<-remainders]) ++ rest
    where
        tokens = (\seq -> (getTokens sMap seq, seq)) <$> seqs
        tok2Seq = fmap addOrIfNecessary <$> (toList $ fromListWith (++) <$> concat $ (\(toks, seq)-> (\tok -> (tok, [seq])) <$> toks) <$> tokens)
        rewriteDerivative (name, seq) =
            {--if length seq == 1
                then seq
                else --}
                    [Link name] ++ (fromJust $ derivative sMap name seq)
        derivatives = rewriteDerivative <$> tok2Seq
        remainders = [] --(\(seqToks, seq) -> (foldl' (>>=) (Just seq) ((remainder sMap) <$> seqToks))) <$> tokens
leftFactor2 _ seq = seq

gI = fmap fixG (loadGrammar "../../qqqq.spec")

smI = fmap (sequenceMap . fixG) gI

sq = [Or [[Link "num"],[Link "command"]]]

sqs = [[Link "num"],[Link "command"]]

tks sm = (\seq -> (getTokens sm seq, seq)) <$> sqs

remainders sm =
    (\(seqToks, seq) -> (foldl' (>>=) (Just seq) ((remainder sm) <$> seqToks))) <$> tks sm

tok2Seq sm = fmap addOrIfNecessary <$> (toList $ fromListWith (++) <$> concat $ (\(toks, seq)-> (\tok -> (tok, [seq])) <$> toks) <$> tks sm)

derivatives sm = (\(name, seq) -> ([Link name] ++ (fromJust $ derivative sm name seq))) <$> tok2Seq sm






