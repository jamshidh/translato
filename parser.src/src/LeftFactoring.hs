{-# OPTIONS_GHC -Wall #-}
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
    leftFactor,
    leftFactorSequenceMap
) where

import Prelude hiding (lookup)

import Data.Functor
import Data.Function
import Data.List hiding (lookup)
import Data.Map hiding (map, null, foldl')
import Data.Maybe

import EnhancedString
import Format
import Grammar
import GrammarTools
import SequenceMap

--import JDebug

leftFactor::Bool->SequenceMap->Sequence->Sequence
leftFactor shouldExpandLinks sm = leftFactor' . (prepareForLeftFactor sm ? shouldExpandLinks)
    where
        f ? condition = if condition then f else id

leftFactor'::Sequence->Sequence
leftFactor' (Or []:rest) = leftFactor' rest
leftFactor' (Or [sq]:rest) = leftFactor' (sq ++ rest)
leftFactor' (Or items:rest) =
    orify (recombine <$> sortAndGroupUsing factorClass (splitFirstTok <$> items)) ++ leftFactor' rest
    where
        factorClass fp = (firstTok fp, null $ outValue fp)  -- Used to fingerprint the token

leftFactor' (x:rest) = x:leftFactor' rest
leftFactor' [] = []

sortAndGroupUsing::(Eq b, Ord b)=>(a->b)->[a]->[[a]]
sortAndGroupUsing f = groupBy ((==) `on` f) . sortBy (compare `on` f)



---------------------------

--This first section is a set of tools to remove the initial 'token' from a sequence, including
--any whitespace or 'Out' data, then recombine once the factoring is done.

--This represents a Sequence, with its first 'token' split away from the rest.
--It is kind of like the first element of a list (split away with a 'head'), except it contains
--extra information held in the first few (non token) items of a sequence, like Out value and default WS.
--Also, note that the analogy breaks apart, in that theRemainder holds the 'tail' information.
data FirstParsedSeq =
    FirstParsedSeq{
        firstTok::Maybe Expression,
        outValue::EString,
        defltWSValue::Maybe DefaultWS,
        theRemainder::Sequence
    } deriving (Show)

--This is like 'head' for FirstParsedSeq.
splitFirstTok::Sequence->FirstParsedSeq
splitFirstTok (expr@(Link _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (expr@(TextMatch _ _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (expr@(Character _ _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (expr@(List _ _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (expr@(SepBy _ _ _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (expr@(Or _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (expr@FallBack:rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (Out eString:rest) = fp{outValue=eString ++ outValue fp}
    where fp = splitFirstTok rest
splitFirstTok (WhiteSpace defltWS:rest) = FirstParsedSeq (Just $ WhiteSpace FutureWS) (e "") (Just defltWS) rest
splitFirstTok [] = FirstParsedSeq{
        firstTok = Nothing,
        outValue = e "",
        defltWSValue = Nothing,
        theRemainder = []
}
splitFirstTok sq = error ("Missing case in splitFirstTok: " ++ format sq)



recombine::[FirstParsedSeq]->Sequence
recombine [] = error "Huh, shouldn't be here"
recombine fps =
    case (defltWSs, outs) of
        ([Just defltWS], [outVal]) -> outify outVal ++ [WhiteSpace defltWS]
            ++ leftFactor' (orify (theRemainder <$> fps))
        ([Just defltWS], _) -> [Out [FutureItem Nothing], WhiteSpace defltWS]
            ++ leftFactor' (orify ((\fp -> Out [ItemInfo (outValue fp)]:theRemainder fp) <$> fps))
        (_, [outVal]) -> outify outVal ++ maybeToList uniqueFirstTok
            ++ leftFactor' (orify ((\fp -> outify (DelayedWS <$> (maybeToList $ defltWSValue fp)) ++ theRemainder fp) <$> fps))
        (_, _) -> [Out [FutureItem Nothing]] ++ maybeToList uniqueFirstTok
            ++ leftFactor' (orify ((\fp -> Out ([ItemInfo (outValue fp)] ++ (DelayedWS <$> (maybeToList $ defltWSValue fp))):theRemainder fp) <$> fps))
    where
        defltWSs = nub (defltWSValue <$> fps)
        outs = nub (outValue <$> fps)
        [uniqueFirstTok] = nub (firstTok <$> fps) -- This must be a single item, or there was a bug
        outify x = if null x then [] else [Out x]


leftFactorSequenceMap::Bool->SequenceMap->SequenceMap
leftFactorSequenceMap shouldExpandLinks sm = leftFactor shouldExpandLinks sm <$> sm




-----------------------------------------------

--This last section is tools to expand as many "Link"s as needed to compare the beginnings of
--sequences in an Or, and factor them out.



prepareForLeftFactor::SequenceMap->Sequence->Sequence
prepareForLeftFactor sMap [Or sequences] = orify $ expandEStart <$> expandToToken <$> sequences
    where
        expandToToken::Sequence->Sequence
        expandToToken (c:rest) | c `elem` stopList = c:rest
        expandToToken (Link linkName:rest) =
                case lookup linkName sMap of
                    Nothing -> error ("Unknown link name in prepareForLeftFactor: " ++ linkName)
                    Just sq -> expandToToken (sq ++ rest)
        expandToToken (Out eString:rest) = Out eString `prepend` expandToToken rest
        expandToToken (Or seqs:rest) = orify (expandToToken <$> seqs) ++ rest
        expandToToken sq = sq

        expandEStart::Sequence->Sequence
        expandEStart (Out eString:Or seqs:rest) = orify ((Out eString `prepend`) <$> seqs) ++ rest
        expandEStart (Or seqs:rest) = orify (expandEStart <$> seqs) ++ rest
        expandEStart x = x

        stopList = getStopList (getChainOfFirsts sMap =<< sequences)
prepareForLeftFactor _ sq = sq




getFirst::Sequence->Maybe [Expression]
getFirst [] = Nothing
getFirst (Or seqs:_) = concat <$> sequence (getFirst <$> seqs)
getFirst (expr@(Link _):_) = Just [expr]
getFirst (expr@(TextMatch _ _):_) = Just [expr]
getFirst (expr@(Character _ _):_) = Just [expr]
getFirst (SepBy 0 _ _:_) = error "getFirst doesn't handle 'SepBy 0' (yet?)."
getFirst (SepBy minCount sq sep:rest) = getFirst (sq ++ SepBy (minCount-1) sq sep:rest)
getFirst (_:rest) = getFirst rest

getChainOfFirsts::SequenceMap->Sequence->[[Expression]]
getChainOfFirsts _ [] = error "getChainOfFirsts called with empty sequence"
getChainOfFirsts sm sq = expr2ChainOfFirsts =<< firsts
    where
        expr2ChainOfFirsts::Expression->[[Expression]]
        expr2ChainOfFirsts expr@(Link linkName) =
            (++ [expr]) <$> getChainOfFirsts sm (linkName2Seq sm linkName)
        expr2ChainOfFirsts expr = [[expr]]
        firsts = case getFirst sq of
            Nothing -> error ("Error calling getFirst with sq = " ++ format sq)
            Just x -> x

linkName2Seq::SequenceMap->String->Sequence
linkName2Seq sm linkName = case lookup linkName sm of
            Just sq->sq
            Nothing->error ("Unknown link name in getChainOfFirsts: " ++ linkName)

removeDuplicateTails::[Sequence]->[Sequence]
removeDuplicateTails seqs =
    removeDuplicateTailsOnGroup =<< (groupBy ((==) `on` last) $ sortBy (compare `on` last) seqs)
    where
        removeDuplicateTailsOnGroup::[Sequence]->[Sequence]
        removeDuplicateTailsOnGroup [x] = [x]
        removeDuplicateTailsOnGroup seqs' = init <$> seqs'


getStopList::[Sequence]->[Expression]
--getStopList::Ord a=>[[a]]->[a]
getStopList lists = last <$> removeDuplicateTails $ getPrefixes lists

getPrefixes::[Sequence]->[Sequence]
--getPrefixes::(Eq a, Ord a)=>[[a]]->[[a]]
getPrefixes seqs = getLongestPrefix <$> (groupBy ((==) `on` head) $ sortBy (compare `on` head) seqs)

getLongestPrefix::[Sequence]->Sequence
--getLongestPrefix::Eq a=>[[a]]->[a]
getLongestPrefix lists =
    case nub $ listToMaybe <$> lists of
        [Just x]->x:getLongestPrefix (tail <$> lists)
        _->[]







