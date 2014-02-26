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
leftFactor shouldExpandLinks sm sq = --jtrace ("\n================\n" ++ (show $ prepareForLeftFactor sm sq) ++ "\n---------------------\n") $
  leftFactor' shouldExpandLinks sm $ (prepareForLeftFactor sm ? shouldExpandLinks) sq
--leftFactor shouldExpandLinks sm = leftFactor' shouldExpandLinks sm . (prepareForLeftFactor sm ? shouldExpandLinks)
    where
        f ? condition = if condition then f else id

leftFactor'::Bool->SequenceMap->Sequence->Sequence
--leftFactor' shouldExpandLinks sm [Or [[], []]] = []
leftFactor' shouldExpandLinks sm (Or []:rest) = leftFactor' shouldExpandLinks sm rest
leftFactor' shouldExpandLinks sm (Or [sq]:rest) = leftFactor' shouldExpandLinks sm (sq ++ rest)
leftFactor' shouldExpandLinks sm (Or items:rest) =
    orify (recombine shouldExpandLinks sm <$> sortAndGroupUsing factorClass (splitFirstTok <$> items)) ++ leftFactor' shouldExpandLinks sm rest
    where
        factorClass fp = firstTok fp  -- Used to fingerprint the token
--        factorClass fp = (firstTok fp, null $ outValue fp)  -- Used to fingerprint the token

leftFactor' shouldExpandLinks sm (x:rest) = x:leftFactor' shouldExpandLinks sm rest
leftFactor' _ _ [] = []

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
    } deriving (Eq, Show)

--This is like 'head' for FirstParsedSeq.
splitFirstTok::Sequence->FirstParsedSeq
splitFirstTok (expr@(Link _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (expr@(TextMatch _ _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (expr@(Character _ _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
--splitFirstTok (expr@(List 0 _):rest) = error "Don't support splitFirstTok for List0"
splitFirstTok (expr@(List _ _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (expr@(SepBy _ _ _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (expr@(Or _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (expr@(Priority _):rest) = FirstParsedSeq (Just expr) (e "") Nothing rest
splitFirstTok (Out eString:rest) = fp{outValue=eString ++ outValue fp}
    where fp = splitFirstTok rest
splitFirstTok (WhiteSpace [] defltWS:rest) = FirstParsedSeq (Just $ WhiteSpace [] FutureWS) (e "") (Just defltWS) rest
splitFirstTok [] = FirstParsedSeq{
        firstTok = Nothing,
        outValue = e "",
        defltWSValue = Nothing,
        theRemainder = []
}
splitFirstTok sq = error ("Missing case in splitFirstTok: " ++ format sq)



recombine::Bool->SequenceMap->[FirstParsedSeq]->Sequence
recombine _ _ [] = error "Huh, shouldn't be here"
recombine shouldExpandLinks sm fps =
    case (uniqueFirstTok, defltWSs, outs) of
        (Nothing, _, _) -> --If there is no firstTok, the remainder of the sequence is just ws and outs....  This is only allowed if all seqs are identical, else the choice is ambiguous.  This is needed if an option appears at the end of two similar production rules (ie- x=>A B?; x=> A C?;, which expands to x=>A Or[B,[]]; x=>A Or[C,[]];, or x=>A Or[B,C,[],[]] after left factoring.... notice the repeat idenitcal []'s).
          case nub fps of
            [uniqueFirstParsedSeq] -> (outify =<< outs) ++ (wsify =<< defltWSs) ++ theRemainder uniqueFirstParsedSeq
            _ -> error "ambiguity in recombine"
        (_, [Just defltWS], [outVal]) -> outify outVal ++ [WhiteSpace [] defltWS]
            ++ leftFactor shouldExpandLinks sm (orify (theRemainder <$> fps))
        (_, [Just defltWS], _) -> [Out [FutureItem Nothing], WhiteSpace [] defltWS]
            ++ leftFactor shouldExpandLinks sm (orify ((\fp -> Out [ItemInfo (outValue fp)]:theRemainder fp) <$> fps))
        (_, _, [outVal]) -> outify outVal ++ maybeToList uniqueFirstTok
            ++ leftFactor shouldExpandLinks sm (orify ((\fp -> outify (DelayedWS <$> (maybeToList $ defltWSValue fp)) ++ theRemainder fp) <$> fps))
        (_, _, _) -> [Out [FutureItem Nothing]] ++ maybeToList uniqueFirstTok
            ++ leftFactor shouldExpandLinks sm (orify ((\fp -> Out ([ItemInfo (outValue fp)] ++ (DelayedWS <$> (maybeToList $ defltWSValue fp))):theRemainder fp) <$> fps))
    where
        defltWSs = nub (defltWSValue <$> fps)
        outs = nub (outValue <$> fps)
        [uniqueFirstTok] = nub (firstTok <$> fps) -- This must be a single item, or there was a bug
        outify x = if null x then [] else [Out x]
        wsify::Maybe DefaultWS->Sequence
        wsify x = case x of 
          Nothing -> [] 
          Just x' -> [WhiteSpace [] x']


leftFactorSequenceMap::Bool->SequenceMap->SequenceMap
leftFactorSequenceMap shouldExpandLinks sm = leftFactor shouldExpandLinks sm <$> sm




-----------------------------------------------

--This last section is tools to expand as many "Link"s as needed to compare the beginnings of
--sequences in an Or, and factor them out.



prepareForLeftFactor::SequenceMap->Sequence->Sequence
prepareForLeftFactor sMap [Or sequences] = orify $ expandEStart <$> expandToToken <$> sequences
    where
        expandToToken::Sequence->Sequence
        expandToToken (Link linkName:rest) 
          | containsMinimalExpansionPoint sMap linkName minimalExpansionPoints =
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

        minimalExpansionPoints = getMinimalExpansionPoints (getChainOfFirsts sMap =<< sequences)
prepareForLeftFactor _ sq = sq



containsMinimalExpansionPoint::SequenceMap->String->[Expression]->Bool
containsMinimalExpansionPoint sMap linkName minimalExpansionPoints =
  case lookup linkName sMap of
    Nothing -> error ("Unknown link name in prepareForLeftFactor: " ++ linkName)
    Just sq -> or ((`elem` concat (getChainOfFirsts sMap sq)) <$> minimalExpansionPoints)

--Note- The "Maybe" was for the case that an expression ended without expecting any characters.
--This doesn't seem to be needed anymore....  Instead I return an [EOE] (which stands for "End Of Element").
--This is a valid possibility, and just indicates that one of the options in the current item is that the
--element has finished, and the only way to disambiguate is to compare the options with remaining characters
--to characters needed after the element (if this is ambiguous, then you have a problem).
--Since this still feels to be on shaky grounds (like everthing in this file module), I will leave
--the Maybe in for now, as well as the check for a Maybe in getChainOfFirsts, but in the future, if this
--seems to be working, feel free to turn the signature of getFirst to "Sequence->[Expression]"
getFirst::Sequence->Maybe [Expression]
getFirst [] = Just [EOE]
getFirst (Or seqs:rest) = concat <$> sequence (getFirst <$> (++ rest) <$> seqs)
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
            Nothing -> error ("Error calling getFirst (perhaps the grammer has a list of items that can be zero length) with sq = " ++ format sq)
            Just x -> x

linkName2Seq::SequenceMap->String->Sequence
linkName2Seq sm linkName = case lookup linkName sm of
            Just sq->sq
            Nothing->error ("Unknown link name in getChainOfFirsts: " ++ linkName)

--I should remove this dead code, but don't want to do so until some time has passed, and I am sure that
--the new algorithm works well.
{-removeDuplicateTails::[Sequence]->[Sequence]
removeDuplicateTails seqs =
    removeDuplicateTailsOnGroup =<< (groupBy ((==) `on` last) $ sortBy (compare `on` last) seqs)
    where
        removeDuplicateTailsOnGroup::[Sequence]->[Sequence]
        removeDuplicateTailsOnGroup [x] = [x]
        removeDuplicateTailsOnGroup seqs' = init <$> seqs'-}


--A minimal expansion point is an expression that might lie in the chain of firsts....  Wherever it does,
--we must expand the first link in the sequence until the "minimal expansion point" bubbles to the top.
--Something is named a "minimal expansion point" because it exists in two different (Or) cases at the begining
--of a sequence, and needs to be left factored out.
getMinimalExpansionPoints::[Sequence]->[Expression]
--getStopList::Ord a=>[[a]]->[a]
getMinimalExpansionPoints lists = last <$> getPrefixes lists

getPrefixes::[Sequence]->[Sequence]
--getPrefixes::(Eq a, Ord a)=>[[a]]->[[a]]
getPrefixes seqs = 
  getLongestPrefix <$> (groupBy ((==) `on` head) $ sortBy (compare `on` head) seqs)

getLongestPrefix::[Sequence]->Sequence
--getLongestPrefix::Eq a=>[[a]]->[a]
getLongestPrefix lists =
    case nub $ listToMaybe <$> lists of
        [Just x]->x:getLongestPrefix (tail <$> lists)
        _->[]







