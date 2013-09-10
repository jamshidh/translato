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
import Grammar
import GrammarTools
import SequenceMap

import JDebug

leftFactor::SequenceMap->Sequence->Sequence
leftFactor sm =
{--    jtrace ("--------" ++ (formatSequence $ concatOuts (fromJust $ lookup "command" sm))) $
    jtrace ("--------" ++ (formatSequence $ concatOuts $ prepareForLeftFactor sm $ concatOuts (fromJust $ lookup "command" sm))) $
    jtrace ("--------" ++ (formatSequence $ prepareForLeftFactor sm $ concatOuts $ prepareForLeftFactor sm $ concatOuts (fromJust $ lookup "command" sm))) $--}
    leftFactor' . concatOuts . prepareForLeftFactor sm . concatOuts . prepareForLeftFactor sm

leftFactor'::Sequence->Sequence
leftFactor' (Or []:rest) = concatOuts $ leftFactor' rest
leftFactor' (Or [sq]:rest) = concatOuts $ leftFactor' (sq ++ rest)
leftFactor' (Or items:rest) = --jtrace ("Left factoring: " ++ intercalate "\n  " (map show items)) $
    (orIfy (makeSeq <$> theMap)) ++ leftFactor' rest
    where
        theMap = toList (fromListWith (++) (fmap (replicate 1) <$> splitFirstTok <$> items))
        makeSeq::(Maybe Sequence, [Sequence])->Sequence
--        makeSeq (Just [EmptyEStart, exp], [EInfo name atts:oneSeq]) = EStart name atts:exp:oneSeq
        makeSeq (Just [Out [FutureItem Nothing], expr], seqs) | length firstItems == 1 =
            case firstItems of
                [Out [ItemInfo eString]] ->
                    Out eString `prepend` (expr `prepend` (concatOuts $ leftFactor' [Or (tail <$> seqs)]))
                where
                    firstItems = nub [exp2|exp2@(Out _):_<-seqs]
        makeSeq (Just first, rest2) = first +++ (concatOuts $ leftFactor' $ orIfy rest2)
        makeSeq (Nothing, rest2) = orIfy rest2
leftFactor' (x:rest) = x:(concatOuts $ leftFactor' rest)
leftFactor' [] = []

prepareForLeftFactor::SequenceMap->Sequence->Sequence
prepareForLeftFactor sMap [Or sequences] = orIfy $ expandEStart <$> expandToToken <$> sequences
    where
        expandToToken (c:rest) | c `elem` stopList = c:rest
        expandToToken (Link linkName:rest) =
                case lookup linkName sMap of
                    Nothing -> error ("Unknown link name in prepareForLeftFactor: " ++ linkName)
                    Just sq -> expandToToken (sq +++ rest)
        expandToToken (Out eString:rest) = Out eString `prepend` expandToToken rest
        expandToToken (Or seqs:rest) = orIfy (expandToToken <$> seqs) +++ rest
        expandToToken sq = sq
        expandEStart::Sequence->Sequence
        expandEStart (Out eString:Or seqs:rest) = orIfy ((Out eString `prepend`) <$> seqs) +++ rest
        expandEStart (Or seqs:rest) = orIfy (expandEStart <$> seqs) +++ rest
        expandEStart x = x
        stopList = getStopList sequences
prepareForLeftFactor _ sq = sq

concatOuts::Sequence->Sequence
concatOuts (Out eString1:Out eString2:rest) = concatOuts (Out (eString1 ++ eString2):rest)
concatOuts (Or seqs:rest) = (orIfy $ concatOuts <$> seqs) ++ concatOuts rest
concatOuts (expr:rest) = expr:concatOuts rest
concatOuts [] = []
--concatOuts seq = error ("Missing case in concatOuts: " ++ show seq)

splitFirstTok::Sequence->(Maybe Sequence, Sequence)
splitFirstTok (Link linkName:rest) = (Just [Link linkName], rest)
splitFirstTok (TextMatch text itemName:rest) = (Just [TextMatch text itemName], rest)
splitFirstTok (Character charset itemName:rest) = (Just [Character charset itemName], rest)
splitFirstTok (List count sq:rest) = (Just [List count sq], rest)
splitFirstTok (SepBy count sq sep:rest) = (Just [SepBy count sq sep], rest)
--splitFirstTok (Out estring:rest) = (Just [Out estring], rest)
splitFirstTok (Or seqs:rest) = (Just [Or seqs], rest)
splitFirstTok (FallBack:rest) = (Just [FallBack], rest)
splitFirstTok (WhiteSpace deflt:rest) = (Just [WhiteSpace " "], rest)
splitFirstTok (Out eString1:Out eString2:rest) = splitFirstTok (Out (eString1 ++ eString2):rest)
splitFirstTok (Out [ItemInfo eString]:rest) = (nextTok, Out [ItemInfo eString]:nextSeq)
    where (nextTok, nextSeq) = splitFirstTok rest
splitFirstTok (Out eString:rest) = (nextTok >>= Just . (Out [FutureItem Nothing]:), Out [ItemInfo eString]:nextSeq)
    where (nextTok, nextSeq) = splitFirstTok rest
splitFirstTok [] = (Nothing, [])
splitFirstTok sq = error ("Missing case in splitFirstTok: " ++ formatSequence sq)

leftFactorSequenceMap::SequenceMap->SequenceMap
leftFactorSequenceMap sm = fmap (leftFactor sm) sm





getFirst::Sequence->Expression
getFirst (expr@(Link _):_) = expr
getFirst (expr@(TextMatch _ _):_) = expr
getFirst (expr@(Character _ _):_) = expr
getFirst (_:rest) = getFirst rest

getChainOfFirsts::SequenceMap->Sequence->[Expression]
getChainOfFirsts sm sq =
    case first of
        Link linkName->case lookup linkName sm of
            Just seq'->first:getChainOfFirsts sm seq'
            Nothing->error ("Unknown link name in getChainOfFirsts: " ++ linkName)
        _->[first]
    where first = getFirst sq

getStopList::Ord a=>[[a]]->[a]
getStopList lists = last <$> getPrefixes lists

getPrefixes::(Eq a, Ord a)=>[[a]]->[[a]]
getPrefixes = getLongestPrefix . groupBy ((==) `on` head) . sortBy (compare `on` head)

getLongestPrefix::Eq a=>[[a]]->[a]
getLongestPrefix lists =
    case nub $ listToMaybe <$> lists of
        [Just x]->x:getLongestPrefix (tail <$> lists)
        _->[]







