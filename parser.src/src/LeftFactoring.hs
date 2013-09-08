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
    leftFactorSequenceMap,
    leftTest
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

--import JDebug

verifyHead x = if null x then error "leftFactor called with empty sequence" else head x

normalizedEHead::[Expression]->Maybe Expression
normalizedEHead [] = Nothing
normalizedEHead (WhiteSpace _:_) = Just (WhiteSpace " ")
normalizedEHead x = Just (head x)

leftFactor::SequenceMap->Sequence->Sequence
leftFactor sm =
{--    jtrace ("--------" ++ (formatSequence $ concatOuts (fromJust $ lookup "command" sm))) $
    jtrace ("--------" ++ (formatSequence $ concatOuts $ prepareForLeftFactor sm $ concatOuts (fromJust $ lookup "command" sm))) $
    jtrace ("--------" ++ (formatSequence $ prepareForLeftFactor sm $ concatOuts $ prepareForLeftFactor sm $ concatOuts (fromJust $ lookup "command" sm))) $--}
    leftFactor' . concatOuts . prepareForLeftFactor sm . concatOuts . prepareForLeftFactor sm

leftFactor'::Sequence->Sequence
leftFactor' (Or []:rest) = concatOuts $ leftFactor' rest
leftFactor' (Or [seq]:rest) = concatOuts $ leftFactor' (seq ++ rest)
leftFactor' (Or items:rest) = --jtrace ("Left factoring: " ++ intercalate "\n  " (map show items)) $
    (orIfy (makeSeq <$> theMap)) ++ leftFactor' rest
    where
        theMap = toList (fromListWith (++) (fmap (replicate 1) <$> splitFirstTok <$> items))
        makeSeq::(Maybe Sequence, [Sequence])->Sequence
--        makeSeq (Just [EmptyEStart, exp], [EInfo name atts:oneSeq]) = EStart name atts:exp:oneSeq
        makeSeq (Just [Out [FutureItem Nothing], exp], seqs) | length firstItems == 1 =
            case firstItems of
                [Out [ItemInfo eString]] ->
                    Out eString `prepend` (exp `prepend` (concatOuts $ leftFactor' [Or (tail <$> seqs)]))
                where
                    firstItems = nub [exp2|exp2@(Out _):_<-seqs]
        makeSeq (Just first, rest2) = first +++ (concatOuts $ leftFactor' $ orIfy rest2)
        makeSeq (Nothing, rest2) = orIfy rest2
leftFactor' (x:rest) = x:(concatOuts $ leftFactor' rest)
leftFactor' [] = []

prepareForLeftFactor::SequenceMap->Sequence->Sequence
prepareForLeftFactor sMap [Or seqs] = orIfy $ expandEStart <$> expandToToken <$> seqs
    where
        expandToToken (c:rest) | c `elem` stopList = c:rest
        expandToToken (Link name:rest) =
                case lookup name sMap of
                    Nothing -> error ("Unknown link name in prepareForLeftFactor: " ++ name)
                    Just seq -> expandToToken (seq +++ rest)
        expandToToken (Out eString:rest) = Out eString `prepend` expandToToken rest
        expandToToken (Or seqs:rest) = orIfy (expandToToken <$> seqs) +++ rest
        expandToToken seq = seq
        expandEStart::Sequence->Sequence
        expandEStart (Out eString:Or seqs:rest) = orIfy ((Out eString `prepend`) <$> seqs) +++ rest
        expandEStart (Or seqs:rest) = orIfy (expandEStart <$> seqs) +++ rest
        expandEStart x = x
        stopList = getStopList seqs
prepareForLeftFactor sMap seq = seq

concatOuts::Sequence->Sequence
concatOuts (Out eString1:Out eString2:rest) = concatOuts (Out (eString1 ++ eString2):rest)
concatOuts (Or seqs:rest) = (orIfy $ concatOuts <$> seqs) ++ concatOuts rest
concatOuts (e:rest) = e:concatOuts rest
concatOuts [] = []
--concatOuts seq = error ("Missing case in concatOuts: " ++ show seq)

splitFirstTok::Sequence->(Maybe Sequence, Sequence)
splitFirstTok (Link name:rest) = (Just [Link name], rest)
splitFirstTok (TextMatch text name:rest) = (Just [TextMatch text name], rest)
splitFirstTok (Character charset name:rest) = (Just [Character charset name], rest)
splitFirstTok (List count seq:rest) = (Just [List count seq], rest)
--splitFirstTok (Out estring:rest) = (Just [Out estring], rest)
splitFirstTok (Or seqs:rest) = (Just [Or seqs], rest)
splitFirstTok (FallBack:rest) = (Just [FallBack], rest)
splitFirstTok (WhiteSpace _:rest) = (Just [WhiteSpace " "], rest)
splitFirstTok (Out eString1:Out eString2:rest) = splitFirstTok (Out (eString1 ++ eString2):rest)
splitFirstTok (Out [ItemInfo eString]:rest) = (nextTok, Out [ItemInfo eString]:nextSeq)
    where (nextTok, nextSeq) = splitFirstTok rest
splitFirstTok (Out eString:rest) = (nextTok >>= Just . (Out [FutureItem Nothing]:), Out [ItemInfo eString]:nextSeq)
    where (nextTok, nextSeq) = splitFirstTok rest
splitFirstTok [] = (Nothing, [])
splitFirstTok seq = error ("Missing case in splitFirstTok: " ++ formatSequence seq)

getTheOne::[a]->a
getTheOne [a] = a

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

leftFactorSequenceMap::SequenceMap->SequenceMap
leftFactorSequenceMap sm = fmap (leftFactor sm) sm





getTokens::SequenceMap->Sequence->[String]
getTokens sMap (Link name:rest) =
    case lookup name sMap of
        Nothing -> error ("Unknown link name in getTokens: " ++ name)
        Just seq -> if isTokenSeq seq then [name] else getTokens sMap seq
getTokens sMap (Or seqs:rest) = nub (concat $ getTokens sMap <$> seqs)
getTokens sMap (Out _:rest) = getTokens sMap rest
getTokens _ seq = error ("Missing case in getTokens: " ++ formatSequence seq)

getFirst::Sequence->Expression
getFirst (e@(Link _):_) = e
getFirst (e@(TextMatch _ _):_) = e
getFirst (e@(Character _ _):_) = e
getFirst (_:rest) = getFirst rest

getChainOfFirsts::SequenceMap->Sequence->[Expression]
getChainOfFirsts sm seq =
    case first of
        Link name->case lookup name sm of
            Just seq'->first:getChainOfFirsts sm seq'
            Nothing->error ("Unknown link name in getChainOfFirsts: " ++ name)
        _->[first]
    where first = getFirst seq

getStopList::Ord a=>[[a]]->[a]
getStopList lists = last <$> getPrefixes lists

getPrefixes::(Eq a, Ord a)=>[[a]]->[[a]]
getPrefixes = getLongestPrefix . groupBy ((==) `on` head) . sortBy (compare `on` head)

getLongestPrefix::Eq a=>[[a]]->[a]
getLongestPrefix lists =
    case nub $ listToMaybe <$> lists of
        [Just x]->x:getLongestPrefix (tail <$> lists)
        _->[]

leftTest::Grammar->IO ()
leftTest g =
    do
        let sMap = sequenceMap g
        --putStrLn (formatSequenceMap sMap)
        let seq = fromJust $ lookup "command" sMap
        --putStrLn (formatSequence $ concatOuts $ prepareForLeftFactor sMap seq)
        putStrLn (formatSequence $ prepareForLeftFactor sMap seq)











