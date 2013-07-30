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
import Data.List hiding (lookup)
import Data.Map hiding (map, null, foldl')
import Data.Maybe

import EnhancedString
import Grammar
import GrammarTools
import SequenceMap

import JDebug

verifyHead x = if null x then error "leftFactor called with empty sequence" else head x

normalizedEHead::[Expression]->Maybe Expression
normalizedEHead [] = Nothing
normalizedEHead (WhiteSpace _:_) = Just (WhiteSpace " ")
normalizedEHead x = Just (head x)

leftFactor::SequenceMap->Sequence->Sequence
leftFactor sm = leftFactor' . concatOuts . prepareForLeftFactor sm

leftFactor'::Sequence->Sequence
leftFactor' (Or []:rest) = leftFactor' rest
leftFactor' (Or [seq]:rest) = leftFactor' (seq ++ rest)
leftFactor' (Or items:rest) = --jtrace ("Left factoring: " ++ intercalate "\n  " (map show items)) $
    (orIfy (makeSeq <$> theMap)) ++ leftFactor' rest
    where
        theMap = toList (fromListWith (++) (fmap (replicate 1) <$> splitFirstTok <$> items))
        makeSeq::(Maybe Sequence, [Sequence])->Sequence
--        makeSeq (Just [EmptyEStart, exp], [EInfo name atts:oneSeq]) = EStart name atts:exp:oneSeq
        makeSeq (Just [Out [FutureItem], exp], seqs) | length firstItems == 1 =
            case firstItems of
                [Out [ItemInfo eString]] -> Out eString:exp:(leftFactor' [Or (tail <$> seqs)])
                where
                    firstItems = nub [exp2|exp2@(Out _):_<-seqs]
        makeSeq (Just first, rest2) = first ++ (leftFactor' $ orIfy rest2)
        makeSeq (Nothing, rest2) = orIfy rest2
leftFactor' (x:rest) = x:leftFactor' rest
leftFactor' [] = []

prepareForLeftFactor::SequenceMap->Sequence->Sequence
prepareForLeftFactor sMap [Or seqs] = orIfy $ expandEStart <$> expandToToken <$> seqs
    where
        expandToToken seq@(Link name:rest) | isToken sMap name = seq
        expandToToken (Link name:rest) =
                case lookup name sMap of
                    Nothing -> error ("Unknown link name in prepareForLeftFactor: " ++ name)
                    Just seq -> expandToToken (seq ++ rest)
        expandToToken (Out eString:rest) = Out eString:expandToToken rest
        expandToToken (Or seqs:rest) = orIfy (expandToToken <$> seqs) ++ rest
        expandToToken seq = seq
        expandEStart::Sequence->Sequence
        expandEStart (Out eString:Or seqs:rest) = orIfy ((Out eString:) <$> seqs) ++ rest
        expandEStart (Or seqs:rest) = orIfy (expandEStart <$> seqs) ++ rest
        expandEStart x = x
prepareForLeftFactor sMap seq = seq

concatOuts::Sequence->Sequence
concatOuts (Out eString1:Out eString2:rest) = concatOuts (Out (eString1 ++ eString2):rest)
concatOuts (Or seqs:rest) = (orIfy $ concatOuts <$> seqs) ++ concatOuts rest
concatOuts (e:rest) = e:concatOuts rest
concatOuts [] = []
--concatOuts seq = error ("Missing case in concatOuts: " ++ show seq)

splitFirstTok::Sequence->(Maybe Sequence, Sequence)
splitFirstTok (Link name:rest) = (Just [Link name], rest)
splitFirstTok (TextMatch text:rest) = (Just [TextMatch text], rest)
splitFirstTok (Character charset:rest) = (Just [Character charset], rest)
splitFirstTok (List count seq:rest) = (Just [List count seq], rest)
--splitFirstTok (Out estring:rest) = (Just [Out estring], rest)
splitFirstTok (Or seqs:rest) = (Just [Or seqs], rest)
splitFirstTok (FallBack:rest) = (Just [FallBack], rest)
splitFirstTok (WhiteSpace _:rest) = (Just [WhiteSpace " "], rest)
splitFirstTok (Out eString1:Out eString2:rest) = splitFirstTok (Out (eString1 ++ eString2):rest)
splitFirstTok (Out [ItemInfo eString]:rest) = (nextTok, Out [ItemInfo eString]:nextSeq)
    where (nextTok, nextSeq) = splitFirstTok rest
splitFirstTok (Out eString:rest) = (nextTok >>= Just . (Out [FutureItem]:), Out [ItemInfo eString]:nextSeq)
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

leftTest::Grammar->IO ()
leftTest g =
    do
        let sMap = sequenceMap g
        putStrLn (formatSequenceMap sMap)
        let seq = fromJust $ lookup "command" sMap
        putStrLn (formatSequence $ concatOuts $ prepareForLeftFactor sMap seq)











