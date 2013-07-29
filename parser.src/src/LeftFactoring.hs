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

leftFactor::SequenceMap->Sequence->Sequence
leftFactor sm = leftFactor' . (prepareForLeftFactor sm)

leftFactor'::Sequence->Sequence
leftFactor' (Or []:rest) = leftFactor' rest
leftFactor' (Or [seq]:rest) = leftFactor' (seq ++ rest)
leftFactor' (Or items:rest) = --jtrace ("Left factoring: " ++ intercalate "\n  " (map show items)) $
    (orIfy (makeSeq <$> theMap)) ++ leftFactor' rest
    where
        theMap = toList (fromListWith (++) (fmap (replicate 1) <$> splitFirstTok <$> items))
        makeSeq::(Maybe Sequence, [Sequence])->Sequence
--        makeSeq (Just [EmptyEStart, exp], [EInfo name atts:oneSeq]) = EStart name atts:exp:oneSeq
        makeSeq (Just [EmptyEStart, exp], seqs) | length firstItems == 1 =
            case firstItems of
                [EInfo name atts] -> EStart name atts:exp:(leftFactor' [Or (tail <$> seqs)])
                where
                    firstItems = nub [exp2|exp2@(EInfo _ _):_<-seqs]
        makeSeq (Just first, rest2) = first ++ (leftFactor' $ orIfy rest2)
--        makeSeq (Just first, rest2) = first ++ (leftFactor' (simpleEInfoLeftFactor $ orIfy rest2))
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
        expandToToken (EStart name atts:rest) = EStart name atts:expandToToken rest
        expandToToken (Or seqs:rest) = orIfy (expandToToken <$> seqs) ++ rest
        expandToToken seq = seq
        expandEStart::Sequence->Sequence
        expandEStart (EStart name atts:Or seqs:rest) = orIfy ((EStart name atts :) <$> seqs) ++ rest
        expandEStart (Or seqs:rest) = orIfy (expandEStart <$> seqs) ++ rest
        expandEStart x = x
prepareForLeftFactor sMap seq = seq

splitFirstTok::Sequence->(Maybe Sequence, Sequence)
splitFirstTok (Link name:rest) = (Just [Link name], rest)
splitFirstTok (TextMatch text:rest) = (Just [TextMatch text], rest)
splitFirstTok (Character charset:rest) = (Just [Character charset], rest)
splitFirstTok (List count seq:rest) = (Just [List count seq], rest)
splitFirstTok (AStart name:rest) = (Just [AStart name], rest)
splitFirstTok (AEnd:rest) = (Just [AEnd], rest)
splitFirstTok (Or seqs:rest) = (Just [Or seqs], rest)
splitFirstTok (FallBack:rest) = (Just [FallBack], rest)
splitFirstTok (WhiteSpace _:rest) = (Just [WhiteSpace " "], rest)
splitFirstTok (EStart name atts:rest) = (nextTok >>= Just . (EmptyEStart:), EInfo name atts:nextSeq)
    where (nextTok, nextSeq) = splitFirstTok rest
splitFirstTok (EInfo name atts:rest) = (nextTok, EInfo name atts:nextSeq)
    where (nextTok, nextSeq) = splitFirstTok rest
splitFirstTok (EEnd name:rest) = (Just [EEnd name], rest)
splitFirstTok [] = (Nothing, [])
splitFirstTok seq = error ("Missing case in splitFirstTok: " ++ formatSequence seq)

getTheOne::[a]->a
getTheOne [a] = a

simpleEInfoLeftFactor::Sequence->Sequence
simpleEInfoLeftFactor seq@(Or seqs:rest) | or (null <$> seqs) = jtrace "1" $ seq
simpleEInfoLeftFactor seq@(Or seqs:rest) | length [exp|exp@(EInfo _ _):_<-seqs] /= length seq = --jtrace ("2: " ++ formatSequence seq) $
    seq
simpleEInfoLeftFactor seq@(Or seqs:rest) | length (nub [exp|exp@(EInfo _ _):_<-seqs]) /= 1 = jtrace "3" $ seq
simpleEInfoLeftFactor seq@(Or seqs:rest) = jtrace "4" $ eInfoExp:orIfy (tail <$> seqs) ++ rest
    where
        eInfoExp::Expression
        eInfoExp = getTheOne $ (nub [exp|exp@(EInfo _ _):_<-seqs])
simpleEInfoLeftFactor x = jtrace "5" $ x


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
derivative sMap name (Or seqs:rest) = Just (orIfy [justSeq|Just justSeq<-derivative sMap name <$> seqs])
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
remainder sMap name (Or seqs:rest) = Just (orIfy [justSeq|Just justSeq<-(remainder sMap name) <$> seqs])
remainder _ name seq = error ("Missing case in remainder: seq=" ++ formatSequence seq ++ ", name=" ++ show name)

leftFactor22::SequenceMap->Sequence->Sequence
leftFactor22 sMap (Or seqs:rest) = orIfy (derivatives ++ [item|Just item<-remainders]) ++ rest
    where
        tokens = (\seq -> (getTokens sMap seq, seq)) <$> seqs
        tok2Seq = fmap orIfy <$> (toList $ fromListWith (++) <$> concat $ (\(toks, seq)-> (\tok -> (tok, [seq])) <$> toks) <$> tokens)
        rewriteDerivative (name, seq) =
            {--if length seq == 1
                then seq
                else --}
                    [Link name] ++ (fromJust $ derivative sMap name seq)
        derivatives = rewriteDerivative <$> tok2Seq
        remainders = [] --(\(seqToks, seq) -> (foldl' (>>=) (Just seq) ((remainder sMap) <$> seqToks))) <$> tokens
leftFactor22 _ seq = seq

s = putStrLn . formatSequence

gI = loadGrammar "../../qqqq.spec"

smI = fmap sequenceMap gI

sm=fromList [("element",[Or [[EStart "element" [],TextMatch "b",WhiteSpace "\n",EEnd "element"],[EStart "element" [],TextMatch "a",EEnd "element"]]]),("file",[EStart "file" [],WhiteSpace "\n",Link "element",WhiteSpace "\n",EEnd "file"]),("ident",[EStart "ident" [],WhiteSpace " ",Character (CharSet False [CharRange 'a' 'z',CharRange 'A' 'Z']),Or [[Character (CharSet False [WordChar]),List 0 [Character (CharSet False [WordChar])]],[]],FallBack,EEnd "ident"]),("node",[Or [[Link "element"],[Link "element"]]])]

sq = (fromJust $ lookup "element" sm)

items = [[EStart "element" [],TextMatch "b",WhiteSpace "\n",EEnd "element"],[EStart "element" [],TextMatch "a",EEnd "element"]]

theMap = toList (fromListWith (++) (fmap (replicate 1) <$> splitFirstTok <$> items))
