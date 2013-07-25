-------------------------------------------------------------------------------
--
-- Module      :  Context
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

module Context (
    Context (..),
    --grammar2Context,
    --postSequence,
    --postSeqShow,
    --classParseType,
    --name2Class
) where

import Prelude hiding (lookup)

import Data.Functor
import Data.List hiding (lookup)
import Data.Map hiding (map, union, null, filter)
import qualified Data.Map as M (map)
import Data.Maybe

import Colors
import EnhancedString as E hiding (EStart, EEnd)
import Grammar as G
import LeftFactoring
import OperatorNames
import XPath

import JDebug

type LinkName = String

data Context = Context {
    grammar::Grammar,
    sequences::Map LinkName Sequence,
    ruleMap::Map String [Rule],
    seq2Separator::Sequence->Sequence
    }

{--postSeqShow::Context->String
postSeqShow cx = "Post Sequences:\n"
    ++ intercalate "\n\n"
            (map (\cl -> blue (className cl) ++ ": " ++ show (postSequence cx cl)) classesWithPostSequence)
        where classesWithPostSequence = filter (\cl -> classParseType (grammar cx) cl == Block) (classes (grammar cx))--}

listMapWith::Ord k=>(a->k)->[a]->Map k [a]
listMapWith f items = fromListWith (++) ((\x -> (f x, [x])) <$> items)

{--grammar2Context::Grammar->Context
grammar2Context g =
    Context {
        grammar=g,
        sequences=M.map
                (replicate 1 . Or . map (addPriority 1).  map fullSequence)
                ruleMap,

        ruleMap=ruleMap,
        seq2Separator=grammarSeq2Separator g
        }
            where ruleMap=listMapWith name (classes g >>= rulesForClass g)--}


addPriority::Int->Sequence->(Int, Sequence)
addPriority priority seq = (priority, seq)

{--postSequence::Context->Class->Sequence
postSequence cx cl = leftFactor [Or
    (addPriority 1 <$> ((classRules >>= rule2PostSequence)
    ++ (operator2PostSequence cl (class2AllOpSymbols (grammar cx) cl))))]
        where classRules = fromJust (lookup (className cl) (rules cx))--}


operator2PostSequence::Class->[OperatorSymbol]->[Sequence]
operator2PostSequence cl [] = []
operator2PostSequence cl symbols =
    map symbolSeq (zip [1..] symbols)
        where symbolSeq (priority, symbol) =
                [G.InfixTag priority ((op2Name.textMatches) symbol), LinkStream (className cl)]

textMatches::Sequence->String
textMatches (TextMatch matchString:rest) = matchString ++ (textMatches rest)
textMatches (_:rest) = textMatches rest
textMatches [] = []



{--name2Class::Grammar->String->Maybe Class
name2Class g theName = lookup theName (fromList (map (\c -> (className c, c)) (classes g)))--}

