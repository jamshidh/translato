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

import Grammar as G

import JDebug

type LinkName = String

data Context = Context {
    grammar::Grammar,
    sequences::Map LinkName Sequence,
    ruleMap::Map String [Rule],
    seq2Separator::Sequence->Sequence
    }

listMapWith::Ord k=>(a->k)->[a]->Map k [a]
listMapWith f items = fromListWith (++) ((\x -> (f x, [x])) <$> items)
