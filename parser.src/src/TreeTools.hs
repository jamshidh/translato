-----------------------------------------------------------------------------
--
-- Module      :  TreeTools
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

module TreeTools (
    cleanForest,
    cleanTree
) where

import Data.Functor
import Data.Tree

cleanTree::Tree a->Tree [a]
cleanTree (Node {rootLabel=c, subForest=[next]})=Node { rootLabel=c:rootLabel nextResult, subForest=subForest nextResult }
    where nextResult = cleanTree next
cleanTree (Node {rootLabel=c, subForest=subForest}) = Node {rootLabel=[c], subForest=cleanTree <$> subForest}

cleanForest::Forest a->Forest [a]
cleanForest forest = cleanTree <$> forest

