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
    cleanTree,
    treeTake,
    forestTake,
    safeDrawForest,
    safeDrawTree
) where

import Data.Functor
import Data.Tree

safeDrawTree = drawTree . treeTake 3
safeDrawForest = drawForest . forestTake 3

cleanTree::Tree a->Tree [a]
cleanTree (Node {rootLabel=c, subForest=[next]})=Node { rootLabel=c:rootLabel nextResult, subForest=subForest nextResult }
    where nextResult = cleanTree next
cleanTree (Node {rootLabel=c, subForest=subForest}) = Node {rootLabel=[c], subForest=cleanTree <$> subForest}

cleanForest::Forest a->Forest [a]
cleanForest forest = cleanTree <$> forest

treeTake::Int->Tree a->Tree a
treeTake 0 _ = error "tree can't be truncated"
treeTake count tree = Node{rootLabel=rootLabel tree,subForest=forestTake (count-1) (subForest tree)}

forestTake::Int->Forest a->Forest a
forestTake 0 _ = []
forestTake count trees =
    trees >>= (\tree -> [Node{rootLabel=rootLabel tree,subForest=forestTake (count-1) (subForest tree)}])
