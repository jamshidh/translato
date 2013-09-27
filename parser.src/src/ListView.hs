{-# LANGUAGE ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  ListView
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

module ListView (
    DataExtractor(..),
    addColumn,
    listView
) where

import Data.CaseInsensitive
import Graphics.UI.Gtk

import DOM

class Format a where
    format::a->String

instance Format Int where
    format = show

instance Format String where
    format x = x

instance Format (CI String) where
    format = original


data DataExtractor a b =forall b.(Format b, Ord b)=>DataExtractor (a->b)

listView::ListStore b->[(String, DataExtractor b c)]->IO DOM
listView storeSource columns = do
    --I don't know why I can't just use storeSource2 everywhere, but it causes the app to crash.
    --For now I don't care, I'll do what works without understanding it.
    storeSource2 <- treeModelSortNewWithModel storeSource

    treeView <- treeViewNewWithModel storeSource2

    {- Do I need this stuff?  It works without it, and I am content with the typechecking that
        Haskell already does (It is probably better than this).
        _STRING_COLUMN :: ColumnId (Int, String) String
        _STRING_COLUMN = makeColumnIdString 1

        treeModelSetColumn storeSource _STRING_COLUMN (show . fst)-}

    {- Also, I don't think I need this-
       treeSortableSetDefaultSortFunc storeSource2 $ Just $ sortFunc storeSource id-}

    mapM_ (\(position, (name, extractor)) -> addColumn position name storeSource storeSource2 extractor treeView) (zip [1..] columns)

    return DOM{widget=castToWidget treeView}

--sortFunc::TypedTreeModelClass model=>model row->TreeIter->IO row
sortFunc::Ord b=>ListStore a->(a->b)->TreeIter->TreeIter->IO Ordering
sortFunc rawmodel selector iter1 iter2 =
  do x1 <- treeModelGetRow rawmodel iter1
     x2 <- treeModelGetRow rawmodel iter2
     return (compare (selector x1) (selector x2))

addColumn::Int->String->ListStore a->TypedTreeModelSort a->DataExtractor a b->TreeView->IO()
addColumn columnID name storeSource storeSource2 (DataExtractor f) treeView = do
    treeViewColumn <- treeViewColumnNew
    renderer <- cellRendererTextNew
    treeViewColumnSetTitle treeViewColumn name
    treeViewColumnSetSortColumnId treeViewColumn columnID
    treeSortableSetSortFunc storeSource2 columnID $ sortFunc storeSource f
    cellLayoutPackStart treeViewColumn renderer True
    cellLayoutSetAttributes treeViewColumn renderer storeSource $ \e -> [cellText := format $ f e]
    treeViewAppendColumn treeView treeViewColumn
    return ()


