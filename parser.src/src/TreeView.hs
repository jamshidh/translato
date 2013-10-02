-----------------------------------------------------------------------------
--
-- Module      :  TreeView
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

module TreeView (
    treeView,
    treeViewColumn,
    cellRendererText,
    cellRendererPixbuf
) where

import Control.Monad
import Data.Functor
import Graphics.UI.Gtk

import DOM

treeView::(TreeModelClass (model row), TypedTreeModelClass model)=>
                [WidgetModifier p TreeView]->[model row->IO TreeViewColumn]->model row->IO (DOM p)
treeView attModifiers columnCreators dataStore = do
    treeView <- treeViewNewWithModel dataStore
    columns <- sequence (($ dataStore) <$> columnCreators)
    forM_ columns (treeViewAppendColumn treeView)
    ids <- applyModifiers treeView attModifiers
    return DOM{widget=castToWidget treeView, childAttrs=[attr treeView|CAtr attr <- attModifiers], ids=ids}

treeViewColumn::(CellRendererClass cell, TreeModelClass (model row), TypedTreeModelClass model)=>
                        [IO (cell, row->[AttrOp cell])]->model row->IO TreeViewColumn
treeViewColumn rendererCreators dataStore = do
    column <- treeViewColumnNew
    renderers <- sequence rendererCreators
    forM_ renderers (\(renderer, extractor) -> do
            cellLayoutPackStart column renderer True
            cellLayoutSetAttributes column renderer dataStore extractor
        )
    return column

cellRenderer::IO o->ReadWriteAttr o c b->(a->b)->IO (o, a->[AttrOp o])
cellRenderer rendererCreator attribute extractor = do
    renderer <- rendererCreator
    return (renderer, \row -> [attribute := extractor row])

cellRendererText = cellRenderer cellRendererTextNew cellTextMarkup . (Just .)
cellRendererPixbuf = cellRenderer cellRendererPixbufNew cellPixbuf
