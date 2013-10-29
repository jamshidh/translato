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
    cellRendererPixbuf,
    cellRendererStockPixbuf
) where

import Control.Applicative
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
    applyModifiers treeView attModifiers

    return DOM{widget=castToWidget treeView, childAttrs=[attr treeView|CAtr attr <- attModifiers], uiManagers=[]}

treeViewColumn::(CellRendererClass cell, TreeModelClass (model row), TypedTreeModelClass model)=>
                        [TreeViewColumn->model row->IO (cell)]->model row->IO TreeViewColumn
treeViewColumn rendererCreators dataStore = do
    column <- treeViewColumnNew
    sequence (rendererCreators <*> [column] <*> [dataStore])
    return column

cellRenderer::(CellRendererClass cell, TreeModelClass (model row), TypedTreeModelClass model)=>
                        IO cell->ReadWriteAttr cell c b->(row->b)->TreeViewColumn->model row->IO CellRenderer
cellRenderer rendererCreator attribute extractor column dataStore = do
    renderer <- rendererCreator
    cellLayoutPackStart column renderer True
    cellLayoutSetAttributes column renderer dataStore $ \row -> [attribute := extractor row]
    return (castToCellRenderer renderer)

cellRendererText::(TreeModelClass (model row), TypedTreeModelClass model)=>
                        (row->String)->TreeViewColumn->model row->IO CellRenderer
cellRendererText = cellRenderer cellRendererTextNew cellTextMarkup . (Just .)

cellRendererPixbuf::(TreeModelClass (model row), TypedTreeModelClass model)=>
                        (row->Pixbuf)->TreeViewColumn->model row->IO CellRenderer
cellRendererPixbuf = cellRenderer cellRendererPixbufNew cellPixbuf

cellRendererStockPixbuf::(TreeModelClass (model row), TypedTreeModelClass model)=>
                        (row->String)->TreeViewColumn->model row->IO CellRenderer
cellRendererStockPixbuf = cellRenderer cellRendererPixbufNew cellPixbufStockId
