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
    treeView
) where

import Control.Monad
import Data.Functor
import Graphics.UI.Gtk

import DOM

treeView::[WidgetModifier p TreeView]->[TreeViewColumn]->IO (DOM p)
treeView attModifiers columns = do
    treeView <- treeViewNew
    forM_ columns (treeViewAppendColumn treeView)
    ids <- applyModifiers treeView attModifiers
    return DOM{widget=castToWidget treeView, childAttrs=[attr treeView|CAtr attr <- attModifiers], ids=ids}


