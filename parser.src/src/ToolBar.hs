-----------------------------------------------------------------------------
--
-- Module      :  ToolBar
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

module ToolBar (
    addToolBarToWindow,
    ImageId(..),
    Item(..)
) where

import Graphics.UI.Gtk

import JDebug

data ImageId = Stock String | File String

data Item = Item ImageId (Maybe String) (IO())

addToolBarToWindow::BoxClass a=>Window->a->[Item]->IO ()
addToolBarToWindow window parent menu = do
    toolbar <- toolbarNew
    toolbarSetStyle toolbar ToolbarIcons
    mapM_ (addToolButton toolbar) menu
    boxPackStart parent toolbar PackNatural 0

    where
        addToolButton::Toolbar->Item->IO ()
        addToolButton toolbar (Item imageId tooltip f) = do
            image <- case imageId of
                Stock stockId -> imageNewFromStock stockId IconSizeLargeToolbar
                File filename -> imageNewFromFile filename
            toolButton <- toolButtonNew (Just image) Nothing
            onToolButtonClicked toolButton f
            case tooltip of
                Just val -> do
                    tooltips <- tooltipsNew
                    toolItemSetTooltip toolButton tooltips val ""
                Nothing -> return ()
            toolbarInsert toolbar toolButton (-1)

