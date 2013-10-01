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
    toolbar,
    ImageId(..),
    Item(..)
) where

import Graphics.UI.Gtk

import DOM

--import JDebug

data ImageId = Stock String | File String

data Item p = Item ImageId (Maybe String) (IO())

--toolButton::[WidgetModifier p Toolbar]->ImageId->Maybe String->IO()->[Item a]->IO (DOM p)
--toolButton attModifiers imageId tooltip f menu = do
--    image <- case imageId of
--        Stock stockId -> imageNewFromStock stockId IconSizeLargeToolbar
--        File filename -> imageNewFromFile filename
--    toolButton <- toolButtonNew (Just image) Nothing
--    onToolButtonClicked toolButton f
--    case tooltip of
--        Just val -> do
--            tooltips <- tooltipsNew
--            toolItemSetTooltip toolButton tooltips val ""
--        Nothing -> return ()
--    toolbarInsert toolbar toolButton (-1)


toolbar::[WidgetModifier p Toolbar]->[Item a]->IO (DOM p)
toolbar attModifiers menu = do
    toolbar <- toolbarNew
    toolbarSetStyle toolbar ToolbarIcons
    mapM_ (addToolButton toolbar) menu
    let ids = case [(name, castToWidget toolbar)|ID name <- attModifiers] of
                [] -> []
                [oneId] -> [oneId]
                _ -> error "You can only have one ID in a widget"
    return DOM{widget=castToWidget toolbar, childAttrs=[attr toolbar|CAtr attr <- attModifiers], ids=ids}

    where
        addToolButton::Toolbar->Item a->IO ()
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


