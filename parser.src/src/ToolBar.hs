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
    Item(..)
) where

import Graphics.UI.Gtk
--import Graphics.UI.Gtk.MenuComboToolbar.Menu

import JDebug

data Item = Item StockId (Maybe String) (IO())

--Note- I am doing something dumb here, but I don't know of a better way to do it.
--I am converting a haskell data tree to an xml string representing the menu, then using a uimanager
--to create the action widget.  The xml string->xml->widget part would seem to be unneeded, but
--it seems that Haskell only has full support for widgets created this way.  In particular, I don't
--think I can create an accelerator on a menu widget created directly.

addToolBarToWindow::BoxClass a=>Window->a->[Item]->IO ()
addToolBarToWindow window parent menu = do
    actionGroup <- actionGroupNew "Editor"
    (toolBarXMLString, actionGroup) <- toolBarItems2MenuData menu

    ui <- uiManagerNew
    uiManagerAddUiFromString ui toolBarXMLString
    uiManagerInsertActionGroup ui actionGroup 0


    maybeMenubar <- uiManagerGetWidget ui "/ui/toolbar"
    let menubar = case maybeMenubar of
            (Just x) -> x
            Nothing -> error "Cannot get menubar from string."
    boxPackStart parent menubar PackNatural 0
    accelGroup <- uiManagerGetAccelGroup ui
    windowAddAccelGroup window accelGroup


toolBarItems2MenuData::[Item]->IO (String, ActionGroup)
toolBarItems2MenuData menu = do
    actionGroup <- actionGroupNew "The Menu"
    filledActionGroup <- treeString2ActionGroup flattened 1 actionGroup
    return ("<ui><toolbar>" ++ treeString2XML flattened 1 ++ "</toolbar></ui>",
                filledActionGroup)
    where
        treeString2XML::[Item]->Int->String
        treeString2XML [] _ = ""
        treeString2XML (Item _ _ _:rest) position =
            "<toolitem name='name" ++ show position ++ "' action='menu_action_" ++ show position ++ "' />"
            ++ treeString2XML rest (position + 1)

        treeString2ActionGroup::[Item]->Int->ActionGroup->IO ActionGroup
        treeString2ActionGroup [] _ actionGroup = return actionGroup
        treeString2ActionGroup (Item stockId maybeToolTip actionFunction:rest) position actionGroup = do
            action <- actionNew ("menu_action_" ++ show position) ("name" ++ show position) maybeToolTip (Just stockId)
            onActionActivate action actionFunction
            actionGroupAddAction actionGroup action
            treeString2ActionGroup rest (position + 1) actionGroup

        flattened = menu










