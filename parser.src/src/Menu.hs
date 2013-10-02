-----------------------------------------------------------------------------
--
-- Module      :  Menu
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

module Menu (
    menu,
    MenuTree(..)
) where

import Graphics.UI.Gtk

import DOM

--import JDebug

data MenuTree = TrSubMenu String [MenuTree] Bool | TrItem String (Maybe String) (IO())

--Note- I am doing something dumb here, but I don't know of a better way to do it.
--I am converting a haskell data tree to an xml string representing the menu, then using a uimanager
--to create the action widget.  The xml string->xml->widget part would seem to be unneeded, but
--it seems that Haskell only has full support for widgets created this way.  In particular, I don't
--think I can create an accelerator on a menu widget created directly.

menu::[WidgetModifier p MenuBar]->[MenuTree]->IO (DOM p)
menu attModifiers menu = do
    actionGroup <- actionGroupNew "Editor"
    (menuXMLString, actionGroup) <- menuTree2MenuData menu

    uiManager <- uiManagerNew
    uiManagerAddUiFromString uiManager menuXMLString
    uiManagerInsertActionGroup uiManager actionGroup 0


    maybeMenubar <- uiManagerGetWidget uiManager "/ui/menubar"
    let menuBar = case maybeMenubar of
            (Just x) -> x
            Nothing -> error "Cannot get menubar from string."
    let ids = case [(name, castToWidget menuBar)|ID name <- attModifiers] of
                [] -> []
                [oneId] -> [oneId]
                _ -> error "You can only have one ID in a widget"
    return DOM{widget=menuBar, childAttrs=[attr (castToMenuBar menuBar)|CAtr attr <- attModifiers], uiManagers=[uiManager], ids=ids}


menuTree2MenuData::[MenuTree]->IO (String, ActionGroup)
menuTree2MenuData menu = do
    actionGroup <- actionGroupNew "The Menu"
    filledActionGroup <- treeString2ActionGroup flattened 1 actionGroup
    return ("<ui><menubar>" ++ treeString2XML flattened 1 ++ "</menubar></ui>",
                filledActionGroup)
    where
        treeString2XML::TreeString->Int->String
        treeString2XML [] _ = ""
        treeString2XML (Start name:rest) position =
            "<menu name='" ++ name ++ "' action='menu_action_" ++ show position ++ "'>"
            ++ treeString2XML rest (position + 1)
        treeString2XML (End:rest) position = "</menu>" ++ treeString2XML rest position
        treeString2XML (Data (TrItem name accelerator action):rest) position =
            "<menuitem name='" ++ name ++ "' action='menu_action_" ++ show position ++ "' />"
            ++ treeString2XML rest (position + 1)

        treeString2ActionGroup::TreeString->Int->ActionGroup->IO ActionGroup
        treeString2ActionGroup [] _ actionGroup = return actionGroup
        treeString2ActionGroup (Start name:rest) position actionGroup = do
            action <- actionNew ("menu_action_" ++ show position) name (Just "tooltip") Nothing
            actionGroupAddAction actionGroup action
            treeString2ActionGroup rest (position + 1) actionGroup
        treeString2ActionGroup (End:rest) position actionGroup =
            treeString2ActionGroup rest position actionGroup
        treeString2ActionGroup (Data (TrItem name accelerator actionFunction):rest) position actionGroup = do
            action <- actionNew ("menu_action_" ++ show position) name (Just "tooltip") Nothing
            onActionActivate action actionFunction
            actionGroupAddActionWithAccel actionGroup action accelerator
            treeString2ActionGroup rest (position + 1) actionGroup

        flattened = flatten menu

---------------------

data TreeChar = Start String | Data MenuTree | End
type TreeString = [TreeChar]

flatten::[MenuTree]->TreeString
flatten (TrSubMenu name subMenu _:rest) = [Start name] ++ flatten subMenu ++ (End:flatten rest)
flatten (item@(TrItem _ _ _):rest) = Data item:flatten rest
flatten [] = []













