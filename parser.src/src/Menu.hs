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
    addMenuToWindow,
    MenuTree(..)
) where

import Graphics.UI.Gtk
--import Graphics.UI.Gtk.MenuComboToolbar.Menu

import JDebug

data MenuTree = TrSubMenu String [MenuTree] Bool | TrItem String (Maybe String) (IO())

{-createMenu::[MenuTree]->IO MenuBar
createMenu tree = do
    menu <- menuBarNew
    --let menuItems = menuTree2MenuItem <$> tree

    addMenuItemsToMenu menu (menuTree2MenuItem <$> tree)
    return menu

addMenuItemsToMenu::MenuShellClass a=>a->[IO MenuItem]->IO ()
addMenuItemsToMenu menu [first] = do
    first' <- first
    menuShellAppend menu first'
addMenuItemsToMenu menu (first:rest) = do
    first' <- first
    menuShellAppend menu first'
    addMenuItemsToMenu menu rest
addMenuItemsToMenu _ [] = error "huh?"

menuTree2MenuItem::MenuTree->ActionGroup->IO (String, ActionGroup)
menuTree2MenuItem (TrSubMenu name trSubItems rightJustify) actionGroup = do
    menu <- menuNew
    menuItem <- menuItemNewWithLabel name
    menuItemSetSubmenu menuItem menu
    addMenuItemsToMenu menu (menuTree2MenuItem <$> trSubItems)
    menuItemSetRightJustified menuItem rightJustify
    return menuItem
menuTree2MenuItem (TrItem name accelerator action) actionGroup = do
--    onActivateLeaf item action
    return ()-}

--Note- I am doing something dumb here, but I don't know of a better way to do it.
--I am converting a haskell data tree to an xml string representing the menu, then using a uimanager
--to create the action widget.  The xml string->xml->widget part would seem to be unneeded, but
--it seems that Haskell only has full support for widgets created this way.  In particular, I don't
--think I can create an accelerator on a menu widget created directly.

addMenuToWindow::BoxClass a=>Window->a->[MenuTree]->IO ()
addMenuToWindow window parent menu = do
    actionGroup <- actionGroupNew "Editor"
    (menuXMLString, actionGroup) <- menuTree2MenuData menu

    ui <- uiManagerNew
    uiManagerAddUiFromString ui menuXMLString
    uiManagerInsertActionGroup ui actionGroup 0


    maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
    let menubar = case maybeMenubar of
            (Just x) -> x
            Nothing -> error "Cannot get menubar from string."
    boxPackStart parent menubar PackNatural 0
    accelGroup <- uiManagerGetAccelGroup ui
    windowAddAccelGroup window accelGroup


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
            action <- actionNew ("menu_action_" ++ show position) name (Just "tooltip") (Just stockQuit)
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













