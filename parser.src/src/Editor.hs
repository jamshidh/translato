-----------------------------------------------------------------------------
--
-- Module      :  Editor
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

module Editor (
    edit
) where

import Control.Monad
import Data.Functor
import Data.Text hiding (head)
import Data.Text.Encoding
import Graphics.UI.Gtk
import Graphics.UI.Gtk.MenuComboToolbar.Menu
import Graphics.UI.Gtk.Multiline.TextIter
import Graphics.UI.Gtk.Multiline.TextView
import System.IO

import Grammar
import JDebug

uiDef =
  "<ui>\
  \  <menubar>\
  \    <menu name=\"File\" action=\"FileAction\">\
  \      <menuitem name=\"New\" action=\"NewAction\" />\
  \      <menuitem name=\"Open\" action=\"OpenAction\" />\
  \      <menuitem name=\"Save\" action=\"SaveAction\" />\
  \      <menuitem name=\"SaveAs\" action=\"SaveAsAction\" />\
  \      <separator/>\
  \      <menuitem name=\"Exit\" action=\"ExitAction\"/>\
  \      <placeholder name=\"FileMenuAdditions\" />\
  \    </menu>\
  \    <menu name=\"Edit\" action=\"EditAction\">\
  \      <menuitem name=\"Cut\" action=\"CutAction\"/>\
  \      <menuitem name=\"Copy\" action=\"CopyAction\"/>\
  \      <menuitem name=\"Paste\" action=\"PasteAction\"/>\
  \    </menu>\
  \  </menubar>\
  \  <toolbar>\
  \    <placeholder name=\"FileToolItems\">\
  \      <separator/>\
  \      <toolitem name=\"New\" action=\"NewAction\"/>\
  \      <toolitem name=\"Open\" action=\"OpenAction\"/>\
  \      <toolitem name=\"Save\" action=\"SaveAction\"/>\
  \      <separator/>\
  \    </placeholder>\
  \    <placeholder name=\"EditToolItems\">\
  \      <separator/>\
  \      <toolitem name=\"Cut\" action=\"CutAction\"/>\
  \      <toolitem name=\"Copy\" action=\"CopyAction\"/>\
  \      <toolitem name=\"Paste\" action=\"PasteAction\"/>\
  \      <separator/>\
  \    </placeholder>\
  \  </toolbar>\
  \</ui>"

data MenuTree = TrSubMenu String [MenuTree] Bool | TrItem String (IO())

createMenu::[MenuTree]->IO MenuBar
createMenu tree = do
    menu <- menuBarNew
    --let menuItems = menuTree2MenuItem <$> tree

    addMenuItemsToMenu menu (menuTree2MenuItem <$> tree)
    return menu

addMenuItemsToMenu::MenuShellClass a=>a->[IO MenuItem]->IO ()
addMenuItemsToMenu menu [first] = jtrace "one" $ do
    first' <- first
    menuShellAppend menu first'
addMenuItemsToMenu menu (first:rest) = jtrace "many" $ do
    first' <- first
    menuShellAppend menu first'
    addMenuItemsToMenu menu rest
addMenuItemsToMenu _ [] = error "huh?"

menuTree2MenuItem::MenuTree->IO MenuItem
menuTree2MenuItem (TrSubMenu name trSubItems rightJustify) = do
    menu <- menuNew
    menuItem <- menuItemNewWithLabel name
    menuItemSetSubmenu menuItem menu
    addMenuItemsToMenu menu (menuTree2MenuItem <$> trSubItems)
    menuItemSetRightJustified menuItem rightJustify
    return menuItem
menuTree2MenuItem (TrItem name action) = do
    item <- menuItemNewWithMnemonic name
    onActivateLeaf item action
    return item

edit::Grammar->IO ()
edit g = do
    initGUI
    window <- windowNew
    windowSetTitle window "Editor"

    scrolledTextView <- scrolledWindowNew Nothing Nothing
    textView <- textViewNew
    set scrolledTextView [ containerChild := textView ]

    resetButton <- buttonNewWithLabel "Reset"
    onClicked resetButton (resetBuffer textView)

    menu <- createMenu
            [
                TrSubMenu "File"
                    [
                        TrItem "Open" (loadBuffer textView window),
                        TrItem "Save" (saveBuffer textView),
                        TrItem "_Quit" mainQuit
                    ] False,
                TrSubMenu "Edit"
                    [
                        TrItem "Find" mainQuit
                    ] False,
                TrSubMenu "Help"
                    [
                        TrItem "about" mainQuit
                    ] True
            ]


    --set quitItem [ useUnderLine := True ]

    --widgetShowAll menu

    vbox <- vBoxNew False 10
    hbox <- hBoxNew False 10
    --set hbox [ containerChild := outputButton, containerChild := resetButton ]
    boxPackStart hbox resetButton PackGrow 0

    boxPackStart vbox menu PackNatural 0
    boxPackStart vbox hbox PackNatural 0
    boxPackStart vbox scrolledTextView PackGrow 0
    set window [ containerBorderWidth := 10,
        containerChild := vbox, windowDefaultWidth := 400, windowDefaultHeight := 300 ]
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

saveBuffer::TextView->IO ()
saveBuffer tv =
    do
        txtBuff <- textViewGetBuffer tv
        startIt <- textBufferGetStartIter txtBuff
        endIt <- textBufferGetEndIter txtBuff
--        compTime <- getClockTime
        srcString <- textBufferGetText txtBuff startIt endIt True

  --      buffer <- textViewGetBuffer tv
        let filename = "/home/jim/translato/qqqq3.html"
        --fileHandle<-openFile filename WriteMode
        writeFile filename srcString
        --hClose fileHandle

resetBuffer::TextView->IO ()
resetBuffer tv =
    do
        let initialText = pack "qqqq"
        buf <- textViewGetBuffer tv
        tagTable <- textBufferGetTagTable buf
        textBufferSetByteString buf (encodeUtf8 initialText)
        tag <- textTagNew (Just "qqqq")
        set tag [textTagUnderline := UnderlineError, textTagBackground := "Pink"]
        textTagTableAdd tagTable tag
        start <- textBufferGetIterAtOffset buf 0
        end <- textBufferGetIterAtOffset buf 2
        textBufferApplyTag buf tag start end

loadBuffer::TextView->Window->IO ()
loadBuffer tv window =
    do
        Just filename <- openOpenFileDialog window
        fileHandle<-openFile filename ReadMode
        contents<-hGetContents fileHandle

        buf <- textViewGetBuffer tv
        tagTable <- textBufferGetTagTable buf
        textBufferSetText buf contents
        hClose fileHandle

        {--txtBuff <- textViewGetBuffer tv
        startIt <- textBufferGetStartIter txtBuff
        endIt <- textBufferGetEndIter txtBuff
        textBufferSetByteString txtBuff startIt endIt True--}

{--chooseFile::IO String
chooseFile =
    do
        fileChooser <- fileChooserNew--}

openOpenFileDialog :: Window -> IO (Maybe String)
openOpenFileDialog parentWindow = do
    dialog <- fileChooserDialogNew
                (Just "Select coding file")
                (Just parentWindow)
                FileChooserActionOpen
                [("gtk-open"   , ResponseAccept)
                    ,("gtk-cancel" , ResponseCancel)]

    widgetShow dialog
    response <- dialogRun dialog
    widgetHide dialog
    case response of
       ResponseAccept -> do
            Just fileName <- fileChooserGetFilename dialog
            return (Just fileName)
       ResponseCancel -> return Nothing
       ResponseDeleteEvent -> return Nothing




