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

import Data.Text
import Data.Text.Encoding
import Graphics.UI.Gtk
import Graphics.UI.Gtk.MenuComboToolbar.Menu
import Graphics.UI.Gtk.Multiline.TextIter
import Graphics.UI.Gtk.Multiline.TextView
import System.IO

import Grammar

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

edit::Grammar->IO ()
edit g = do
    initGUI
    window <- windowNew
    windowSetTitle window "qqqq"
    menu <- menuBarNew
    openItem <- menuItemNewWithLabel "Open"
    saveItem <- menuItemNewWithLabel "Save"
    quitItem <- menuItemNewWithLabel "Quit"

    menuShellAppend menu openItem
    menuShellAppend menu saveItem
    menuShellAppend menu quitItem

    --widgetShowAll menu

    scrolledTextView <- scrolledWindowNew Nothing Nothing
    textView <- textViewNew
    set scrolledTextView [ containerChild := textView ]

    loadButton <- buttonNewWithLabel "Load"
    onClicked loadButton (loadBuffer textView window)

    outputButton <- buttonNewWithLabel "Save"
    onClicked outputButton (saveBuffer textView)

    resetButton <- buttonNewWithLabel "Reset"
    onClicked resetButton (resetBuffer textView)


    vbox <- vBoxNew False 10
    hbox <- hBoxNew False 10
    --set hbox [ containerChild := outputButton, containerChild := resetButton ]
    boxPackStart hbox loadButton PackGrow 0
    boxPackStart hbox outputButton PackGrow 0
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




