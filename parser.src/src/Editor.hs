{-# LANGUAGE TemplateHaskell #-}

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
    edit,
    editMain
) where

import Control.Monad
import Data.Functor
import Data.IORef
import Data.Maybe
import Data.Text hiding (head, concat)
import Data.Text.Encoding
import Graphics.UI.Gtk
import Graphics.UI.Gtk.MenuComboToolbar.Menu
import Graphics.UI.Gtk.Multiline.TextIter
import Graphics.UI.Gtk.Multiline.TextView
import System.Console.GetOpt as O
import System.IO
import Text.Regex

import ArgOpts
import Grammar
import GrammarTools
import Menu

import JDebug

uiDef =
  "<ui>\
  \  <menubar>\
  \    <menu name=\"File\" action='FileAction'>\
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
  \    <menu name=\"Help\" action=\"HelpAction\">\
  \      <menuitem name=\"About\" action=\"AboutAction\"/>\
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


edit::Grammar->String->IO ()
edit g fileNameString = do
    initGUI
    window <- windowNew
    fileNameRef <- newIORef fileNameString
    windowSetTitle window fileNameString

    scrolledTextView <- scrolledWindowNew Nothing Nothing
    textView <- textViewNew
    set scrolledTextView [ containerChild := textView ]

    resetButton <- buttonNewWithLabel "Reset"
    onClicked resetButton (resetBuffer textView)

    vbox <- vBoxNew False 10
    hbox <- hBoxNew False 10

    addMenuToWindow window vbox
        (
            [
                TrSubMenu "_File"
                    [
                        TrItem "Open" Nothing (promptAndLoadBuffer textView window),
                        TrItem "Save" Nothing (saveBuffer fileNameRef textView),
                        TrItem "Save As...." Nothing (saveBufferAs fileNameRef textView window),
                        TrItem "_Quit" (Just "<Control>q") mainQuit
                    ] False,
                TrSubMenu "Edit"
                    [
                        TrItem "Find" Nothing mainQuit
                    ] False,
                TrSubMenu "Help"
                    [
                        TrItem "about" Nothing mainQuit
                    ] True
            ]
            )


    --set hbox [ containerChild := outputButton, containerChild := resetButton ]
    set window [ containerBorderWidth := 10,
        containerChild := vbox, windowDefaultWidth := 800, windowDefaultHeight := 600 ]

    loadBuffer fileNameString textView window


    {-maybeToolbar <- uiManagerGetWidget ui "/ui/toolbar"
    let toolbar = case maybeToolbar of
            (Just x) -> x
            Nothing -> error "Cannot get toolbar from string."
    boxPackStart vbox toolbar PackNatural 0-}

    boxPackStart hbox resetButton PackGrow 0

    boxPackStart vbox hbox PackNatural 0
    boxPackStart vbox scrolledTextView PackGrow 0

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

saveBuffer::IORef String->TextView->IO ()
saveBuffer fileNameRef tv =
    do
        txtBuff <- textViewGetBuffer tv
        startIt <- textBufferGetStartIter txtBuff
        endIt <- textBufferGetEndIter txtBuff
--        compTime <- getClockTime
        srcString <- textBufferGetText txtBuff startIt endIt True

  --      buffer <- textViewGetBuffer tv
        filename <- readIORef fileNameRef
        --fileHandle<-openFile filename WriteMode
        writeFile filename srcString
        --hClose fileHandle

saveBufferAs::IORef String->TextView->Window->IO ()
saveBufferAs fileNameRef tv window =
    do
        Just filename <- openOpenFileDialog FileChooserActionSave window
        windowSetTitle window filename
        writeIORef fileNameRef filename
        saveBuffer fileNameRef tv

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

loadBuffer::String->TextView->Window->IO ()
loadBuffer filename tv window =
    do
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

promptAndLoadBuffer::TextView->Window->IO ()
promptAndLoadBuffer tv window =
    do
        Just fileName <- openOpenFileDialog FileChooserActionOpen window
        loadBuffer fileName tv window
        windowSetTitle window fileName

{--chooseFile::IO String
chooseFile =
    do
        fileChooser <- fileChooserNew--}

openOpenFileDialog::FileChooserAction->Window->IO (Maybe String)
openOpenFileDialog action parentWindow = do
    let openButton = case action of
                        FileChooserActionOpen -> "gtk-open"
                        _ -> "gtk-save"
    dialog <- fileChooserDialogNew
                (Just "Select coding file")
                (Just parentWindow)
                action
                [(openButton   , ResponseAccept)
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

-----------------------

data Options = Options { specFileName::Maybe String, fileName::String, qqqq::Int } deriving (Show, Read)
deflt = Options { specFileName=Nothing, fileName="fred", qqqq=1 }

getExtension x =
    case matchRegex (mkRegex "\\.([^\\.]+$)") x of
        Just [x] -> x
        _ -> error "You need to supply the spec filename when the inputFileName doesn't have an extension"

fixOptions::Options->Options
fixOptions o@Options{specFileName=Nothing} = o{specFileName=Just specFileName}
    where
        specFileName = getExtension (fileName o) ++ ".spec"

editMain::[String]->IO ()
editMain args = do
    let options =
            fixOptions
                ($(arg2Opts ''Options ["fileName"]) args deflt)
    grammar <- loadGrammar (fromJust $ specFileName options)
    Editor.edit grammar (fileName options)




