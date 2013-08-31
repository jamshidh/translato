{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

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
import Control.Monad.Trans
import Data.CaseInsensitive
import Data.Functor
import Data.IORef
import Data.Maybe
import Data.Text hiding (head, concat)
import Data.Text.Encoding
import Graphics.UI.Gtk
import Graphics.UI.Gtk.MenuComboToolbar.Menu
import Graphics.UI.Gtk.ModelView
import Graphics.UI.Gtk.ModelView.TreeView
import Graphics.UI.Gtk.Multiline.TextIter
import Graphics.UI.Gtk.Multiline.TextView
import System.Console.GetOpt as O
import System.IO
import Text.Regex

import ArgOpts
import Grammar
import GrammarTools
import ListView
import Menu
import ToolBar

import JDebug

data Error = Error { line::Int, message::CI String }

edit::Grammar->String->IO ()
edit g fileNameString = do
    initGUI
    window <- windowNew
    fileNameRef <- newIORef fileNameString
    windowSetTitle window fileNameString
    --windowSetHasFrame window True
    --windowSetDecorated window False
    --windowSetFrameDimensions window 50 50 50 50

    scrolledTextView <- scrolledWindowNew Nothing Nothing
    textView <- textViewNew
    set scrolledTextView [ containerChild := textView ]

    resetButton <- buttonNewWithLabel "Reset"
    onClicked resetButton (resetBuffer textView)

    vbox <- vBoxNew False 0
    hbox <- hBoxNew True 0

    addMenuToWindow window vbox
        (
            [
                TrSubMenu "_File"
                    [
                        TrItem "Open" Nothing (promptAndLoadBuffer textView window),
                        TrItem "Save" (Just "<Control>s") (saveBuffer fileNameRef textView),
                        TrItem "Save As...." Nothing (saveBufferAs fileNameRef textView window),
                        TrItem "_Quit" (Just "<Control>q") mainQuit
                    ] False,
                TrSubMenu "Edit"
                    [
                        TrItem "Find" Nothing mainQuit
                    ] False,
                TrSubMenu "Help"
                    [
                        TrItem "about" Nothing showAboutDialog
                    ] True
            ]
            )

    addToolBarToWindow window vbox
        (
            [
                Item (Stock stockOpen) (Just "Open File") (promptAndLoadBuffer textView window),
                Item (Stock stockSave) (Just "Save File") (saveBuffer fileNameRef textView),
                Item (Stock stockSaveAs) (Just "Save File As....") (saveBufferAs fileNameRef textView window),
                Item (Stock stockQuit) (Just "Quit the program") mainQuit,
                Item (Stock stockFind) (Just "Find....") mainQuit,
                Item (Stock stockAbout) (Just "About") showAboutDialog,
                Item (Stock stockAbout) Nothing showAboutDialog,
                Item (File "redBall.png") (Just "Validate") showAboutDialog
            ]
        )


    --set hbox [ containerChild := outputButton, containerChild := resetButton ]
    set window [ containerBorderWidth := 4,
        containerChild := vbox, windowDefaultWidth := 800, windowDefaultHeight := 600 ]

    loadBuffer fileNameString textView window

    boxPackStart hbox resetButton PackGrow 0

    boxPackStart vbox hbox PackNatural 0

    vPaned <- vPanedNew
    boxPackStart vbox vPaned PackGrow 0

    panedPack1 vPaned scrolledTextView True True


    storeSource <- listStoreNew
        [
            Error {line=1, message="World"},
            Error {line=11, message="Bee, you ate the flowers!"},
            Error {line=20, message="You ate the flowers!"},
            Error {line=5, message="carl, you ate the flowers!"},
            Error 2 "abcd"
        ]

    addListBoxToWindow window vPaned storeSource [("Line #", DataExtractor line), ("message", DataExtractor message)]


----------------------------------

    statusBarBox <- hBoxNew False 0
    boxPackStart vbox statusBarBox PackNatural 0



    statusBar <- statusbarNew
    statusBarButton <- buttonNewWithLabel "qqqq"
    validImage <- imageNewFromFile "redBall.png"
    --validImage <- imageNewFromStock stockYes IconSizeMenu

    boxPackStart statusBarBox statusBarButton PackNatural 0
    boxPackEnd statusBarBox validImage PackNatural 0

    label <- labelNew (Just "qqqq")

    boxPackStart statusBarBox label PackNatural 0

    id <- statusbarGetContextId statusBar "qqqq"

    statusbarSetHasResizeGrip statusBar True

    statusbarPush statusBar id "qqqq"



----------------

    onDestroy window mainQuit
    widgetShowAll window

    Rectangle _ _ width height <- widgetGetAllocation vPaned

    panedSetPosition vPaned width

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
        maybeFileName <- openOpenFileDialog FileChooserActionOpen window
        case maybeFileName of
            Just fileName -> do
                loadBuffer fileName tv window
                windowSetTitle window fileName
            Nothing -> return ()

{--chooseFile::IO String
chooseFile =
    do
        fileChooser <- fileChooserNew--}

showAboutDialog::IO()
showAboutDialog = do
    aboutDialog <- aboutDialogNew

    set aboutDialog [
            aboutDialogProgramName := "Parser Editor",
            aboutDialogVersion := "Alpha",
            aboutDialogAuthors := ["Jamshid Hormuzdiar"]
            --aboutDialogName :: AboutDialogClass self => Attr self StringSource
            --aboutDialogCopyright :: AboutDialogClass self => Attr self StringSource
            --aboutDialogComments :: AboutDialogClass self => Attr self StringSource
            --aboutDialogLicense :: AboutDialogClass self => Attr self (Maybe String)Source
            --aboutDialogWebsite :: AboutDialogClass self => Attr self StringSource
            --aboutDialogWebsiteLabel :: AboutDialogClass self => Attr self StringSource
            --aboutDialogDocumenters :: AboutDialogClass self => Attr self [String]Source
            --aboutDialogArtists :: AboutDialogClass self => Attr self [String]Source
            --aboutDialogTranslatorCredits :: AboutDialogClass self => Attr self StringSource
            --aboutDialogLogo :: AboutDialogClass self => ReadWriteAttr self Pixbuf (Maybe Pixbuf)Source
            --aboutDialogLogoIconName :: AboutDialogClass self => ReadWriteAttr self String (Maybe String)Source
            --aboutDialogWrapLicense :: AboutDialogClass self => Attr self BoolSource
       ]

    dialogRun aboutDialog
    widgetHide aboutDialog

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




